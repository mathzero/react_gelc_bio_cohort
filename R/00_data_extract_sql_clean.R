# 00_data_extract_sql_clean.R
# Clear workspace, load packages and functions, extract and clean REACT GE/LC data

# -------------------------------------------------------------------------
# 1. Setup
# -------------------------------------------------------------------------
rm(list = ls(all = TRUE))
outpath <- file.path(getwd(), "output")
figpath <- file.path(getwd(), "plots")

# Load required packages and helper functions
source("G:/shared_working_folder/function_scripts/load_packages.R", local = TRUE)
load_packages(c("odbc", "parsedate", "janitor", "DBI", "tidyverse", "datapasta",
                "ggplot2", "purrr", "OverReact", "ggvenn", "ggVennDiagram", "UpSetR"))

source("G:/shared_working_folder/function_scripts/create_subfolder.R", local = TRUE)
source("G:/shared_working_folder/function_scripts/full_join_multiple_datasets.R", local = TRUE)
source("G:/shared_working_folder/projects/react_ge_lc_wrangle/code/00_functions.R", local = TRUE)
source("G:/shared_working_folder/function_scripts/save_styled_table.R", local = TRUE)
source("G:/shared_working_folder/projects/react_ge_lc_wrangle/code/00_bits_and_pieces.R", local = TRUE)

library(dbplyr)

# -------------------------------------------------------------------------
# 2. Database connection
# -------------------------------------------------------------------------
con <- dbConnect(
  odbc::odbc(),
  Driver  = "Oracle in instantclient_23_5",
  DBQ     = "se-enclaves-db02.sm.med.ic.ac.uk:1521/react",
  SVC     = "REACT_V",
  UID     = odbc::quote_value("MWHITAKER"),
  PWD     = odbc::quote_value("$pr1ngTim3="),
  timeout = 10
)

# -------------------------------------------------------------------------
# 3. Table extraction helper
# -------------------------------------------------------------------------
extractTable <- function(tabname) {
  df <- DBI::dbGetQuery(con, dbplyr::sql(paste0("SELECT * FROM ", tabname))) %>%
    collect()
  message(sprintf("%s: %d rows, %d cols", tabname, nrow(df), ncol(df)))
  return(df)
}

# -------------------------------------------------------------------------
# 4. Registration: GE
# -------------------------------------------------------------------------
df_ge_reg <- extractTable("MILD_COVID_V2_REACT_GE_EXPORT_V") %>%
  janitor::clean_names()

df_ge_reg <- df_ge_reg %>%
  arrange(hvp_appointment_date_and_time) %>%
  group_by(subject_id) %>%
  slice_head(n = 1) %>%
  ungroup()

df_ge_reg_symps <- extractTable("MILD_COVID_V2_REACT_GE_SYMPTOMS_LC_V") %>%
  janitor::clean_names()

# Correct janky unicode in symptom labels
df_ge_reg_symps$covid_symptom[grepl("Shortness of breat", df_ge_reg_symps$covid_symptom)] <-
  "Shortness of breath (compared with what’s normal for you)"

df_ge_reg_symps <- df_ge_reg_symps %>%
  left_join(symptoms_df, by = c("covid_symptom" = "symptom")) %>%
  pivot_wider(
    id_cols      = subject_id,
    names_from   = symptom_duration_code,
    values_from  = symptom_duration
  ) %>%
  select(-`NA`) %>%
  mutate(subject_id = as.numeric(subject_id)) %>%
  mutate(across(-subject_id, ~ na_if(., "N/A")))

df_ge_reg <- df_ge_reg %>%
  left_join(df_ge_reg_symps, by = "subject_id") %>%
  janitor::clean_names()

# -------------------------------------------------------------------------
# 5. Registration: LC
# -------------------------------------------------------------------------
df_lc_reg <- extractTable("REACT_SURVEY_LC_REGISTRATION_V") %>%
  janitor::clean_names() %>%
  filter(status == "COMPLETE", !grepl("T35T", react_id)) %>%
  arrange(last_update_date) %>%
  group_by(subject_id) %>%
  slice_head(n = 1) %>%
  ungroup()

df_lc_reg_2 <- extractTable("REACT_SURVEY_LCF_REGISTRATION_V") %>%
  janitor::clean_names() %>%
  filter(status == "COMPLETE", !grepl("T35T", react_id))

# Harmonise symptom durations
df_lc_reg <- df_lc_reg %>%
  mutate(across(
    all_of(tolower(symptoms_df$symptom_duration_code)),
    durationHarmoniser
  ))

df_ge_reg <- df_ge_reg %>%
  mutate(across(
    setdiff(tolower(symptoms_df$symptom_duration_code), c("symptoms_duration_98", "symptoms_duration_99")),
    durationHarmoniser
  ))

# Add registration metadata
df_lc_reg <- df_lc_reg %>%
  mutate(
    registered_under  = "LC",
    registration_date = as.Date(last_update_date),
    when_positive     = as.Date(when_positive),
    symptom_onset     = as.Date(when_suspected_positive),
    symptomatic_anysymp = case_when(
      symptoms_yn == 1 ~ "Yes",
      symptoms_yn == 2 ~ "No",
      TRUE             ~ "Unknown"
    ),
    symptomatic_anysymp_2weeks = case_when(
      symptoms_gt2w == 1 ~ "Yes",
      symptoms_gt2w == 2 ~ "No",
      symptoms_gt2w == 3 ~ "Unsure",
      TRUE               ~ NA_character_
    )
  )

df_ge_reg <- df_ge_reg %>%
  mutate(
    registered_under           = "GE",
    registration_date          = as.Date(hvp_appointment_date_and_time),
    when_positive              = as.Date(first_positive_test_date, "%d/%m/%Y"),
    symptom_onset              = as.Date(onset_date, "%d/%m/%Y"),
    symptomatic_anysymp        = experienced_any_symptoms,
    symptomatic_anysymp_2weeks = experienced_long_symptom
  )

# Combine GE & LC registration
regcols <- intersect(names(df_lc_reg), names(df_ge_reg))
df_reg  <- joinMyDatasets(list(df_lc_reg[, regcols], df_ge_reg[, regcols])) %>%
  filter(!is.na(subject_id)) %>%
  arrange(subject_id) %>%
  group_by(subject_id) %>%
  arrange(registration_date) %>%
  slice_head(n = 1) %>%
  ungroup()

# Clean up symptom columns
df_reg <- df_reg %>%
  select(-symptoms_duration_30) %>%
  mutate(
    symptoms_duration_28 = coalesce(symptoms_duration_28, symptoms_duration_29)
  ) %>%
  select(-symptoms_duration_29)

# Symptom count features
symp_names <- tolower(sympnames_type_df_corrected$lc_registration_names)
df_reg <- df_reg %>%
  mutate(
    sympcount_0_2_weeks     = rowSums(across(all_of(symp_names), ~ . == "0-2 weeks"), na.rm = TRUE),
    sympcount_2_4_weeks     = rowSums(across(all_of(symp_names), ~ . == "2-4 weeks"), na.rm = TRUE),
    sympcount_1_3_months    = rowSums(across(all_of(symp_names), ~ . == "1-3 months"), na.rm = TRUE),
    sympcount_3_6_months    = rowSums(across(all_of(symp_names), ~ . == "3-6 months"), na.rm = TRUE),
    sympcount_7_plus_months = rowSums(across(all_of(symp_names), ~ . == "7+ months"), na.rm = TRUE),
    sympcount_3_plus_months = sympcount_3_6_months + sympcount_7_plus_months,
    sympcount_under_3_months = sympcount_0_2_weeks + sympcount_2_4_weeks + sympcount_1_3_months,
    sympcount_2_plus_weeks   = sympcount_2_4_weeks + sympcount_1_3_months + sympcount_3_6_months + sympcount_7_plus_months,
    sympcount_4_plus_weeks   = sympcount_1_3_months + sympcount_3_6_months + sympcount_7_plus_months,
    sympcount_anyduration    = rowSums(across(all_of(symp_names), ~ !is.na(.)), na.rm = TRUE),
    onset_date_combined      = coalesce(symptom_onset, when_positive),
    onset_date_combined      = case_when(
      onset_date_combined >= registration_date ~ NA_Date_,
      onset_date_combined < as.Date("2019-01-12") ~ NA_Date_,
      TRUE ~ onset_date_combined
    ),
    days_since_symptom_onset = as.numeric(registration_date - onset_date_combined),
    days_since_symptom_onset = case_when(
      days_since_symptom_onset > 750 | days_since_symptom_onset < 0 ~ NA_real_,
      TRUE ~ days_since_symptom_onset
    )
  )

# Correct extreme dates
df_reg$when_positive[df_reg$when_positive == as.Date("2920-05-01")] <- as.Date("2020-05-01")
df_reg$when_positive[df_reg$when_positive <= as.Date("2019-12-01")]  <- NA

# -------------------------------------------------------------------------
# 6. Clinic data
# -------------------------------------------------------------------------
df_ge_clinic <- extractTable("REACT_GE_LC_MSS_RESULTS_V") %>%
  janitor::clean_names()

df_lc_clinic <- bind_rows(
  extractTable("REACT_LC_ICL_RESULTS_V"),
  extractTable("REACT_GEPLUS_ICL_RESULTS_V")
) %>%
  janitor::clean_names()

# Split LC clinic visits
df_lc_clinic_1 <- df_lc_clinic %>% filter(grepl("LCC", project_stage))
df_lc_clinic_2 <- df_lc_clinic %>% filter(!grepl("LCC", project_stage))

# Wrangle clinic visits
df_ge_clinic <- wrangleClinicData(
  clinic_data      = df_ge_clinic,
  clinic_ge_or_lc  = "ge",
  obs_no           = 1,
  keepvars         = c("subject_id", "react_id", "gcc_barcode")
) %>%
  rename(u_passcode = react_id, barcode = gcc_barcode)

df_lc_clinic_1 <- wrangleClinicData(
  clinic_data      = df_lc_clinic_1,
  clinic_ge_or_lc  = "lc",
  obs_no           = 1,
  keepvars         = c("subject_id", "react_id", "screening_end",
                       "screening_start", "screening_mins", "consent_start",
                       "consent_end", "consent_status", "form_version",
                       "pis_version", "site_name")
)

df_lc_clinic_2 <- wrangleClinicData(
  clinic_data      = df_lc_clinic_2,
  clinic_ge_or_lc  = "lc",
  obs_no           = 2,
  keepvars         = c("subject_id", "react_id", "screening_end",
                       "screening_start", "screening_mins", "consent_start",
                       "consent_end", "consent_status")
)

# Merge repeat visits for LC2
mergeColumns <- function(x) {
  if (all(is.na(x))) {
    NA
  } else if (is.numeric(x)) {
    mean(x, na.rm = TRUE)
  } else {
    tail(na.omit(x), 1)
  }
}

df_lc_clinic_2 <- df_lc_clinic_2 %>%
  group_by(subject_id) %>%
  summarise(across(everything(), mergeColumns), .groups = "drop")

# Combine GE & LC1 clinic data
df_clinic_1 <- joinMyDatasets(list(df_ge_clinic, df_lc_clinic_1)) %>%
  filter(!is.na(subject_id)) %>%
  group_by(subject_id) %>%
  summarise(across(everything(), mergeColumns), .groups = "drop")

# -------------------------------------------------------------------------
# 7. Assay data
# -------------------------------------------------------------------------
df_assay <- extractTable("GELC_CLINIC_ASSAY_RESULTS_V") %>%
  janitor::clean_names()

df_assay_1 <- df_assay %>%
  filter(!startsWith(barcode, "LCR")) %>%
  group_by(subject_id) %>%
  mutate(misscount = rowSums(is.na(cur_data()))) %>%
  arrange(-misscount, clinic_started) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(-misscount)

df_assay_2 <- df_assay %>%
  filter(startsWith(barcode, "LCR"))

# -------------------------------------------------------------------------
# 8. Illumina data
# -------------------------------------------------------------------------
system_test_passcodes <- c("V346TXDF")

df_illumina <- extractTable("REACT_GE_SAMPLE_BARCODES_V") %>%
  janitor::clean_names() %>%
  filter(passcode != "_UNKNOWN", !passcode %in% system_test_passcodes)

df_illumina_1 <- df_illumina %>% filter(project_stage != "LCR")
df_illumina_2 <- df_illumina %>% filter(project_stage == "LCR")

# -------------------------------------------------------------------------
# 9. Surveys
# -------------------------------------------------------------------------
df_survey <- extractTable("REACT_GE_LCC_IPSOS_SURVEY_V") %>%
  janitor::clean_names() %>%
  select(subject_id, everything()) %>%
  mutate(date_of_survey = as.Date(datetime))

df_survey <- df_survey %>%
  group_by(subject_id) %>%
  arrange(date_of_last_access) %>%
  slice_head(n = 1) %>%
  ungroup()

df_survey_2 <- extractTable("REACT_LCR_V") %>%
  janitor::clean_names()

# Load and tidy survey data dictionary
dict_1 <- openxlsx::read.xlsx(
  "G:/shared_working_folder/projects/react_ge_lc_wrangle/data/react-server/metadata-v1.xlsx",
  sheet = 1, startRow = 2
) %>%
  janitor::clean_names()

dict_2 <- openxlsx::read.xlsx(
  "G:/shared_working_folder/projects/react_ge_lc_wrangle/data/react-server/metadata-v1.xlsx",
  sheet = 2, startRow = 2
)

vals <- tolower(zoo::na.locf(dict_2$Value))
dict_2 <- dict_2 %>%
  mutate(Value = vals) %>%
  select(-X4) %>%
  rename(Variable = Value, Value = X2, Description = Label) %>%
  mutate(Description = if_else(Description == "quoted", "Yes",
                        if_else(Description == "not quoted", "No", Description)))

dict_comb <- full_join(dict_1, dict_2)

write_csv(dict_comb, file = file.path(getwd(), "data/survey_data_dictionary.csv"))

# Map survey responses
replaceSurveyEntries <- function(dataset, datadict) {
  vars <- unique(datadict$Variable)
  pb   <- progress::progress_bar$new(format = "Replacing [:bar] :percent", total = length(vars))
  for (var in vars) {
    map <- datadict %>%
      filter(Variable == var) %>%
      select(Value, Description) %>%
      deframe()
    if (var %in% names(dataset)) {
      dataset[[var]] <- as.character(map[as.character(dataset[[var]])])
    }
    pb$tick()
  }
  dataset
}

df_survey_named   <- replaceSurveyEntries(df_survey, dict_comb) %>%
  mutate(across(where(is.character), ~ na_if(., "-66"))) %>%
  mutate(across(where(is.numeric),   ~ na_if(., -66))) %>%
  mutate(across(where(is.numeric),   ~ na_if(., -77)))

df_survey_2_named <- replaceSurveyEntries(df_survey_2, dict_comb) %>%
  mutate(across(where(is.character), ~ na_if(., "-66"))) %>%
  mutate(across(where(is.numeric),   ~ na_if(., -66))) %>%
  mutate(across(where(is.numeric),   ~ na_if(., -77)))

# -------------------------------------------------------------------------
# 10. React original data & LC designation
# -------------------------------------------------------------------------
dfRes      <- readRDS("G:/shared_working_folder/saved_objects/react_1_react_2_combined.rds")
dfRes_gelc <- dfRes %>% filter(subject_id %in% df_reg$subject_id)

lc_des <- df_reg %>%
  left_join(dfRes_gelc, by = "subject_id") %>%
  group_by(subject_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    lc_categorical = case_when(
      symptomatic_anysymp == "No"                  ~ "Asymptomatic",
      symptomatic_anysymp_2weeks == "No"           ~ "Non-persistent symptoms",
      sympcount_3_plus_months > 0                  ~ "Long COVID",
      sympcount_anyduration > 0                    ~ "Non-persistent symptoms",
      TRUE                                         ~ "Non-persistent symptoms"
    ),
    lc_binary = if_else(lc_categorical == "Long COVID", 1, 0)
  ) %>%
  select(subject_id, lc_categorical, lc_binary)

df_reg <- df_reg %>% left_join(lc_des, by = "subject_id")

# -------------------------------------------------------------------------
# 11. Assemble output list and save
# -------------------------------------------------------------------------
out <- list(
  registration_data = df_reg,
  clinic_data = list(
    clinical_tests = list(
      clinical_tests_t0 = df_clinic_1,
      clinical_tests_t1 = df_lc_clinic_2
    ),
    assay = list(
      assay_t0 = df_assay_1,
      assay_t1 = df_assay_2
    ),
    illumina = list(
      illumina_t0 = df_illumina_1,
      illumina_t1 = df_illumina_2
    )
  ),
  health_survey = list(
    survey_t0       = df_survey_named,
    survey_t1       = df_survey_2_named,
    data_dictionary = dict_comb
  ),
  original_react_data = dfRes_gelc
)

save_path    <- "G:/shared_working_folder/saved_objects/ge_lc_wrangled.rds"
saveRDS(out, file = save_path)
archive_dir  <- file.path(dirname(save_path), "archive")
if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)
archive_path <- file.path(
  archive_dir,
  paste0("ge_lc_wrangled_", Sys.Date(), ".rds")
)
file.copy(save_path, archive_path, overwrite = TRUE)

log_path <- "G:/shared_working_folder/saved_objects/ge_lc_wrangled_versions.csv"
md5_hash <- tools::md5sum(save_path)
log_entry <- data.frame(timestamp = Sys.time(), md5 = unname(md5_hash))
write.table(log_entry, file = log_path, sep = ",",
            row.names = FALSE, col.names = !file.exists(log_path), append = TRUE)

# -------------------------------------------------------------------------
# 12. Invitees analysis
# -------------------------------------------------------------------------
df_invite_gelc <- readRDS("G:/shared_working_folder/saved_objects/ge_lc_invites.rds")
dfRes_invite   <- dfRes %>%
  filter(subject_id %in% df_invite_gelc$subject_id) %>%
  distinct(subject_id, .keep_all = TRUE) %>%
  full_join(df_reg, by = "subject_id") %>%
  mutate(
    invite_status = if_else(subject_id %in% df_reg$subject_id, "Registered", "Invited but did not register"),
    study_cat     = case_when(
      study == 1 ~ "REACT-1",
      study == 2 ~ "REACT-2",
      TRUE        ~ "Other"
    )
  )

df_reg_noninvitees <- df_reg %>%
  filter(!subject_id %in% df_invite_gelc$subject_id)
write_csv(df_reg_noninvitees[, 1:2], file = "registered_by_not_on_invite_Table.csv")

# Demographics table for invitees
rowvars           <- c("study_cat", "age", "sex", "ethnic_new",
                       "bmi_cat", "smokenow_cat", "imd_quintile_cat", "edu_cat")
rowvar_names      <- c("Original study participation", "Age", "Sex",
                       "Ethnicity", "BMI", "Smoking", "IMD", "Education")
names(rowvar_names) <- rowvars

tab_invitees <- OverReact::tableOne(
  dat                   = dfRes_invite,
  colvar                = "invite_status",
  rowvars               = rowvars,
  cov_names             = rowvar_names,
  statistical_test      = TRUE,
  formatPvalsForEpiPaper = TRUE
)
OverReact::savePrettyExcelWorkbook(
  listOfTables = list(tab_invitees = tab_invitees),
  workbookName = "invitees_demographics_table",
  outpath      = outpath
)

# -------------------------------------------------------------------------
# 13. Venn and UpSet diagrams
# -------------------------------------------------------------------------
venndat <- list(
  "Attended clinic"   = df_clinic_1$subject_id,
  "Has bloods"        = df_assay_1$subject_id,
  "Illumina results"  = df_illumina_1$subject_id[df_illumina_1$illumina_success == 1]
)

ggVennDiagram(venndat, label_alpha = 0, set_color = "black", label_color = "black") +
  scale_fill_gradient(low = "white", high = "red")

upsetdat <- UpSetR::fromList(venndat)
UpSetR::upset(upsetdat, order.by = "freq")
grid::grid.text("REACT GE/LC baseline data", x = 0.7, y = 0.97, gp = grid::gpar(fontsize = 15))
