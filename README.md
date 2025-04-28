# REACT GE/LC Data Wrangling

## Overview

Wrangled react GE/LC data is saved in the REACT enclave, as a list object:

```
E:/shared_working_folder/saved_objects/ge_lc/ge_lc_wrangled.rds
```

Rather than creating one huge 'master' data frame, the [individual data sources](#data-sources) are saved as named elements in the list object, and can be joined as required with the `subject_id` variable – a unique participant identifier.

Any time a new export of the data is created (by running 00_data_extract_sql_clean.R), the old version is moved to an archive folder (```E:/shared_working_folder/saved_objects/ge_lc/archive```), and a log of the new export is recorded in ```E:/shared_working_folder/saved_objects/ge_lc/ge_lc_wrangled_versions.csv```.

**Note:** All cross-dataset joining should be done using `subject_id`. This is a unique identifier. Other identifiers (e.g. `u_password`) may have different values for the same individual if they attended baseline clinic twice in error (although instances like this should have been de-duped).

**Note:** In the GE clinical data, there are multiple versions of height, weight, waist and grip strength. These include `_value`, `_integervalue` and `_decimal value` versions. The reason is that GE switched the way they recorded data half way through. So we either have integer+decimal values (which need to be combined), or we have a standard value. Hence the `ifelse` logic in the clinic wrangling function. If one is `NA`, the other should be present.


### Data sources

Data in the list are:

- **registration_data:** Short survey that all participants filled out on registration. Contains COVID symptom information (used for deriving long COVID designation), plus age group and sex. N = 13,454.
- **clinic_data:**
  - **clinical_tests:** Physical in-clinic health tests e.g. sit to stand, grip test, FEV, etc.
    - T0 = baseline (N = 10,790)
    - T1 = follow-up (N = 2,325)
  - **assay:** Blood data (BP, HDL, LDL, etc).
    - T0 = baseline (N = 10,790)
    - T1 = follow-up (N = 2,120)
  - **illumina:** Illumina metadata for each participant.
    - T0 = baseline (N = 10,997)
    - T1 = follow-up (N = 2,120)
- **health_survey:** Extensive questionnaire completed by participants at or before clinical visit.
  - **Note:** for various reasons, a large number of people did not complete the health survey.
  - T0 = baseline (N = 10,066)
  - T1 = follow-up (N = 2,075)
  - `data_dictionary`: as described.
- **original_react_data:** Original survey data from REACT-1 or REACT-2, for anybody who had any data captured as part of the GE/LC study.
  - **Note:** 36 people were registered on the GE pathway who were NOT part of the REACT programme.

## Variables Created

A list of all new columns created throughout the data extraction and cleaning process, and descriptions of how each is derived.

### Registration Metadata

- `registered_under`: Source of registration ("LC" or "GE"), set based on dataset origin.
- `registration_date`: Date of registration, parsed from `last_update_date` for LC or `hvp_appointment_date_and_time` for GE.
- `when_positive`: Date participant tested positive, converted to `Date` from `when_positive` or `first_positive_test_date`.
- `symptom_onset`: Date of symptom onset, parsed from `when_suspected_positive` or `onset_date`.
- `symptomatic_anysymp`: Binary indicator ("Yes"/"No"/"Unknown") for any symptoms, mapped from `symptoms_yn` (LC) or `experienced_any_symptoms` (GE).
- `symptomatic_anysymp_2weeks`: Binary indicator for symptoms lasting >2 weeks, mapped from `symptoms_gt2w` (LC) or `experienced_long_symptom` (GE).

### Symptom Duration Features

- **`sympcount_0_2_weeks`**: Count of symptoms with duration "0-2 weeks".
- **`sympcount_2_4_weeks`**: Count of symptoms with duration "2-4 weeks".
- **`sympcount_1_3_months`**: Count of symptoms with duration "1-3 months".
- **`sympcount_3_6_months`**: Count of symptoms with duration "3-6 months".
- **`sympcount_7_plus_months`**: Count of symptoms with duration "7+ months".
- **`sympcount_3_plus_months`**: Sum of `sympcount_3_6_months` and `sympcount_7_plus_months`.
- **`sympcount_under_3_months`**: Sum of `sympcount_0_2_weeks`, `sympcount_2_4_weeks`, and `sympcount_1_3_months`.
- **`sympcount_2_plus_weeks`**: Sum of counts for symptoms lasting 2 weeks or more.
- **`sympcount_4_plus_weeks`**: Sum of counts for symptoms lasting at least 4 weeks.
- **`sympcount_anyduration`**: Total number of symptoms with any non-missing duration.
- **`onset_date_combined`**: Earliest valid date between `symptom_onset` and `when_positive`, with filtering for unrealistic dates.
- **`days_since_symptom_onset`**: Numeric difference between `registration_date` and `onset_date_combined`, with values outside [0-750] set to `NA`.

### Clinic Visit Features (from `wrangleClinicData`)

- **`mean_systolic_bp`**: Mean of three systolic blood pressure readings.
- **`mean_diastolic_bp`**: Mean of three diastolic blood pressure readings.
- **`weight_kg`**: Weight in kg, averaged across up to two measurements (integer + decimal).
- **`height_cm`**: Height in cm from measurement or decoded integer + decimal reading.
- **`waist_cm`**: Waist circumference in cm averaged across measurements.
- **`heartrate_resting_bpm`**: Resting heart rate (first measurement or average for LC).
- **`handgrip_strength_left`**: Average left hand grip strength measurement.
- **`handgrip_strength_right`**: Average right hand grip strength measurement.
- **`grip_dominant_hand`**: Dominant hand label ("Right"/"Left"), inferred from metadata.
- **`grip_strength_dominant`**: Grip strength of dominant hand, inferred if missing.
- **`grip_strength_nondominant`**: Grip strength of non-dominant hand.
- **`fevtest_1`**: FEV1 measure averaged across up to three spirometry readings.
- **`fevtest_6`**: FEV6 measure averaged across spirometry readings.
- **`fevtest_1_6_ratio`**: Ratio of FEV1 to FEV6, with invalid combinations set to `NA`.
- **`clinic_date`**: Date of clinic visit parsed from timestamp metadata.
- **`hasbloods`**: "Yes"/"No" indicating whether blood tubes were filled.
- **`sittest_stands_count`**: Count of sit-to-stand exercise reps.
- **`sittest_pre_test_heartrate`**: Heart rate before stand test.
- **`sittest_pre_test_o2_saturation`**: Oxygen saturation before test.
- **`sittest_post_test_heartrate`**: Heart rate after stand test.
- **`sittest_post_test_o2_saturation`**: Oxygen saturation after test.

### Survey Features

- **`date_of_survey`**: Date part of the survey timestamp (`datetime` field).

### Long Covid Designation

- **`lc_categorical`**: Categorical Long COVID designation ("Asymptomatic", "Non-persistent symptoms", "Long COVID"), defined by symptom persistence and counts.
- **`lc_binary`**: Binary flag (1 = Long COVID; 0 = all other categories).


## Long COVID designation

The top-level definition of long COVID is whether a person experienced one or more symptoms for 12 weeks or longer. This information is derived from the registration survey (all surveys are saved here: https://www.imperial.ac.uk/medicine/research-and-impact/groups/react-study/studies/react-long-covid/react-long-covid-materials/).

The question logic is visualised in the flow chart below:

![Alt text](https://github.com/mathzero/react_gelc_bio_cohort/blob/main/GE_LC%20long%20covid%20designation%20logic%20(2).png "Long COVID designation")

## Data Extract Script Notes

### 00_data_extract_sql_clean.R

#### Overview

This script extracts, cleans, and assembles REACT GE/LC data from an SQL database and from a combined dataframe of the original REACT-1 and REACT-2 survey and test data. It processes registration, clinic, assay, Illumina, and survey datasets, computes long COVID designations, compiles results into a structured list, saves outputs. At the end of the script (post-save) invitee demographics is generated, plus Venn/UpSet diagrams to show intersections of data availability.

#### Sections

1. **Setup**
   - `outpath`: folder for output files
   - `figpath`: folder for plot figures
   - Clear environment and load helper scripts and packages.
2. **Database Connection**
   - `con`: ODBC connection to the REACT Oracle database.
3. **Table Extraction Helper**
   - `extractTable(tabname)`: queries the given table name and returns a data frame with a message.
4. **Registration Data**
   - **GE registration** (`df_ge_reg`, `df_ge_reg_symps`)
     - cleaned and deduplicated GE registration records.
     - COVID symptoms data, cleaned, Unicode corrected, pivoted to wide format, and joined to `df_ge_reg`.
   - **LC registration** (`df_lc_reg`, `df_lc_reg_2`)
     - filtered LC registration records (`COMPLETE` status, first visit per subject).
     - follow-up LC registration records.
   - **Combined registration** (`df_reg`)
     - merged GE and LC datasets on intersecting columns, filtered uniques, and cleaned duplicate and symptom columns.
     - Additional computed variables include `registered_under`, `registration_date`, `when_positive`, `symptom_onset`, `symptomatic_anysymp`, `symptomatic_anysymp_2weeks`, symptom duration variables (harmonised via `durationHarmoniser`), symptom counts (`sympcount_0_2_weeks`, `sympcount_2_4_weeks`, …, `sympcount_anyduration`), `onset_date_combined`, `days_since_symptom_onset`.
5. **Clinic Data**
   - `df_ge_clinic`, `df_lc_clinic`: raw clinic visit data.
   - `df_lc_clinic_1`, `df_lc_clinic_2`: first and second LC visits, wrangled via `wrangleClinicData`.
   - `df_clinic_1`: merged GE and LC first visits, duplicates merged by `mergeColumns`.
6. **Assay Data**
   - `df_assay`: raw assay results.
   - `df_assay_1`: baseline assay deduplicated by missingness.
   - `df_assay_2`: follow-up assay.
7. **Illumina Metadata (genome/transcriptome)**
   - `df_illumina`: raw sample barcode data.
   - `df_illumina_1`: baseline Illumina metadata.
   - `df_illumina_2`: follow-up Illumina metadata.
8. **Surveys**
   - `df_survey`: cleaned survey responses (first per subject).
   - `df_survey_2`: recall survey data.
   - `dict_1`, `dict_2`, `dict_comb`: survey data dictionary construction and cleaning.
   - `df_survey_named`, `df_survey_2_named`: labeled survey datasets with NA recodings.
9. **React Original Data & Long COVID Designation**
   - `dfRes`: original React RDS data.
   - `dfRes_gelc`: subset for subjects in `df_reg`.
   - `lc_des`: long COVID categorical and binary designation per subject.
10. **Assemble Output**
    - `out`: list containing `registration_data`, `clinic_data`, `health_survey`, and `original_react_data`.
11. **Saving Output and Logging**
    - `save_path`, `archive_dir`, `archive_path`: saving and archiving RDS files.
    - `log_path`, `md5_hash`, `log_entry`: logging version info.
12. **Invitees Analysis**
    - `df_invite_gelc`: invitees dataset.
    - `dfRes_invite`: joined invitees and registration, with `invite_status` and `study_cat`.
    - `tab_invitees`: demographics table saved via `savePrettyExcelWorkbook`.
13. **Venn and UpSet Diagrams**
    - `venndat`: list of subject_id vectors for each data modality.
    - `upsetdat`: UpSetR data object.
    - Generated plots with `ggVennDiagram` and `UpSetR::upset`.

---
