#' First clear the environment of variables
rm(list=ls(all=TRUE))

#setwd("G:/Group/")
outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")

#' Pull in functions needed

#' Pull in packages needed
source("G:/shared_working_folder/function_scripts/create_subfolder.R", local = T)
source("G:/shared_working_folder/function_scripts/full_join_multiple_datasets.R", local = T)
source("G:/shared_working_folder/projects/react_ge_lc_wrangle/code/00_functions.R", local = T)
source("G:/shared_working_folder/function_scripts/save_styled_table.R", local = T)
source("G:/shared_working_folder/function_scripts/load_packages.R", local = T)
source("G:/shared_working_folder/projects/react_ge_lc_wrangle/code/00_bits_and_pieces.R", local = T)
source("G:/shared_working_folder/projects/react_ge_lc_wrangle/code/00_functions.R", local = T)
source("G:/shared_working_folder/projects/react_ge_lc_wrangle/code/00_bits_and_pieces.R", local = T)

# load packages
load_packages(package.list = c("odbc","parsedate","janitor","DBI","tidyverse","lemon","patchwork","lsa",
                               "purrr","OverReact","prevalence","openxlsx","extrafont","paletteer",
                               "apcluster","ape","dendextend","cluster","circlize","ComplexHeatmap",
                               "patchwork", "OverReact","sharp","wesanderson"))


#read in data
df_list <- readRDS("G:/shared_working_folder/saved_objects/ge_lc_wrangled.rds")

# get dfs from list
df_reg=df_list$registration_data
dfRes=df_list$original_react_data %>% 
  group_by(subject_id) %>% 
  arrange(date) %>% 
  slice_tail(n=1) %>% 
  ungroup()
df_clinic=df_list$clinic_data$clinical_tests$clinical_tests_t0
df_clinic_t1=df_list$clinic_data$clinical_tests$clinical_tests_t1
df_assay=df_list$clinic_data$assay$assay_t0
df_assay_t1=df_list$clinic_data$assay$assay_t1
df_illumina=df_list$clinic_data$illumina$illumina_t0

# create subfolder
createMySubfolder("eda")


# Master table 1 ----------------------------------------------------------

### join registration data
df_master <- df_reg %>% left_join(dfRes)

# REACT-1 participants
df_master <- df_master %>% 
  mutate(study_cat=case_when(study==1 ~ "REACT-1",
                             study==2 ~ "REACT-2",
                             T~ "Separate recruitment"))
df_master$bmi_cat %>% table()

# Attended clinic
df_master <- df_master %>% 
  mutate(
    
    attended_baseline_clinic=case_when(subject_id%in%df_clinic$subject_id ~ "Yes",
                         T ~ "No"),
    attended_followup_clinic=case_when(subject_id%in%df_clinic_t1$subject_id ~ "Yes",
                                       T ~ "No"),
    
    # bloods
    has_bloods=case_when(subject_id%in%df_assay$subject_id ~ "Yes",
                         T ~ "No"),
   
    has_bloods_t1=case_when(subject_id%in%df_assay_t1$subject_id ~ "Yes",
                                          T ~ "No")
  )


setdiff(df_clinic$subject_id,df_reg$subject_id) # 3 people are in clinic but not in registration 303634863 303760852 303878408

# LC levels
df_master <- df_master %>% mutate(lc_categorical =
                                    factor(lc_categorical,
                                              levels=c("Unknown",
                                                       "Asymptomatic",
                                                       "Non-persistent symptoms",
                                                       "Long COVID")))



# define colvars
colvar="lc_categorical"

# define rowvars
rowvars=c("study_cat","attended_baseline_clinic","attended_followup_clinic","has_bloods","has_bloods_t1",
          "age","sex","ethnic_new","bmi_cat","smokenow_cat","imd_quintile_cat","edu_cat")
rowvar_names=c("Study","Attended clinic at baseline","Attended clinic at follow-up",
               "Succesful clinical blood draw at baseline",
               "Succesful clinical blood draw at follow-up",
               "Age","Sex","Ethnicity","BMI","Smoking","IMD","Education")
rowvar_names_list=as.list(rowvar_names)
names(rowvar_names_list) <-rowvars 

# table one
tab1 <- OverReact::tableOne(dat = df_master,rowvars = rowvars,
                            colvar = colvar,include_percentages = T,
                            cov_names = rowvar_names_list,
                            rowwise_precentages = T,confint = F,
                            includeNAsRowvar = F,statistical_test = T,
                            formatPvalsForEpiPaper = T
                              )
tab1


### Table one clinical only

# table one
tab1clinic <- OverReact::tableOne(dat = df_master %>% filter(subject_id%in%df_assay$subject_id),
                                  rowvars = rowvars,
                            colvar = colvar,include_percentages = T,
                            cov_names = rowvar_names_list,
                            rowwise_precentages = T,confint = F,
                            includeNAsRowvar = F,statistical_test = T,
                            formatPvalsForEpiPaper = T
)
tab1clinic



### Table one clinic two only only

# table one
tab1clinic2 <- OverReact::tableOne(dat = df_master %>% filter(subject_id%in%df_assay_t1$subject_id),
                                  rowvars = rowvars,
                                  colvar = colvar,include_percentages = T,
                                  cov_names = rowvar_names_list,
                                  rowwise_precentages = T,confint = F,
                                  includeNAsRowvar = F,statistical_test = T,
                                  formatPvalsForEpiPaper = T
)
tab1clinic2


OverReact::savePrettyExcelWorkbook(listOfTables = list(tab1_registrants=tab1,
                                            tab1_clinic_attendees=tab1clinic,
                                            tab1_clinic_follow_up=tab1clinic2),
                        workbookName = "table_one_ge_lc",outpath = outpath)



# Clinical data tables ----------------------------------------------------



rowvars_clinical <- c("height_cm", "weight_kg", "waist_cm",
                      "mean_systolic_bp", "mean_diastolic_bp", "heartrate_resting_bpm", 
                      "grip_strength_dominant", "grip_strength_nondominant", 
                      "fevtest_1", "fevtest_6", "fevtest_1_6_ratio", 
                       "sittest_stands_count", "sittest_pre_test_heartrate", "sittest_post_test_heartrate", 
                      "sittest_pre_test_o2_saturation", 
                      "sittest_post_test_o2_saturation")

rowvar_names_clincal=c("Height (cm)", "Weight (kg)", "Waist measurement (cm)",
                       "Mean systolic BP", "Mean diastolic BP", "Resting heart rate (BPM)", 
                       
                       "Grip strength (dominant hand)", 
                       "Grip strength (non-dominant hand)",  "FEV 1", "FEV 6", "FEV 1:6 ratio", 
                       "Sit-stand count", "Sit-stand pre-test heart rate", "Sit-stand post-test heart rate", 
                       "Sit-stand pre-test 02 saturation", 
                       "Sit-stand post-test 02 saturation")
rowvar_names_list_clinical=as.list(rowvar_names_clincal)
names(rowvar_names_list_clinical) <- rowvars_clinical

table_clinical <- OverReact::tableOne(dat = df_clinic %>% 
                                        # filter(subject_id%in%df_assay$subject_id) %>% 
                                        left_join(df_master %>% select(- u_passcode, -react_id)),
                                      rowvars = rowvars_clinical,
                                      colvar = colvar,
                                      include_percentages = T,
                                      cov_names = rowvar_names_list_clinical,
                                      rowwise_precentages = T,confint = F,includeNAsColvar = F,
                                      includeNAsRowvar = F,statistical_test = T,
                                      formatPvalsForEpiPaper = T)



table_clinical_t1 <- OverReact::tableOne(dat = df_clinic_t1 %>% 
                                        # filter(subject_id%in%df_assay$subject_id) %>% 
                                        left_join(df_master %>% select(- u_passcode, -react_id)),
                                      rowvars = rowvars_clinical,
                                      colvar = colvar,
                                      include_percentages = T,
                                      cov_names = rowvar_names_list_clinical,
                                      rowwise_precentages = T,confint = F,includeNAsColvar = F,
                                      includeNAsRowvar = F,statistical_test = T,
                                      formatPvalsForEpiPaper = T)


OverReact::savePrettyExcelWorkbook(listOfTables = list(table_clinical_t0=table_clinical,
                                                       table_clinical_t1=table_clinical_t1),
                                   workbookName = "table_one_clinical_ge_lc",outpath = outpath)


# Clinical data -----------------------------------------------------------


# join 
df_clinic_two_obs <- df_clinic %>%
  mutate(timepoint="Baseline") %>% 
  filter(subject_id%in%df_clinic_t1$subject_id) %>% 
  select(-"u_passcode",-"barcode",-"form_version",-"pis_version",-"site_name")%>% 
  rbind(df_clinic_t1%>%
          mutate(timepoint="Follow-up \n(approx 6-12 months after baseline)")) %>% 
  left_join(df_master %>% select(subject_id,lc_categorical))


# create plots
makePlot <- function(df=df_clinic_two_obs,var="sittest_stands_count",varname="Grip strength (dominant hand)"){
  
  min_y=min(df[[var]], na.rm=T)
  max_y=max(df[[var]], na.rm=T)
  
  max_y=1.2*max_y
  min_y=0.9*min_y
  
  
  p=df %>% 
    ggplot(aes(x=timepoint,y=!!sym(var), col=lc_categorical)) +
    # geom_line(aes(group=subject_id,col=lc_categorical), alpha=0.1,col="grey") +
    # geom_point(aes(group=subject_id,col=lc_categorical),alpha=0.3,size=0.5) +
    geom_boxplot(aes(col=lc_categorical))+
    OverReact::theme_mw() +
    # facet_wrap(.~lc_categorical,ncol=1)+
      ylim(c(min_y,max_y))+
    scale_colour_manual(values = c( "#FCBBA1", "#FC9272",  "#EF3B2C", "#CB181D" , "#67000D")) +
    labs(col="Long COVID status", y=varname, x="",
         title=varname)
  p
  return(p)
}

plts_list=list()
for(i in 1:length(rowvars_clinical)){
  plts_list[[i]] <- makePlot(df_clinic_two_obs,var =rowvars_clinical[[i]],varname = rowvar_names_clincal[[i]] )
  OverReact::saveREACTplot(p = plts_list[[i]],figpath = figpath,filename = paste0("clinical_dist_to_t1",rowvars_clinical[[i]]),
                           width = 6,height = 8,savePDF = T,filetypes = "png")
}
pwrap=wrap_plots(plts_list, guides = "collect")
OverReact::saveREACTplot(p = pwrap,figpath = figpath,filename = paste0("clinical_dist_to_t1","panel"),
                         width = 18,height = 12,savePDF = T,filetypes = "png")




# Symptom duration --------------------------------------------------------


options(scipen = 999)
plotAgeSymptomDistrib <- function(dat,title = "Distribution of symptom duration by age",
                                  subtitlesuffix=NULL,xnudge=1, position = "stack", age_var="age"){
  
  dat$myvar = pull(dat,age_var)
  p_age_symp_dur <- dat %>% 
    mutate(any_symptom_max_with_asymp=factor(case_when(symptomatic_anysymp == "No" ~ "Asymptomatic",
                                                       symptomatic_anysymp_2weeks=="No" ~ "0-2 weeks",
                                                       symptomatic_anysymp_2weeks=="Don't know/I can't remember" ~ "Unknown",
                                                       symptomatic_anysymp_2weeks=="Unsure as have symptoms but not yet for two weeks" ~ "Unknown",
                                                       sympcount_7_plus_months >0 ~"7+ months",
                                                       sympcount_3_6_months> 0 ~"3-6 months",
                                                       sympcount_1_3_months> 0 ~"1-3 months",
                                                       sympcount_2_4_weeks >0 ~"2-4 weeks",
                                                       sympcount_0_2_weeks >0 ~"0-2 weeks",
                                                       TRUE~ "Unknown"), 
                                             levels = c("Unknown",
                                                        "Asymptomatic",
                                                        "0-2 weeks",
                                                        "2-4 weeks","1-3 months",
                                                        "3-6 months","7+ months"
                                             ))) %>% 
    filter(!is.na(any_symptom_max_with_asymp), !is.na(myvar)) %>% 
    ggplot(aes(x=ifelse(test = sex == "Male", 1, -1), 
               y= factor(myvar))) + 
    geom_bar(stat="identity",  aes(),colour ="white", position = position,size=0.2) +
    geom_bar(stat = "identity",aes(fill =any_symptom_max_with_asymp,
                                   group =any_symptom_max_with_asymp), position = position) +
    OverReact::theme_mw() +
    {if(age_var=="age")
      scale_y_discrete(breaks=factor(18:91), limits=as.character(c(18:91)))}+
    scale_x_symmetric(labels = abs) +
    scale_fill_brewer(palette = "Reds") +
    geom_vline(xintercept = 0,  col="white", size=0.2) +
    annotate("text", x= xnudge, y = 0, vjust=0,hjust=0,size=6, fontface="bold", col="grey80",
             label = "Men")  +
    annotate("text", x= -xnudge, y = 0, vjust=0,size=6, hjust=1,fontface="bold", col="grey80",
             label = "Women")  +
    labs(x = "N", y= "Age", fill = "Symptom \nduration", title = title,
         subtitle=paste0("In ",nrow(dat), " ",subtitlesuffix)) +
    theme(axis.text.y = element_text(size=6))
  return(p_age_symp_dur)
}

# get plots
p_age_symp_dur=plotAgeSymptomDistrib(dat = df_master,subtitlesuffix = "registered participants", xnudge=130)
p_age_symp_dur
OverReact::saveREACTplot(p = p_age_symp_dur,figpath = figpath,filename = "sx_duration_by_age_registrants",
                         width = 7,height = 7,savePDF = T,filetypes = "png")

# get plots in clinic attendees
p_age_symp_dur_clinic=plotAgeSymptomDistrib(dat = df_master%>% filter(subject_id%in%df_assay$subject_id),
                                            subtitlesuffix = "participants who attended clinic at baseline", xnudge=130)
p_age_symp_dur_clinic

OverReact::saveREACTplot(p = p_age_symp_dur_clinic,figpath = figpath,filename = "sx_duration_by_age_clinic_t0",
                         width = 7,height = 7,savePDF = T,filetypes = "png")
# get plots in follow-up clinic attendees
p_age_symp_dur_clinic_t1=plotAgeSymptomDistrib(dat = df_master%>% filter(subject_id%in%df_assay_t1$subject_id),
                                            subtitlesuffix = "participants who attended follow-up clinic", xnudge=30)
p_age_symp_dur_clinic_t1

OverReact::saveREACTplot(p = p_age_symp_dur_clinic_t1,figpath = figpath,filename = "sx_duration_by_age_clinic_t1",
                         width = 7,height = 7,savePDF = T,filetypes = "png")



# Time of infection -------------------------------------------------------

#' Note a large proportion of asymptomatic infections were discovered from +ve Ab tests in REACT 2, hence no date of 
#' previous infection
p_infection=df_master %>% 
  mutate(lc_categorical=factor(lc_categorical,
                               levels=c("Unknown","Asymptomatic", "Non-persistent symptoms lasting less than 2 weeks", 
                                         "Non-persistent symptoms lasting more than 2 weeks", "Long COVID"
                                        ))) %>% 
  ggplot(aes(x=date_prior_infection,fill=(lc_categorical)))+
  geom_histogram()+
  OverReact::theme_mw() +
  labs(x="Date of infection",y="N",fill="Symptom status") +
  scale_fill_brewer(palette = "Reds")
  


OverReact::saveREACTplot(p = p_infection,figpath = figpath,filename = "date_of_infection_plot",
                           width = 8,height = 5,savePDF = T,filetypes = "png")
  
  
#' positive test
p_positive=df_master %>% 
  mutate(lc_categorical=factor(lc_categorical,
                               levels=c("Unknown","Asymptomatic", "Non-persistent symptoms lasting less than 2 weeks", 
                                        "Non-persistent symptoms lasting more than 2 weeks", "Long COVID"
                               ))) %>% 
  ggplot(aes(x=when_positive,fill=(lc_categorical)))+
  geom_histogram()+
  OverReact::theme_mw() +
  labs(x="Date of positive test",y="N",fill="Symptom status") +
  scale_fill_brewer(palette = "Reds")
  
OverReact::saveREACTplot(p = p_positive,figpath = figpath,filename = "date_of_pos_test_plot",
                         width = 8,height = 5,savePDF = T,filetypes = "png")




# Symptom duration --------------------------------------------------------

# change symptom name to lower
sympnames_type_df_corrected$symptom_duration_code <- tolower(sympnames_type_df_corrected$lc_registration_names)

# make plot
p_sx=df_master %>% 
  filter(lc_categorical%in%c("Asymptomatic",
                             "Non-persistent symptoms lasting less than 2 weeks",
                             "Non-persistent symptoms lasting more than 2 weeks", 
                             "Long COVID")) %>% 
  mutate(nobs=nrow(.)) %>% 
  select(c(tolower(sympnames_type_df_corrected$lc_registration_names),nobs)) %>% 
  pivot_longer(cols = -nobs) %>% 
  group_by(nobs,name,value) %>% 
  summarise(n=n()) %>% 
  left_join(sympnames_type_df_corrected,by=c("name"="symptom_duration_code")) %>% 
  mutate(value=factor(value,levels = c("0-2 weeks", "2-4 weeks", 
                                     "1-3 months", "3-6 months", "7+ months")),
         percentage = 100*n/nobs) %>% 
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  mutate(ordering=percentage[value=="7+ months"]) %>% 
  ungroup() %>% 
  ggplot(aes(x=percentage,y=reorder(symptoms_desc,ordering),fill=value))+
  geom_bar(position="stack", stat="identity", col = "white",size=0.2) +
  OverReact::theme_mw(legend.position = c(1,0),legend.justification =c(1,0) ) +
  scale_fill_brewer(palette = "Reds") +
  labs(fill="Symptom \nduration",y="",x="% of respondents")
p_sx

OverReact::saveREACTplot(p = p_sx,figpath = figpath,filename = "symptom_duration_plot",
                         width = 8,height = 7,savePDF = T,filetypes = "png")



# make plot
p_sx_lc=df_master %>% 
  filter(lc_categorical%in%c("Non-persistent symptoms lasting more than 2 weeks", 
                             "Long COVID")) %>% 
  mutate(nobs=nrow(.)) %>% 
  select(c(tolower(sympnames_type_df_corrected$lc_registration_names),nobs)) %>% 
  pivot_longer(cols = -nobs) %>% 
  group_by(nobs,name,value) %>% 
  summarise(n=n()) %>% 
  left_join(sympnames_type_df_corrected,by=c("name"="symptom_duration_code")) %>% 
  mutate(value=factor(value,levels = c("0-2 weeks", "2-4 weeks", 
                                       "1-3 months", "3-6 months", "7+ months")),
         percentage = 100*n/nobs) %>% 
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  mutate(ordering=percentage[value=="7+ months"]) %>% 
  ungroup() %>% 
  ggplot(aes(x=percentage,y=reorder(symptoms_desc,ordering),fill=value))+
  geom_bar(position="stack", stat="identity", col = "white",size=0.2) +
  OverReact::theme_mw(legend.position = c(1,0),legend.justification =c(1,0)) +
  scale_fill_brewer(palette = "Reds") +
  labs(fill="Symptom \nduration",y="",x="% of respondents who reported persistent symptoms")
p_sx_lc

OverReact::saveREACTplot(p = p_sx_lc,figpath = figpath,filename = "symptom_duration_plot_lc",
                         width = 8,height = 7,savePDF = T,filetypes = "png")



# plot for those who attended follow up

# make plot
p_sx_fu=df_master %>% 
  filter(lc_categorical%in%c("Asymptomatic",
                             "Non-persistent symptoms lasting less than 2 weeks",
                             "Non-persistent symptoms lasting more than 2 weeks", 
                             "Long COVID"),
         subject_id%in%df_assay_t1$subject_id) %>% 
  mutate(nobs=nrow(.)) %>% 
  select(c(tolower(sympnames_type_df_corrected$symptom_duration_code),nobs)) %>% 
  pivot_longer(cols = -nobs) %>% 
  group_by(nobs,name,value) %>% 
  summarise(n=n()) %>% 
  left_join(sympnames_type_df_corrected,by=c("name"="symptom_duration_code")) %>% 
  mutate(value=factor(value,levels = c("0-2 weeks", "2-4 weeks", 
                                       "1-3 months", "3-6 months", "7+ months")),
         percentage = 100*n/nobs) %>% 
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  mutate(ordering=percentage[value=="7+ months"]) %>% 
  ungroup() %>% 
  ggplot(aes(x=percentage,y=reorder(symptoms_desc,ordering),fill=value))+
  geom_bar(position="stack", stat="identity", col = "white",size=0.2) +
  OverReact::theme_mw(legend.position = c(1,0),legend.justification =c(1,0) ) +
  scale_fill_brewer(palette = "Reds") +
  labs(fill="Symptom \nduration",y="",x="% of 2,075 respondents who attended clinic twice")
p_sx_fu

OverReact::saveREACTplot(p = p_sx_fu,figpath = figpath,filename = "symptom_duration_plot_follow_up",
                         width = 8,height = 7,savePDF = T,filetypes = "png")





# Symptom clustering ------------------------------------------------------
sympnames_type_df_corrected <- sympnames_type_df_corrected[1:27,]

binaryConverter <- function(x, posvals=c("3-6 months","7+ months")){
  out=case_when(x%in% posvals~ 1,
                T ~ 0)
  return(out)
}

continuousConverter <- function(x){
  out=case_when(x=="7+ months" ~ 250,
                x=="3-6 months" ~ 126,
                x=="1-3 months" ~ 42,
                x=="2-4 weeks" ~ 21,
                x=="0-2 weeks" ~ 7,
                T ~ 0)
  return(out)
}








lc_matrix=df_reg %>% 
  filter(lc_binary==1) %>%
  select(starts_with("symptoms_dura")) %>% 
  mutate_if(is.character,  binaryConverter) %>% as.data.frame()
class(lc_matrix)
colnames(lc_matrix) <- sympnames_type_df_corrected$symptoms_desc_short
lc_matrix <- lc_matrix[,1:27] # get rid of the pesky variable

# get jaccard distance
dist_jacc=proxy::dist(lc_matrix,by_rows = F,method = "Jaccard")
# dist_cos=lsa::cosine(x = as.matrix(lc_matrix))
# dist_cos=as.matrix(lc_matrix)/sqrt(rowSums(as.matrix(lc_matrix)*as.matrix(lc_matrix)))


# consensus clustering with sharp
sharp_clus=sharp::Clustering(xdata =dist_jacc,nc = 2:13,seed = 123,K = 1000,linkage = "ward.D2")

# calibration plot
png(filename = paste0(figpath,"calibration_plot_clusters_stab.png"),width = 7,height = 8,units = "in",res = 300)
# par(mar=c(18,2,2,2))
sharp::CalibrationPlot(sharp_clus)
dev.off()
# 
# par(mar=c(18,18,2,2))
# plot(sharp_clus)


# infer structure from 
co_membership_avg=apply(sharp_clus$coprop,c(1,2),mean)
distance_matrix=1-sharp::ConsensusMatrix(sharp_clus)
dist_stab=as.dist(distance_matrix)


# run hierarchical clustering
set.seed(123)
clust=hclust(dist_stab,method = sharp_clus$methods$linkage)

# get optimal K
k = length(unique(sharp::Clusters(sharp_clus)))



# assign data key
clust_assignments=cutree(tree =clust,k = k)
sympnames_type_df_corrected$cluster=clust_assignments
mycols=c( "#9986A5","#0B775E","#B40F20","#39312F","#DD8D29","#5BBCD6","#8D8680","#E2D200","#5B1A18","#FAD510","#35274A","#0B775E")

mycols_filt=mycols[1:k]

# dend plot
dend <- as.dendrogram(clust)
current_order=order.dendrogram(dend)
current_order_letters=sympnames_type_df_corrected$cluster[1:27][rev(current_order)]
mycols_filt_ordered=mycols_filt[rank(unique(current_order_letters))]
legend_names=rev(sort(unique(sympnames_type_df_corrected[1:27,]$cluster[current_order])))

# legend_names=paste0("Cluster ",rev(toupper(letters)[1:k]),": ",unique(data_key$cluster_name[current_order]))
dend_coloured <- dend %>% 
  color_branches(k = k,col=rev(mycols_filt_ordered)) %>% 
  color_labels(k = k,col=rev(mycols_filt_ordered))


png(filename = paste0(figpath,"dendrogram.png"),width = 14,height = 10,units = "in",res = 300)

par(mar=c(10,2,10,20))
par(xpd=T)
plot(dend_coloured,horiz=T, xlab="Distance",cex=0.8)
abline(v =clust$height[length(clust$height)- (k-1)], col="black", lty=2, lwd=1)
legend("topleft",
       legend=rev(legend_names),
       fill=mycols_filt,
       border="white",
       bty="n", cex=0.8,
       inset = c(0.2,-0.25)
)
dev.off()




# Heatmap -----------------------------------------------------------------

descs <- sympnames_type_df_corrected$symptoms_desc_short

freq_hesitant <- lc_matrix %>% 
  pivot_longer(colnames(lc_matrix)) %>% 
  group_by(name) %>% 
  summarise(n=n(),
            n_pos=sum(value,na.rm=T),
            mean_pos=mean(value,na.rm=T)) 


# get rowwise total
sums_df=data.frame(desc=descs,total=colSums(lc_matrix,na.rm = T))
sums_df$perc=round(100*sums_df$total/nrow(lc_matrix),1)

# Get correlation
cormat_ref=lc_matrix %>% select_if(is.numeric) %>% cor(use = "pairwise.complete.obs")
colnames(cormat_ref) <- rownames(cormat_ref) <- descs

# plot heatmap
myCols=c("#002147", "#D4EFFC")
col_fun=colorRamp2(breaks = c(-0.25,0,0.2,0.4), colors = c("#DD2501","white",myCols[[2]],myCols[[1]]))



### Create heatmap
my.complex.heatmap.hes.cor <- ComplexHeatmap::Heatmap(cormat_ref,
                                      name = "Correlation",
                                      column_title = ("Persistent symptoms in LC/GE cohort"),
                                      
                                      row_gap = unit(2.5,"mm"),
                                      left_annotation = rowAnnotation(
                                        Total =anno_barplot(sums_df$total, gp=gpar(fill=myCols[[1]]),border=F,
                                                            axis_param = list(direction = "reverse"))),
                                      top_annotation = columnAnnotation(
                                        '%' =anno_barplot(sums_df$perc, gp=gpar(fill=myCols[[1]]),border=F
                                                          # axis_param = list(direction = "reverse"))
                                        )),
                                      # column_order = colnames(result_df)[c(2:ncol(result_df))],
                                      # row_order = roworder,
                                      # row_split =k,
                                      # column_split = k,
                                      row_split=sympnames_type_df_corrected$cluster,
                                      column_split=sympnames_type_df_corrected$cluster,
                                      
                                      row_names_gp=gpar(fontsize=9),
                                      column_names_gp=gpar(fontsize=9),
                                      column_title_gp=gpar(fontsize=12,fontface="bold"),
                                      # cluster_row_slices = T,
                                      # cluster_rows=clust,
                                      # cluster_columns =clust,
                                      
                                      show_row_dend = T,
                                      show_column_dend = F,
                                      row_dend_width = unit(50,"mm"),
                                      
                                      heatmap_legend_param = list(at=c(-0.25,0,0.25,0.5), col_fun = col_fun),
                                      
                                      # column_split = t0_cluster_cols$cluster_results$pamobject$clustering,
                                      column_gap = unit(2.5,"mm"),
                                      rect_gp = gpar(col = "white", lwd = 1),
                                      cell_fun = function(j,i,x,y,width, height, fill){
                                        grid.text(sprintf("%.2f", cormat_ref[i,j]), 
                                                  x, y, gp=gpar(fontsize=6, col = 
                                                                  ifelse(cormat_ref[i,j]>0.3,"white","black")))
                                      },
                                      col = col_fun
)

my.complex.heatmap.hes.cor

png(filename = paste0(figpath,"/cor_heatmap_clustered.png"),width = 13,height = 10.5,units = "in",res = 300)
print(my.complex.heatmap.hes.cor)
dev.off()



# Binary 7+ months --------------------------------------------------------


lc_matrix=df_reg %>% 
  filter(lc_binary==1) %>%
  select(starts_with("symptoms_dura")) %>% 
  mutate_if(is.character,  binaryConverter,posvals=c("7+ months"))

names(lc_matrix) <- sympnames_type_df_corrected$symptoms_desc_short
lc_matrix <- lc_matrix[,1:27] # get rid of the pesky variable

# get jaccard distance
dist_jacc=proxy::dist(lc_matrix,by_rows = F,method = "Jaccard")
# dist_cos=lsa::cosine(x = as.matrix(lc_matrix))
# dist_cos=as.matrix(lc_matrix)/sqrt(rowSums(as.matrix(lc_matrix)*as.matrix(lc_matrix)))


# consensus clustering with sharp
sharp_clus=sharp::Clustering(xdata =dist_jacc,nc = 2:13,seed = 123,K = 1000,linkage = "ward.D2")

# calibration plot
png(filename = paste0(figpath,"calibration_plot_clusters_stab_7_months_binary.png"),width = 7,height = 8,units = "in",res = 300)
sharp::CalibrationPlot(sharp_clus)
dev.off()



# infer structure from 
co_membership_avg=apply(sharp_clus$coprop,c(1,2),mean)
distance_matrix=1-sharp::ConsensusMatrix(sharp_clus)
dist_stab=as.dist(distance_matrix)


# run hierarchical clustering
set.seed(123)
clust=hclust(dist_stab,method = sharp_clus$methods$linkage)

# get optimal K
k = length(unique(sharp::Clusters(sharp_clus)))



# assign data key
clust_assignments=cutree(tree =clust,k = k)
sympnames_type_df_corrected$cluster=clust_assignments
mycols=c( "#9986A5","#0B775E","#B40F20","#39312F","#DD8D29","#5BBCD6","#8D8680","#E2D200","#5B1A18","#FAD510","#35274A","#0B775E")

mycols_filt=mycols[1:k]

# dend plot
dend <- as.dendrogram(clust)
current_order=order.dendrogram(dend)
current_order_letters=sympnames_type_df_corrected$cluster[1:27][rev(current_order)]
mycols_filt_ordered=mycols_filt[rank(unique(current_order_letters))]
legend_names=rev(sort(unique(sympnames_type_df_corrected[1:27,]$cluster[current_order])))

# legend_names=paste0("Cluster ",rev(toupper(letters)[1:k]),": ",unique(data_key$cluster_name[current_order]))
dend_coloured <- dend %>% 
  color_branches(k = k,col=rev(mycols_filt_ordered)) %>% 
  color_labels(k = k,col=rev(mycols_filt_ordered))


png(filename = paste0(figpath,"7_oplus_months_binary_dendrogram.png"),width = 14,height = 10,units = "in",res = 300)

par(mar=c(10,2,10,20))
par(xpd=T)
plot(dend_coloured,horiz=T, xlab="Distance",cex=0.8)
abline(v =clust$height[length(clust$height)- (k-1)], col="black", lty=2, lwd=1)
legend("topleft",
       legend=rev(legend_names),
       fill=mycols_filt,
       border="white",
       bty="n", cex=0.8,
       inset = c(0.2,-0.25)
)
dev.off()




# Continuous version ------------------------------------------------------




### Continuous version ###
lc_matrix_cont=df_reg %>%
  # filter(lc_binary==1) %>%
  select(starts_with("symptoms_dura")) %>% 
  mutate_if(is.character,  continuousConverter)


names(lc_matrix_cont) <- sympnames_type_df_corrected$symptoms_desc_short
lc_matrix_cont <- lc_matrix_cont[,1:27] # get rid of the pesky variable

# get jaccard distance
dist_jacc_cont=proxy::dist(lc_matrix_cont,by_rows = F)


# consensus clustering with sharp
sharp_clus_cont=sharp::Clustering(xdata =dist_jacc_cont,nc = 2:13,seed = 12,K = 1000,linkage="ward.D2")

# calibration plot
png(filename = paste0(figpath,"calibration_plot_clusters_stab_continuous.png"),width = 7,height = 8,units = "in",res = 300)
# par(mar=c(18,2,2,2))
sharp::CalibrationPlot(sharp_clus_cont)
dev.off()
# 
# par(mar=c(18,18,2,2))
# plot(sharp_clus)


# infer structure from 
co_membership_avg_cont=apply(sharp_clus_cont$coprop,c(1,2),mean)
distance_matrix_cont=1-sharp::ConsensusMatrix(sharp_clus_cont)
dist_stab_cont=as.dist(distance_matrix_cont)


# run hierarchical clustering
set.seed(123)
clust_cont=hclust(dist_stab_cont,method = sharp_clus_cont$methods$linkage)

# get optimal K
k_cont = length(unique(sharp::Clusters(sharp_clus_cont)))
k_cont=7


# assign data key
clust_assignments_cont=cutree(tree =clust_cont,k = k_cont)
sympnames_type_df_corrected$cluster_cont=c(clust_assignments_cont)

mycols_filt_cont=mycols[1:k_cont]

# dend plot
dend_cont <- as.dendrogram(clust_cont)
current_order_cont=order.dendrogram(dend_cont)
current_order_letters_cont=sympnames_type_df_corrected$cluster_cont[1:27][rev(current_order_cont)]
mycols_filt_ordered_cont=mycols_filt_cont[rank(unique(current_order_letters_cont))]
legend_names_cont=rev(sort(unique(sympnames_type_df_corrected$cluster_cont[current_order_cont])))

# legend_names=paste0("Cluster ",rev(toupper(letters)[1:k]),": ",unique(data_key$cluster_name[current_order]))
dend_coloured_cont <- dend_cont %>% 
  color_branches(k = k_cont,col=rev(mycols_filt_ordered_cont)) %>% 
  color_labels(k = k_cont,col=rev(mycols_filt_ordered_cont))
png(filename = paste0(figpath,"dendrogram_cont.png"),width = 14,height = 10,units = "in",res = 300)
par(mar=c(10,2,10,20))
par(xpd=T)
plot(dend_coloured_cont,horiz=T, xlab="Distance",cex=0.8)
abline(v =clust_cont$height[length(clust_cont$height)- (k_cont-1)], col="black", lty=2, lwd=1)
legend("topleft",
       legend=rev(legend_names_cont),
       fill=mycols_filt_cont,
       border="white",
       bty="n", cex=0.8,
       inset = c(0.2,-0.25)
)
dev.off()

