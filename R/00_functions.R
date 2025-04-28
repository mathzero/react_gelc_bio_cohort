## GGplot Nobs function
StatNobs <- ggproto("StatNobs",Stat,
                    compute_layer=function(data,scales,params){
                      data$n_obs=sum(!is.na(data$x) & !is.na(data$y))
                      data
                    },
                    required_aes=c("x","y")
                    )



# ggplot code to add nobs
geom_n_obs <- function(mapping=NULL,data=NULL,stat="n_obs",
                       position="identity",..., na.rm=FALSE,
                       show.legend=NA,inherit.aes=TRUE
                       ){
  layer(stat=StatNobs,data=data,mapping=mapping,geom="text",
        position=position, show.legend = show.legend,inherit.aes=inherit.aes,
        params=list(label=after_stat(n_obs),hjust=1.1,vjust=1.5,size=5,colour="black", ...)
  )
  }







# function to count NAs
count_na <- function(x){
  sum(is.na(x))
}

# Function to get duplicated IDs
getDupes <- function(x){
  out=x[duplicated(x)]
  return(out)
}


# Function to do rowMeansClean without NAs

rowMeansClean <- function(x,dp=2){
  out=rowMeans(x, na.rm = T)
  out[is.nan(out)] <- NA_real_
  
  return(round(out,digits = dp))
}


# Quick cleaning function #
outlierCleaner=function(x,range=c(50,200)){
  for_removal=(x<range[[1]] | x>range[[2]]) & !is.na(x)
  print(paste0("Replacing ", sum(for_removal)," outliers"))
  x[for_removal] <- NA_real_
  return(x)
}



# Create function to harmonise all symptom duration data
durationHarmoniser <- function(vect){
  newvect <- case_when(as.character(vect) %in% c("0-2 weeks","0 - 2 weeks","1") ~ "0-2 weeks",
                       as.character(vect) %in% c("2-4 weeks", "2 - 4 weeks","2") ~ "2-4 weeks",
                       as.character(vect) %in% c("1-2 months", "1 - 3 months","3") ~ "1-3 months",
                       as.character(vect) %in% c("3-6 months", "4 - 6 months","4") ~ "3-6 months",
                       as.character(vect) %in% c("7+ months","5") ~ "7+ months",
                       TRUE ~ NA_character_)
  return(newvect)                 
}


# define function to take non-null value or average
unifierFunc <- function(vect){
  a=vect[[1]]
  b=vect[[2]]
  
  # test for blanks / nans
  if(is.na(a) | is.nan(a) | is.null(a)){
    out=b
  }else if(is.na(b) | is.nan(b) | is.null(b)){
    out=a
  }
  
  # numeric averages
  else if(!is.na(a) & !is.na(b) & is.numeric(a) & is.numeric(b)){
    out=mean(c(a,b), na.rm=T)
  }
  
  else{
    out=a
  }
  
  return(out)
}





# Quick and dirty extractor function
extractTable <- function(tabname){
  tab_db=DBI::dbGetQuery(con,dbplyr::sql(paste0("select * from ",tabname)))
  tab <- tab_db %>% collect()
  print(paste("N obs:", dim(tab)[[1]]))
  print(paste("N cols:", dim(tab)[[2]]))
  # print(head(tab))
  return(tab)
}

integerDecimalCombiner <- function(i,d){
  return(i+d/10)
}







wrangleClinicData <-function(clinic_data, 
                             clinic_ge_or_lc = "ge",
                             obs_no = 1, 
                             add_obs_num_to_colnames=F,
                               keepvars = c("customer_policy","react_id", "customer_insurance","customer_insurancecompanytype",
                            "customer_gender","age_now","gcc_barcode","iclagreement_agreementrules",
                             "iclagreement_genomicsparticipantsignatureicl_signatureid",
                             "iclagreement_genomicsparticipantsignatureicl_date"))
  {
  
  # create new df with only useful vars
  new_df=clinic_data %>% dplyr::select(all_of(keepvars))

  
  if(clinic_ge_or_lc == "ge"){


    ### Blood pressure ###
    
    ## Systolic ##
    mean_systolic_bp_var=paste0("mean_systolic_bp")
    # GE #
    new_df[[mean_systolic_bp_var]] =rowMeansClean(clinic_data[c("physical_bloodpressure_firstreading_systolic",
                                                              "physical_bloodpressure_secondreading_systolic",
                                                              "physical_bloodpressure_thirdreading_systolic")]) 
    # check distribution
    # bp_syst_hist=new_df[[mean_systolic_bp_var]]%>% hist()
    

    ## Disystolic ##
    mean_diastolic_bp_var=paste0("mean_diastolic_bp")
    
    # GE #
    new_df[[mean_diastolic_bp_var]] =rowMeansClean(clinic_data[c("physical_bloodpressure_firstreading_diastolic",
                                                               "physical_bloodpressure_secondreading_diastolic",
                                                               "physical_bloodpressure_thirdreading_diastolic")]) 
    
    # check distribution
    # bp_diast_hist=  new_df[[mean_diastolic_bp_var]] %>% hist()
    
    
    
    ### WEIGHT ###
    
    # mutate all at the same time
    clinic_data <- clinic_data %>% mutate(across(c(physical_weight_firstmeasurementitem_integervalue,
                                                         physical_weight_secondmeasurementitem_integervalue,
                                                         physical_weight_firstmeasurementitem_value,
                                                         physical_weight_secondmeasurementitem_value),outlierCleaner,
                                                       range = c(40,200))) 
    
    
    
    ## Wieght ##
    weight_kg_var=paste0("weight_kg")
    w1=rowMeansClean(clinic_data[c("physical_weight_firstmeasurementitem_value",
                                   "physical_weight_secondmeasurementitem_value")])
    clinic_data <- clinic_data %>% mutate(physical_weight_firstmeasurementitem_value2=physical_weight_firstmeasurementitem_integervalue+physical_weight_firstmeasurementitem_decimalvalue/10,
                                          physical_weight_secondmeasurementitem_value2=physical_weight_secondmeasurementitem_integervalue+physical_weight_secondmeasurementitem_decimalvalue/10)
    w2=rowMeansClean(clinic_data[c("physical_weight_firstmeasurementitem_value2",
                                   "physical_weight_secondmeasurementitem_value2")])
    
    new_df[[weight_kg_var]]<-ifelse(is.na(w1),w2,w1)
    
    
    
    
    ### HEIGHT ###

    # remove outliers
    clinic_data <- clinic_data %>% mutate(across(c(physical_heightmeasurement_measurementvalue,
                                                   physical_heightmeasurement_measurementitem_integervalue
    ),outlierCleaner,
    range = c(50,220))) 
    
    # merge integer and decimal values
    clinic_data <- clinic_data %>% mutate(physicalheight_firstmeasurementitem_value2=
                                            physical_heightmeasurement_measurementitem_integervalue+
                                            physical_heightmeasurement_measurementitem_decimalvalue/10)
    height_cm_var=paste0("height_cm")
    
    # add new variable
    new_df[[height_cm_var]] <-ifelse(is.na(clinic_data$physical_heightmeasurement_measurementvalue),clinic_data$physicalheight_firstmeasurementitem_value2,clinic_data$physical_heightmeasurement_measurementvalue)
    
    
    
    ### WAIST ###

        
    # remove outliers
    clinic_data <- clinic_data %>% mutate(across(c(physical_waist_firstmeasurementitem_value,
                                                         physical_waist_secondmeasurementitem_value
    ),outlierCleaner,
    range = c(30,200))) 
    
    # merge integer and decimal values
    clinic_data <- clinic_data %>% mutate(physical_waist_firstmeasurementitem_value2=
                                            physical_waist_firstmeasurementitem_integervalue+
                                            physical_waist_firstmeasurementitem_decimalvalue/10)
    w1=rowMeansClean(clinic_data[c("physical_waist_firstmeasurementitem_value",
                                   "physical_waist_secondmeasurementitem_value")])
    w2=outlierCleaner(clinic_data$physical_waist_firstmeasurementitem_value2,range = c(30,200))
    
    # variable
    waist_cm_var=paste0("waist_cm")
    
    # GE #
    new_df[[waist_cm_var]] <-ifelse(is.na(w1),w2,w1)
    
    
    ### RESTING HEART RATE
    
    # variable
    heartrate_resting_bpm_var=paste0("heartrate_resting_bpm")
    # get data
    new_df[[heartrate_resting_bpm_var]] <- clinic_data$sittest_firstheartrate
    

    ### GRIP STRENGTH ###
    
    # Now we can take an average
    myvar=paste0("handgrip_strength_left")
    
    clinic_data <- clinic_data %>% 
      mutate(gripstrength_leftgripfirstmeasurement2=integerDecimalCombiner(gripstrength_leftgripfirstmeasurement_integervalue,gripstrength_leftgripfirstmeasurement_decimalvalue),
             gripstrength_leftgripsecondmeasurement2=integerDecimalCombiner(gripstrength_leftgripsecondmeasurement_integervalue,gripstrength_leftgripsecondmeasurement_decimalvalue),
             )
    
    gl1=rowMeansClean(clinic_data[c("gripstrength_leftgripfirstmeasurement",
                                    "gripstrength_leftgripsecondmeasurement")])
    gl2=rowMeansClean(clinic_data[c("gripstrength_leftgripfirstmeasurement2",
                                    "gripstrength_leftgripsecondmeasurement2")])
    
    
    # GE #
    new_df[[myvar]] <-ifelse(is.na(gl1),gl2,gl1)
    
    
    # Grip strength right
    
    myvar=paste0("handgrip_strength_right")
    
    
    clinic_data <- clinic_data %>% 
      mutate(gripstrength_rightgripfirstmeasurement2=integerDecimalCombiner(gripstrength_rightgripfirstmeasurement_integervalue,gripstrength_rightgripfirstmeasurement_decimalvalue),
             gripstrength_rightgripsecondmeasurement2=integerDecimalCombiner(gripstrength_rightgripsecondmeasurement_integervalue,gripstrength_rightgripsecondmeasurement_decimalvalue),
      )
    
    gr1=rowMeansClean(clinic_data[c("gripstrength_rightgripfirstmeasurement",
                                    "gripstrength_rightgripsecondmeasurement")])
    gr2=rowMeansClean(clinic_data[c("gripstrength_rightgripfirstmeasurement2",
                                    "gripstrength_rightgripsecondmeasurement2")])
    
    
    
    
    # GE #
    new_df[[myvar]] <- new_df[[myvar]] <-ifelse(is.na(gr1),gr2,gr1)
    
    
    ## DOMINANT HAND 
    grip_dominant_hand_var=paste0("grip_dominant_hand")
    new_df[[grip_dominant_hand_var]] <- case_when(clinic_data$gripstrength_dominanthand == "Right" ~ "Right",
                                                  clinic_data$gripstrength_dominanthand == "Left" ~ "Left",
                                                  T ~ NA_character_)
    
    
    # get dominant hand grip strength # 
    # Here we will infer the dominant hand when data is missing, 
    # by taking the stronger of the two measurements
    # mutate to get dominant hand (clean)
    myvar=paste0("grip_strength_dominant")
    
    new_df <- new_df %>% 
      mutate(grip_strength_dominant = case_when(grip_dominant_hand == "Right" ~ handgrip_strength_right,
                                                 grip_dominant_hand == "Left" ~ handgrip_strength_left,
                                                 is.na(grip_dominant_hand) ~ pmax(handgrip_strength_right,handgrip_strength_left),
                                                 T ~ NA_real_),
             grip_strength_nondominant = case_when(grip_dominant_hand == "Left" ~ handgrip_strength_right,
                                                    grip_dominant_hand == "Right" ~ handgrip_strength_left,
                                                    is.na(grip_dominant_hand) ~ pmin(handgrip_strength_right,handgrip_strength_left),
                                                    T ~ NA_real_))
    
    
    
    
    ### FEV TESTs ###
    
    ## GE ##
    myvar=paste0("fevtest_1")
    
    # FEV1 # 
    new_df[[myvar]]=rowMeansClean(clinic_data[,c("spirometry_spirometrymeasurement_firstmeasurement_fev1",
                                               "spirometry_spirometrymeasurement_secondmeasurement_fev1",
                                               "spirometry_spirometrymeasurement_thirdmeasurement_fev1")]
    )
    
    # FEV 6 #
    myvar=paste0("fevtest_6")
    new_df[[myvar]]=rowMeansClean(clinic_data[,c("spirometry_spirometrymeasurement_firstmeasurement_fev6",
                                               "spirometry_spirometrymeasurement_secondmeasurement_fev6",
                                               "spirometry_spirometrymeasurement_thirdmeasurement_fev6")])
    
    ### Now get average ratio
    myvar=paste0("fevtest_1_6_ratio")
    new_df[[myvar]]=case_when(new_df$fevtest_1 == 0 | new_df$fevtest_6 == 0 |
                                is.na(new_df$fevtest_1) | is.na(new_df$fevtest_6) ~ NA_real_,
                              T~new_df$fevtest_1/new_df$fevtest_6)
    
    
    # Add clinic date
    new_df$clinic_date = as.Date(parsedate::parse_date(substr(x = clinic_data$meta_endedat,
                                                              start = 0,stop = 10)))
    
    # bloods yes/no
    new_df$hasbloods = case_when(clinic_data$gelblood_tube_tube3 %in% c("Filled", "Partially filled") |
                                   clinic_data$gelblood_tube_tube4 %in% c("Filled", "Partially filled")  ~ "Yes",
                          TRUE ~ "No")
    
    
    
    
    ### Sit-stand tests
    myvar=paste0("sittest_stands_count")
    new_df[[myvar]]=clinic_data$sittest_standscount
    
    myvar=paste0("sittest_pre_test_heartrate")
    new_df[[myvar]]=clinic_data$sittest_firstheartrate
    
    myvar=paste0("sittest_pre_test_o2_saturation")
    new_df[[myvar]]=clinic_data$sittest_firstsaturationpercentage
    
    myvar=paste0("sittest_post_test_heartrate")
    new_df[[myvar]]=clinic_data$sittest_secondheartrate
    
    myvar=paste0("sittest_post_test_o2_saturation")
    new_df[[myvar]]=clinic_data$sittest_secondsaturationpercentage
    
    
    
    
    
  } 
  else if(clinic_ge_or_lc == "lc"){
  
    
    ### Blood pressure ###
    
    ## Systolic ##
    mean_systolic_bp_var=paste0("mean_systolic_bp")
    
    # GE #
    new_df[[mean_systolic_bp_var]] =rowMeansClean(clinic_data[c("systolic_bp1",
                                                              "systolic_bp2",
                                                              "systolic_bp3")]) 
    # check distribution
    # bp_syst_hist=new_df[[mean_systolic_bp_var]]%>% hist()
    
    
    
    ## Disystolic ##
    mean_diastolic_bp_var=paste0("mean_diastolic_bp")
    
    # LC #
    new_df[[mean_diastolic_bp_var]] =rowMeansClean(clinic_data[c("diastolic_bp1",
                                                               "diastolic_bp2",
                                                               "diastolic_bp3")])
    
    # check distribution
    # bp_diast_hist=  new_df[[mean_diastolic_bp_var]] %>% hist()
    
    
    
    ### WEIGHT ###
    clinic_data <- clinic_data %>% mutate(across(weight1:weight2, \(x) outlierCleaner(x, range = c(40,200)))) 
    
    
    
    ## Wieght ##
    weight_kg_var=paste0("weight_kg")
    new_df[[weight_kg_var]]<-rowMeansClean(clinic_data[c("weight1", "weight2")])
    
    
    
    
    
    ### HEIGHT ###
    
    # remove outliers
    clinic_data <- clinic_data %>% mutate(across(c(height1),
                                                       outlierCleaner,range = c(50,220))) 
    height_cm_var=paste0("height_cm")
    new_df[[height_cm_var]] <-clinic_data$height1
    
    
    
    ### WAIST ###
    
    # remove outliers
    clinic_data <- clinic_data %>% mutate(across(c(waist1,
                                                         waist2
    ),outlierCleaner,
    range = c(30,200))) 
    
    # variable
    waist_cm_var=paste0("waist_cm")
    
    # GE #
    new_df[[waist_cm_var]] <-rowMeansClean(clinic_data[c("waist1",
                                                       "waist2",
                                                       "waist3")])
    
    
    ### RESTING HEART RATE
    
    # variable
    heartrate_resting_bpm_var=paste0("heartrate_resting_bpm")
    # get data
    new_df[[heartrate_resting_bpm_var]] <- rowMeansClean(clinic_data[c("heart_rate_bpm1",
                                                                     "heart_rate_bpm2",
                                                                     "heart_rate_bpm3")])
    
    
    
    ### GRIP STRENGTH ###
    
    # Now we can take an average
    myvar=paste0("handgrip_strength_left")
    
    # GE #
    new_df[[myvar]] <-rowMeansClean(clinic_data[c("handgrip_strength_left1", "handgrip_strength_left2")])
    
    myvar=paste0("handgrip_strength_right")
    
    # GE #
    new_df[[myvar]] <-rowMeansClean(clinic_data[c("handgrip_strength_right1", "handgrip_strength_right2")])
    
    
    ## DOMINANT HAND 
    grip_dominant_hand_var=paste0("grip_dominant_hand")
    new_df[[grip_dominant_hand_var]] <- case_when(clinic_data$grip_dominant_hand == "Right" ~ "Right",
                                                  clinic_data$grip_dominant_hand == "Left" ~ "Left",
                                                  T ~ NA_character_)
    
    
    # get dominant hand grip strength # 
    # Here we will infer the dominant hand when data is missing, 
    # by taking the stronger of the two measurements
    # mutate to get dominant hand (clean)
    myvar=paste0("grip_strength_dominant")
    
    new_df <- new_df %>% 
      mutate(grip_strength_dominant = case_when(grip_dominant_hand == "Right" ~ handgrip_strength_right,
                                                 grip_dominant_hand == "Left" ~ handgrip_strength_left,
                                                 is.na(grip_dominant_hand) ~ pmax(handgrip_strength_right,handgrip_strength_left),
                                                 T ~ NA_real_),
             grip_strength_nondominant = case_when(grip_dominant_hand == "Left" ~ handgrip_strength_right,
                                                    grip_dominant_hand == "Right" ~ handgrip_strength_left,
                                                    is.na(grip_dominant_hand) ~ pmin(handgrip_strength_right,handgrip_strength_left),
                                                    T ~ NA_real_))
    
    
    
    
    ### FEV TESTs ###
    
    ## GE ##
    myvar=paste0("fevtest_1")
    
    # FEV1 # 
    new_df[[myvar]]=rowMeansClean(clinic_data[,c("fev1_1",
                                               "fev1_2",
                                               "fev1_3")])
    
    # FEV 6 #
    myvar=paste0("fevtest_6")
    new_df[[myvar]]=rowMeansClean(clinic_data[,c("fev6_1",
                                               "fev6_2",
                                               "fev6_3")])
    
    ### Now get average ratio
    myvar=paste0("fevtest_1_6_ratio")
    new_df[[myvar]]=case_when(new_df$fevtest_1 == 0 | new_df$fevtest_6 == 0 |
                                is.na(new_df$fevtest_1) | is.na(new_df$fevtest_6) ~ NA_real_,
                              T~new_df$fevtest_1/new_df$fevtest_6)
    
    
    
    
    # Add clinic date
    new_df$clinic_date = as.Date(parsedate::parse_date(substr(x = clinic_data$screening_end,
                                                              start = 0,stop = 10)))
    
    
    
    
    # bloods yes/no
    new_df$hasbloods = case_when(clinic_data$st3_blood_taken %in% c("FILLED","PARTIAL") |
                                   clinic_data$st4_blood_taken %in% c("FILLED","PARTIAL") ~ "Yes",
                                 TRUE ~ "No")
    
    
    
    ### Sit-stand tests
    myvar=paste0("sittest_stands_count")
    new_df[[myvar]]=clinic_data$count_of_stands
    
    myvar=paste0("sittest_pre_test_heartrate")
    new_df[[myvar]]=clinic_data$heart_rate_before
    
    myvar=paste0("sittest_pre_test_o2_saturation")
    new_df[[myvar]]=clinic_data$sp02_before
    
    myvar=paste0("sittest_post_test_heartrate")
    new_df[[myvar]]=clinic_data$heart_rate_after
    
    myvar=paste0("sittest_post_test_o2_saturation")
    new_df[[myvar]]=clinic_data$sp02_after
    
    
    }

  
  
  # add observation number to column names
  if(add_obs_num_to_colnames){
    amendindex=!colnames(new_df)%in%keepvars
    colnames(new_df)[amendindex] <- paste0(colnames(new_df)[amendindex],obs_no)
    
  }

  
  return(new_df)
  
}






# Clustering functions ----------------------------------------------------


# define function to 
createBinaryMatrix <- function(df,duration="3-6 months",symp_durations){
  indx= which(symp_durations==duration)
  df_bin=data.frame(matrix(nrow=nrow(df), ncol=ncol(df)))
  for(i in 1:ncol(df)){
    df_bin[,i]=as.numeric(df[,i]%in%symp_durations[indx:length(symp_durations)])
  }
  colnames(df_bin)=colnames(df)
  return(df_bin)
  
}




createHeatmapCoccur <- function(cooccurmat,mymatrixname,marginal_counts, colclust){
  
  
  myheatmap_cooccur_first=ComplexHeatmap::Heatmap(name = "% symptom \noccurrence or\nco-occurrence",
                                                  cooccurmat,
                                                  cluster_rows = F,
                                                  cluster_columns = colclust,
                                                  cell_fun = function(j,i,x,y,width, height, fill){
                                                    grid.text(cooccurmat[i,j],
                                                              x, y, gp=gpar(fontsize=6, col =
                                                                              ifelse(cooccurmat[i,j]>40,
                                                                                     "white",
                                                                                     "black")))
                                                  },
                                                  row_title_gp = gpar(fontsize=7, fontface="bold"),
                                                  column_title_gp = gpar(fontsize=7, fontface="bold"),
                                                  row_split = sympnames_type_df$symptom_type,
                                                  column_split =colclust,
                                                  row_names_gp = gpar(fontsize=9),
                                                  column_names_gp = gpar(fontsize=9),
                                                  top_annotation = columnAnnotation(
                                                    `%`=anno_barplot(marginal_counts, gp=gpar(fill=myCols[[1]]),
                                                                     border=F,height = unit(1.7,"cm"),
                                                                     ylim = c(0,70),
                                                                     # axis_param = list(direction = "reverse")
                                                    )),
                                                  column_title = mymatrixname,
                                                  row_gap = unit(1.5,"mm"),
                                                  column_gap = unit(1.5,"mm"),
                                                  rect_gp = gpar(col = "white", lwd = 1),
                                                  col = circlize::colorRamp2(breaks = c(00,40,80),
                                                                             colors = c("white",
                                                                                        myCols[[2]],
                                                                                        myCols[[1]])))
  
  return(myheatmap_cooccur_first)
}
