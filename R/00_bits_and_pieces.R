
symptoms_df=data.frame(
  symptom_duration_code = c(
    "SYMPTOMS_DURATION_01",
    "SYMPTOMS_DURATION_02",
    "SYMPTOMS_DURATION_03",
    "SYMPTOMS_DURATION_04",
    "SYMPTOMS_DURATION_05",
    "SYMPTOMS_DURATION_06",
    "SYMPTOMS_DURATION_07",
    "SYMPTOMS_DURATION_08",
    "SYMPTOMS_DURATION_09",
    "SYMPTOMS_DURATION_10",
    "SYMPTOMS_DURATION_11",
    "SYMPTOMS_DURATION_12",
    "SYMPTOMS_DURATION_13",
    "SYMPTOMS_DURATION_14",
    "SYMPTOMS_DURATION_15",
    "SYMPTOMS_DURATION_16",
    "SYMPTOMS_DURATION_17",
    "SYMPTOMS_DURATION_18",
    "SYMPTOMS_DURATION_19",
    "SYMPTOMS_DURATION_20",
    "SYMPTOMS_DURATION_21",
    "SYMPTOMS_DURATION_22",
    "SYMPTOMS_DURATION_23",
    "SYMPTOMS_DURATION_24",
    "SYMPTOMS_DURATION_25",
    "SYMPTOMS_DURATION_26",
    "SYMPTOMS_DURATION_27",
    "SYMPTOMS_DURATION_28",
    "SYMPTOMS_DURATION_29",
    "SYMPTOMS_DURATION_30",
    "SYMPTOMS_DURATION_98",
    "SYMPTOMS_DURATION_99"
  ),
  symptom = c("Fever",
              "Loss of sense of smell",
              "Sore throat",
              "Runny nose",
              "Sneezing",
              "Persistent cough",
              "Shortness of breath (compared with what’s normal for you)",
              "Chest pain/tightness",
              "Loss of appetite (skipping meals)",
              "Diarrhoea",
              "Vomiting",
              "Itchy eyes",
              "Headache",
              "Fatigue",
              "Joint pain/aches",
              "Muscle pain/aches",
              "Loss or change to sense of taste",
              "Nausea",
              "Confusion/brain fog/forgetfulness",
              "Dizziness/vertigo",
              "Fast or irregular heartbeat",
              "Numbness or tingling somewhere in the body",
              "Skin issues (itchy, scaly, redness, etc)",
              "Vision issues",
              "Ringing in the ears (tinnitus)",
              "Hair loss",
              "Difficulty sleeping",
              "Sudden swelling of face or lips",
              "Red/purple sores or blisters on your feet (including toes)",
              "Leg swelling (thrombosis)",
              "Prefer not to say symptom",
              "Other symptom"
  )
)












# Old ---------------------------------------------------------------------





symptoms_desc=c("Fever","Loss of sense of smell","Sore throat","Runny nose",
  "Sneezing","Persistent cough",
  "Shortness of breath (compared with what’s normal for you)",
"Chest pain/tightness","Loss of appetite (skipping meals)",
"Diarrhoea","Vomiting","Itchy eyes","Headache","Fatigue",
"Joint pain/aches","Muscle pain/aches","Loss or change to sense of taste",
"Nausea","Confusion/brain fog/forgetfulness","Dizziness/vertigo",
"Fast or irregular heartbeat","Numbness or tingling somewhere in the body",
"Skin issues (itchy, scaly, redness, etc)","Vision issues",
"Ringing in the ears (tinnitus)","Hair loss","Difficulty sleeping",
"Sudden swelling of face or lips",
"Red/purple sores or blisters on your feet (including toes)",
"Leg swelling (Thrombosis)")


symptoms_desc_short=c("Fever","Loss of sense of smell","Sore throat","Runny nose",
                "Sneezing","Persistent cough",
                "Shortness of breath",
                "Chest pain/tightness","Loss of appetite",
                "Diarrhoea","Vomiting","Itchy eyes","Headache","Fatigue",
                "Joint pain/aches","Muscle pain/aches","Loss or change to sense of taste",
                "Nausea","Confusion/brain fog/forgetfulness","Dizziness/vertigo",
                "Fast or irregular heartbeat","Numbness or tingling",
                "Skin issues","Vision issues",
                "Ringing in the ears","Hair loss","Difficulty sleeping",
                "Sudden swelling of face or lips",
                "Red/purple sores or blisters on feet)",
                "Leg swelling (Thrombosis)")



symp_names <-   c("fever","loss_of_sense_of_smell",
                  "sore_throat","runny_nose","sneezing","persistent_cough",
                  "shortness_of_breath_compared_with_whats_normal_for_you","chest_pain_tightness",
                  "loss_of_appetite_skipping_meals","diarrhoea","vomiting",
                  "itchy_eyes",
                  "headache","fatigue" ,"joint_pain_aches","muscle_pain_aches",
                  "loss_or_change_to_sense_of_taste",
                  "nausea",
                  "confusion_brain_fog_forgetfulness","dizziness_vertigo", 
                  "fast_or_irregular_heartbeat",
                   "numbness_or_tingling_somewhere_in_the_body", 
                  "skin_issues_itchy_scaly_redness_etc",
                  "vision_issues" ,
                  "ringing_in_the_ears_tinnitus","hair_loss",
                  "difficulty_sleeping",
                  "sudden_swelling_of_face_or_lips", "red_purple_sores_or_blisters_on_your_feet_including_toes", "leg_swelling_thrombosis")


sympnames_type_df <- data.frame(symp_names,
                                symptoms_desc=symptoms_desc,
                                symptoms_desc_short=symptoms_desc_short,
                                symptom_type = c( "Influenza-like","Smell/taste",
                                                  rep("Coryzal (cold-like)", 4),
                                                  rep("Respiratory/cardiac", 2),                                                  
                                                  rep("Gastrointestinal",3),
                                                  rep("Other",1),
                                                  rep("Influenza-like",4),
                                                  "Smell/taste",
                                                  rep("Gastrointestinal",1),
                                                  rep("Neurological",2),
                                                  "Respiratory/cardiac",
                                                  rep("Neurological",1),
                                                  rep("Other",1),
                                                  rep("Neurological",1),
                                                  rep("Other",2),
                                                  "Influenza-like",
                                                  rep("Other",3)
                                ),
                                symptype=paste0("symptype_",1:30),
                                symptlong=paste0("symptlong_",1:30),
                                lc_registration_names=c(paste0("symptoms_0",1:9),
                                                        paste0("symptoms_",10:27),
                                                        NA_character_,
                                                        paste0("symptoms_",29),
                                                        NA_character_))


symp_durations=c("0-2 weeks","2-4 weeks","1-3 months", "3-6 months", "7+ months")




# Cov name list updated
ge_cov_names=list(all_participants="All participants",
                  gender.y="Sex",
                  age_group ="Age",
                  experienced_long_symptom = "Self-declared long COVID"
)




# Updated symptoms df(post corrections) -----------------------------------------------------



symptoms_desc_corrected=c("Fever","Loss of sense of smell","Sore throat","Runny nose",
                "Sneezing","Persistent cough",
                "Shortness of breath (compared with what’s normal for you)",
                "Chest pain/tightness","Loss of appetite (skipping meals)",
                "Diarrhoea","Vomiting","Itchy eyes","Headache","Fatigue",
                "Joint pain/aches","Muscle pain/aches","Loss or change to sense of taste",
                "Nausea","Confusion/brain fog/forgetfulness","Dizziness/vertigo",
                "Fast or irregular heartbeat","Numbness or tingling somewhere in the body",
                "Skin issues (itchy, scaly, redness, etc)","Vision issues",
                "Ringing in the ears (tinnitus)","Hair loss","Difficulty sleeping"
                # "Red/purple sores or blisters on your feet (including toes)",
                # "Leg swelling (Thrombosis)"
)


symptoms_desc_short_corrected=c("Fever","Loss of sense of smell","Sore throat","Runny nose",
                      "Sneezing","Persistent cough",
                      "Shortness of breath",
                      "Chest pain/tightness","Loss of appetite",
                      "Diarrhoea","Vomiting","Itchy eyes","Headache","Fatigue",
                      "Joint pain/aches","Muscle pain/aches","Loss or change to sense of taste",
                      "Nausea","Confusion/brain fog/forgetfulness","Dizziness/vertigo",
                      "Fast or irregular heartbeat","Numbness or tingling",
                      "Skin issues","Vision issues",
                      "Ringing in the ears","Hair loss","Difficulty sleeping"
                      # "",
                      # "Leg swelling (Thrombosis)"
)



symp_names_corrected <-   c("fever","loss_of_sense_of_smell",
                  "sore_throat","runny_nose","sneezing","persistent_cough",
                  "shortness_of_breath_compared_with_whats_normal_for_you","chest_pain_tightness",
                  "loss_of_appetite_skipping_meals","diarrhoea","vomiting",
                  "itchy_eyes",
                  "headache","fatigue" ,"joint_pain_aches","muscle_pain_aches",
                  "loss_or_change_to_sense_of_taste",
                  "nausea",
                  "confusion_brain_fog_forgetfulness","dizziness_vertigo", 
                  "fast_or_irregular_heartbeat",
                  "numbness_or_tingling_somewhere_in_the_body", 
                  "skin_issues_itchy_scaly_redness_etc",
                  "vision_issues" ,
                  "ringing_in_the_ears_tinnitus","hair_loss",
                  "difficulty_sleeping"
                  # "sudden_swelling_of_face_or_lips_sores_on_feet"
                  # "red_purple_sores_or_blisters_on_your_feet_including_toes", "leg_swelling_thrombosis"
)


sympnames_type_df_corrected <- data.frame(symp_names_corrected,
                                symptoms_desc=symptoms_desc_corrected,
                                symptoms_desc_short=symptoms_desc_short_corrected,
                                symptom_type = c( "Influenza-like","Smell/taste",
                                                  rep("Coryzal (cold-like)", 4),
                                                  rep("Respiratory/cardiac", 2),                                                  
                                                  rep("Gastrointestinal",3),
                                                  rep("Other",1),
                                                  rep("Influenza-like",4),
                                                  "Smell/taste",
                                                  rep("Gastrointestinal",1),
                                                  rep("Neurological",2),
                                                  "Respiratory/cardiac",
                                                  rep("Neurological",1),
                                                  rep("Other",1),
                                                  rep("Neurological",1),
                                                  rep("Other",2),
                                                  "Influenza-like"
                                                  # rep("Other",1)
                                ),
                                symptype=paste0("symptype_",1:27),
                                symptlong=paste0("symptlong_",1:27),
                                lc_registration_names=c(paste0("symptoms_duration_0",1:9),
                                                        paste0("symptoms_duration_",10:27)
                                                        # NA_character_,
                                                        # paste0("symptoms_",29),
                                                        # NA_character_
                                )
)


