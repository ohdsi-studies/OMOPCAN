cancer_types = list()

cancer_types[["respiratory"]] = list(
  types = c("larynx", "lung", "mesothelioma"),
  stratification = c("asthma","copd", "former_current_smoker" , "non_smoker", "obesitycharybdis","cardiovascular_disease", "hypertension")
)

cancer_types[["digestive"]] = list(
  types = c("esophagus", "colorectal", "colon", "rectum", "anus", "stomach", "liver", "pancreatic", "gallbladder"),
  stratification = c("t2_diabetes","viral_hepatitis", "former_current_smoker" , "non_smoker", "obesitycharybdis","cardiovascular_disease", "hypertension", "hpv")
)

cancer_types[["male"]] = list(
  types = c("penis", "prostate", "testis"), 
  stratification = c("hpv", "former_current_smoker" , "non_smoker", "obesitycharybdis")
)

cancer_types[["female"]] = list(
  types = c("breast", "vulva", "vagina", "cervix", "corpus_uteri", "ovary"),
  stratification = c("hpv", "former_current_smoker" , "non_smoker", "obesitycharybdis","menopausal_status", "depressive_disorder", "t2_diabetes")
)

cancer_types[["hematologic"]] = list(
  types = c("hodgkin", "non_hodgkin", "multiple_myeloma", "leukaemia"),
  stratification = c("hiv", "autoimmune_conditions", "obesitycharybdis","former_current_smoker" , "non_smoker")
)

cancer_types[["oral"]] = list(
  types = c("lip_oral", "salivary_gland", "oropharynx", "nasopharynx", "hypopharynx"),
  stratification = c("hpv", "former_current_smoker" , "non_smoker", "obesitycharybdis")
)

cancer_types[["urinary"]] = list(
  types = c("kidney", "bladder"),
  stratification = c("chronic_kidney_disease", "former_current_smoker" , "non_smoker", "obesitycharybdis","hypertension")
)

cancer_types[["skin"]] = list(
  types = c("melanoma_skin", "nmsc_broad", "nmsc_narrow"),
  stratification = c("former_current_smoker" , "non_smoker", "obesitycharybdis")
)

cancer_types[["other"]] = list(
  types = c("brain", "kaposi", "thyroid"),
  stratification = c("hiv", "former_current_smoker" , "non_smoker", "obesitycharybdis")
)
