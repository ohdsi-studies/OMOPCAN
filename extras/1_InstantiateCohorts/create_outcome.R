#Initialize variables
strat_var = c()
cdm <- omopgenerics::emptyCohortTable(cdm, name="conditions_st", overwrite = TRUE)
cdm <- omopgenerics::emptyCohortTable(cdm, name="outcome", overwrite = TRUE)

# Instantiate outome subsetting from outcome_all
log4r::info(logger, paste0(paste0("Cancers: "), paste(toupper(cancer_types_gr$cohort_name), collapse = ", ")))
log4r::info(logger, "Create outcome cohort")
cdm$outcome <- cdm$outcome_all %>% 
  CohortConstructor::subsetCohorts( 
    cohortId = cancer_types_gr$cohort_definition_id,
    name = "outcome" 
    )

#Add demographics
log4r::info(logger, "Add demographics")
cdm$outcome <- cdm$outcome %>%
  PatientProfiles::addDemographics(ageGroup = ageGroupList) %>% 
  rename(age_gr = age_group) %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) 

#Filter out exlusions
log4r::info(logger, "Apply exclusion criteria to selected cancer cohorts")
cdm$outcome <- cdm$outcome %>% 
  dplyr::filter(prior_observation >= daysPriorObservation) %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) 
cdm$outcome <- cdm$outcome %>% 
  CDMConnector::recordCohortAttrition(
    reason = "Excluded patients without sufficient prior history"
    )

cdm$outcome <- cdm$outcome %>%
  dplyr::filter(cohort_start_date >= as.Date(startdate)) %>% 
  dplyr::filter(cohort_start_date <= as.Date(enddate)) %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) 
cdm$outcome <- cdm$outcome %>% 
  CDMConnector::recordCohortAttrition(
    reason="Excluded patients with with index date outside of the study period"
    )

cdm$outcome <- cdm$outcome %>% 
  dplyr::filter(age_gr != "None")%>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE) 
cdm$outcome <- cdm$outcome %>% 
  CDMConnector::recordCohortAttrition(
    reason="Excluded patients with age outside of defined age groups" )

cdm$outcome <- cdm$outcome %>% 
  dplyr::filter(sex %in% c("Male", "Female"))%>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE)
cdm$outcome <- cdm$outcome  %>% 
  CDMConnector::recordCohortAttrition(reason="Excluded patients with no sex defined")


if(cancer_group == "male"){
  cdm$outcome <- cdm$outcome %>% 
    dplyr::filter(sex == "Male")%>% 
    compute(name = "outcome", temporary = FALSE, overwrite = TRUE)
  cdm$outcome <- cdm$outcome  %>% 
    CDMConnector::recordCohortAttrition(reason="Excluded female patients")
}

if(cancer_group == "female"){
  cdm$outcome <- cdm$outcome %>% 
    dplyr::filter(sex == "Female")%>% 
    compute(name = "outcome", temporary = FALSE, overwrite = TRUE)
  cdm$outcome <- cdm$outcome  %>% 
    CDMConnector::recordCohortAttrition(reason="Excluded male patients") 
}

# Add study period windows
cdm$outcome <- cdm$outcome %>% 
  mutate(study_period = case_when(
    cohort_start_date >= as.Date("2000-01-01") & cohort_start_date < as.Date("2005-01-01") ~ "2000-2004",
    cohort_start_date >= as.Date("2005-01-01") & cohort_start_date < as.Date("2010-01-01") ~ "2005-2009",
    cohort_start_date >= as.Date("2010-01-01") & cohort_start_date < as.Date("2015-01-01") ~ "2010-2014",
    cohort_start_date >= as.Date("2015-01-01") & cohort_start_date < as.Date("2020-01-01") ~ "2015-2019",
    cohort_start_date >= as.Date("2020-01-01") & cohort_start_date < as.Date("2025-01-01") ~ "2020-2024",
    TRUE ~ "Outside Range" # Dates outside the specified range
    )
  ) %>% 
  compute(name = "outcome", temporary = FALSE, overwrite = TRUE)

if(nrow(covariable_counts)>0 & !isRegistry){
  # Select stratifications depending on cancer group-----------------------------
  log4r::info(logger, paste0("Group stratifications: ", paste(toupper(cancer_types[[cancer_group]]$stratification), collapse = ", ")))
  stratif_gr <- covariable_counts %>% 
    filter(cohort_name %in% cancer_types[[cancer_group]]$stratification) %>% 
    filter(number_subjects > minimum_counts)
  
  if ("tobacco" %in% cancer_types[[cancer_group]]$stratification & any(grepl("smoker", covariable_counts$cohort_name))){
    n_smoker <-  covariable_counts %>%
      dplyr::filter(grepl("smoker", cohort_name)) %>%
      dplyr::summarise(n = sum(number_subjects, na.rm = TRUE)) %>%
      dplyr::pull(n)
    
    if(n_smoker>minimum_counts){
      stratif_gr <- stratif_gr %>% 
        union_all(covariable_counts %>% filter(grepl("smoker",cohort_name)))
    }
  }
  
  if (nrow(stratif_gr)>0){
    log4r::info(logger, paste0("Stratification variables: ", paste(toupper(stratif_gr$cohort_name), collapse = ", ")))
    cdm$conditions_st <- cdm$conditions_all %>% 
      CohortConstructor::subsetCohorts( 
        cohortId = stratif_gr$cohort_definition_id,
        name = "conditions_st" 
      )
    
    # Add stratifications to denominator
    log4r::info(logger, "Add stratifications to denominator cohort")
    cdm$denominator <- cdm$denominator %>%
      select(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")) %>% 
      PatientProfiles::addCohortIntersectFlag(
        targetCohortTable = "conditions_st",
        window = list(c(-Inf, -1)),
        nameStyle = "{cohort_name}"
      ) %>% 
      compute(name="denominator", overwrite = TRUE, temporary = FALSE)
    
    if (any(grepl("smoker", colnames(cdm$denominator)))){
      cdm$denominator <- cdm$denominator %>% 
        mutate(
          smoking_status = case_when(
            former_current_smoker == 1 ~ "former_current_sometimes",
            non_smoker == 1 ~ "never",
            TRUE ~ "missing")
        ) %>% 
        select(-former_current_smoker, -non_smoker) %>% 
        compute(name="denominator", overwrite = TRUE, temporary = FALSE)
    } 
    
    strat_var = setdiff(colnames(cdm$denominator), 
      c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
    )
  } 
}

## Survival: ------------------------------------------------------------------
#1) Exclude subjects with anymalignancy prior to index date
cdm$outcome_surv <- cdm$outcome %>%
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "exclusion",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    # targetStartDate = startdate,
    # targetEndDate = enddate ,
    window = list(c(-Inf, -1)),
    nameStyle = "anymalignancy"
  )  %>%
  dplyr::filter(anymalignancy == 0) %>%
  dplyr::select(-anymalignancy) %>%
  compute(name = "outcome_surv", temporary = FALSE, overwrite = TRUE)

attr(cdm$outcome_surv, "cohort_set") <- attr(cdm$outcome_surv, "cohort_set") %>%
  dplyr::mutate(cohort_name = paste0(cohort_name, "_survival"))

cdm$outcome_surv <- cdm$outcome_surv  %>%
  CDMConnector::recordCohortAttrition(
    reason="Exclude patients with any prior history of maglinancy (ex skin cancer)"
  )

#2) Exclude multiple outcomes same date 
cdm$outcome_surv <- cdm$outcome_surv %>% 
  left_join(
    cdm$exclusion %>% 
    # cdm$outcome_all %>% 
      group_by(subject_id, cohort_start_date) %>% 
      summarise(n_cancers = n_distinct(cohort_definition_id)),
    by=c("subject_id", "cohort_start_date")
  ) %>% 
  filter(n_cancers == 1) %>% 
  compute(name="outcome_surv", overwrite = TRUE, temporary = FALSE)

cdm$outcome_surv <- cdm$outcome_surv  %>% 
  CDMConnector::recordCohortAttrition(reason="Excluded individuals with multiple cancers at same day") 

# ------------------------------------------------------------------------------
# Export cohort counts and attrition
log4r::info(logger, "Export cohort counts and attrition tables")
attritions <- list()
counts <- list()
cohorts <- c( "outcome", "outcome_surv", "exclusion" ) 
if(!isRegistry){ cohorts <- c(cohorts, "denominator", "conditions_all", "conditions_st", "medications") }

for (nm in cohorts) {
  if (omopgenerics::isTableEmpty(cdm[[nm]])){
    log4r::info(logger, paste0(nm, " table is empty"))
  }else{
    counts[[nm]] <- CohortCharacteristics::summariseCohortCount(cdm[[nm]])
    attritions[[nm]] <- CohortCharacteristics::summariseCohortAttrition(cdm[[nm]])
  }
}

counts <- omopgenerics::bind(counts)
counts %>% 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_summarise_cohort_count.csv"),
    path = output_folder
  )

attritions <- omopgenerics::bind(attritions)
attritions %>% 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_summarise_cohort_attrition.csv"),
    path = output_folder
  )


