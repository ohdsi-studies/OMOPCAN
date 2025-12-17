log4r::info(logger, "Summarise charactertistics: demographics") 
cdm$outcome %>%
  CohortCharacteristics::summariseCharacteristics(
    strata = list(c("age_gr", "sex")),
    ageGroup = ageGroupList
  ) %>%
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts,
    path = output_folder,
    fileName = paste0(omopgenerics::cdmName(cdm), "_summarise_characteristics_demographics.csv")
  )


if(!isRegistry){
  log4r::info(logger, "Summarise charactertistics: comorbidities") 
  
  otherVar <-  c("study_period")
  
  if("smoking_status" %in% strat_var){
    otherVar <- c(otherVar, "smoking_status")
    cdm$outcome <- cdm$outcome %>%
      left_join(
        cdm$denominator %>% select(subject_id, smoking_status),
        by="subject_id"
      ) %>% 
      compute(name="outcome", overwrite = TRUE, temporary = FALSE)
  }
  
  if(!omopgenerics::isTableEmpty(cdm$conditions_all)){
    cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        # demographics = FALSE,
        strata = list(c("age_gr", "sex")),
        ageGroup = ageGroupList,
        cohortIntersectFlag = list(
          "Conditions prior to index date" = list(
            targetCohortTable = "conditions_all",
            window = c(-Inf, -1)
          ),
          "Conditions prior and up to 365 days before index date" = list(
            targetCohortTable = "conditions_all",
            window = c(-Inf, -366)
          ),
          "Conditions 365 and up to 31 days before index date" = list(
            targetCohortTable = "conditions_all",
            window = c(-365, -31)
          ),
          "Conditions 30 and up to 1 day before index date" = list(
            targetCohortTable = "conditions_all",
            window = c(-30, -1)
          ),
          "Conditions on index date" = list(
            targetCohortTable = "conditions_all",
            window = c(0, 0)
          )
        ),
        otherVariables = otherVar
      ) %>% 
      omopgenerics::exportSummarisedResult(
        minCellCount = minimum_counts,
        path = output_folder,
        fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_characteristics_conditions.csv")
      )
  }
  
  if(!omopgenerics::isTableEmpty(cdm$medications)){
    log4r::info(logger, "Summarise charactertistics: medications") 
    cdm$outcome %>%
      CohortCharacteristics::summariseCharacteristics(
        # demographics = FALSE,
        strata = list(c("age_gr", "sex")),
        ageGroup = ageGroupList,
        cohortIntersectFlag = list(
          "Medications 365 days prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-365, -1)
          ),
          "Medications 365 to 31 days prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-365, -31)
          ),
          "Medications 30 to 1 day prior to index date" = list(
            targetCohortTable = "medications",
            window = c(-30, -1)
          ),
          "Medications on index date" = list(
            targetCohortTable = "medications",
            window = c(0, 0)
          ),
          "Medications 1 to 30 days after index date" = list(
            targetCohortTable = "medications",
            window = c(1, 30)
          ),
          "Medications 31 to 90 days after index date" = list(
            targetCohortTable = "medications",
            window = c(31, 90)
          ) 
        )
        # ,
        # otherVariables = otherVar
      ) %>% 
      omopgenerics::exportSummarisedResult(
        minCellCount = minimum_counts,
        path = output_folder,
        fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_characteristics_drugs.csv")
        )
  }
  
  log4r::info(logger, "Large scale characterisation: conditions")
  cdm$outcome %>% 
    CohortCharacteristics::summariseLargeScaleCharacteristics(
      window = list(
        # Shared windows for both condition_occurrence and drug_era
        c(-Inf, -1),     #anytimeprior
        c(-Inf, -366),   # >1y before
        c(-365, -31),    # 1y-1m before
        c(-30, -1),      # 30d-1d before
        c(0, 0)         # Index date
      ),
      eventInWindow = "condition_occurrence",  # Only conditions
      includeSource = TRUE,
      minimumFrequency = 0.1
    ) %>% 
    omopgenerics::exportSummarisedResult(
      minCellCount = minimum_counts,
      path = output_folder,
      fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_large_scale_characteristics_conditions.csv")
    )
    
  log4r::info(logger, "Large scale characterisation: medications")
  cdm$outcome %>% 
    CohortCharacteristics::summariseLargeScaleCharacteristics(
      window = list(
        # Shared windows for both condition_occurrence and drug_era
        c(-Inf, -1),     #anytimeprior
        c(-Inf, -366),   # >1y before
        c(-365, -31),    # 1y-1m before
        c(-30, -1),      # 30d-1d before
        c(0, 0),         # Index date
        # Additional windows for drug_era only
        c(1, 30),        # 1-30d after
        c(31, 90)        # 1m-90d after
      ),
      episodeInWindow = "drug_era",    # Only drug eras
      includeSource = TRUE,
      minimumFrequency = 0.1
    ) %>% 
    omopgenerics::exportSummarisedResult(
      minCellCount = minimum_counts,
      path = output_folder,
      fileName = paste0(omopgenerics::cdmName(cdm),"_summarise_large_scale_characteristics_drugs.csv")
      )
}

  

