if(cdm$death %>% head(5) %>% count() %>% pull("n") > 0) {
  
  log4r::info(logger, "Add stratification variables to outcome cohort") 
  if(!omopgenerics::isTableEmpty(cdm$conditions_st) & !isRegistry){
    cdm$outcome_surv <- cdm$outcome_surv %>% 
      left_join(
        cdm$denominator %>% select(subject_id, !!strat_var),
        by="subject_id"
      ) %>% 
      compute(name="outcome_surv", overwrite = TRUE, temporary = FALSE)
  }
  
  #Create new cohort splitting in time periods
  log4r::info(logger, "Estimate survival per study period") 
 
  # Create death cohort with all relevant columns
  log4r::info(logger, "Generate death cohort") 
  cdm$death_cohort <- CohortConstructor::deathCohort(cdm,"death_cohort")
  
  #Avoid error in estimate survival: get cohorts with death records
  ids_surv <- cdm$outcome_surv %>% 
    group_by(cohort_definition_id )%>% 
    inner_join(
      cdm$death %>% 
        select(subject_id=person_id)
      ) %>% 
    tally() %>% 
    pull(cohort_definition_id)
  ids_dif <- setdiff(cancer_types_gr$cohort_definition_id, ids_surv )
  if(length(ids_dif) != 0){
    log4r::info(logger, 
        paste0("No death records for: ",
                cancer_types_gr[cancer_types_gr$cohort_definition_id %in% ids_dif,"cohort_name"] ))
  }
  
  log4r::info(logger, "Estimate single event survival") 
  CohortSurvival::estimateSingleEventSurvival(
    cdm,
    targetCohortTable = "outcome_surv",
    targetCohortId = ids_surv,
    outcomeCohortTable = "death_cohort",
    censorOnCohortExit = TRUE,
    minimumSurvivalDays = 1,
    estimateGap = 365,
    strata = 
      append(as.list(c("study_period","age_gr", "sex", strat_var)), 
         append(list(c("age_gr","sex"), c("study_period","sex"), c("study_period","age_gr"), c("study_period","age_gr", "sex")),
            append(lapply(strat_var, function(x) c("study_period", x)),
              append(lapply(strat_var, function(x) c("age_gr", x)), 
                 append(lapply(strat_var, function(x) c("sex", x)),
                    append(lapply(strat_var, function(x) c("age_gr","sex", x)),
                        lapply(strat_var, function(x) c("age_gr","sex","study_period", x))
                      )
                    )
                  )
                )
              )
            )
  )%>%
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts,
    fileName =  paste0(db_name,"_survival.csv"),
    path = output_folder
  )
  
}
  


