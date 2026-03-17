# Estimate Incidence ----
log4r::info(logger, "Estimate registry prevalence") 

#Partial prevalence: change end date for start date + 5 years
cdm$outcome_prev <- cdm$outcome |> 
  CohortConstructor::padCohortDate(
    days = 1825,
    cohortDate = "cohort_end_date",
    indexDate = "cohort_start_date",
    name = "outcome_prev"
  )

#Compute prevalence
prev <- estimateRegistryPointPrevalence( 
  cdm = cdm,
  denominatorTableName = "denominator_counts",
  outcomeTable = "outcome_prev",
  interval = "years",
  timePoint = "end"
) 

prev %>% 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_prevalence.csv"),
    path = output_folder
  )

  
  
  

