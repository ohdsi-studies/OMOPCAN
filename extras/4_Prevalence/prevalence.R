# Estimate Incidence ----
log4r::info(logger, "Estimate prevalence") 

IncidencePrevalence::estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = c("years", "overall"),
  strata = as.list(strat_var),
) %>% 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_prevalence.csv"),
    path = output_folder
  )


  
  

