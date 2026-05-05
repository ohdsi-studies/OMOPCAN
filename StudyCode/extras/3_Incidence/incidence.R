# Estimate Incidence 
log4r::info(logger, "Estimate incidence") 

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  # censorTable = "exclusion",
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  strata = as.list(strat_var),
  completeDatabaseIntervals = TRUE
) 

inc %>% 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_incidence.csv"),
    path = output_folder
  )

  
