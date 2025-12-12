# Estimate Incidence 
log4r::info(logger, "Estimate registry incidence") 

inc <- estimateRegistryIncidence(
  cdm = cdm,
  denominatorTableName = "denominator_counts",
  outcomeTable = "outcome",
  # censorTable = "exclusion", 
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE
)

inc %>% 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_incidence.csv"),
    path = output_folder
  )



  