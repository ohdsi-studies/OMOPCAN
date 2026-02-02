# Estimate Incidence ----
log4r::info(logger, "Estimate registry prevalence") 

prev <- estimateRegistryPeriodPrevalence( 
  cdm = cdm,
  denominatorTableName = "denominator_counts",
  outcomeTable = "outcome",
  interval = c("years", "overall")
) 

prev %>% 
  omopgenerics::exportSummarisedResult(
    minCellCount = minimum_counts, 
    fileName =  paste0(db_name,"_prevalence.csv"),
    path = output_folder
  )

  
  
  

