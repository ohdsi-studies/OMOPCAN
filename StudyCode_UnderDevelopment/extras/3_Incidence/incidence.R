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


log4r::info(logger, "Compute period incidence for IRR")
if(nrow(inc)>0){
  # Get unique study periods
  unique_periods <- inc %>% 
    filter(result_id == 1 & strata_name == "overall" & estimate_name=="outcome_count") %>% 
    visOmopResults::splitAdditional() %>% 
    filter(analysis_interval == "years") %>% 
    mutate(study_period = case_when(
      incidence_start_date >= as.Date("2000-01-01") & incidence_start_date < as.Date("2005-01-01") ~ "2000-2004",
      incidence_start_date >= as.Date("2005-01-01") & incidence_start_date < as.Date("2010-01-01") ~ "2005-2009",
      incidence_start_date >= as.Date("2010-01-01") & incidence_start_date < as.Date("2015-01-01") ~ "2010-2014",
      incidence_start_date >= as.Date("2015-01-01") & incidence_start_date < as.Date("2020-01-01") ~ "2015-2019",
      incidence_start_date >= as.Date("2020-01-01") & incidence_start_date < as.Date("2025-01-01") ~ "2020-2024",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(study_period)) %>% 
    group_by(group_level, study_period) %>% 
    mutate(
      n_rows = n(),
      full_study_period = if_else(n_rows == 5, study_period, NA_character_)
    ) %>%
    ungroup() %>% 
    filter(!is.na(full_study_period)) %>% 
    distinct(full_study_period) %>% 
    pull(full_study_period)

  for (period in unique_periods) {
    log4r::info(logger, paste0("Estimate period incidence during ", period))
    
    cdm <- IncidencePrevalence::generateDenominatorCohortSet(
      cdm = cdm,
      name = paste0("denominator_study_period_", gsub("-", "_", period)),
      cohortDateRange = as.Date(date_ranges[[period]]),
      ageGroup = append(ageGroupList, list("0 to 150" = c(0,150)), after = 0),
      sex = sexGroup,
      daysPriorObservation = daysPriorObservation
    )

    IncidencePrevalence::estimateIncidence(
      cdm = cdm,
      denominatorTable = paste0("denominator_study_period_", gsub("-", "_", period)),
      outcomeTable = "outcome",
      interval = c("overall"),
      outcomeWashout = Inf,
      repeatedEvents = FALSE,
      completeDatabaseIntervals = TRUE
    ) %>% 
      omopgenerics::exportSummarisedResult(
        minCellCount = minimum_counts, 
        fileName =  paste0(db_name,"_inc_period_",period,".csv"),
        path = output_folder
      )
    
  }
}



  