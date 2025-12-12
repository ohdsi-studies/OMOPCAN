getRegistryIncidence <- function(cdm,
                                 denominatorTable,
                                 outcomeTable,
                                 denominatorCohortId,
                                 outcomeCohortId,
                                 interval,
                                 outcomeWashout,
                                 repeatedEvents,
                                 tablePrefix,
                                 analysisId)
  {

  if (!is.null(outcomeWashout)) {
    if (is.na(outcomeWashout)) {
      outcomeWashout <- NULL
    }
  }

  # Get denominator cohort population for entire study period
  denomPop <- denominatorTable %>%
    dplyr::filter(.data$cohort_definition_id ==
                    .env$denominatorCohortId) %>%
    dplyr::select(-"cohort_definition_id")
  
  if (nrow(denomPop) == 0){
    errorCondition("Table with denominator counts is empty.")
  }
  
  # we assume complete years intervals
  studyStartEnd = denomPop %>%
    summarise(max= as.Date(paste0(max(.data$year), "-12-31")),
              min= as.Date(paste0(min(.data$year), "-01-01")))

  # Check which population strata we are considering in the denominator (sex/age)
  sex_strata = denomPop %>% 
    dplyr::pull(sex) %>% 
    unique()
  sex_strata = ifelse(sex_strata == "Both", NA, sex_strata)

  age_strata = denomPop %>% 
    dplyr::pull(age_gr) %>% 
    unique()
  age_strata = ifelse(age_strata == "Overall", NA, age_strata)

  # Get outcome cohort table
  studyPopOutcome <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$outcome_cohort_id == .env$outcomeCohortId) %>%
    dplyr::mutate(
      cohort_start_date= studyStartEnd$min, 
      cohort_end_date=ifelse(!is.na(outcome_start_date), outcome_start_date, studyStartEnd$max)
      ) %>% 
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, outcome_start_date,  outcome_prev_end_date, age_gr, sex) 
    # dplyr::select(subject_id, outcome_start_date, outcome_end_date,  outcome_prev_end_date, age_gr, sex)


  attrition <- recordAttrition(
    table = studyPopOutcome,
    id = "subject_id",
    reasonId = 11,
    reason = "Starting analysis outcome population"
  )
  

  # Filter outcome cohort based on denominator strata
  if(!is.na(sex_strata)){
    studyPopOutcome <- studyPopOutcome %>%
      dplyr::filter(.data$sex == .env$sex_strata)
  }
  attrition <- recordAttrition(
    table = studyPopOutcome,
    id = "subject_id",
    reasonId = 13,
    reason = "Exclude individuals based on sex"
  )
  
  if(!is.na(age_strata)){
    studyPopOutcome <- studyPopOutcome %>%
      dplyr::filter(.data$age_gr == .env$age_strata)
  }
  attrition <- recordAttrition(
    table = studyPopOutcome,
    id = "subject_id",
    reasonId = 14,
    reason = "Exclude individuals based on age"
  )
  
  #check nº of individuals in the head of outcome cohort
  nStudyPopOutcome <- studyPopOutcome %>%
    utils::head(10) %>%
    dplyr::tally() %>%
    dplyr::pull("n")

  if(nStudyPopOutcome > 0){

    #Exclude participants with previous outcome
    if (is.null(outcomeWashout)) {
      # exclude anyone with a previous outcome
      studyPopOutcome <- studyPopOutcome %>%
        dplyr::filter(
          is.na(.data$outcome_prev_end_date) &
            .data$cohort_start_date <= .data$cohort_end_date
          )
    } else {
      # otherwise add the washout to the previous outcome
      outcomeWashoutPlusOne <- as.integer(outcomeWashout + 1)
      studyPopOutcome <- studyPopOutcome %>%
        dplyr::mutate(outcome_prev_end_date = as.Date(.data$outcome_prev_end_date)) %>%
        dplyr::mutate(outcome_prev_end_date = dplyr::if_else(
          is.na(.data$outcome_prev_end_date),
          as.Date(.data$outcome_prev_end_date),
          as.Date(!!CDMConnector::dateadd("outcome_prev_end_date",
                                          {{ outcomeWashoutPlusOne }},
                                          interval = "day"
          ))
        )) %>% 
        dplyr::mutate(cohort_start_date = dplyr::if_else(
          is.na(.data$outcome_prev_end_date) |
            (.data$cohort_start_date > .data$outcome_prev_end_date),
          .data$cohort_start_date,
          .data$outcome_prev_end_date
        )) %>%
        dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)


      if (repeatedEvents == FALSE &&
          sum(!is.na(studyPopOutcome %>%
                     dplyr::pull(.data$outcome_start_date))) > 0) {
      
        studyPopOutcome <-  studyPopOutcome %>%
          dplyr::filter(!is.na(.data$outcome_start_date)) %>% 
          dplyr::group_by(.data$subject_id) %>%
          dplyr::filter(.data$outcome_start_date ==
                          min(.data$outcome_start_date, na.rm = TRUE)) %>%
          dplyr::distinct() %>% 
          dplyr::ungroup() 
        
      }
    }

    attrition <- recordAttrition(
      table = studyPopOutcome,
      id = "subject_id",
      reasonId = 13,
      reason = "Excluded due to prior event (do not pass outcome washout during study period)",
      existingAttrition = attrition
    )

  }

  nStudyPopOutcome <- studyPopOutcome %>%
    utils::head(10) %>%
    dplyr::tally() %>%
    dplyr::pull("n")

  # study dates
  # based on the earliest start and latest end years in the denominator
  if (nStudyPopOutcome > 0) {

    if (interval == "overall") {
      # for overall we just go from start to end
      studyDays <- dplyr::tibble(
        time = "overall",
        start_time = studyStartEnd$min,
        end_time = studyStartEnd$max
      )
    } else {
      studyDays <- getStudyDays(
        startDate = studyStartEnd$min,
        endDate = studyStartEnd$max,
        timeInterval = interval,
        completeDatabaseIntervals = TRUE
      )
    }
  } else {
    studyDays <- dplyr::tibble()
  }

  if (nrow(studyDays) == 0) {
    ir <- dplyr::tibble() #return empty table
  }


  if (nrow(studyDays) > 0) {
    # fetch incidence rates looping through each time interval
    ir <- list()
    for (i in seq_len(nrow(studyDays))) {
      workingStartTime <- studyDays$start_time[i]
      workingEndTime <- studyDays$end_time[i]
      
      year_i <- as.integer(format(workingStartTime, "%Y"))
      days_in_year <- if (lubridate::leap_year(year_i)) 366 else 365
      
      if (interval == "overall"){
        n_persons_denom = denomPop %>% 
          dplyr::summarise(n_mean = as.integer(mean(population))) %>% 
          dplyr::pull(n_mean)
        person_days <-  denomPop %>% 
          mutate(person_days = population * if_else(lubridate::leap_year(year), 366, 365)) %>%
          summarise(total_person_days = sum(person_days)) %>%
          pull(total_person_days)
      } else{
        n_persons_denom = denomPop %>% 
          dplyr::filter(year == year_i) %>% 
          dplyr::pull(population)
        person_days <-  n_persons_denom * days_in_year
      }

      if (n_persons_denom > 0) {
        # Remove outcome_start_date if not during period
        workingPop <- studyPopOutcome %>%
          dplyr::mutate(outcome_start_date = dplyr::if_else(
            .data$outcome_start_date <= .env$workingEndTime &
              .data$outcome_start_date >= .env$workingStartTime,
            as.Date(.data$outcome_start_date),
            as.Date(NA)
          )) %>% 
          dplyr::filter(!is.na(.data$outcome_start_date)) %>%
          mutate(remaining_days = as.integer(workingEndTime - cohort_end_date))

        ir[[paste0(i)]] <- workingPop %>%
          # dplyr::filter(!is.na(.data$outcome_start_date)) %>%
          dplyr::summarise(
            n_events = n(), 
            n_days_remaining = sum(remaining_days, na.rm = TRUE)
          ) %>%
          dplyr::mutate(n_persons = .env$n_persons_denom) %>%
          dplyr::mutate(person_days = .env$person_days - .data$n_days_remaining) %>%
          dplyr::mutate(incidence_start_date = .env$workingStartTime) %>%
          dplyr::mutate(incidence_end_date = .env$workingEndTime) %>%
          select(-c("n_days_remaining")) %>% 
          collect()
        
        # print(paste0(analysisId, " year " ,year_i, " count ", ir[[paste0(i)]] %>% pull(n_events), 
        # ". Washout: ",outcomeWashout, ", RE:", repeatedEvents, ", Int: ", interval))

        ir[[paste0(i)]] <- ir[[paste0(i)]] %>%
          dplyr::mutate(strata_name = "Overall", strata_level = "Overall")
        
      } else{
        
        ir[[paste0(i)]] <- dplyr::tibble(
          n_events=0, 
          n_persons =0, 
          person_days =0,
          incidence_start_date = .env$workingStartTime,
          incidence_end_date = .env$workingEndTime,
          strata_name = "Overall", 
          strata_level = "Overall"
          )         
        warning(paste0("No population data for denominator id ", denominatorCohortId, " during year ", year_i))
      }
    }

    ir <- dplyr::bind_rows(ir) %>%
      dplyr::mutate(person_years = round(.data$person_days / 365.25, 3)) %>%
      dplyr::mutate(
        incidence_100000_pys =
          round(((.data$n_events / .data$person_years) * 100000), 3)
      )
  }

  # study design related variables
  if(is.null(outcomeWashout)){
    outcomeWashout <- "inf"
  }
  analysisSettings <- dplyr::tibble(
    analysis_outcome_washout = .env$outcomeWashout,
    analysis_repeated_events = .env$repeatedEvents,
    analysis_interval = .env$interval,
    analysis_complete_database_intervals = TRUE
  )

  # return list
  results <- list()
  results[["ir"]] <- ir
  results[["analysis_settings"]] <- analysisSettings
  results[["attrition"]] <- attrition

  return(results)
}

