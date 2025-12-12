getRegistryPrevalence <- function(cdm,
                          denominatorTable,
                          denominatorCohortId,
                          outcomeTable,
                          outcomeCohortId,
                          type,
                          interval,
                          timePoint,
                          tablePrefix,
                          analysisId
                          ) {


  # Get denominator cohort population for entire study period
  denomPop <- denominatorTable %>%
    dplyr::filter(.data$cohort_definition_id ==
                    .env$denominatorCohortId) %>%
    dplyr::select(-"cohort_definition_id")
  
  if (nrow(denomPop) == 0){
    errorCondition("Table with denominator counts is empty.")
  }
  
  studyStartEnd = denomPop %>%
    summarise(max= as.Date(paste0(max(.data$year), "-12-31")),
              min= as.Date(paste0(min(.data$year), "-01-01")))

  # Check which population strata we are considering in the denominator (sex/age)
  sex_strata = denomPop %>% 
    pull(sex) %>% 
    unique()
  sex_strata = ifelse(sex_strata == "Both", NA, sex_strata)
  
  age_strata = denomPop %>% 
    pull(age_gr) %>% 
    unique()
  age_strata = ifelse(age_strata == "Overall", NA, age_strata)

  ## keeping outcome of interest
  studyPop <- cdm[[outcomeTable]] %>%
        dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) %>%
        dplyr::rename("outcome_start_date" = "cohort_start_date",
                      "outcome_end_date" = "cohort_end_date") %>%
        dplyr::select(subject_id, outcome_start_date, outcome_end_date, age_gr, sex)


  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reasonId = 11,
    reason = "Starting analysis population"
  )

  # Filter outcome cohort based on denominator strata
  if(!is.na(sex_strata)){
    studyPop <- studyPop %>%
      filter(.data$sex == .env$sex_strata)
  }
  if(!is.na(age_strata)){
    studyPop <- studyPop %>%
      filter(.data$age_gr == .env$age_strata)
  }
  
  # Remove outcome if not during period
  studyPop <- studyPop %>%
    dplyr::filter(
      .data$outcome_end_date >= as.Date(studyStartEnd$min),
      .data$outcome_start_date <= as.Date(studyStartEnd$max)
    )
  
  attrition <- recordAttrition(
    table = studyPop,
    id = "subject_id",
    reasonId = 12,
    reason = "Not observed during the complete database interval",
    existingAttrition = attrition
  )

  # get studyDays as a function of inputs
  studyDays <- getStudyDays(
    startDate = as.Date(studyStartEnd$min),
    endDate =  as.Date(studyStartEnd$max),
    timeInterval = interval,
    completeDatabaseIntervals = TRUE,
    type = type,
    timePoint = timePoint
  )

  if (nrow(studyDays) == 0) {
    # if no study days we´ll return an empty tibble
    pr <- dplyr::tibble()

  } else {
    # fetch prevalence
    # looping through each time interval
    pr <- vector(mode = "list", length = length(studyDays$time))

    for (i in seq_along(studyDays$time)) {
      workingStart <- studyDays$start_time[i]
      workingEnd <- studyDays$end_time[i]

      year_i <- as.integer(format(workingStart, "%Y"))
      
      n_persons_denom = denomPop %>% 
        filter(year == year_i) %>% 
        pull(population)

      if (n_persons_denom > 0) {

        workingPop <- studyPop %>%
          dplyr::mutate(outcome_start_date = dplyr::if_else(
            .data$outcome_start_date <= .env$workingEnd &
              .data$outcome_end_date >= .env$workingStart, 
            as.Date(.data$outcome_start_date),
            as.Date(NA)
          ))

        result <- workingPop %>%
          dplyr::summarise(
            n_persons = .env$n_persons_denom,
            n_cases = dplyr::n_distinct(.data$subject_id[
              !is.na(.data$outcome_start_date) ])
          ) %>% 
          collect()

        pr[[paste0(i)]] <- dplyr::tibble(
          n_population = result$n_persons,
          n_cases = result$n_cases,
          strata_name = "Overall", 
          strata_level = "Overall"
        )

      } else {
        
        pr[[paste0(i)]] <- dplyr::tibble(
          n_population =0, 
          n_cases =0,
          strata_name = "Overall", 
          strata_level = "Overall")
        
        warning(paste0("No population data for denominator id ", denominatorCohortId, " during year ", year_i))
      }

      pr[[paste0(i)]] <- dplyr::tibble(cbind(pr[[paste0(i)]], studyDays[i, ]))

    }

    pr <- dplyr::bind_rows(pr) %>%
      dplyr::mutate(prevalence = .data$n_cases / .data$n_population) %>%
      dplyr::select(dplyr::any_of(c(
        "n_cases", "n_population",
        "prevalence", "start_time", "end_time",
        "strata_name", "strata_level"
      ))) %>%
      dplyr::rename("prevalence_start_date" = "start_time") %>%
      dplyr::rename("prevalence_end_date" = "end_time")
  }


  results <- list()
  results[["pr"]] <- pr
  results[["attrition"]] <- attrition

  return(results)
}

# getStratifiedPrevalenceResult <- function(workingPop, workingStrata){
#
#   # include ongoing in current time of interest
#   result <- workingPop %>%
#     dplyr::group_by(dplyr::pick(.env$workingStrata)) %>%
#     dplyr::summarise(
#       n_persons = dplyr::n_distinct(.data$subject_id),
#       n_cases = dplyr::n_distinct(.data$subject_id[
#         !is.na(.data$outcome_start_date) &
#           .data$outcome_start_date <= .data$cohort_end_date &
#           .data$outcome_end_date >= .data$cohort_start_date
#       ])
#     ) %>%
#     dplyr::ungroup()
#
#
#   result <- result %>%
#     tidyr::unite("strata_level",
#                  c(dplyr::all_of(.env$workingStrata)),
#                  remove = FALSE, sep = " and ") %>%
#     dplyr::mutate(strata_name = !!paste0(workingStrata, collapse = " and ")) %>%
#     dplyr::relocate("strata_level", .after = "strata_name") %>%
#     dplyr::select(!dplyr::any_of(workingStrata))
#
#   return(result)
#
# }
