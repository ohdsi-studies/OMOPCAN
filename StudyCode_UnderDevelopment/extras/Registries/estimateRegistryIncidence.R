estimateRegistryIncidence <- function(cdm,
                              denominatorTableName,
                              outcomeTable,
                              denominatorCohortId = NULL,
                              outcomeCohortId = NULL,
                              interval = "years",
                              outcomeWashout = Inf,
                              repeatedEvents = FALSE,
                              minCellCount = 5) {

  startCollect <- Sys.time()

  tablePrefix <- paste0(
    sample(letters, 5, TRUE) |> paste0(collapse = ""), "_inc"
  )

  # help to avoid formatting errors
  if (is.character(interval)) {
    interval <- tolower(interval)
  }

  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- omopgenerics::cohortCount(cdm[[outcomeTable]]) %>%
      dplyr::pull("cohort_definition_id") %>% 
      sort()
  }

  ## add outcome from attribute
  outcomeRef <- omopgenerics::settings(cdm[[outcomeTable]]) %>%
    dplyr::filter(.env$outcomeCohortId %in% .data$cohort_definition_id) %>%
    dplyr::collect("cohort_definition_id", "cohort_name") %>%
    dplyr::rename("outcome_cohort_id" = "cohort_definition_id",
                  "outcome_cohort_name" = "cohort_name")

  if(nrow(outcomeRef) == 0){
    cli::cli_abort(message = c("Specified outcome IDs not found in the cohort set of
                    {paste0('cdm$', outcomeTable)}",
                               "i" = "Run CDMConnector::cohort_set({paste0('cdm$', outcomeTable)})
                   to check which IDs exist"))
  }
  
  # Check outcome table columns
  requiredCols <- c("cohort_definition_id", "subject_id", "cohort_start_date", 
                    "cohort_end_date", "age_gr", "sex")
  missingCols <- setdiff(requiredCols, colnames(cdm[[outcomeTable]]))
  
  if (length(missingCols) > 0) {
    errorCondition(paste0(
      "The following required columns are missing from outcome table: ",
      paste(missingCols, collapse = ", ")
    ))
  }
  
  # Get denominator counts
  if (!omopgenerics::isTableEmpty(cdm[[denominatorTableName]])){
    # if not given, use all denominator and outcome cohorts
    if (is.null(denominatorCohortId)) {
      denominatorCohortId <- cdm[[denominatorTableName]] %>%
        dplyr::filter(!is.na(population) & population > 0 ) %>%
        dplyr::distinct(cohort_definition_id) %>%
        dplyr::pull("cohort_definition_id") %>% 
        sort()
    }

    denominatorTable <- cdm[[denominatorTableName]] %>%
      dplyr::filter(cohort_definition_id %in% denominatorCohortId) %>% 
      dplyr::collect()

    #It works for complete year intervals
    studyStartEnd = denominatorTable %>%
      summarise(max= as.Date(paste0(max(.data$year), "-12-31")),
                min= as.Date(paste0(min(.data$year), "-01-01")))

    denominator_settings <- denominatorTable %>%
      select(-population, -year) %>%
      mutate(cohort_name = paste0("denominator_cohort_", cohort_definition_id),
             days_prior_observation = 0,
             start_date = studyStartEnd$min,
             end_date = studyStartEnd$max) %>%
      distinct()
  }

  # Get outcomes in selected outcome ids and rename variables
  cdm[[paste0(tablePrefix, "_inc_1")]] <- cdm[[outcomeTable]] %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$outcomeCohortId) %>%
    dplyr::rename(
      "outcome_cohort_id" = "cohort_definition_id",
      "outcome_start_date" = "cohort_start_date",
      "outcome_end_date" = "cohort_end_date"
    ) %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_1"),
      temporary = FALSE,
      overwrite = TRUE
    )


  # Join outcomes during study period with the latest outcome before study period (if any)
  cdm[[paste0(tablePrefix, "_inc_2")]] <- cdm[[paste0(tablePrefix, "_inc_1")]] %>%
    # most recent outcome starting before cohort start per person
    dplyr::filter(.data$outcome_start_date < studyStartEnd$min) %>%
    dplyr::group_by(
      .data$subject_id,
      .data$outcome_cohort_id
    ) %>%
    dplyr::filter(.data$outcome_start_date ==
                    max(.data$outcome_start_date, na.rm = TRUE)) %>%
    dplyr::union_all(
      # all starting during cohort period
      cdm[[paste0(tablePrefix, "_inc_1")]]   %>%
        dplyr::filter(.data$outcome_start_date >= studyStartEnd$min) %>%
        dplyr::filter(.data$outcome_start_date <= studyStartEnd$max)
    ) %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_2"),
      temporary = FALSE,
      overwrite = TRUE
    )

  # Order multiple outcomes for the same subject/outcome_id based on outcome start date
  cdm[[paste0(tablePrefix, "_inc_3")]] <-  cdm[[paste0(tablePrefix, "_inc_2")]] %>%
    dplyr::group_by(
      .data$subject_id,
      .data$outcome_cohort_id
    ) %>%
    dbplyr::window_order(.data$outcome_start_date) %>%
    dplyr::mutate(index = rank()) %>%
    dplyr::ungroup() %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_3"),
      temporary = FALSE,
      overwrite = TRUE
    )

  # Create "outcome_prev_end_date" for consecutive outcomes (then apply washout)
  cdm[[paste0(tablePrefix, "_inc_4")]] <- cdm[[paste0(tablePrefix, "_inc_3")]] %>%
    dplyr::select(
      c(outcome_cohort_id, subject_id, outcome_start_date, outcome_end_date, age_gr, sex, index)
    ) %>%
    dplyr::full_join(
      cdm[[paste0(tablePrefix, "_inc_3")]] %>%
        dplyr::select(
          c(outcome_cohort_id, subject_id, outcome_prev_end_date = outcome_end_date, index, sex)
        ) %>%
        dplyr::mutate(index = .data$index + 1) ,
      by = c("outcome_cohort_id", "subject_id",  "index", "sex")
    ) %>%
    dplyr::group_by(outcome_cohort_id, subject_id) %>%
    dplyr::arrange(outcome_cohort_id, subject_id, index) %>%
    tidyr::fill(age_gr, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::select(-"index")%>%
    #CHECK THIS!!! should we filter out NA start dates?? filter(!is.na(outcome_start_date))
    dplyr::distinct() %>%
    dplyr::compute(
      name = paste0(tablePrefix, "_inc_4"),
      temporary = FALSE,
      overwrite = TRUE
    )

  studySpecs <- tidyr::expand_grid(
    outcome_cohort_id = outcomeCohortId,
    denominator_cohort_id = denominatorCohortId,
    interval = interval,
    complete_database_intervals = TRUE,
    outcome_washout = outcomeWashout,
    repeated_events = repeatedEvents
  )
  if (any(is.infinite(outcomeWashout))) {
    studySpecs$outcome_washout[
      which(is.infinite(studySpecs$outcome_washout))
    ] <- NA
  }
  studySpecs <- studySpecs %>%
    dplyr::mutate(analysis_id = as.character(dplyr::row_number()))
  studySpecs <- split(
    studySpecs,
    studySpecs[, c("analysis_id")]
  )

  # get irs
  counter <- 0
  irsList <- lapply(studySpecs, function(x) {
    counter <<- counter + 1
    message(glue::glue(
      "Getting incidence for analysis {counter} of {length(studySpecs)}"
    ))

    workingInc <- getRegistryIncidence(
      cdm = cdm,
      denominatorTable = denominatorTable,
      denominatorCohortId = x$denominator_cohort_id,
      outcomeTable = paste0(tablePrefix, "_inc_4"),
      outcomeCohortId = x$outcome_cohort_id,
      interval = x$interval,
      outcomeWashout = x$outcome_washout,
      repeatedEvents = x$repeated_events,
      tablePrefix = tablePrefix,
      analysisId = x$analysis_id
    )

    workingIncIr <- workingInc[["ir"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    workingIncAnalysisSettings <- workingInc[["analysis_settings"]] %>%
      dplyr::mutate(
        outcome_cohort_id = x$outcome_cohort_id,
        denominator_cohort_id = x$denominator_cohort_id,
        analysis_min_cell_count = .env$minCellCount,
        analysis_id = x$analysis_id
      ) %>%
      dplyr::relocate("analysis_id") %>%
      dplyr::mutate(analysis_outcome_washout = as.character(.data$analysis_outcome_washout))

    workingIncAttrition <- workingInc[["attrition"]] %>%
      dplyr::mutate(analysis_id = x$analysis_id) %>%
      dplyr::relocate("analysis_id")

    result <- list()
    result[["ir"]] <- workingIncIr
    result[["analysis_settings"]] <- workingIncAnalysisSettings
    result[["attrition"]] <- workingIncAttrition


    return(result)
  })

  irsList <- purrr::flatten(irsList)

  # analysis settings
  analysisSettings <- irsList[names(irsList) == "analysis_settings"]
  analysisSettings <- dplyr::bind_rows(analysisSettings,.id = NULL)
  analysisSettings <- analysisSettings %>%
    dplyr::left_join(
      # omopgenerics::settings(cdm[[denominatorTable]]) %>%
      denominator_settings %>%
        dplyr::rename("cohort_id" = "cohort_definition_id") %>%
        dplyr::rename_with(
          .cols = dplyr::everything(),
          function(x) {
            paste0("denominator_", x)
          }
        ),
      by = "denominator_cohort_id"
    )

  attrition <- irsList[names(irsList) == "attrition"]
  attrition <- dplyr::bind_rows(attrition,.id = NULL) %>%
    # dplyr::select(-"denominator_cohort_id") %>%
    dplyr::relocate("analysis_id")

  # incidence estimates
  irs <- irsList[names(irsList) == "ir"]
  # to tibble
  irs <- dplyr::bind_rows(irs,.id = NULL)

  # get confidence intervals
  if (nrow(irs) > 0) {
    irs <- irs %>%
      dplyr::bind_cols(incRateCiExact(
        as.double(irs$n_events),
        irs$person_years
      ))
  }

  CDMConnector::dropTable(
    cdm = cdm,
    name = dplyr::starts_with(paste0(tablePrefix, "_inc_"))
  )
  CDMConnector::dropTable(
    cdm = cdm,
    name = dplyr::starts_with(paste0(tablePrefix, "_analysis_"))
  )

  analysisSettings <- analysisSettings %>%
    dplyr::left_join(outcomeRef, by = "outcome_cohort_id") %>%
    dplyr::relocate("outcome_cohort_id", .after = "analysis_id") %>%
    dplyr::relocate("outcome_cohort_name", .after = "outcome_cohort_id") %>%
    dplyr::mutate(cdm_name = attr(cdm, "cdm_name"))

  ## settings
  analysisSettings <- analysisSettings |>
    dplyr::mutate(
      result_id = as.integer(.data$analysis_id),
      result_type = "incidence",
      package_name = "IncidencePrevalenceRegistries",
      package_version = "1.0"
    ) |>
    dplyr::select(!dplyr::ends_with("_cohort_id"))|>
    dplyr::select(!dplyr::ends_with("_cohort_definition_id")) |>
    dplyr::select(c(
      "result_id", "result_type", "package_name", "package_version",
      "analysis_outcome_washout", "analysis_repeated_events",
      "analysis_interval", "analysis_complete_database_intervals"),
      dplyr::starts_with("denominator_"), dplyr::starts_with("outcome_")
    )
  
  ## result
  if (!"strata_name" %in% colnames(irs)) {
    irs <- irs |>
      visOmopResults::uniteStrata()
  }
  if(nrow(irs) == 0){
    irs <- omopgenerics::emptySummarisedResult()
  } else {
    irs <- irs |>
      dplyr::distinct() |>
      dplyr::mutate("analysis_id" = as.integer(.data$analysis_id)) |>
      dplyr::rename(
        "result_id" = "analysis_id",
        "outcome_count" = "n_events",
        "denominator_count" = "n_persons"
      )|>
      dplyr::left_join(
        analysisSettings |>
          dplyr::select(c(
            "result_id", "denominator_cohort_name", "outcome_cohort_name", "analysis_interval"
          )),
        by = "result_id"
      ) |>
      visOmopResults::uniteGroup("denominator_cohort_name") |>
      visOmopResults::uniteAdditional(cols = c("incidence_start_date", "incidence_end_date", "analysis_interval")) |>
      dplyr::rename("variable_level" = "outcome_cohort_name") |>
      dplyr::mutate("variable_name" = "outcome_cohort_name")

    irs <- irs |>
      mutate(across(c(denominator_count, outcome_count, person_days, person_years,
                      incidence_100000_pys, incidence_100000_pys_95CI_lower,
                      incidence_100000_pys_95CI_upper), as.double)) %>%
      tidyr::pivot_longer(
        cols = c("denominator_count", "outcome_count", "person_days", "person_years",
                 "incidence_100000_pys", "incidence_100000_pys_95CI_lower",
                 "incidence_100000_pys_95CI_upper"),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "estimate_value" = as.character(.data$estimate_value),
        "estimate_type" = dplyr::if_else(
          grepl("count", .data$estimate_name), "integer", "numeric"
        ),
        "cdm_name" = attr(cdm, "cdm_name"),
        "strata_name" = dplyr::if_else(.data$strata_name == "Overall", "overall", gsub(" and ", " &&& ", .data$strata_name)),
        "strata_level" = dplyr::if_else(.data$strata_level == "Overall", "overall", gsub(" and ", " &&& ", .data$strata_level))
      )
  }
  
  irs <- omopgenerics::newSummarisedResult(
    x = irs,
    settings = analysisSettings |> 
      dplyr::select(!c("denominator_cohort_name")) |>
      dplyr::mutate(dplyr::across(-"result_id", as.character))
  )

 attritionSR <- attrition |>
    dplyr::distinct() |>
    dplyr::mutate("analysis_id" = as.integer(.data$analysis_id)) |>
    dplyr::rename(
      "result_id" = "analysis_id",
    ) |>
    dplyr::left_join(
      analysisSettings |>
        dplyr::select(c(
          "result_id", "denominator_cohort_name", "outcome_cohort_name"
        )),
      by = "result_id"
    ) |>
   dplyr::rename(
     "variable_level" = "outcome_cohort_name"
   ) |>
    visOmopResults::uniteGroup("denominator_cohort_name") |>
    tidyr::pivot_longer(
      cols = c(
        "number_records", "number_subjects", "excluded_records",
        "excluded_subjects"
      ),
      names_to = "variable_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      "estimate_name" = "count",
      "estimate_value" = as.character(.data$estimate_value),
      "estimate_type" = "integer",
      "cdm_name" = attr(cdm, "cdm_name")
    ) |>
    visOmopResults::uniteStrata("reason") |>
    visOmopResults::uniteAdditional("reason_id") |>
    dplyr::relocate(omopgenerics::resultColumns()) |>
    omopgenerics::newSummarisedResult(settings = analysisSettings |>
                                        dplyr::select(!c("denominator_cohort_name")) |>
                                        dplyr::mutate(result_type = "incidence_attrition") |>
                                        dplyr::mutate(dplyr::across(-"result_id", as.character)))

 irs <- omopgenerics::bind(irs, attritionSR) |>
   omopgenerics::suppress(minCellCount = minCellCount)

  dur <- abs(as.numeric(Sys.time() - startCollect, units = "secs"))
  message(glue::glue(
    "Overall time taken: {floor(dur/60)} mins and {dur %% 60 %/% 1} secs"
  ))

  return(irs)
}


incRateCiExact <- function(ev, pt) {
  return(dplyr::tibble(
    incidence_100000_pys_95CI_lower =
      ((stats::qchisq(p = 0.025, df = 2 * ev) / 2) / pt) * 100000,
    incidence_100000_pys_95CI_upper =
      ((stats::qchisq(p = 0.975, df = 2 * (ev + 1)) / 2) / pt) * 100000
  ))
}

