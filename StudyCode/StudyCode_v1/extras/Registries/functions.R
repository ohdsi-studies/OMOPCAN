getStudyDays <- function(startDate,
                         endDate,
                         timeInterval,
                         completeDatabaseIntervals,
                         type = "period",
                         timePoint = NULL) {
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  
  if (timeInterval == "weeks") {
    weekCorrection <- lubridate::days(1)
  } else {
    weekCorrection <- lubridate::days(0)
  }
  if (type == "point") {
    unit <- substr(timeInterval, 1, nchar(timeInterval) - 1)
    startDay <- lubridate::floor_date(startDate, unit = unit)
    studyDays <- getStudyDaysElements(startDay, endDate, timeInterval) %>%
      dplyr::mutate(start_time = .data$start_time +
                      weekCorrection +
                      switch(timePoint,
                             "start" = lubridate::days(0),
                             "middle" = switch(timeInterval,
                                               "weeks" = lubridate::days(3),
                                               "months" = lubridate::days(14),
                                               "quarters" = months(1) + lubridate::days(14),
                                               "years" = months(6)
                             ),
                             "end" = switch(timeInterval,
                                            "weeks" = lubridate::days(6),
                                            "months" = months(1) - lubridate::days(1),
                                            "quarters" = months(3) - lubridate::days(1),
                                            "years" = lubridate::years(1) - lubridate::days(1)
                             )
                      )) %>%
      dplyr::rename("time" = .env$timeInterval) %>%
      dplyr::mutate(time = as.character(.data$time)) %>%
      dplyr::select("time", "start_time") %>%
      dplyr::mutate(end_time = .data$start_time) %>%
      dplyr::filter(.data$start_time >= startDate) %>%
      dplyr::filter(.data$start_time <= endDate)
  } else {
    studyDays <- getStudyDaysElements(startDate, endDate, "days")
    studyDays <- studyDays %>%
      dplyr::mutate(overall = "overall") %>%
      dplyr::rename("time" = .env$timeInterval) %>%
      dplyr::mutate(time = as.character(.data$time)) %>%
      dplyr::rename("dates" = "start_time") %>%
      dplyr::group_by(.data$time) %>%
      dplyr::summarise(
        start_time = min(.data$dates, na.rm = TRUE),
        end_time = max(.data$dates, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
    if (completeDatabaseIntervals == TRUE) {
      if (timeInterval == "weeks") {
        studyDays <- studyDays %>%
          dplyr::filter(difftime(studyDays$end_time,
                                 studyDays$start_time,
                                 units = "days"
          ) == 6)
      }
      if (timeInterval %in% c("months", "quarters", "years")) {
        studyDays <- studyDays %>%
          dplyr::filter(.data$start_time ==
                          lubridate::floor_date(.data$start_time,
                                                unit = timeInterval
                          ) +
                          weekCorrection) %>%
          dplyr::filter(.data$end_time == lubridate::floor_date(
            .data$end_time,
            unit = timeInterval
          ) + weekCorrection + switch(timeInterval,
                                      "months" = months(1) - lubridate::days(1),
                                      "quarters" = months(3) - lubridate::days(1),
                                      "years" = lubridate::years(1) - lubridate::days(1)
          ))
      }
    }
  }
  return(studyDays)
}

getStudyDaysElements <- function(s, e, i) {
  x <- dplyr::tibble(start_time = seq.Date(
    from = s,
    to = e,
    by = i
  ))
  x <- x %>%
    dplyr::mutate(isoweek = lubridate::isoweek(.data$start_time)) %>%
    dplyr::mutate(month = lubridate::month(.data$start_time)) %>%
    dplyr::mutate(quarter = quarters(.data$start_time)) %>%
    dplyr::mutate(year = lubridate::year(.data$start_time)) %>%
    dplyr::mutate(years = glue::glue("{year}")) %>%
    dplyr::mutate(months = dplyr::if_else(.data$month < 10,
                                          paste0(.data$year, "_0", .data$month),
                                          paste0(.data$year, "_", .data$month)
    )) %>%
    dplyr::mutate(quarters = glue::glue("{year}_{quarter}")) %>%
    dplyr::mutate(
      year =
        dplyr::if_else(.data$month == 1 & .data$isoweek > 50,
                       .data$year - 1,
                       .data$year
        )
    ) %>%
    dplyr::mutate(weeks = dplyr::if_else(.data$isoweek < 10,
                                         paste0(.data$year, "_0", .data$isoweek),
                                         paste0(.data$year, "_", .data$isoweek)
    ))
  return(x)
}

obscureCounts <- function(x,
                          minCellCount = 5,
                          substitute = NA) {
  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_true(is.data.frame(x),
                         add = errorMessage
  )
  checkmate::assertTRUE(
    all(c("n_events", "incidence_100000_pys") %in% names(x)) ||
      all(c("n_cases", "prevalence") %in% names(x))
  )
  checkmate::assertFALSE(
    all(c("n_events", "person_months", "ir") %in% names(x)) &&
      all(c("n_cases", "n_population", "prevalence") %in% names(x))
  )
  checkmate::assert_numeric(minCellCount,
                            add = errorMessage
  )
  checkmate::assertTRUE(is.numeric(substitute) || is.na(substitute))
  
  # report initial assertions
  checkmate::reportAssertions(collection = errorMessage)
  
  if (c("n_events") %in% names(x)) {
    # initialise result_obscurred as FALSE
    # will replace with true below if obscured
    x$cohort_obscured <- "FALSE"
    x$result_obscured <- "FALSE"
    
    x[x$n_persons < minCellCount & x$n_persons > 0, c("cohort_obscured")] <- "TRUE"
    x[x$n_persons < minCellCount & x$n_persons > 0, c(
      "n_persons",
      "person_days",
      "person_years"
    )] <- substitute
    x[x$n_events < minCellCount & x$n_events > 0, c("result_obscured")] <- "TRUE"
    x[x$n_events < minCellCount & x$n_events > 0, c(
      "n_events", "incidence_100000_pys",
      "incidence_100000_pys_95CI_lower",
      "incidence_100000_pys_95CI_upper"
    )] <- substitute
  }
  if (c("n_cases") %in% names(x)) {
    x$population_obscured <- "FALSE"
    x$cases_obscured <- "FALSE"
    x$result_obscured <- "FALSE"
    
    x[x$n_population < minCellCount & x$n_population > 0, c("population_obscured")] <- "TRUE"
    x[x$n_population < minCellCount & x$n_population > 0, c("n_population")] <- substitute
    x[x$n_cases < minCellCount & x$n_cases > 0, c("cases_obscured")] <- "TRUE"
    x[x$n_cases < minCellCount & x$n_cases > 0, c("n_cases")] <- substitute
    x[x$population_obscured == "TRUE" | x$cases_obscured== "TRUE", c("result_obscured")] <- "TRUE"
    x[x$population_obscured == "TRUE" | x$cases_obscured== "TRUE", c(
      "prevalence",
      "prevalence_95CI_lower",
      "prevalence_95CI_upper"
    )] <- substitute
  }
  
  return(x)
}


obscureAttrition <- function(x,
                             minCellCount = 5,
                             substitute = NA){
  x <- x %>%
    dplyr::mutate(number_records = dplyr::if_else(.data$number_records < .env$minCellCount &
                                                    .data$number_records > 0,
                                                  substitute, as.character(.data$number_records)),
                  number_subjects = dplyr::if_else(.data$number_subjects < .env$minCellCount &
                                                     .data$number_subjects > 0,
                                                   substitute, as.character(.data$number_subjects)),
                  excluded_records = dplyr::if_else(.data$excluded_records < .env$minCellCount &
                                                      .data$excluded_records > 0,
                                                    substitute, as.character(.data$excluded_records)),
                  excluded_subjects = dplyr::if_else(.data$excluded_subjects < .env$minCellCount &
                                                       .data$excluded_subjects > 0,
                                                     substitute, as.character(.data$excluded_subjects)))
  
}


recordAttrition <- function(table,
                            id = "person_id",
                            existingAttrition = NULL,
                            reasonId,
                            reason) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(any(class(table) %in%
                              c("tbl_dbi", "tbl", "data.frame", "tibble")))
  checkmate::assertCharacter(id, add = errorMessage)
  checkmate::assertIntegerish(reasonId, add = errorMessage)
  checkmate::assertCharacter(reason, null.ok = TRUE, add = errorMessage)
  if (!is.null(existingAttrition)) {
    checkmate::assertTRUE(any(class(existingAttrition) %in%
                                c("data.frame", "tbl")))
  }
  checkmate::reportAssertions(collection = errorMessage)
  attrition <- dplyr::tibble(
    number_records = as.integer(table %>%
                                  dplyr::tally() %>%
                                  dplyr::pull()),
    number_subjects = as.integer(table %>%
                                   dplyr::select(.env$id) %>%
                                   dplyr::distinct() %>%
                                   dplyr::tally() %>%
                                   dplyr::pull()),
    reason_id = as.integer(.env$reasonId),
    reason = .env$reason
  )
  
  if (!is.null(existingAttrition)) {
    attrition <- dplyr::bind_rows(existingAttrition, attrition) %>%
      dplyr::mutate(
        excluded_records =
          as.integer(dplyr::lag(.data$number_records) - .data$number_records)
      ) %>%
      dplyr::mutate(
        excluded_subjects =
          as.integer(dplyr::lag(.data$number_subjects) - .data$number_subjects)
      )
  } else {
    attrition <- attrition %>%
      dplyr::mutate(excluded_records = NA_integer_) %>%
      dplyr::mutate(excluded_subjects = NA_integer_)
  }
  
  return(attrition)
}



