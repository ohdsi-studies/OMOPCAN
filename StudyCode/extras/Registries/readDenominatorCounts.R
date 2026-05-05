readDenominatorCounts <- function(file) {
  
  tableInput <- tryCatch(
    { 
      if(grepl("\\.xlsx$", file, ignore.case = TRUE)) {
        readxl::read_xlsx(path = file)
      } else if (grepl("\\.csv$", file, ignore.case = TRUE)) {
        readr::read_csv(file)
      } else {
        stop("No .xlsx or .csv file found.")
      }
    },
    error = function(e) {
      message("Error reading registry population: ",e$message)
      return(NULL) 
    }
  )
  tablePopulation <- NULL
  if(!is.null(tableInput)){
    colnames_raw <- tolower(names(tableInput))
    col_map <- list(
      Region     = which(stringr::str_detect(colnames_raw, "region|area|country|city|name")),
      Year       = which(stringr::str_detect(colnames_raw, "year|period")),
      Sex        = which(stringr::str_detect(colnames_raw, "sex|gender")),
      AgeGroup   = which(stringr::str_detect(colnames_raw, "age") & stringr::str_detect(colnames_raw, "gr")),
      Population = which(stringr::str_detect(colnames_raw, "pop|count"))
    )
    required_cols <- c("Year", "Sex", "AgeGroup", "Population")
    missing_required <- sapply(col_map[required_cols], length) == 0
    if (any(missing_required)) {
      stop("Missing required columns: ", paste(names(which(missing_required)), collapse = ", "))
    }
    if (length(col_map$Region) == 0) {
      tableInput$Region <- attr(cdm, "cdm_name")
      col_map$Region <- which(names(tableInput) == "Region")
    }
    #Rename and format table
    tablePopulation <- tableInput %>%
      select(
        Region     = all_of(names(tableInput)[col_map$Region[1]]),
        Year       = all_of(names(tableInput)[col_map$Year[1]]),
        Sex        = all_of(names(tableInput)[col_map$Sex[1]]),
        AgeGroup   = all_of(names(tableInput)[col_map$AgeGroup[1]]),
        Population = all_of(names(tableInput)[col_map$Population[1]])
      ) %>%
      mutate(
        Year = as.integer(Year),
        Population = as.numeric(Population),
        Sex = dplyr::case_when(
          tolower(Sex) == "both" ~ "overall",
          TRUE ~ tolower(Sex)
        ),
        AgeGroup = tolower(AgeGroup)
      )
    # Validación de Sex and AgeGroup
    sex_values <- unique(tablePopulation$Sex)
    invalid_sex <- setdiff(sex_values, c("overall", "male", "female"))
    if (length(invalid_sex) > 0) {
      warning("Invalid values found in 'Sex': ", paste(invalid_sex, collapse = ", "))
    }
    if (!any(tablePopulation$AgeGroup == "overall")) {
      warning("The 'AgeGroup' column must include at least one 'overall' category.")
    }
  }
  return(tablePopulation)
}
