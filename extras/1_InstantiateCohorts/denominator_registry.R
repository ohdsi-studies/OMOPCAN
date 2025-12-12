# Generate denominator 
log4r::info(logger, "Read registry population counts")
tablePopulation <- readDenominatorCounts(file= filepop)

denominator_counts <- getRegistryDenominator(
  data_input = tablePopulation,
  # region_input = registryRegion,
  start_year_input = lubridate::year(startdate),
  end_year_input = lubridate::year(enddate),
  strata_age = TRUE ,
  strata_sex = TRUE
)

log4r::info(logger, "Insert registry population counts in the cdm")
cdm <- omopgenerics::insertTable(
  cdm, name="denominator_counts", 
  denominator_counts %>% 
    dplyr::select(cohort_definition_id, year, sex, age_gr, age_gr_n, population), 
  overwrite = TRUE, 
  temporary = FALSE
)

log4r::info(logger, "Extract sex and age groups from registry population counts")
sexGroup = sort(unique(denominator_counts$sex))
ageGroup = denominator_counts %>% distinct(age_gr_n, age_gr) %>% arrange(age_gr_n) %>% pull(age_gr)
ageGroup = ageGroup[!grepl("overall", ageGroup, ignore.case=TRUE)]
ageGroupList <- setNames(
  lapply(ageGroup, function(g) {
    if (stringr::str_detect(g, "to") || stringr::str_detect(g, "-")) {
      nums <- as.numeric(stringr::str_split(g, " to ", simplify = TRUE))
      return(nums)
    }
    if (stringr::str_detect(g, "over") || stringr::str_detect(g, "+")) {
      x <- as.numeric(stringr::str_extract(g, "\\d+"))
      return(c(x, 150))
    }
    return(NA)
  }),
  ageGroup
)
