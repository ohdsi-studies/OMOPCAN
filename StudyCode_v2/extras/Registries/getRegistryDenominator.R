getRegistryDenominator <- function(data_input =NULL,
                                   region_input=NULL,
                                   start_year_input,
                                   end_year_input = NULL,
                                   strata_age = TRUE,
                                   strata_sex = TRUE){
  
  start_year_input = as.integer(start_year_input)
  if(!is.null(end_year_input)){
    end_year_input  = as.integer(end_year_input)
  }else{
    end_year_input  = as.integer(start_year_input)
  }
  
  # Read input table or search region into eurostat data
  if (is.null(data_input)){
    eurost_df_long <- readRDS(file = "./data_eurostat/eurostat_data.rds")
    filtered_data_y <- eurost_df_long %>% 
      dplyr::filter(
        grepl(
          .env$region_input, 
          .data$City, 
          ignore.case = TRUE
          )
        )%>% 
      dplyr::mutate(dplyr::across(where(is.character), tolower))
    if (nrow(filtered_data_y)==0){
      errorCondition("No available data for provided region.")
    }
  } else {
    filtered_data_y <- data_input %>% 
      dplyr::mutate(
        Country = Region, 
        City = Region, 
        CityCode = Region, 
        CountryFlag = 1
        ) %>% 
      dplyr::select(-Region) %>% 
      dplyr::mutate(dplyr::across(where(is.character), tolower))
    if (nrow(filtered_data_y)==0){
      errorCondition("Input data seems to have wrong format. Check documentation.")
    }
  }

  if (nrow(filtered_data_y) == 0) {
    errorCondition("No available data for provided region / wrong input data.")
  } else {
    years_with_overall_data <- filtered_data_y %>%
      dplyr::filter(
        .data$AgeGroup == "overall" & 
          .data$Sex == "overall" & 
          !is.na(.data$Population)
        ) %>%
      dplyr::distinct(Year)
    if (nrow(years_with_overall_data)==0){
      errorCondition("No overall population counts.")
    }

    #Check if age stratified counts is continuous and complete over age ranges
    AgeRangesCont <- filtered_data_y %>%
      dplyr::group_by(.data$Year) %>%
      dplyr::filter(!is.na(.data$Population))  %>%
      dplyr::mutate(
        LowerBound = dplyr::case_when(
          .data$AgeGroup == "overall" ~ -1,
          TRUE ~ as.numeric(stringr::str_extract(.data$AgeGroup, "^\\d+"))
        ),
        UpperBound = dplyr::case_when(
          .data$AgeGroup == "overall" ~ -1,
          stringr::str_detect(.data$AgeGroup, "\\+|inf")  ~ Inf,
          stringr::str_detect(AgeGroup, "to|-|_|/") ~
            as.numeric(stringr::str_extract(AgeGroup, "(?<=to |-|_|/)\\d+")),
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::arrange(.data$Year, .data$LowerBound) %>%
      dplyr::distinct(.data$Year, .data$LowerBound, .data$UpperBound, .keep_all = TRUE) %>%
      dplyr::mutate(
        AgeCont = ifelse(
        row_number() == 1 | 
          .data$LowerBound - lag(.data$UpperBound, default = first(.data$UpperBound)) == 1 | 
          .data$LowerBound - lag(.data$UpperBound, default = first(.data$UpperBound)) == 0,
        TRUE, 
        FALSE
        )
      )%>%
      dplyr::mutate(AgeCont = ifelse(any(.data$AgeCont == FALSE), FALSE, TRUE)) %>%
      dplyr::ungroup() %>% 
      select(Year, AgeGroup, LowerBound, UpperBound, AgeCont) 
    

    if (!any(AgeRangesCont$AgeCont)){
      warning("Counts stratified by age groups is not available or incorrectly formatted.")
    }
    
    years_with_age_data = tibble::tibble()
    years_with_sex_data = tibble::tibble()

    if (strata_age & any(AgeRangesCont$AgeCont)){
      # Add variable CompleteData=1 if data is complete for all age ranges
      filtered_data_y = filtered_data_y %>%
        dplyr::group_by(.data$Year) %>%
        dplyr::filter(!is.na(.data$Population))  %>%
        dplyr::left_join(AgeRangesCont, by=c("Year", "AgeGroup")) %>%
        dplyr::mutate(
          overall_pop = any(stringr::str_detect(.data$AgeGroup, "overall") & 
                  stringr::str_detect(.data$Sex, "overall") &
                  !is.na(.data$Population)
                ),
          str_age = any((stringr::str_detect(.data$AgeGroup, "0|\\+|inf") |
                  (stringr::str_detect(.data$AgeGroup, "\\d+") & 
                     as.numeric(stringr::str_extract(.data$AgeGroup, "\\d+")) >= 100)) &
                !stringr::str_detect(.data$AgeGroup, "overall") &
                !is.na(.data$Population)
            )
        ) %>%
        dplyr::mutate(
          CompleteData = 
            ifelse(.data$str_age & .data$overall_pop & .data$AgeCont,  1, 0)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-overall_pop, -str_age, -AgeCont )

      # See which years have complete counts for age stratification
      years_with_age_data <- filtered_data_y %>%
        dplyr::filter(.data$CompleteData==1) %>%
        dplyr::distinct(.data$Year) %>%
        dplyr::filter(.data$Year %in% years_with_overall_data$Year)
      
      if (nrow(years_with_age_data)==0){
        warning("Counts stratified by age groups are not available for complete age ranges or incorrectly formatted.")
      }
    }

    if (strata_sex){
      # See which years have complete counts for sex stratification
      years_with_sex_data <- filtered_data_y %>%
        dplyr::group_by(.data$Year) %>%
        dplyr::filter(.data$Sex!= "overall" & !is.na(.data$Population)) %>%
        dplyr::distinct(.data$Year) %>%
        dplyr::filter(.data$Year %in% years_with_overall_data$Year) %>%
        dplyr::ungroup()
      
      if (nrow(years_with_sex_data)==0){
        warning("Data stratified by sex is not available or incorrectly formatted.")
      }
    }
    
    filtered_data_all = c()

    for (year_i in start_year_input:end_year_input) {
      
      if (strata_age & nrow(years_with_age_data)>0){
        # Select closest year with complete data to the input_year
        closest_year <- years_with_age_data %>%
          dplyr::mutate(year_diff = abs(.data$Year - year_i )) %>%
          dplyr::arrange(.data$year_diff) %>%
          dplyr::slice(1) %>%
          dplyr::pull(.data$Year)

        # Select dataframe for closest_year and add AgeGroupsN 
        filtered_data = filtered_data_y %>%
          dplyr::filter(.data$Year == .env$closest_year) %>%
          dplyr::rename(DataPopYear = Year ) %>%
          dplyr::mutate(Year = year_i) %>%
          dplyr::select(-CompleteData, -CountryFlag, -CityCode) %>%
          dplyr::arrange(.data$LowerBound, .data$UpperBound) %>%
          dplyr::mutate(AgeGroupsN = dense_rank(.data$LowerBound) - 1) %>%
          dplyr::select(-LowerBound, -UpperBound)

        # Compute total population
        Pop_total <- filtered_data %>%
          dplyr::filter(.data$Sex == "overall" & .data$AgeGroup =="overall") %>%
          dplyr::pull(.data$Population)

        if(strata_sex & nrow(years_with_sex_data)>0) {
          # Compute population percentages of total population (sex/age groups)
          filtered_data <- filtered_data %>%
            dplyr::group_by(.data$AgeGroup, .data$Sex) %>%
            dplyr::mutate(pop_perc_of_total = .data$Population / Pop_total * 100) %>%
            dplyr::ungroup()

        } else {
          # Compute population percentages of total population (Age groups)
          filtered_data <- filtered_data %>%
            dplyr::filter(.data$Sex == "overall") %>%
            dplyr::group_by(.data$AgeGroup) %>%
            dplyr::mutate(pop_perc_of_total = .data$Population / Pop_total * 100) %>%
            dplyr::ungroup()
        }
      } else {

        if(strata_sex & nrow(years_with_sex_data)>0) {
          # Select closest year with sex data to the input_year
          closest_year <- years_with_sex_data %>%
            dplyr::mutate(year_diff = abs(.data$Year - year_i )) %>%
            dplyr::arrange(.data$year_diff) %>%
            dplyr::filter(row_number() == 1) %>%
            dplyr::pull(.data$Year)

          filtered_data <- filtered_data_y %>%
            dplyr::filter(.data$AgeGroup == "overall" & !is.na(.data$Population)) %>%
            dplyr::filter(.data$Year == .env$closest_year) %>%
            dplyr::mutate(AgeGroupsN = 0) %>%
            dplyr::rename(DataPopYear = Year ) %>%
            dplyr::mutate(Year = year_i)

          # Compute total population
          Pop_total <- filtered_data %>%
            dplyr::filter(.data$Sex == "overall") %>%
            dplyr::pull(.data$Population)

          # Compute population percentages of total population (sex)
          filtered_data <- filtered_data %>%
            dplyr::group_by(.data$Sex) %>%
            dplyr::mutate(pop_perc_of_total = .data$Population / Pop_total * 100) %>%
            dplyr::ungroup()

        } else {
          # Select closest year with overall data to the input_year
          closest_year <- years_with_overall_data %>%
            dplyr::mutate(year_diff = abs(.data$Year - year_i )) %>%
            dplyr::arrange(.data$year_diff) %>%
            dplyr::slice(1) %>%
            dplyr::pull(.data$Year)

          filtered_data <- filtered_data_y %>%
            dplyr::filter(.data$AgeGroup == "overall" & 
                   .data$Sex == "overall" & 
                   !is.na(.data$Population)) %>%
            dplyr::filter(Year == .env$closest_year) %>%
            dplyr::mutate(AgeGroupsN = 0) %>%
            dplyr::rename(DataPopYear = Year ) %>%
            dplyr::mutate(Year = year_i) %>%
            dplyr::mutate(pop_perc_of_total = 100)
        }
      }
      # Bind all years into single table
      filtered_data_all = dplyr::bind_rows(filtered_data_all, filtered_data)
    }
    
    # Format table
    filtered_data_all <- filtered_data_all %>%
      dplyr::rename(
        region = City, 
        year = Year, 
        datapopyear=DataPopYear, 
        sex=Sex,
        age_gr=AgeGroup, 
        age_gr_n = AgeGroupsN, 
        population=Population
        ) %>%
      dplyr::mutate(sex = if_else(sex == "overall", "both", sex)) %>%
      dplyr::mutate(
        sex = stringr::str_to_title(sex),
        age_gr = ifelse(age_gr== "overall", stringr::str_to_title(age_gr),age_gr)
      )%>%
      dplyr::select(region, year, datapopyear, sex, age_gr, age_gr_n, 
             population, pop_perc_of_total)
    
    # Create cohort definition id for each denominator
    strataCombinations <-  base::expand.grid(
        sex = dplyr::distinct(filtered_data_all, sex) %>% 
          dplyr::pull(sex),
        age_gr = dplyr::distinct(filtered_data_all, age_gr) %>% 
          dplyr::pull(age_gr)
      ) %>%
      dplyr::mutate(cohort_definition_id = row_number())

    # #Define to which cohort denominators contribute each subject (select cohort_denom_ids)
    # strataCombinations <- strataCombinations %>%
    #   dplyr::mutate(
    #     all_cohort_ids = as.character(
    #       purrr::map2(
    #       .data$age_gr, .data$sex,
    #       ~ c(
    #         strataCombinations %>%
    #           dplyr::filter(.data$age_gr== "Overall" & .data$sex == "Both") %>%
    #           dplyr::pull(cohort_definition_id),
    #         strataCombinations %>%
    #           dplyr::filter(.data$age_gr== "Overall" & .data$sex == .y) %>%
    #           dplyr::pull(cohort_definition_id),
    #         strataCombinations %>%
    #           dplyr::filter(.data$age_gr == .x) %>%
    #           dplyr::filter(.data$sex == "Both" | .data$sex == .y) %>%
    #           dplyr::pull(cohort_definition_id)
    #       ) %>% 
    #         unique()
    #       )
    #     )
    #   )
    
    denominatorTable <- filtered_data_all %>%
      dplyr::left_join(strataCombinations, by=c("age_gr", "sex"))

    return(denominatorTable)
  }
}



