# Generate cancer cohorts ------------------------------------------------------
log4r::info(logger, "Instantiate cancer cohorts")
#Instantiate all cancers
all_cancer_concepts <- CDMConnector::readCohortSet(
  path = here::here("extras", "1_InstantiateCohorts", "Cohorts", "AllCancers" ))
cdm <- CDMConnector::generateCohortSet(
  cdm,
  cohortSet = all_cancer_concepts,
  name = "outcome_all",
  overwrite = TRUE
  )

# instantiate exclusion cohort -------------------------------------------------
log4r::info(logger, "Instantiate exclusion")
all_cancer_codes <- CodelistGenerator::codesFromCohort(
  path = here::here("extras", "1_InstantiateCohorts", "Cohorts", "AllCancers") ,
  cdm = cdm)
# any prior history of malignancy
codelistExclusion <- CodelistGenerator::codesFromConceptSet(
  here::here("extras", "1_InstantiateCohorts", "Cohorts", "Exclusion"), 
  cdm
)

#Exluir codigos de non_melanoma_broad !!



codelistExclusion <- list(Reduce(union_all, c(all_cancer_codes, codelistExclusion)))
names(codelistExclusion) <- "anymalignancy"
cdm <- CDMConnector::generateConceptCohortSet(
  cdm = cdm,
  conceptSet = codelistExclusion,
  name = "exclusion",
  limit = "all",
  overwrite = TRUE
)

# instantiate covariables --------------------------------------------------------
cdm <- omopgenerics::emptyCohortTable(cdm, name="conditions_all", overwrite = TRUE)

if(!isRegistry){
  log4r::info(logger, "Instantiate conditions")
  codelistConditions <- CodelistGenerator::codesFromConceptSet(
    here::here("extras","1_InstantiateCohorts","Cohorts","Conditions"),
    cdm
  )
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm, 
    conceptSet = codelistConditions,
    name = "conditions",
    overwrite = TRUE
  )
  
  # instantiate obesity using diagnosis and measurements and bind to "conditions_all"
  log4r::info(logger, "Instantiate obesity BMI")
  obesity_cohorts <- CDMConnector::readCohortSet(
    here::here("extras","1_InstantiateCohorts","Cohorts","Obesity")
  )
  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm,
    cohortSet = obesity_cohorts,
    name = "obesity",
    computeAttrition = TRUE,
    overwrite = TRUE
  )
  cdm <- cdm$conditions %>%
    omopgenerics::bind(cdm$obesity, name="conditions_all")
  
  # instantiate menopausal status  and bind to "conditions_all"
  log4r::info(logger, "Instantiate menopausal status")
  cdm[["menopausal_status"]] <- cdm %>% 
    CohortConstructor::demographicsCohort(
      name = "menopausal_status",
      ageRange = c(50, 150),
      sex = c("Female"),
      minPriorObservation = daysPriorObservation
    ) 
  cdm[["menopausal_status"]] <- cdm[["menopausal_status"]] %>% 
    omopgenerics::newCohortTable(
      cohortSetRef = tibble::tibble(cohort_definition_id=1L, cohort_name="menopausal_status"),
      cohortAttritionRef = omopgenerics::attrition(cdm[["menopausal_status"]])
    ) %>% 
    compute(name="menopausal_status", overwrite=TRUE, temporary=FALSE)
  cdm <- cdm$conditions_all %>% 
    omopgenerics::bind(cdm$menopausal_status, name="conditions_all")  
  
  # instantiate tobacco smoking and bind to "conditions_all"
  log4r::info(logger, "Instantiate tobacco smoking")
  tobacco_cohorts <- CDMConnector::readCohortSet(
    here::here("extras","1_InstantiateCohorts","Cohorts","Tobacco")
  )
  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm, 
    cohortSet = tobacco_cohorts, 
    name = "tobacco",
    computeAttrition = TRUE,
    overwrite = TRUE
  )
  cdm <- cdm$conditions_all %>% 
    omopgenerics::bind(cdm$tobacco, name="conditions_all")
  
  # instantiate medications -------------------------------------------------------
  log4r::info(logger, "Instantiate medications")
  codelistMedications <- CodelistGenerator::codesFromConceptSet(
    here::here("extras","1_InstantiateCohorts", "Cohorts","Medications"), 
    cdm
  )
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm, 
    conceptSet = codelistMedications,
    gapEra = 30,
    name = "medications"
  )
} 