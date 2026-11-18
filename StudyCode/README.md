# Temporal trends in epidemiology and patient characteristics of 36 cancers: A multinational population-based cohort study using OMOP databases to investigate CANcer (OMOPCAN)
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Cohort study**
- Tags: **@andreapistillo @irelopsan**
- Study lead: **Irene Lopez Sanchez, Elena Roel Herranz, Talita Duarte-Salles**
- Study lead forums tag: **@irelopsan, @andreapistillo**

The main aim of this project is to serve as a proof-of-concept for using real-world data mapped to the Observational Medical Outcomes Partnership (OMOP) common data model (CDM) to monitor cancer epidemiology over time and characterise patients’ clinical history and outcomes.

## What's new in Version 2

1. Updated packages that were causing issues for some data partners, particularly dbplyr and CohortSurvival
2. Improved the smoking status definitions
3. Created a new HPV definition based on measurements for stratified analyses
4. Removed the exclusion of the opposite sex in sex-specific cancer cohorts
5. Fixed an issue affecting characteristic stratification in the incidence and survival analyses
6. Added broader age categories (0-19, 20-49, and ≥50 years), which will be used for characterisation, incidence, prevalence, and survival analyses. This update also applies to cancer registries.
7. Incorporated age standardisation for incidence and survival analyses.
8. Improved error handling through the use of tryCatch, making the study code more robust against execution failures.
9. Updated the Shiny application. Visualisations of age-standardised incidence and survival results are still under development; in the meantime, these outputs can be reviewed in the generated CSV files.

## How to run the study

**Although the code is optimised for efficiency, it may still require considerable time and server memory. If possible, we recommend running it during periods of low server activity.**

1. Download or clone this repository. You can do this by selecting Code > Download ZIP or by using GitHub Desktop.
2. Open the project OMOPCAN_Study_v2.Rproj in RStudio. When the project is open, its name will appear in the top-right corner of your RStudio session:
  - Install the necessary packages for the study by activating and restoring the renv. 
  - Open and work through the CodeToRun.R, which is the primary file you'll need. Execute the lines in the file, adding your database-specific information as instructed in the comments (database name, data source type and wheather the database contains mortality data to perform the survival analysis). Follow the instructions to run the script.
3. After running the analysis, you will find a results folder with one subfolder per each cancer group.

*If your database is TMUCRD, please run CodeToRun_TMUCRD.R. 

## Exploring results with the Shiny application

After running the study code:
1. Open the R project located in the shiny folder.
2. Restore the environment using renv.
3. Open global.R.
4. Click Run App.
A "+" icon is available in the upper-right corner of each tab, providing guidance on the outputs that should be reviewed.



