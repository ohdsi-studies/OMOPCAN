# Time trends in epidemiology and patient characteristics of 34 cancer types
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Cohort study**
- Tags: **-**
- Study lead: **Irene Lopez Sanchez**
- Study lead forums tag: **@irelopsan**
- Study start date: **-**
- Study end date: **-**
- Protocol: **-**
- Publications: **-**
- Results explorer: **-**

The main aim of this study is to assess, through a federated approach, the value of RWD mapped to OMOP CDM in monitoring cancer epidemiology over time and understanding patient characteristics, disease management and outcomes.

## How to run

1. Download this entire repository. You can do this by selecting Code > Download ZIP or by using GitHub Desktop.

2. Open the project OMOPCAN_Study.Rproj in RStudio. When the project is open, its name will appear in the top-right corner of your RStudio session.
  2.1. Install the necessary packages for the study by activating and restoring the renv. 
  2.2. Open and work through the CodeToRun.R, which is the primary file you'll need. Execute the lines in the file, adding your database-specific information as instructed in the comments.
  2.3. To run the study, use the command source(here::here("RunStudy.R"))

3. After running the analysis, you will find a results folder with one subfolder per each cancer group.

