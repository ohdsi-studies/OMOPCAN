# CohortDiagnostics Time Trends in Cancer
This code is designed to run cohort diagnostics on the following outcome cohorts for the Time Trends in Cancer study:
- Lip, oral cavity
- Salivary glands
- Oropharynx
- Nasopharynx
- Hypopharynx
- Esophagus
- Stomach
- Colorectal (colon, rectum and anus)
- Liver (including intrahepatic bile ducts)
- Gallbladder
- Pancreas
- Larynx
- Lung (including trachea and bronchus)
- Melanoma of skin
- NMSC (excluding basal cell carcinoma)
- Mesothelioma
- Kaposi sarcoma
- Female breast
- Vulva
- Vagina
- Cervix uteri
- Corpus uteri
- Ovary
- Penis
- Prostate
- Testis
- Kidney (including renal pelvis)
- Bladder
- Brain, central nervous system
- Thyroid
- Hodgkin lymphoma
- Non-Hodgkin lymphoma
- Multiple myeloma (including immunoproliferative diseases)
- Leukemia

## To Run
1) Download this entire repository. You can do this by selecting Code > Download ZIP or by using GitHub Desktop. 
2) Open the project <i>CohortDiagnostics.Rproj</i> in RStudio. When the project is open, its name will appear in the top-right corner of your RStudio session.
3) Install the necessary packages for the study by activating and restoring the renv. Please note that the required R version is 4.4.1.
4) Open and work through the <i>CodeToRun.R</i>, which is the primary file you'll need. Execute the lines in the file, adding your database-specific information as instructed in the comments. To run the study, use the command <i>(source(here("RunAnalysis.R"), echo=TRUE)</i>
5) After running the analysis, merge the results with the provided line. You will find a zip folder with results in your output folder.
7) To check your results, run the following command <i>CohortDiagnostics::launchDiagnosticsExplorer()</i>. It may prompt you to install OhdsiShinyModules, if not already installed. We advise you to skip the updates of other packages. 
