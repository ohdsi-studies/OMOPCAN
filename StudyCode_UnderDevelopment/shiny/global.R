renv::activate()
renv::restore()

library(bslib)
library(CohortCharacteristics)
library(DiagrammeR)
library(dplyr)
library(DT)
library(ggplot2)
library(glue)
library(gt)
library(markdown)
library(omopgenerics)
library(OmopSketch)
library(purrr)
library(readr)
library(rlang)
library(shiny)
library(shinyWidgets)
library(sortable)
library(tidyr)
library(visOmopResults)
library(yaml)

#You need to paste the result ZIP file in "/data" folder

#Bring processing functions 
source(file.path(getwd(), "functions.R"))

# DP Preprocess data if it has not been done
fileData <- here::here("data", "shinyData.RData")
if (!file.exists(fileData)) {
  source(here::here("data", "preprocess_dp.R"))
} else{
  load(fileData)
}


