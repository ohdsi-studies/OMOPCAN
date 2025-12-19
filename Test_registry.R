#Activate and restore renv
renv::activate()
renv::restore()

library(dplyr) 

# Set the short name/acronym for your database (e.g. "SIDIAP", "IPCI", "CPRD", etc..)
# Please do not use "omop" or "cdm" for db_name
db_name <-"..."

# database connection details
server_dbi <- "..."
user       <- "..."
password   <- "..."
port       <- "..."
host       <- "..."

# DBI driver (change based on your dbms)
drv <- RPostgres::Postgres()

# Connection
# See https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html for examples
db <- DBI::dbConnect(
  drv,
  host = host,
  port = port,
  dbname = server_dbi,
  user = user, 
  password = password
)


# Set database details -----

# The name of the schema that contains the omop tables
cdm_database_schema <- "..."

# The name of the schema where results tables will be created 
results_database_schema <- "..."

# prefix to be used for tables created in the write schema
stem_table <- "..."

# Study parameters:

# Data availability in your database: start and end dates 
startdate <- as.Date("YYYY-MM-DD") 
enddate <- as.Date("YYYY-MM-DD") 

# If your database is a cancer registry set to TRUE and select corresponding path to population counts
isRegistry <- TRUE
# filepop <- here::here("extras/Registries/data_population/norway_dpop.csv")
# filepop <- here::here("extras/Registries/data_population/geneve_dpop.csv")
filepop <- here::here("extras/Registries/data_population/netherlands_dpop.csv")

# If the database contains death data 
executeSurvival <- TRUE

# minimum counts that can be displayed according to data governance
minimum_counts <- 10

# create cdm reference 
cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdm_database_schema,
  writeSchema = results_database_schema,
  writePrefix = stem_table,
  cdmName = db_name
)

cancersToRun = c("respiratory","female")

executeInstantiateCohorts <- FALSE
executeCharacterisation <- FALSE

if(isRegistry){
  source(here::here("RunStudyRegistry.R"))
}else{
  source(here::here("RunStudy.R"))
}

# Close connection
CDMConnector::cdmDisconnect(cdm)
print("Thanks for running this study!")
