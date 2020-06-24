
# Packages
library(DBI) # Database connection
library(odbc) # Database connection
library(tidyverse) # Data manipulation


#### Credentials ----
#' stringsAsFactors=F prevents defaultconversion to factors in dataframes
#' fileEncoding: UTF-8-BOM prevents csv header issues
cred <- read.csv("database/credentials.csv", stringsAsFactors = F, fileEncoding = "UTF-8-BOM")

#### Server Connection ----
#' Gets credentials from external csv 
#' encoding = latin1 for character encodings
con <- dbConnect(odbc(),Driver = "SQL Server", 
                 Server = cred$database, 
                 Database = "MedAware.Curation.A", 
                 UID = cred$user, 
                 PWD = cred$password, 
                 encoding = "UTF-8")

#### Queries ----
query_studyArmTx <- read_file("./database/curation_armtx_odbc.sql") # Study, Arm, Treatment query
efficacies_query <- read_file("./database/curation_efficacies_odbc.sql") # Study and Efficacy quuery

# Sending queries to database
studyArmTx <- dbGetQuery(con, statement = query_studyArmTx) # Sending queries to database
efficacies <- dbGetQuery(con, statement = efficacies_query)

#### Data Formatting ----
raw_data <- studyArmTx %>%
  distinct() %>%
  full_join(efficacies,
            by = c("SourceId", "MedAwareId", "ObservationId", "ObservationTreatmentGroupId", "State", "Time_IsBaseline", "Time_Interval", "StudyArea", "Time_Unit"))

#### Closing ----
dbDisconnect(con) # Closes the database connection
# rm(list=setdiff(ls(), "raw_data_central")) # Removes all variables NOT in the file output


rm(cred)
rm(con)
rm(query_studyArmTx)
rm(efficacies_query)
rm(studyArmTx)
rm(efficacies)