library(googlesheets4)
library(lubridate)
library(plotly)
library(tidyverse)
library(RagGrid)

sheets_auth()

# Themes and Colors
source("preprocess/theme.R")                            # Output: color_*, plotly_options_ts, theme_ts

# Data Queries
source("preprocess/data_queries.R")                     # Output: raw_data

# Data Cleaning
source("preprocess/external_symptomClassification.R")   # Output: raw_data
source("preprocess/data_cleaning.R")                    # Output: data_final, tx_immuno, tx_antiviral

# Analysis
source("preprocess/analysis_symptomTimeline.R")         # Output: symptomTimeline_data, symptomTimeline_graph, g_symptomTreemap
source("preprocess/analysis_publications.R")            # Output: publication_data, publicationGeography_data, g_pubTime, g_patientAge_sex
source("preprocess/analysis_biomarkers.R")              # Output: bioTimeline_data, bioTimeline_select_options, g_bioCounts


# RData Write
save.image("covid_data.RData")


