# Queries MedAware Curation Database for Symptoms, Scans, and COVID Related AEs
# Writes Database results to google sheet for external classification
# Checks google sheet against current database results and notifies of unclassified data
# Reads back in data post-classification to be used in analysis


symptom_gs <- sheets_read("17W_8yfRX17sQMkL6LwAQMXyNb8Mb7mCAd2x-w0uvqVk", sheet = "Symptoms")


symptoms <- raw_data %>% 
  filter(Measurement_Name %>% str_detect("Symptom") | 
           Measurement_Name %>% str_detect("\\(CT\\)") |
           Measurement_Name %>% str_detect("Radiograph") | 
           Measurement_Name %>% str_detect(regex("X-Ray", ignore_case = T)) | 
           Measurement_Type == "Adverse Event") %>%
  select(Measurement_Name) %>%
  distinct()


if(any(!symptoms$Measurement_Name %in% symptom_gs$Measurement_Name)){
  print(cat("New Symptoms Found in Curation. Edit Google Sheets.\nhttps://docs.google.com/spreadsheets/d/17W_8yfRX17sQMkL6LwAQMXyNb8Mb7mCAd2x-w0uvqVk/edit"))
  
  
  gs <- sheets_get("17W_8yfRX17sQMkL6LwAQMXyNb8Mb7mCAd2x-w0uvqVk")
  
  new_symptoms <- symptoms %>%
    filter(!Measurement_Name %in% symptom_gs$Measurement_Name) %>%
    mutate(Measurement_Class = "")
  
  sheets_append(new_symptoms, ss = gs, sheet = "Symptoms")
  
  rm(new_symptoms)
  rm(gs)
  stop()
}


symptom_classification <- sheets_read("17W_8yfRX17sQMkL6LwAQMXyNb8Mb7mCAd2x-w0uvqVk", sheet = "Symptoms", na = "NA")

raw_data <- raw_data %>%
  left_join(symptom_classification, 
            by = "Measurement_Name")


rm(symptom_classification)
rm(symptom_gs)
rm(symptoms)
