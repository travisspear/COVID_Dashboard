


symptomTimeline_data <- data_final %>%
  select(SourceId, Arm_Name, Measurement_Name, Measurement_EventCriteriaScore, 
         Measurement_EventCriteriaUnitNumerator, Time_Unit, Time_Interval,
         Measurement_Class, Measurement_Number, meta_outcome) %>%
  distinct() %>% 
  # group_by(SourceId, Arm_Name) %>%
  # mutate(Patient_Outcome = case_when(any(Measurement_Name %>% str_detect("Death|Mortality")) ~ 1,
  #                  any(Measurement_Name %>% str_detect("Discharge|(Length of Hospital Stay)")) ~ 0,
  #                  TRUE ~ .5)) %>%
  # ungroup() %>%
  filter(Measurement_Name %>% str_detect(regex("Symptom|radiograph|x-ray|\\(ct\\)", ignore_case = T))) %>%
  mutate(Time_Factor = case_when(Time_Unit == "At Admission" ~ 0,
                                 Time_Unit == "Days Before Admission" ~ -1,
                                 Time_Unit == "Days After Admission" ~ 1,
                                 Time_Unit == "Days After Symptom Onset" ~ 1,
                                 Time_Unit == "Days Before Symptom Onset" ~ -1,
                                 Time_Unit == "At Symptom Onset" ~ -999)) %>%
  mutate(Measurement_EventCriteriaScore = case_when(
    !is.na(Measurement_EventCriteriaScore) & Measurement_EventCriteriaUnitNumerator == "day" ~ Measurement_EventCriteriaScore,
    !is.na(Measurement_EventCriteriaScore) & Measurement_EventCriteriaUnitNumerator == "week" ~ Measurement_EventCriteriaScore * 7,
    !is.na(Measurement_EventCriteriaScore) & Measurement_EventCriteriaUnitNumerator == "month" ~ Measurement_EventCriteriaScore * 30,
    TRUE ~ Measurement_EventCriteriaScore
  )) %>%
  mutate(Time_Rescaled = case_when(
    !is.na(Time_Interval) ~ Time_Factor * Time_Interval,
    is.na(Time_Interval) & Time_Unit %in% c("At Admission", "At Symptom Onset") ~ 0,
    TRUE ~ Time_Factor * Time_Interval
  )) %>%
  group_by(SourceId, Arm_Name) %>%
  mutate(Time_SymptomStart = case_when(
    Time_Rescaled == min(Time_Rescaled, na.rm = T) ~ 0,
    Time_Rescaled != min(Time_Rescaled, na.rm = T) ~ Time_Rescaled - min(Time_Rescaled, na.rm = T)
  )) %>%
  group_by(SourceId, Arm_Name, Measurement_Name) %>%
  mutate(Time_SymptomEnd = case_when(
    !is.na(Measurement_EventCriteriaScore) ~ Time_SymptomStart + Measurement_EventCriteriaScore,
    n() > 1 ~ max(Time_SymptomStart, na.rm = T)
    # is.na(Measurement_EventCriteriaScore) &  ~ Time_SymptomStart + max(Time_Rescaled, na.rm = T)
  )) %>%
  group_by(SourceId, Arm_Name) %>%
  mutate(Time_SymptomEnd = ifelse(Time_SymptomEnd < 0, 0, Time_SymptomEnd)) %>%
  group_by(Measurement_Name) %>%
  # filter(!all(is.na(Time_InitialSymptoms))) %>%
  select(SourceId, Arm_Name, Measurement_Name, Time_SymptomStart, Time_SymptomEnd, Measurement_Class, Measurement_Number, meta_outcome) %>%
  distinct() %>% 
  # group_by(Measurement_Name, Measurement_Class) %>%
  # filter(n() >= 5) %>% 
  # summarise(Time_MeanOnset = mean(Time_SymptomStart, na.rm = T),
  #           Time_MeanOnset_SD = sd(Time_SymptomStart, na.rm = T),
  #           Time_MeanEnd = mean(Time_SymptomEnd, na.rm = T),
  #           Time_MeanEnd_SD = sd(Time_SymptomEnd, na.rm = T),
  #           Time_MeanEnd_max = max(Time_MeanEnd, na.rm = T),
  #           Time_MeanOnset_min = min(Time_SymptomStart, na.rm = T),
  #           # Patient_Outcome = mean(Patient_Outcome, na.rm = T),
  #           n = n()) %>%
  # mutate(Time_MeanOnset_min = ifelse((Time_MeanOnset - Time_MeanOnset) < 0, 0, Time_MeanOnset + Time_MeanOnset_SD)) %>%
  ungroup() %>%
  mutate(Measurement_Name = str_remove_all(Measurement_Name, "Symptom - ")) %>%
  group_by(SourceId, Arm_Name, Measurement_Name, meta_outcome) %>%
  filter(Time_SymptomStart == min(Time_SymptomStart, na.rm = T))

 

symptomTimeline_graph <- symptomTimeline_data %>%
  filter(Measurement_Class != "NA") %>%
  # group_by(Measurement_Class) %>%
  # filter(n() > 10 | Measurement_Class == "Death") %>%
  group_by(Measurement_Class) %>%
  summarise(Time_MeanOnset = mean(Time_SymptomStart, na.rm = T),
            Time_MeanEnd = mean(Time_SymptomEnd, na.rm = T),
            # Patient_Outcome = mean(Patient_Outcome, na.rm = T),
            Time_MeanEnd_max = max(Time_SymptomEnd, na.rm = T),
            Time_MeanOnset_min = min(Time_SymptomStart, na.rm = T),
            n = n(),
            patients = sum(Measurement_Number, na.rm = T)) %>%
  ungroup() %>%
  mutate(Measurement_Class = factor(Measurement_Class, levels = c("Cardiac", "Co-Infection", "Death", "Dermal", "Gastrointestinal", "General", "Hematological", "Hepatic", "Immunological", "Metabolic",
                                                                  "Musculoskeletal", "Nervous", "Opthalmic", "Pancreatic", "Psychological", "Renal", "Respiratory", "Sinus"))) %>%
  filter(n > 10 | Measurement_Class == "Death") %>%
  ggplot() +
  geom_segment(aes(x = Time_MeanOnset, xend = Time_MeanEnd,
                   y = reorder(Measurement_Class, -Time_MeanOnset),
                   yend = reorder(Measurement_Class, -Time_MeanOnset)
                   # color = Measurement_Class,
                   ),
               color = "black",
               size = 1) +
  geom_point(aes(x = Time_MeanOnset,
                 y = reorder(Measurement_Class, -Time_MeanOnset),
                 fill = Measurement_Class,
                 size = (n)*2,
                 text = paste0("Symptom Type: ", Measurement_Class,
                               "\nOccurrence in Literature: ", n,
                               "\nPatients: ", patients,
                               "\nAverage Time of Onset: ", round(Time_MeanOnset, 2), " Days After Initial Symptoms")),
             pch = 21,
             color = "black") +
  scale_size_continuous(range = c(2,10)) +
  scale_fill_brewer(palette = "Purples") +
  # viridis::scale_fill_viridis(discrete = TRUE) +
  theme_ts
  # scale_color_manual(values = c("darkred", "black", "brown", "grey50", "red", "darkgreen", "pink", "purple", "darkblue", "lightblue", "pink")) 
  # scale_fill_manual(values = c("darkred", "black", "brown", "grey50", "red", "darkgreen", "pink", "purple", "darkblue", "lightblue"))



# ggplotly(symptomTimeline_graph, tooltip = "text")



# symptomTimeline_data %>%
#   filter(Measurement_Class != "NA") %>%
#   # group_by(Measurement_Class) %>%
#   # filter(n() > 10 | Measurement_Class == "Death") %>%
#   group_by(Measurement_Class) %>%
#   summarise(Time_MeanOnset = round(mean(Time_SymptomStart, na.rm = T),2),
#             Time_MeanEnd = round(mean(Time_SymptomEnd, na.rm = T),2),
#             # Patient_Outcome = mean(Patient_Outcome, na.rm = T),
#             Time_MeanEnd_max = round(max(Time_SymptomEnd, na.rm = T),2),
#             Time_MeanOnset_min = round(min(Time_SymptomStart, na.rm = T),2),
#             n = n(),
#             patients = round(mean(Measurement_Number, na.rm = T),2)) %>%
#   ungroup() %>%
#   mutate(Measurement_Class = factor(Measurement_Class, levels = c("Cardiac", "Co-Infection", "Death", "Dermal", "Gastrointestinal", "General", "Hematological", "Hepatic", "Immunological", "Metabolic",
#                                                                   "Musculoskeletal", "Nervous", "Opthalmic", "Pancreatic", "Psychological", "Renal", "Respiratory", "Sinus"))) %>%
#   filter(n > 10 | Measurement_Class == "Death") %>%
#   as.matrix() %>%
#   aggrid(theme = "ag-theme-material")




#### Pie Chart ----

library(treemap)
library(d3treeR)

d_symptomTreemap <- symptomTimeline_data %>%
  ungroup() %>%
  select(Measurement_Name, Measurement_Class, Measurement_Number) %>%
  group_by(Measurement_Name, Measurement_Class) %>%
  summarise(Patients = sum(Measurement_Number, na.rm = T),
            n = n()) %>%
  filter(Patients > 0 & !is.na(Measurement_Class))

g_symptomTreemap <- d3tree2(
  treemap(
    d_symptomTreemap, 
    index = c("Measurement_Class", "Measurement_Name"), 
    vSize = "Patients",
    vColor = "Patients",
    type="value",
    palette = "Blues",
    title.legend = NA
  ), 
  rootname = "Symptom Counts",
  celltext = "name",
  valueField = "size"
)


#### Closing ----
rm(d_symptomTreemap)
