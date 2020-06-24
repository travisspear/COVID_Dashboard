
bioTimeline_data <- data_final %>%
  filter(Measurement_ValueType == "Mean") %>%
  filter(Measurement_Type == "Biomarker") %>%
  select(SourceId, Arm_Name, Measurement_Name, Measurement_UnitNumerator, 
         Measurement_UnitDenominator, Measurement_Mean, Time_Unit, 
         Time_Interval, meta_outcome, Arm_Participants) %>%
  distinct() %>% 
  # group_by(SourceId, Arm_Name) %>%
  # mutate(Patient_Outcome = case_when(any(Measurement_Name %>% str_detect("Death|Mortality")) ~ 1,
  #                  any(Measurement_Name %>% str_detect("Discharge|(Length of Hospital Stay)")) ~ 0,
  #                  TRUE ~ .5)) %>%
  # ungroup() %>%
  mutate(Time_Factor = case_when(Time_Unit == "At Admission" ~ 0,
                                 Time_Unit == "Days Before Admission" ~ -1,
                                 Time_Unit == "Days After Admission" ~ 1,
                                 Time_Unit == "Days After Symptom Onset" ~ 1,
                                 Time_Unit == "Days Before Symptom Onset" ~ -1,
                                 Time_Unit == "At Symptom Onset" ~ -999)) %>%
  mutate(Time_Rescaled = case_when(
    !is.na(Time_Interval) ~ Time_Factor * Time_Interval,
    is.na(Time_Interval) & Time_Unit %in% c("At Admission", "At Symptom Onset") ~ 0,
    TRUE ~ Time_Factor * Time_Interval
  )) %>%
  group_by(SourceId, Arm_Name) %>%
  mutate(Time_Scaled = case_when(
    Time_Rescaled == min(Time_Rescaled, na.rm = T) ~ 0,
    Time_Rescaled != min(Time_Rescaled, na.rm = T) ~ Time_Rescaled - min(Time_Rescaled, na.rm = T)
  )) %>%
  group_by(Measurement_Name) %>%
  # filter(!all(is.na(Time_InitialSymptoms))) %>%
  select(SourceId, Arm_Name, Measurement_Name, Measurement_UnitNumerator, 
         Measurement_UnitDenominator, Time_Scaled, Measurement_Mean, meta_outcome, Time_Unit, Arm_Participants) %>%
  distinct() %>% 
  ungroup() %>%
  group_by(Measurement_Name, Measurement_UnitNumerator, Measurement_UnitDenominator) %>%
  mutate(FullName = case_when(!is.na(Measurement_UnitNumerator) & 
                            !is.na(Measurement_UnitDenominator) ~ paste0(Measurement_Name, " (", Measurement_UnitNumerator, "/", Measurement_UnitDenominator, ")"),
                          !is.na(Measurement_UnitNumerator) & is.na(Measurement_UnitDenominator) ~ paste0(Measurement_Name, " (", Measurement_UnitNumerator, ")"))) %>%
  ungroup()


bioTimeline_select_options <- bioTimeline_data %>%
  group_by(FullName) %>%
  filter(n() > 25) %>%
  select(FullName) %>%
  summarise(datapoints = n()) %>%
  arrange(-datapoints)




#### Biomarker Counts ----
g_bioCounts <- bioTimeline_data %>%
  select(SourceId, Arm_Name, Arm_Participants, Measurement_Name) %>%
  distinct() %>%
  group_by(Measurement_Name) %>%
  # summarise(Count = n()) %>%
  summarise(Patients = sum(Arm_Participants, na.rm = T),
            Studies = n_distinct(SourceId)) %>%
  filter(Patients > 10 & Studies > 1) %>%
  ggplot(aes(x = Studies, y = Patients, 
             size = Studies*Patients, 
             text = paste0("Biomarker: ", Measurement_Name,
                           "\nNumber of Studies: ", Studies,
                           "\nNumber of Patients: ", Patients))) +
  geom_point(pch = 21, alpha = .75, fill = color_lightblue) +
  scale_size_continuous(range = c(2,10)) +
  theme_ts +
  theme(axis.text.x = element_blank())
  

# ggplotly(g_bioCounts, tooltip = "text")




