

library(RISmed)

#### PubMed Prep ----
pm_query <- 'covid19 or coronavirus or (sars cov 2 OR (cov and sars)) AND (Case Reports[ptyp] OR Clinical Study[ptyp] OR Clinical Trial[ptyp] OR Clinical Trial, Phase I[ptyp] OR Clinical Trial, Phase II[ptyp] OR Clinical Trial, Phase III[ptyp] OR Clinical Trial, Phase IV[ptyp] OR Controlled Clinical Trial[ptyp] OR Multicenter Study[ptyp] OR Observational Study[ptyp] OR Pragmatic Clinical Trial[ptyp] OR Randomized Controlled Trial[ptyp]) AND "humans"[MeSH Terms] AND English[lang] AND ("2019/01/01"[PDat] : "3000/01/01"[PDat])'

res <- EUtilsGet(EUtilsSummary(pm_query))
pm_df <- data.frame(SourceId = PMID(res),
                    PublicationDate = dmy(paste0("1-", MonthPubmed(res), "-", YearPubmed(res))))


#### Treatment Meta-Groups Prep ----
tx_antiviral <- c('Antiviral Therapy',
                  'Oseltamivir',
                  'Ganciclovir',
                  'Peramivir',
                  'Kaletra',
                  'Veletonavir',
                  'Arbidol',
                  'Lopinavir/Ritonavir',
                  'Immunoglobulin',
                  'Ribavirin',
                  'Lopinavir',
                  'Ritonavir',
                  'Abidor',
                  'Abidol',
                  # Human Recombinant Interferon Antiviral Therapy,
                  'Remdesivir',
                  'Tamiflu',
                  'Zirgan',
                  'Umifenovir',
                  'Kaletra',
                  'Moderiba', 
                  'Rebetol', 
                  'Moderiba Dose Pack', 
                  'Virazole', 
                  'Ribasphere', 
                  'Copegus')
tx_immuno <- c("Interferon (IFN)", "Gammaglobulin", "Immunoglobulin", "Human Recombinant Interferon Antiviral Therapy")



#### Data Cleaning ----
data_final <- raw_data %>%
  mutate(Study_Location = case_when(Study_Location == "China, People's Republic of" ~ "China", 
                                    TRUE ~ Study_Location)) %>%
  # Joining in Publication Dates
  left_join(pm_df, by = "SourceId") %>%
  # Treatment Meta-Groups - Antiviral + Immunotherapy
  group_by(SourceId, Arm_Name) %>%
  mutate(meta_tx = case_when(any(Treatment_Name %in% tx_antiviral) & any(Treatment_Name %in% tx_immuno) ~ "AV_Immuno",
                             any(Treatment_Name %in% tx_antiviral) & !any(Treatment_Name %in% tx_immuno) ~ "AV")) %>%

  # Patient Outcome Meta-Groups - Death + Discharge 
  group_by(SourceId, Arm_Name) %>%
  mutate(meta_outcome = case_when(any(Measurement_Name %>% str_detect(regex("death|mortality", ignore_case = T))) | any(Measurement_Mode == "Death") ~ "Death",
                                  any(Measurement_Name %>% str_detect(regex("discharge|(hospital stay)", ignore_case = T))) ~ "Discharge",
                                  TRUE ~ "Unknown") %>%
           factor(levels = c("Unknown", "Death", "Discharge"))) %>%
  # Fix Missing Percent Male/Female 
  group_by(SourceId, Arm_Name) %>%
  mutate(Arm_PercentMale = ifelse(!is.na(Arm_PercentMale), Arm_PercentMale, 100-Arm_PercentFemale),
         Arm_PercentFemale = ifelse(!is.na(Arm_PercentFemale), Arm_PercentFemale, 100-Arm_PercentMale)) %>%
  ungroup() %>%
  #### Unit Conversion ----
  mutate(Measurement_Mean = case_when(
    Measurement_Name == "Body Temperature" & 
      Measurement_UnitNumerator %>% str_detect("Fahrenheit") ~ round(((Measurement_Mean - 32) * (5/9)),2),
    Measurement_Name == "Lymphocytes" & 
      (Measurement_UnitNumerator == "cells" | is.na(Measurement_UnitNumerator)) & 
      Measurement_UnitDenominator %in% c("µL", "μL", "mm^3") ~ Measurement_Mean / 1000,
    TRUE ~ Measurement_Mean
  )) %>%
  mutate(Measurement_UnitNumerator = case_when(
    Measurement_Name == "Body Temperature" & 
      Measurement_UnitNumerator %>% str_detect("Fahrenheit") ~ "Degrees Celsius",
    Measurement_Name == "Lymphocytes" & 
      (Measurement_UnitNumerator == "cells" | is.na(Measurement_UnitNumerator)) & 
      Measurement_UnitDenominator %in% c("µL", "μL", "mm^3") ~ "10^9 cells",
    
    TRUE ~ Measurement_UnitNumerator
  )) %>%
  mutate(Measurement_UnitDenominator = case_when(
    Measurement_Name == "Lymphocytes" & 
      (Measurement_UnitNumerator == "cells" | is.na(Measurement_UnitNumerator)) & 
      Measurement_UnitDenominator %in% c("µL", "μL", "mm^3") ~ "L",
    TRUE ~ Measurement_UnitDenominator
  )) %>%
  mutate(Measurement_UnitNumerator = case_when(
    Measurement_Name == "Lymphocytes" & 
      Measurement_UnitNumerator %in% c("10^6 cells", "10^3 cells", "10^6 Cells", "10^3 Cells", "10^9 cells") ~ "10^9 Cells",
    TRUE ~ Measurement_UnitNumerator
  )) %>%
  mutate(Measurement_UnitDenominator = case_when(
    Measurement_Name == "Lymphocytes" & 
      Measurement_UnitNumerator %in% c("10^6 cells", "10^3 cells", "10^6 Cells", "10^3 Cells", "10^9 cells", "10^9 Cells") ~ "L",
    TRUE ~ Measurement_UnitDenominator
  )) 
  


  





rm(raw_data)
rm(pm_query)
rm(res)
rm(pm_df)


