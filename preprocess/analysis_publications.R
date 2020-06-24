library(rjson)
library(RISmed)


df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')


publication_data <- data_final %>%
  filter(!is.na(Study_Location)) %>%
  select(SourceId, Study_Location, Arm_Name, Arm_Participants, Treatment_Name, Measurement_Name, Measurement_Number, Time_Unit, Time_Interval) %>%
  distinct() %>%
  group_by(Study_Location) %>%
  summarise(Studies = n_distinct(SourceId),
            Outcomes = n_distinct(Measurement_Name),
            Treatments = n_distinct(Treatment_Name),
            Datapoints = n_distinct(SourceId, Measurement_Name, Measurement_Number, Time_Unit, Time_Interval)
  ) %>%
  left_join(data_final %>%
              filter(!is.na(Study_Location)) %>%
              select(SourceId, Study_Location, Arm_Name, Arm_Participants) %>%
              distinct() %>%
              group_by(Study_Location) %>%
              summarise(Patients = sum(Arm_Participants)),
            by = c("Study_Location"="Study_Location")
  ) %>%
  select(Study_Location, Studies, Patients, Outcomes, Treatments, Datapoints)





publicationGeography_data <- df %>%
  left_join(publication_data, by = c("COUNTRY"="Study_Location")) %>%
  select(-GDP..BILLIONS.) %>%
  mutate(Studies = ifelse(!is.na(Studies), Studies, 0),
         Outcomes = ifelse(!is.na(Outcomes), Outcomes, 0),
         Patients = ifelse(!is.na(Patients), Patients, 0),
         Treatments = ifelse(!is.na(Treatments), Treatments, 0),
         Datapoints = ifelse(!is.na(Datapoints), Datapoints, 0))

#### Geography Graph ----
# # light grey boundaries
# l <- list(color = toRGB("grey10"), width = 0.5)
# 
# # specify map projection/options
# g <- list(
#   showframe = FALSE,
#   showcoastlines = FALSE,
#   projection = list(type = 'Mercator') 
# )
# 
# fig <- plot_geo(publicationGeography_data)
# fig <- fig %>% add_trace(
#   z = ~Studies, color = ~col, colors = 'Blues',
#   text = ~COUNTRY, locations = ~CODE,
#   marker = list(line = l),
#   showscale = FALSE
# )
# fig <- fig %>% layout(
#   geo = g
# ) %>%
#   config(displayModeBar = F) %>%
#   layout(dragmode = FALSE)
# 
# fig

#### Publication Year ----



date_formats <- format(unique(data_final$PublicationDate), "%B\n%Y")

g_pubtime <- data_final %>%
  select(SourceId, PublicationDate) %>%
  distinct() %>%
  mutate(Date = paste0(month(PublicationDate, abbr = FALSE, label = TRUE), "\n", year(PublicationDate)) %>%
           factor(levels = date_formats)) %>%
  group_by(Date, PublicationDate) %>%
  summarise(n = n()) %>%
  mutate(label = paste0(month(PublicationDate, abbr = FALSE, label = TRUE), " ", year(PublicationDate),
                        "\n", n, " Studies")) %>%
  ggplot() +
  geom_bar(aes(x = Date, y = n, text = label), 
           fill = color_blue, color = "black", 
           width = .5, size = 1,
           stat = "identity")  +
  xlab(NULL) + 
  ylab("Published Studies") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_ts +
  theme(panel.grid = element_blank())
  # theme_bw()




#### Patient Characteristics - Age ----
# g_patientAge_death <- data_final %>%
#   select(SourceId, Arm_Name, Arm_MeanAge, meta_outcome) %>%
#   distinct() %>% 
#   ggplot(aes(x = meta_outcome, y = Arm_MeanAge, fill = meta_outcome)) +
#   geom_boxplot(alpha = 1, color = "black") +
#   scale_fill_manual(values = c(color_grey, color_red, color_blue)) + 
#   theme_ts +
#   xlab("Patient Outcome") + 
#   ylab("Age (years)")



g_patientAge_sex <- data_final %>%
  select(SourceId, Arm_Name, Arm_MeanAge, Arm_PercentFemale, Arm_PercentMale) %>%
  distinct() %>% 
  # filter(Arm_PercentFemale == 100 | Arm_PercentMale == 100) %>%
  mutate(meta_sex = case_when(Arm_PercentFemale == 100 ~ "Female",
                              Arm_PercentMale == 100 ~ "Male",
                              TRUE ~ "All")) %>%
  filter(meta_sex != "All") %>%
  ggplot(aes(x = meta_sex, y = Arm_MeanAge, fill = meta_sex)) +
  geom_boxplot(alpha = 1, color = "black") +
  scale_fill_manual(values = c(color_female,color_male)) + 
  theme_ts + 
  xlab(NULL) + 
  ylab("Age (years)")


#### Closing ----
rm(df)
rm(date_formats)
