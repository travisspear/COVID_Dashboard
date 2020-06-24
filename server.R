library(tidyverse)
library(lubridate)
library(DBI)
library(odbc)
library(RagGrid)
library(crosstalk)
library(shinyWidgets)
library(shadowtext)
library(plotly)
library(shiny)
library(argonDash)
library(argonR)
library(crosstalk)
library(scales)
library(treemap)
library(d3treeR)
library(shinydashboardPlus)


server <- function(input, output) {
    
    load("covid_data.RData")
    
#### Symptom Tab ----    
  
  d_symptomTimeline_table <- symptomTimeline_data %>%
    filter(Measurement_Class != "NA") %>%
    rename("Symptom Class" = Measurement_Class) %>%
    # group_by(Measurement_Class) %>%
    # filter(n() > 10 | Measurement_Class == "Death") %>%
    group_by(`Symptom Class`) %>%
    summarise(Patients = sum(Measurement_Number, na.rm = T),
              `Onset Time` = round(mean(Time_SymptomStart, na.rm = T),2),
              `Symptom Resolution Time` = round(mean(Time_SymptomEnd, na.rm = T),2),
              `Onset Time - Maximum` = round(max(Time_SymptomEnd, na.rm = T),2),
              `Onset Time - Minimum` = round(min(Time_SymptomStart, na.rm = T),2),
              `Occurences` = n()) %>%
    ungroup() %>%
    mutate(`Symptom Class` = factor(`Symptom Class`, levels = c("Cardiac", "Co-Infection", "Death", "Dermal", "Gastrointestinal", "General", "Hematological", "Hepatic", "Immunological", "Metabolic",
                                                                "Musculoskeletal", "Nervous", "Ophthalmic", "Pancreatic", "Psychological", "Renal", "Respiratory", "Sinus"))) %>%
    arrange(-Patients)
    d_sypmtomTimeline <- symptomTimeline_data %>%
        filter(Measurement_Class != "NA") %>%
        rename("Symptom Class" = Measurement_Class) %>%
        # group_by(Measurement_Class) %>%
        # filter(n() > 10 | Measurement_Class == "Death") %>%
        group_by(`Symptom Class`) %>%
        summarise(`Onset Time` = round(mean(Time_SymptomStart, na.rm = T),2),
                  `Symptom Resolution Time` = round(mean(Time_SymptomEnd, na.rm = T),2),
                  `Onset Time - Maximum` = round(max(Time_SymptomEnd, na.rm = T),2),
                  `Onset Time - Minimum` = round(min(Time_SymptomStart, na.rm = T),2),
                  patients = sum(Measurement_Number, na.rm = T),
                  n = n()) %>%
        ungroup() %>%
        mutate(`Symptom Class` = factor(`Symptom Class`, levels = c("Cardiac", "Co-Infection", "Death", "Dermal", "Gastrointestinal", "General", "Hematological", "Hepatic", "Immunological", "Metabolic",
                                                                        "Musculoskeletal", "Nervous", "Ophthalmic", "Pancreatic", "Psychological", "Renal", "Respiratory", "Sinus"))) %>%
        filter(patients > 20 | `Symptom Class` == "Death") %>%
        arrange(`Onset Time`)
    

    output$g_symptomTimeline <- renderPlotly({
        
        
      symptomTimeline_graph <- d_sypmtomTimeline %>%
        ggplot() +
        geom_segment(aes(x = `Onset Time`, xend = `Symptom Resolution Time`,
                         y = reorder(`Symptom Class`, -`Onset Time`),
                         yend = reorder(`Symptom Class`, -`Onset Time`)
                         # color = Measurement_Class,
        ),
        color = "black",
        size = 1) +
        geom_point(aes(x = `Onset Time`,
                       y = reorder(`Symptom Class`, -`Onset Time`),
                       fill = `Symptom Class`,
                       size = (n)*2,
                       text = paste0("Symptom Type: ", `Symptom Class`,
                                     "\nOccurrence in Literature: ", n,
                                     "\nPatients: ", patients,
                                     "\nAverage Time of Onset: ", round(`Onset Time`, 2), " Days After Initial Symptoms")),
                   pch = 21,
                   color = "black") +
        scale_size_continuous(range = c(2,10)) +
        ylab("Symptom Class") + 
        xlab("Average Onset Time and Symptom Duration") +
        scale_fill_brewer(palette = "Blues") + 
        # viridis::scale_fill_viridis(discrete = TRUE) +
        theme_ts
        
        ggplotly(symptomTimeline_graph, tooltip = "text") %>%
          plotly_options_ts()
        
    })
    
    
    output$t_symptomTimeline <- renderRagGrid({
      d_symptomTimeline_table %>%
            aggrid(
              theme = "ag-theme-alpine",
              # theme = "ag-theme-material",
                   options = list(pagination = FALSE, gridOptions.rowHeight = 500),
                   width = 6)
        
    })
    
    
    
    observeEvent(input$select_publicationGeography, {
        selected_data = sym(input$select_publicationGeography)
        
        geo_title = paste0(selected_data, " by Country")
        react_publicationGeography <- reactive({
            publicationGeography_data %>%
                select(COUNTRY, CODE, selected_data) %>%
                rename(plotdata = selected_data) %>%
                mutate(col = case_when(plotdata > 0 ~ rescale(log(plotdata), c(.5,1)),
                                       TRUE ~ 0))
        })

        output$g_publicationGeo <- renderPlotly({
            # light grey boundaries
            l <- list(color = toRGB("grey10"), width = 0.5)

            # specify map projection/options
            g <- list(
                showframe = FALSE,
                showcoastlines = FALSE,
                projection = list(type = 'Mercator')
            )

            fig <- plot_geo(react_publicationGeography())
            fig <- fig %>% add_trace(
                z = ~plotdata, color = ~col, colors = 'Blues',
                text = ~COUNTRY, locations = ~CODE,
                marker = list(line = l),
                showscale = FALSE
            )
            fig <- fig %>% 
              layout(geo = g) %>%
              layout(title = geo_title) %>%
              plotly_options_ts()

            fig

        })
        
    })

    
    output$g_symptomtTreemap <- renderD3tree2({
      g_symptomTreemap
    })
    
#### Publication Tab ----   
    output$g_publicationTimeline <- renderPlotly({

        
        
        
        
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
                     fill = color_lightblue, 
                     width = .5, size = .5,
                     stat = "identity",
                     color = "black")  +
            xlab(NULL) + 
            ylab("Published Studies") +
            # scale_y_continuous(expand = c(0, 0)) +
            theme_ts +
            theme(panel.grid = element_blank()) +
            scale_y_continuous(expand = c(0,0)) 
        
        # theme_bw()
        
        ggplotly(g_pubtime, tooltip = "text") %>%
            plotly_options_ts()
        # ggplotly(g_pubtime, tooltip = "text") %>%
        #     plotly_options_ts()
        
        
        
    })
    
    # Overview Stats
    ## Studies: N, N Coutnries
    ## Patients, N, N/Study
    ## Outcomes, N, N/Study
    ## Treatment N, N Antiviral
    ## Data Points: N, N/Study
    observeEvent(input$select_publicationGeography, {
        
        if(input$select_publicationGeography == "Studies"){
            stat_data <- data_final %>%
                select(SourceId, Study_Location) %>%
                filter(!is.na(Study_Location)) %>%
                distinct()
            
            stat_1 <- length(unique(stat_data$SourceId))
            stat_1_name <- "Total Publications"
            
            stat_2 <- length(unique(stat_data$Study_Location))
            stat_2_name <- "Publishing Countries"
            
            icon_1 <- "single-copy-04"
            icon_2 <- "world"
            
            
        } else if(input$select_publicationGeography == "Patients"){
            stat_data <- data_final %>%
                select(SourceId, Arm_Name, Arm_Participants) %>%
                distinct() %>%
                summarise(n_patients = sum(Arm_Participants, na.rm = T),
                          n_patients_study = round(sum(Arm_Participants, na.rm = T) / n_distinct(SourceId), 2))
            
            stat_1 <- stat_data$n_patients
            stat_1_name <- "Total Study Participants"
            
            stat_2 <- stat_data$n_patients_study
            stat_2_name <- "Average Participants per Study"
            
            icon_1 <- "single-02"
            icon_2 <- "archive-2"
            
            
        } else if(input$select_publicationGeography == "Outcomes"){
            stat_data <- data_final %>%
                select(SourceId, Measurement_Name) %>%
                distinct() %>%
                summarise(n_mes = n_distinct(Measurement_Name),
                          n_mes_study = round(n_distinct(Measurement_Name) / n_distinct(SourceId), 2))

            stat_1 <- stat_data$n_mes
            stat_1_name <- "Unique Patient Measurements"
            
            stat_2 <- stat_data$n_mes_study
            stat_2_name <- "Average Measurement per Study"
            
            icon_1 <- "sound-wave"
            icon_2 <- "calendar-grid-58"
            
        } else if(input$select_publicationGeography == "Treatments"){
            stat_data <- data_final %>%
                select(SourceId, Treatment_Name) %>%
                distinct() %>%
                summarise(n_tx = n_distinct(Treatment_Name),
                          n_antiviral = sum(unique(Treatment_Name) %in% tx_antiviral))
            
            stat_1 <- stat_data$n_tx
            stat_1_name <- "Unique Treatments"
            
            stat_2 <- stat_data$n_antiviral
            stat_2_name <- "Antiviral Treatments"
            
            icon_1 <- "ambulance"
            icon_2 <- "atom"
            
        } else if(input$select_publicationGeography == "Datapoints"){
            stat_data <- data_final %>%
                select(SourceId, Arm_Name, Measurement_Name, Measurement_Number, Measurement_Mean, Measurement_Median) %>%
                distinct() %>%
                summarise(n_dp = n(),
                          n_dp_study = round(n() / n_distinct(SourceId), 2))
            
            stat_1 <- stat_data$n_dp
            stat_1_name <- "Total Data Points"
            
            stat_2 <- stat_data$n_dp_study
            stat_2_name <- "Average Data Points per Study"
            
            icon_1 <- "check-bold"
            icon_2 <- "chart-pie-35"
            
        }

        
        output$ui_overviewStats <- renderUI({
        
            # argonCard(
            #   width = 12,
            #   background_color = "info",
              argonRow(
                argonColumn(
                  width = 6,
                  argonInfoCard(value = stat_1, 
                                width = 12,
                                shadow = TRUE,
                                background_color = "secondary",
                                icon_background = "info",
                                # title = p('Total Publications', style = 'color:white;text-align:left;font-size:2em;'),
                                title = stat_1_name,
                                stat = NULL,
                                icon = argonIcon(icon_1))
                  
                ),
                argonColumn(
                  width = 6,
                  argonInfoCard(value = stat_2, 
                                width = 12,
                                shadow = TRUE,
                                background_color = "secondary",
                                icon_background = "info",
                                # title = p('Publishing Countries', style = 'color:white;text-align:left;font-size:2em;'),
                                title = stat_2_name,
                                stat = NULL,
                                icon = argonIcon(icon_2))
                  
                )
                
              )
                
                # title = NULL,
                # width = 12,
            # )
            
        })

    })
    
    output$t_geography <- renderRagGrid({
      
      publication_data %>%
        rename(`Data Points` = 'Datapoints',
               Location = "Study_Location") %>%
        aggrid(
          theme = "ag-theme-alpine",
          # theme = "ag-theme-material",
          options = list(pagination = FALSE))
      
      
    })
    
    
    
    
    
    output$g_patientAge_death <- renderPlotly({
        ggplotly(g_patientAge_death) %>%
            plotly_options_ts() %>%
            layout(plot_bgcolor = "transparent", 
                   paper_bgcolor = "transparent")
    })
    output$g_patientAge_sex <- renderPlotly({
      ggplotly(g_patientAge_sex) %>%
        plotly_options_ts() %>%
        layout(plot_bgcolor = "transparent", 
               paper_bgcolor = "transparent")
    })
    
  


    output$g_patientSex <- renderPlotly({
        data_final %>%
            select(SourceId, Arm_Name, Arm_Participants, Arm_PercentFemale, Arm_PercentMale) %>%
            distinct() %>%
            filter(!is.na(Arm_PercentMale)) %>%
            mutate(Female = (Arm_PercentFemale/100) * Arm_Participants) %>%
            mutate(Male = (Arm_PercentMale/100) * Arm_Participants) %>%
            summarise(Male = round(sum(Male, na.rm = T),0),
                      Female = round(sum(Female, na.rm = T), 0)) %>%
            gather() %>%
            # mutate(label = paste0(key, ": ", round(value, 0))) %>%
            plot_ly(labels = ~key, values = ~value, type = "pie", 
                    textposition = "inside", textinfo = 'label+value',
                    showlegend = FALSE,
                    marker = list(colors = c(color_male,color_female),
                                  line = list(color = "black", width = 2))) %>%
            plotly_options_ts()
        
    })
    
    
       
#### Biomarkers Tab ----
    
    
    output$ui_bioTimeline_select <- renderUI({
      
      pickerInput(
        inputId = "bioTimeline_select",
        label = NULL, 
        width = "100%",
        selected = "Body Temperature (Degrees Celsius)",
        options = list(
          size = 10,
          `live-search` = TRUE,
          style = "btn-default"
        ),
        choices = bioTimeline_select_options$FullName,
        choicesOpt = list(
          subtext = paste("Data Points", 
                          bioTimeline_select_options$datapoints,
                          sep = ": "))
      )
    })
    
    observeEvent(input$bioTimeline_select, {
      
      d_bioTimeline <- bioTimeline_data %>%
        filter(FullName == input$bioTimeline_select)
      
      output$g_bioTimeline <- renderPlotly({
        g_bioTimeline <- ggplot() +
          geom_line(
            data = d_bioTimeline %>%
              group_by(FullName, meta_outcome, Time_Scaled) %>%
              summarise(value = round(weighted.mean(x = Measurement_Mean, w = Arm_Participants), 2)),
            aes(x = Time_Scaled, y = value,
                color = meta_outcome
            ),
            alpha = .75,
            size = 1.5
            
          ) +
          geom_point(
            data = d_bioTimeline,
            aes(x = Time_Scaled, y = Measurement_Mean,
                fill = meta_outcome,
                size = Arm_Participants,
                text = paste0("Time: ", Time_Scaled, " days after Symptom Onset",
                              "\nReported Value: ", Measurement_Mean,
                              "\nPatient(s) Outcome: ", meta_outcome,
                              "\nN Patients: ", Arm_Participants)),
            pch = 21,
            alpha = .75
          ) +
          scale_fill_manual(values = c(color_neutral, color_red, color_lightblue), name = "Patient Outcome") +
          scale_color_manual(values = c(color_neutral, color_red, color_lightblue), guide = FALSE) +
          scale_size_continuous(range = c(3,10), guide = FALSE) +
          xlab("Days After Symptom Onset") +
          ylab(input$bioTimeline_select) +
          theme_ts +
          theme(legend.position = "bottom")
        
        ggplotly(g_bioTimeline, tooltip = "text") %>%
          layout(legend = list(orientation = "v", x = -.1)) %>%
          plotly_options_ts()
        
        
        
        
      })
      
      output$g_bioAdmission <- renderPlotly({
        g_bioAdmission <- bioTimeline_data %>%
          filter(Time_Unit == "At Admission") %>%
          filter(FullName == input$bioTimeline_select) %>%
          ggplot(aes(x = meta_outcome, y = Measurement_Mean, fill = meta_outcome)) + 
          geom_boxplot(alpha = .75) +
          geom_jitter(pch = 21, alpha = .75) +
          xlab("Patient Outcome") +
          ylab(input$bioTimeline_select) +
          scale_fill_manual(values = c(color_neutral, color_red, color_lightblue)) +
          theme_ts
        
        ggplotly(g_bioAdmission, tooltip = "text") %>%
          plotly_options_ts()
        
        
      })
      
      
      
      
    })
    
    
    output$g_bioCounts <- renderPlotly({
      ggplotly(g_bioCounts, tooltip = "text") %>%
        config(displayModeBar = F) %>% 
        layout(dragmode = "zoom") %>%
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)")

    })
    
    
    
}