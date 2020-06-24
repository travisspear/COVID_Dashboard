library(argonDash)
library(argonR)
library(htmltools)
library(shinyWidgets)
library(shiny)
library(argonDash)
library(argonR)
library(plotly)
library(RagGrid)
library(treemap)
library(d3treeR)
library(shinydashboardPlus)

ui <- argonDash::argonDashPage(
    title = "COVID-19",
    description = "Data Exploration and Summary of COVID-19 Publications.",
    author = "Travis Spear",
    header = argonDash::argonDashHeader(
        color = 'info',
        gradient = FALSE,
        # background_img = "Spear_Logo_rotated.svg",
        
        argonRow(
            argonColumn(
                width = 6,
                h4('COVID-19 Data Exploration', style = 'color:white;text-align:left;font-size:2em;'),
                
            )
            # argonColumn(
            #     width = 1,
            #     offset = 5,
            #     argonImage(src = "Spear_Logo_rotated.png", 
            #                width = 200)
            #     
            # )

        ),
        top_padding = 2,
        bottom_padding = 1
        
    ),
    
    body = argonDash::argonDashBody(
        useShinydashboard(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "covid.css")
        ),
        argonTabSet(
            id = "tabset",
            size = "sm",
            width = 12,
            argonTab(
                tabName = "Overview",
                active = TRUE,
                argonR::argonRow(
                    argonColumn(
                        width = 4,
                        argonCard(
                            title = "Publication Timeline",
                            width = 12, 
                            shadow = TRUE,
                            # background_color = "secondary",
                            plotlyOutput("g_publicationTimeline", height = "20vh")
                        ),
                        argonCard(
                            title = NULL,
                            h4('PATIENT AGES', style = 'color:#4298b5;text-align:left;font-size:1.3em;'),
                            tags$hr(),
                            width = 12, 
                            shadow = TRUE,
                            # background_color = "info",
                            plotlyOutput("g_patientAge_sex", height = "25vh"),
                            tags$br(),
                            h4('PATIENT SEX', style = 'color:#4298b5;text-align:left;font-size:1.3em;'),
                            tags$hr(),
                            plotlyOutput("g_patientSex", height = "30vh")
                        )
                        
                        
                    ),
                    
                    argonR::argonCard(
                        shadow = TRUE,
                        title = "Data Geography",
                        # icon = "chart-bar-32",
                        width = 8,
                        plotlyOutput("g_publicationGeo", heigh = "50vh"),
                        argonCard(
                            width = 12,
                            background_color = "info",
                            argonRow(
                                width = 12,
                                center = TRUE,
                                radioGroupButtons(
                                    inputId = "select_publicationGeography",
                                    label = NULL,
                                    choices = c(`<i class='fa fa-file-o'></i>` = "Studies", 
                                                `<i class='fa fa-users'></i>` = "Patients", 
                                                `<i class='fa fa-heartbeat'></i>` = "Outcomes", 
                                                `<i class='fa fa-capsules'></i>` = "Treatments", 
                                                `<i class='fa fa-pie-chart'></i>` = "Datapoints"
                                    ),
                                    
                                    justified = TRUE,
                                    size = "normal",
                                    status = "default",
                                    selected = "Studies",
                                    direction = "horizontal"
                                )
                                
                            ),
                            
                            # argonRow(
                                # width = 12,
                                uiOutput("ui_overviewStats"),
                                # ),
                            tags$hr(),
                            argonRow(
                                width = 12,
                                RagGridOutput("t_geography", height = "300px")
                            )
                            
                        )
                        
                        
                        
                    )
                )
                


            ),
            argonTab(
                tabName = "Symptoms",
                argonRow(
                    width = 12,
                    argonCard(
                        width = 12, 
                        shadow = TRUE,
                        title = "Timeline of Symptom Onset",
                        plotlyOutput("g_symptomTimeline")
                        
                    )
                    
                ),
                argonRow(
                    width = 12,
                    argonCard(
                        width = 6, 
                        shadow = TRUE,
                        d3tree2Output("g_symptomtTreemap")
                    ),
                    argonCard(
                        width = 6, 
                        shadow = TRUE,
                        background_color = "info",
                        RagGridOutput("t_symptomTimeline")
                    )
                )
            ),
            argonTab(
                tabName = "Biomarkers",
                argonRow(
                    width = 12,
                    argonCard(
                        width = 12,
                        # title = uiOutput("ui_bioTimeline_select"),
                        title = "Biomarker Timeline",
                        argonRow(
                            width = 12, 
                            center = FALSE,
                            uiOutput("ui_bioTimeline_select")
                        ),
                        argonRow(
                            width = 12, 
                            center = TRUE,
                            plotlyOutput("g_bioTimeline")
                        )
                    

                        
                    )
                ),
                argonRow(
                    width = 12,
                    argonCard(
                        width = 6,
                        title = "Biomarkers on Hospital Admission",
                        plotlyOutput("g_bioAdmission")
                    ),
                    argonCard(
                        width = 6,
                        title = "Biomaker Prevalence",
                        plotlyOutput("g_bioCounts")
                    )
                    
                        
                )
            ),
            
            # argonTab(
            #     tabName = "Patient Outcomes",
            #     argonCard(
            #         width = 12, 
            #         title = "Coming Soon ..."
            #     )
            # ),
            argonTab(
                tabName = "About",
                argonRow(
                    width = 12, 
                    center = TRUE,
                    argonCard(
                        color = "secondary",
                        title = NULL,
                        width = 6,
                        shadow = TRUE,
                        argonR::argonUser(
                            title = tags$a(href="https://travisspear.github.io", tags$h1('Travis Spear'), target = "_blank"),
                            url = 'travisspear.github.io',
                            src = 'Spear_Picture.jpg',
                            style = "text-align:center",
                            argonRow(
                                width = 2, 
                                center = TRUE,
                                shinydashboardPlus::socialButton(url = "https://www.linkedin.com/in/travis-spear/", type = "linkedin"),
                                shinydashboardPlus::socialButton(url = "https://github.com/travisspear", type = "github")
                            ),
                            br(),
                            tags$h3("About the Project"),
                            div(tags$p(
                                'This project was authored by Travis Spear and uses data from', tags$a(href="https://medawaresystems.com/", 'MedAware Systems, Inc.', target = "_blank"), 
                                ' to provide users with an overview of COVID-19 data beyond what is readily available to the public. Collected data reflects the aggregation of published case reports, 
                                observational studies, and clinical trials conducted on the 2020 viral pandemic. The current data set is incomplete but with over 100 unique studies and 1700 patients,
                                it is easy to see some trends in collected biomarkers and symptoms that could help guide future work.'
                            )),
                            br(),
                            tags$hr(),
                            tags$h3("Interested in Conducting Your Own Analysis?"),
                            div(tags$p(
                                'Please contact ', tags$a(href="https://medawaresystems.com/", 'MedAware Systems, Inc.', target = "_blank"), 
                                ' or ', tags$a(href="https://sohinfo.com/", 'SOHInfo', target = "_blank"), ' for more information on their curated data sets and interactive data portal.'
                            ))
                            
                        )
                       
                        
                        
                        
                    )

                )
                
                )
            

        )
    )
)
