source(file = "reference_sources.R", encoding = "UTF-8")
# Install packages ----
package_list <- c(
  "arrow",
  "scales",
  "plotly",
  "shiny",
  "stringr",
  "shinyWidgets",
  "tidyverse"
)
new_packages <-
  package_list[!(package_list %in% installed.packages()[, "Package"])]
if (length(new_packages))
  install.packages(new_packages)

rm(package_list, new_packages)
# Libraries -----
library(arrow)
library(scales)
library(plotly)
library(shiny)
library(shinyWidgets)
library(stringr)
library(tidyverse)
# Reading the data file------
discriminationDT <- read_parquet(file = "discriminationDT.parquet")  %>% 
  mutate(VisMin  = case_when(VisMin == 'Total, by visible minority group' ~ 'Total – Visible minority', 
                             VisMin == 'Total – Visible minority' ~ 'Total visible minority population', TRUE ~ as.character(VisMin))) %>% 
mutate(Confidence = case_when(Confidence == 'Lower 95% confidence interval, percentage of persons' ~ 'Lower limit', 
                              Confidence == 'Upper 95% confidence interval, percentage of persons' ~ 'Upper limit',TRUE ~ as.character(Confidence))) 


polData <-  read_parquet(file = "polData.parquet")


# Colorblind friendly colors ----
pal <- c("#203BB8","#9C6C0E","#8D0738", "#DE2E28","#005B55","#107EA3","#3D8259","#073B4C","#C91FCC") 
pal_2 <- c("#069F72","#003D47", "#599BA2","#840067")
#pal<- c("#069F72","#003D47", "#599BA2","#840067")
# Visible minority groups-----
vm_10 <-
  c("Total – Visible minority",
    "Total visible minority population",
    "South Asian",
    "Chinese",
    "Black",
    "Filipino",
    "Latin American",
    "Arab",
    "Southeast Asian",
    "Not a visible minority")
## Discrimination data ----
discriminationDT <-
  discriminationDT %>%
  mutate(
    before_since = ifelse(
      Indicator == "Experience(s) of discrimination since the beginning of COVID-19 pandemic",
      "Since the beginning of COVID-19 pandemic",
      str_to_sentence(trimws(
        sub(
          pattern = ".*,",
          replacement = "",
          x = Indicator
        )
      ))
    ),
    before_since = sub(
      pattern = "  ",
      replacement = " ",
      x = before_since
    ),
    ind = case_when(
      Indicator == "Experience(s) of discrimination since the beginning of COVID-19 pandemic" ~ "Experience(s) of discrimination",
      Indicator %in% c(
        "Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic",
        "Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic"
      ) ~ "Discrimination at work or when applying for a job or a promotion",
      Indicator %in% c(
        "Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic",
        "Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic"
      ) ~ "Discrimination in a store, bank or restaurant",
      TRUE ~ trimws(sub(
        pattern = ", .*",
        replacement = "",
        x = Indicator
      ))
    )
  ) %>% 
  mutate(before_since  = case_when(before_since == '5 years before covid-19 pandemic' ~ '5 years before COVID-19 pandemic',
                                   before_since == 'Since the beginning of covid-19 pandemic' ~ 'Since the beginning of COVID-19 pandemic',
                                   TRUE ~ as.character(before_since)))
                                  

## Police data ----
polData <-
  polData %>%
  mutate(motivation_type = ifelse(
    Motivation %in% c(
      "Total police-reported hate crime Race or ethnicity",
      "Race or ethnicity",
      "Religion",
      "Sexual orientation",
      "Language",
      "Disability",
      "Gender",
      "Age",
      "Unknown motivation"
    ),
    "Total police-reported hate crime",
    "Race or ethnicity"
  ))
perceptionDT <- read_parquet(file = "perceptionDT.parquet") %>%
  mutate(Confidence = case_when(Confidence == 'Lower 95% confidence interval, percentage of persons' ~ 'Lower limit', 
                                Confidence == 'Upper 95% confidence interval, percentage of persons' ~ 'Upper limit',TRUE ~ as.character(Confidence)))
# User interface // create layout ----
ui <-
  fluidPage(
        sidebarLayout(
          sidebarPanel(
            width = 3,
            ## 1. Indicator ----
            selectizeInput(
              inputId = "indicator_1",
              label = "Indicator",
              choices = list (
                              "Experience(s) of discrimination",
                              "Experience(s) of discrimination based on ethnicity or culture",
                              "Experience(s) of discrimination based on race or colour",
                              "Experience(s) of discrimination based on religion",
                              "Experience(s) of discrimination based on language",
                              "Discrimination at work or when applying for a job or promotion",
                              "Discrimination when dealing with the police",
                              "Discrimination when in a store, bank or restaurant",
                              "Discrimination when attending school or classes",
                              "Satisfied with personal safety from crime",
                              "Police-reported hate crimes motivated by race or ethnicity, religion and other motives"
              )
            ),
            #### 2.10. Discrimination and victimization ----
            ##### 2.10.1. Discrimination and victimization (part 1) ----
            #'NOTE [discriminationDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Experience(s) of discrimination'
              || input.indicator_1 == 'Experience(s) of discrimination based on ethnicity or culture'
              || input.indicator_1 == 'Experience(s) of discrimination based on race or colour'
              || input.indicator_1 == 'Experience(s) of discrimination based on religion'
              || input.indicator_1 == 'Experience(s) of discrimination based on language'
              || input.indicator_1 == 'Discrimination at work or when applying for a job or promotion'
              || input.indicator_1 == 'Discrimination when dealing with the police'
              || input.indicator_1 == 'Discrimination when in a store, bank or restaurant'
              || input.indicator_1 == 'Discrimination when attending school or classes'",
           
              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "discrimination_vismin", # name this for the server
                label = "Visible minority*", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )
                ),
              ##### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "discrimination_sociodem",
                label = "Sociodemographic characteristics",
                choices = unique(as.character(discriminationDT$char_type))
              ),
              ###### Age ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Age group'",
                selectizeInput(
                  inputId = "discrimination_age",
                  label = "Age group",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age group"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age group"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                  )
              ),
              ###### Gender ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "discrimination_sex",
                  label = "Gender",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Gender"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Gender"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                  )
              ),
              ###### Immigration Status ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Immigration status'",
                selectizeInput(
                  inputId = "discrimination_immigration",
                  label = "Immigration status",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Immigration status"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Immigration status"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                  )
              ),
              ###### Generation Status ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Generation status'",
                selectizeInput(
                  inputId = "discrimination_generation",
                  label = "Generation status",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Generation status"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Generation status"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                  )
              ),
              ###### First official language spoken ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'First official language spoken'",
                selectizeInput(
                  inputId = "discrimination_language",
                  label = "First official language spoken",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "First official language spoken"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "First official language spoken"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                  )
              ),
              ###### Highest certificate, diploma or degree ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Highest certificate, diploma or degree'",
                selectizeInput(
                  inputId = "discrimination_education",
                  label = "Highest certificate, diploma or degree",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Highest certificate, diploma or degree"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Highest certificate, diploma or degree"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                  )
              ),
              # ##### Geography ----
              # selectizeInput(
              #   inputId = "discrimination_geography",
              #   label = "Geography",
              #   choices = unique(as.character(discriminationDT$Geography))
              # ),
              ##### Confidence Interval ----
              pickerInput(
                inputId = "discrimination_conf_interval",
                label = "95% confidence intervals",
                choices = unique(as.character(discriminationDT$Confidence)),
                multiple = TRUE,
                selected = unique(as.character(discriminationDT$Confidence))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )
              )
            ),
            #### 2.10.2 Public services and institutions (Part 3)----
            #'NOTE [healthDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Satisfied with personal safety from crime'",
              # ##### Geography  ----
              # selectizeInput(
              #   inputId = "perception_geography",
              #   label = "Choose a geography",
              #   choices = unique(as.character(perceptionDT$Geography))
              # ),
              ###### Year ----
              # pickerInput(
              #   inputId = "perception_year", # name this for the server
              #   label = "Choose a year of reference", # label of filter
              #   choices = unique(perceptionDT$Year)), # create drop-down list option
              # selected = sort(unique(perceptionDT$Year), decreasing = TRUE)[1],
              # multiple = TRUE), # multi-select
              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "perception_vismin", # name this for the server
                label = "Visible minority*", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "perception_sociodem",
                label = "Sociodemographic characteristics",
                choices = unique(as.character(perceptionDT$char_type))
              ),
              ###### Age ----
              conditionalPanel(
                condition = "input.perception_sociodem == 'Age group'",
                selectizeInput(
                  inputId = "perception_age",
                  label = "Age group",
                  choices = unique(as.character(perceptionDT$Characteristic)[perceptionDT$char_type == "Age group"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(perceptionDT$Characteristic)[perceptionDT$char_type == "Age group"][1],
                  #                   options = list(
                  #                     `actions-box` = TRUE,
                  #                     `deselect-all-text` = "Deselect all",
                  #                     `select-all-text` = "Select all"
                  #                   ))
                )
              ),
              ###### Gender ----
              conditionalPanel(
                condition = "input.perception_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "perception_sex",
                  label = "Gender",
                  choices = unique(as.character(perceptionDT$Characteristic)[perceptionDT$char_type == "Gender"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(perceptionDT$Characteristic)[perceptionDT$char_type == "Gender"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                )
              ),
              ###### Immigration Status ----
              conditionalPanel(
                condition = "input.perception_sociodem == 'Immigration status'",
                selectizeInput(
                  inputId = "perception_immigration",
                  label = "Immigration status",
                  choices = unique(as.character(perceptionDT$Characteristic)[perceptionDT$char_type == "Immigration status"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(perceptionDT$Characteristic)[perceptionDT$char_type == "Immigration status"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                )
              ),
              ##### Confidence Interval ----
              pickerInput(
                inputId = "perception_interval",
                label = "95% confidence intervals",
                choices = unique(as.character(perceptionDT$Confidence)),
                multiple = TRUE,
                selected = unique(as.character(perceptionDT$Confidence))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )
              )
            ),
            ##### 2.10.2. Discrimination and victimization (part 2) ----
            #'NOTE [polData]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Police-reported hate crimes motivated by race or ethnicity, religion and other motives'",
              ##### Year----
              pickerInput(
                inputId = "hate_year",
                label = "Reference year",
                choices = unique(as.character(polData$Year)),
                selected = unique(as.character(polData$Year))[5:10], # I made it 15 to overcapture in case more years come in
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Motivation----
              selectizeInput(
                inputId = "hate_motivation",
                label = "Motivation",
                choices = unique(as.character(polData$motivation_type)),
                selected = unique(as.character(polData$motivation_type))[1],              
              ),
             
              ##### Race or ethnicity ----
              conditionalPanel(
                condition = "input.hate_motivation == 'Race or ethnicity'",
                pickerInput(
                  inputId = "hate_race",
                  label = "Race or ethnicity",
                  choices = list ("Black","South Asian","East or Southeast Asian","Arab or West Asian","White","Indigenous","Multiple races or ethnicities","Other race or ethnicity", "Unknown race or ethnicity"),
                  selected = "Black",
                  #choices = unique(as.character(polData$Motivation)[polData$motivation_type == "Race or ethnicity"]),
                  #selected = unique(as.character(polData$Motivation)[polData$motivation_type == "Race or ethnicity"]),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Deselect all",
                    `select-all-text` = "Select all"
                  )
                )
              ), 
              ##### Total police-reported hate crime -----
              conditionalPanel(
                condition = "input.hate_motivation == 'Total police-reported hate crime'",
                pickerInput(
                  inputId = "hate_police",
                  label = "Total police-reported hate crime",
                  choices = unique(as.character(polData$Motivation)[polData$motivation_type == "Total police-reported hate crime"]),
                  # selected = unique(as.character(polData$Motivation)[polData$motivation_type == "Total police-reported hate crime"]),
                  # multiple = TRUE,
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Deselect all",
                  #   `select-all-text` = "Select all"
                  # )
                )
              ),
              # ##### Geography----
              # selectizeInput(
              #   inputId = "hate_geography",
              #   label = "Geography",
              #   choices = unique(as.character(polData$Geography)),
              #   selected = unique(as.character(polData$Geography))[1],              
              # ),
            
            ),
          ),
          ### Main panel ----
          mainPanel(
          #### 10. Discrimination and victimization ----
          ##### 10.1. Experience(s) of discrimination ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Experience(s) of discrimination'",
            h3(HTML("<b>Percent of population having experience(s) of discrimination</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_1",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          
          ##### 10.2. Experience(s) of discrimination based on ethnicity or culture ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Experience(s) of discrimination based on ethnicity or culture'",
            h3(HTML("<b>Percent of population having experience(s) of discrimination based on ethnicity or culture</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_2",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          
          ##### 10.3. Experience(s) of discrimination based on race or colour ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Experience(s) of discrimination based on race or colour'",
            h3(HTML("<b>Percent of population having experience(s) of discrimination based on race or colour</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_3",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          
          ##### 10.4. Experience(s) of discrimination based on religion ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Experience(s) of discrimination based on religion'",
            h3(HTML("<b>Percent of population having experience(s) of discrimination based on religion</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_4",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          
          ##### 10.5. Experience(s) of discrimination based on language ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Experience(s) of discrimination based on language'",
            h3(HTML("<b>Percent of population having experience(s) of discrimination based on language</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_5",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          
          ##### 10.6. Discrimination at work or when applying for a job or promotion ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Discrimination at work or when applying for a job or promotion'",
            h3(HTML("<b>Percent of population facing discrimination at work or when applying for a job or promotion</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_6",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          
          ##### 10.7. Discrimination when dealing with the police ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Discrimination when dealing with the police'",
            h3(HTML("<b>Percent of population facing discrimination when dealing with the police</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_7",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          
          ##### 10.8. Discrimination when in a store, bank or restaurant ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Discrimination when in a store, bank or restaurant'",
            h3(HTML("<b>Percent of population facing discrimination when in a store, bank or restaurant</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_8",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          
          ##### 10.9. Discrimination when attending school or classes ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Discrimination when attending school or classes'",
            h3(HTML("<b>Percent of population facing discrimination when attending school or classes</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_discrimination_9",
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss)
          ),
          ##### 10.10.  Police-reported hate crimes motivated by race or ethnicity, religion and other motives ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Police-reported hate crimes motivated by race or ethnicity, religion and other motives'",
            h3(HTML("<b>Number of police-reported hate crimes motivated by race or ethnicity, religion and other motives</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_hate_crime",height = 700,
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_ucrs)
          ),
          ##### 10.11.  Satisfied with personal safety from crime ----
          conditionalPanel(
            condition = "input.indicator_1 == 'Satisfied with personal safety from crime'",
            h3(HTML("<b>Percent of the population satisfied with personal safety from crime</b>")),
            br(),
            br(),
            plotlyOutput("plot_vm_perception",height = 500,
                         inline = TRUE),
            br(),
            h4(note),
            h4(source_gss_old)
          ),
          )
        
      
    ))
# Server // determines the data underlying the UI ----
server <- function(input, output, session) {
  #'NOTE [This chart doesn't follow the same x-axis as the other charts]
  func_plot_discrimination <- function(filter_var){
    # Discrimination and victimization ----
    filtered_data <-
      reactive({
        discriminationDT %>%
          filter(
            ind == filter_var,
            VisMin %in% input$discrimination_vismin,
            #Geography == input$discrimination_geography,
            Confidence %in% input$discrimination_conf_interval,
            char_type == input$discrimination_sociodem,
            (Characteristic == input$discrimination_age |
               Characteristic == input$discrimination_sex |
               Characteristic == input$discrimination_immigration |
               Characteristic == input$discrimination_generation |
               Characteristic == input$discrimination_language |
               Characteristic == input$discrimination_education
            )
          )
      })
    
    renderPlotly(ggplotly({
      ggplot(filtered_data()) +
        geom_bar(
          stat = "identity",
          width = 0.6, 
          position = position_dodge(width = 0.9),
          aes(
            x = before_since,
            y = Value,
            colour = Confidence,
            fill = Confidence,
            text = paste0(
              "Visible minority group: ",
              VisMin,
              "<br>",
              "95% confidence interval: ",
              Confidence,
              "<br>",
              "Percent: ",
              format(Value, big.mark = ","),
              "<br>"
              # "Reference period in relation to the COVID-19 pandemic: ",
              # before_since
            )
          )
        ) +
        theme_minimal() +
        theme(legend.position = "bottom", 
              axis.text.x = element_text(color = "black"),
              axis.title.x=element_blank()) +
        scale_fill_manual(values = pal_2) +
        scale_color_manual(values = pal_2) +
        scale_y_continuous(labels = comma) +
        labs(
          y = "percent",
          colour = NULL,
          fill = NULL
        )
    }, 
    tooltip = "text") %>%
      layout(legend = list(orientation = 'h', # show entries horizontally
                           # xanchor = "center",  # use center of legend as anchor
                           x = 0.2,  
                           y = -0.1)) %>%
      layout(legend = list(bgcolor = "transparent", bordercolor = "transparent")) %>%
      layout(legend = list(font = list(size = 12)))
    )
  }
  func_plot_hate <- function(filter_var){
    # Hate crime ----
    filtered_data <-
      reactive({
        polData %>%
          filter(
            Year %in% input$hate_year,
            #Geography == input$hate_geography,
            motivation_type == input$hate_motivation,
            (
              Motivation %in% input$hate_race |
                Motivation == input$hate_police
            )
          )
      })
    
    renderPlotly(ggplotly({
      ggplot(filtered_data(), 
             aes(x = Year,
                 y = Value,
                 colour = Motivation,
                 fill = Motivation,
                 text = paste0(
                   "Reference year: ",
                   Year,
                   "<br>",
                   "Motivation: ",
                   Motivation,
                   "<br>",
                   "Number: ",
                   format(Value, big.mark = ",")
                 )
             )) +
        geom_line(group = 1) + # you need this when you want to make a line graph
        theme_light() + 
        theme(legend.position ="bottom",
              axis.title.x=element_blank()) +
        scale_fill_manual(values = pal) +
        scale_color_manual(values = pal) +
        scale_y_continuous(labels = comma) +
        labs(
          y = "number",
          colour = NULL,
          fill = NULL
        )
    },
    tooltip = "text")%>%
    layout(legend = list(orientation = 'h', # show entries horizontally
                         # xanchor = "center",  # use center of legend as anchor
                         x = 0.0,
                         y = -0.1)) %>%
    layout(legend = list(bgcolor = "transparent", bordercolor = "transparent")) %>%
    layout(legend = list(font = list(size = 12)))
    ) 
  }
  
  
  func_plot_perception<- function(filter_var = NULL)
  {
    filtered_data <-
      reactive({
        perceptionDT %>%
          filter(
            Indicator == filter_var,
            VisMin %in% input$perception_vismin,
            #Year  == input$perception_year,
            #Geography == input$perception_geography,
            Confidence %in% input$perception_interval,
            char_type == input$perception_sociodem,
            (Characteristic == input$perception_age |
               Characteristic == input$perception_sex |
               Characteristic == input$perception_immigration
            )
          )
      })
    
  renderPlotly(ggplotly({
    ggplot(filtered_data()) +
      geom_bar(
        stat = "identity",
        width = 0.6,
        position = position_dodge(width = 0.9),
        aes(
          x = factor(VisMin,c("Not a visible minority",
                              "Southeast Asian",
                              "Arab",
                              "Latin American",
                              "Filipino",
                              "Black",
                              "Chinese",
                              "South Asian",
                              "Total visible minority population",
                              "Total – Visible minority")),
          y = Value,
          colour = Confidence,
          fill = Confidence,
          text = paste0(
            "Visible minority: ",
            VisMin,
            "<br>",
            "95% confidence interval: ",
            Confidence,
            "<br>",
            "Percent: ",
            format(Value, big.mark = ","),
            "<br>",
            "Reference year: ",
            Year
          )
        )
      ) +
      theme_minimal() + 
      coord_flip() +
      theme(legend.position = "right", 
            axis.text.x = element_text(color = "black"),
            axis.title.y = element_blank()) +
      scale_fill_manual(values = pal_2) +
      scale_color_manual(values = pal_2) +
      scale_y_continuous(labels = comma) +
      labs(
        y = "percent",
        colour = NULL,
        fill = NULL
      )
  }, 
  tooltip = "text")
  #%>%
  # layout(legend = list(orientation = 'h', # show entries horizontally
  #                      # xanchor = "center",  # use center of legend as anchor
  #                      x = 0.0,
  #                      y = 1.2)) %>%
  # layout(legend = list(bgcolor = "transparent", bordercolor = "transparent")) %>%
  # layout(legend = list(font = list(size = 12)))
  )
}
  ### Plots ----
  ### 10. Discrimination and victimization ----
  ##### 10.1. Experience(s) of discrimination ----
  output$plot_vm_discrimination_1 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[1])
  
  # ##### 10.2. Experience(s) of discrimination based on ethnicity or culture ----
  output$plot_vm_discrimination_2 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[2])
  
  # ##### 10.3. Experience(s) of discrimination based on race or colour ----
  output$plot_vm_discrimination_3 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[3])
  
  # ##### 10.4. Experience(s) of discrimination based on religion ----
  output$plot_vm_discrimination_4 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[4])
  
  # ##### 10.5. Experience(s) of discrimination based on language ----
  output$plot_vm_discrimination_5 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[5])
  
  # ##### 10.6. Discrimination at work or when applying for a job or promotion ----
  output$plot_vm_discrimination_6 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[6])
  
  # ##### 10.7. Discrimination when dealing with the police ----
  output$plot_vm_discrimination_7<-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[7])
  
  # ##### 10.8. Discrimination when in a store, bank or restaurant ----
  output$plot_vm_discrimination_8 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[8])
  
  # ##### 10.9. Discrimination when attending school or classes ----
  output$plot_vm_discrimination_9 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[9])
  ##### 10.10. Police-reported hate crimes motivated by race or ethnicity, religion and other motives----
  output$plot_vm_hate_crime <- func_plot_hate(filter_var = unique(as.character(polData$ind))[1])
  #### 10.11. Satisfied with personal safety from crime----
  output$plot_vm_perception <- func_plot_perception(filter_var = unique(as.character(perceptionDT$Indicator))[7])
}

# All together now ----
shinyApp(ui, server)


