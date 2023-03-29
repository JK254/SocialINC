source(file = "reference_sources.R", encoding = "UTF-8")
# Install packages ----
package_list <- c(
  "arrow",
  "scales",
  "plotly",
  "shiny",
  "stringr",
  "readxl",
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
library(readxl)
library(shinyWidgets)
library(stringr)
library(tidyverse)

# Colorblind friendly colors ----
pal <- c("#203BB8","#9C6C0E","#8D0738", "#DE2E28","#005B55","#107EA3","#3D8259","#073B4C","#C91FCC") 
pal_2<- c("#069F72","#003D47", "#599BA2","#840067")
#pal<- c("#069F72","#003D47", "#599BA2","#840067")

# Reading the data file------
discrimination_short <- read_excel("discrimination_short.xlsx")
discriminationDT <- read_parquet(file = "discriminationDT.parquet")
discriminationDT <- discriminationDT %>%
mutate(before_since  = case_when(before_since == '5 years before covid-19 pandemic' ~ '5 years before COVID-19 pandemic',
                                 before_since == 'Since the beginning of covid-19 pandemic' ~ 'Since the beginning of COVID-19 pandemic',
                                 before_since == 'Expérience(s) discrimination en raison de la religion' ~ 'Expérience(s) de discrimination en raison de la religion',
                                 TRUE ~ as.character(before_since))) %>%

  mutate(geo_fr = discrimination_short$fr[match(Geography, discrimination_short$en)],
         vismin_fr = discrimination_short$fr[match(VisMin, discrimination_short$en)],
         charact_fr = discrimination_short$fr[match(Characteristic, discrimination_short$en)],
         ind_fr = discrimination_short$fr[match(Indicator, discrimination_short$en)],
         char_type_fr = discrimination_short$fr[match(char_type, discrimination_short$en)],
         ind_2_fr = discrimination_short$fr[match(ind, discrimination_short$en)],
         before_since_fr = discrimination_short$fr[match(before_since, discrimination_short$en)],
         conf_fr = discrimination_short$fr[match(Confidence, discrimination_short$en)])

discriminationDT <-
  discriminationDT %>%
  mutate(
    before_since_fr = ifelse(
      ind_fr == "Expérience(s) de discrimination depuis le début de la pandémie de COVID-19",
      "Depuis le début de la pandémie de COVID-19",
      str_to_sentence(trimws(
        sub(
          pattern = ".*,",
          replacement = "",
          x = ind_fr
        )
      ))
    ),
    before_since_fr = sub(
      pattern = "  ",
      replacement = " ",
      x = before_since_fr
    ),
    ind_2_fr = case_when(
      ind_fr == "Expérience(s) de discrimination depuis le début de la pandémie de COVID-19" ~ "Expérience(s) de discrimination",
      ind_fr %in% c(
        "Discrimination au travail ou au moment de présenter une demande d'emploi ou d'avancement, depuis le début de la pandémie de COVID-19",
        "Discrimination au travail ou au moment de présenter une demande d'emploi ou d'avancement, 5 ans avant la pandémie de COVID-19"
      ) ~ "Discrimination au travail ou au moment de présenter une demande d'emploi ou d'avancement",
      ind_fr %in% c(
        "Discrimination dans un magasin, une banque ou un restaurant, depuis le début de la pandémie de COVID-19",
        "Discrimination dans une magasin, une banque ou un restaurant, 5 ans avant la pandémie de COVID-19"
      ) ~ "Discrimination dans un magasin, une banque ou un restaurant",
      TRUE ~ trimws(sub(
        pattern = ", .*",
        replacement = "",
        x = ind_fr
      ))
    )
  )  %>% 
  mutate(before_since_fr  = case_when(before_since_fr == '5 ans avant la pandémie de covid-19' ~ '5 ans avant la pandémie de COVID-19',
                                   before_since_fr == 'Depuis le début de la pandémie de covid-19' ~ 'Depuis le début de la pandémie de COVID-19',
                                   TRUE ~ as.character(before_since_fr)))%>% 
  mutate(ind_2_fr  = case_when(ind_2_fr == 'Expérience(s) discrimination en raison de la religion' ~ 'Expérience(s) de discrimination en raison de la religion',
                              ind_2_fr == 'Discrimination dans une magasin, une banque ou un restaurant' ~ 'Discrimination dans un magasin, une banque ou un restaurant',
                                      TRUE ~ as.character(ind_2_fr)))
#Visible minority groups ------
vm_10 <- c("Total – Minorités visibles",
           "Total de la population des minorités visibles",                 
           "Sud-Asiatique",                                 
           "Chinois",                                      
           "Noir",                                         
           "Philippin", 
           "Latino-Américain",
           "Arabe",                         
           "Asiatique du Sud-Est")

## Police data ----
polData_short <- read_excel("polData_short.xlsx")
polData <- read_parquet(file = "polData.parquet")
polData <- polData %>% 
  mutate(geo_fr = polData_short$fr[match(Geography, polData_short$en)],
         Motivation_fr = polData_short$fr[match(Motivation, polData_short$en)],
         motivation_type_fr = polData_short$fr[match(motivation_type, polData_short$en)])
polData <-
  polData %>%
  mutate(motivation_type_fr = ifelse(
    Motivation_fr %in% c(
      "Total des crimes haineux déclarés par la police",
      "Race ou origine ethnique",
      "Religion",
      "Orientation sexuelle",
      "Langue",
      "Incapacité",
      "Sexe",
      "Âge",
      "Motivation inconnue"
    ),
    "Total des crimes haineux déclarés par la police",
    "Race ou origine ethnique"
  ))

perceptionDT_short <- read_excel("perceptionDT_short.xlsx")
perceptionDT <- read_parquet(file = "perceptionDT.parquet")
perceptionDT <- perceptionDT %>%
  mutate(geo_fr = perceptionDT_short$fr[match(Geography, perceptionDT_short$en)],
         vismin_fr = perceptionDT_short$fr[match(VisMin, perceptionDT_short$en)],
         charact_fr = perceptionDT_short$fr[match(Characteristic, perceptionDT_short$en)],
         ind_fr = perceptionDT_short$fr[match(Indicator, perceptionDT_short$en)],
         char_type_fr = perceptionDT_short$fr[match(char_type, perceptionDT_short$en)],
         conf_fr = perceptionDT_short$fr[match(Confidence, perceptionDT_short$en)])

# User interface // create layout ----
ui <-
  fluidPage(
        sidebarLayout(
          sidebarPanel(
            width = 3,
            ## 1. Indicator ----
            selectizeInput(
              inputId = "indicator_1",
              label = "Indicateur",
              choices = list (
                "Expérience(s) de discrimination",
                "Expérience(s) de discrimination en raison de l'origine ethnique ou de la culture",
                "Expérience(s) de discrimination en raison de la race ou de la couleur de la peau",
                "Expérience(s) de discrimination en raison de la religion",
                "Expérience(s) de discrimination en raison de la langue",
                "Discrimination au travail ou au moment de présenter une demande d'emploi ou d'avancement",
                "Discrimination dans les rapports avec la police",
                "Discrimination dans un magasin, une banque ou un restaurant",
                "Discrimination à l'école ou en suivant des cours",
                "Est satisfait de sa sécurité personnelle par rapport à la criminalité",
                "Crimes haineux déclarés par la police motivés par la race ou l’origine ethnique, la religion et d’autres motifs"
              )
            ),
            #### 2.10. Discrimination and victimization ----
            ##### 2.10.1. Discrimination and victimization (part 1) ----
            #'NOTE [discriminationDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Expérience(s) de discrimination'
              || input.indicator_1 == 'Expérience(s) de discrimination en raison de l\\'origine ethnique ou de la culture'
              || input.indicator_1 == 'Expérience(s) de discrimination en raison de la race ou de la couleur de la peau'
              || input.indicator_1 == 'Expérience(s) de discrimination en raison de la religion'
              || input.indicator_1 == 'Expérience(s) de discrimination en raison de la langue'
              || input.indicator_1 == 'Discrimination dans les rapports avec la police'
              || input.indicator_1 == 'Discrimination dans un magasin, une banque ou un restaurant'
              || input.indicator_1 == 'Discrimination à l\\'école ou en suivant des cours'",
              
              ###### Visible Minority ----
              #'note_fr [this is the focal variable for this tab]
              pickerInput(
                inputId = "discrimination_vismin", # name this for the server
                label = "Minorité visible*", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Désélectionner tout",
                  `select-all-text` = "Sélectionner tout"
                )
                ),
              
              ###### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "discrimination_sociodem",
                label = "Caractéristiques sociodémographiques",
                choices = unique(as.character(discriminationDT$char_type_fr)),
                
              ),
              ####### Age ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Groupe d\\'âge'",
                selectizeInput(
                  inputId = "discrimination_age",
                  label = "Groupe d'âge",
                  choices = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Groupe d'âge"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Groupe d'âge"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                  )
              ),
              ####### Gender ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Genre'",
                selectizeInput(
                  inputId = "discrimination_sex",
                  label = "Genre",
                  choices = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Genre"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Genre"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                  )
              ),
              ####### Immigration Status ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Statut d\\'immigrant'",
                selectizeInput(
                  inputId = "discrimination_immigration",
                  label = "Statut d'immigrant",
                  choices = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Statut d'immigrant"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Statut d'immigrant"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                  )
              ),
              ####### Generation Status ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Statut des générations'",
                selectizeInput(
                  inputId = "discrimination_generation",
                  label = "Statut des générations",
                  choices = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Statut des générations"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Statut des générations"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                  )
              ),
              ####### Language Spoken ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Première langue officielle parlée'",
                selectizeInput(
                  inputId = "discrimination_language",
                  label = "Première langue officielle parlée",
                  choices = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Première langue officielle parlée"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Première langue officielle parlée"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                  )
              ),
              ####### Education Status ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Plus haut certificat, diplôme ou grade'",
                selectizeInput(
                  inputId = "discrimination_education",
                  label = "Plus haut certificat, diplôme ou grade",
                  choices = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Plus haut certificat, diplôme ou grade"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(discriminationDT$charact_fr)[discriminationDT$char_type_fr == "Plus haut certificat, diplôme ou grade"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                  )
              ),
              # ###### Geography  ----
              # selectizeInput(
              #   inputId = "discrimination_geography",
              #   label = "Choisir une géographie",
              #   choices = unique(as.character(discriminationDT$geo_fr))
              # ),
              ###### Confidence Interval ----
              pickerInput(
                inputId = "discrimination_conf_interval",
                label = "Intervalles de confiance de 95%",
                choices = unique(as.character(discriminationDT$conf_fr)),
                multiple = TRUE, 
                selected = unique(as.character(discriminationDT$conf_fr))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Désélectionner tout",
                  `select-all-text` = "Sélectionner tout"
                )
              ),
            ),
            #### 2.10.2 Public services and institutions (Part 3)----
            #'NOTE [perceptionDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Est satisfait de sa sécurité personnelle par rapport à la criminalité'",
              
              ###### Year ----
              # pickerInput(
              #   inputId = "perception_year", # name this for the server
              #   label = "Année(s) de référence", # label of filter
              #   choices = unique(perceptionDT$Year)), # create drop-down list option
              # selected = sort(unique(perceptionDT$Year), decreasing = TRUE)[1],
              # multiple = TRUE), # multi-select
              # ##### Geography  ----
              # selectizeInput(
              #   inputId = "perception_geography",
              #   label = "Choisir une région géographique",
              #   choices = unique(as.character(perceptionDT$Geography))
              # ),
              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "perception_vismin_fr", # name this for the server
                label = "Minorité visible*", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Désélectionner tout",
                  `select-all-text` = "Sélectionner tout"
                )),
              ##### Selected sociodemographic  ----
              selectizeInput(
                inputId = "perception_sociodem",
                label = "Caractéristiques sociodémographiques",
                choices = unique(as.character(perceptionDT$char_type_fr))
              ),
              ###### Age ----
              conditionalPanel(
                condition = "input.perception_sociodem == 'Groupe d\\'âge'",
                selectizeInput(
                  inputId = "perception_age",
                  label = "Groupe d'âge",
                  choices = unique(as.character(perceptionDT$charact_fr)[perceptionDT$char_type_fr == "Groupe d'âge"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(perceptionDT$charact_fr)[perceptionDT$char_type_fr == "Groupe d'âge"][1],
                  #                   options = list(
                  #                     `actions-box` = TRUE,
                  #                     `deselect-all-text` = "Désélectionner tout",
                  #                     `select-all-text` = "Sélectionner tout"
                  #                   ))
                )
              ),
              ###### Genre ----
              conditionalPanel(
                condition = "input.perception_sociodem == 'Genre'",
                selectizeInput(
                  inputId = "perception_sex",
                  label = "Genre",
                  choices = unique(as.character(perceptionDT$charact_fr)[perceptionDT$char_type_fr == "Genre"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(perceptionDT$charact_fr)[perceptionDT$char_type_fr == "Genre"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                )
              ),
              ###### Statut d’immigration ----
              conditionalPanel(
                condition = "input.perception_sociodem == 'Statut d\\'immigrant'",
                selectizeInput(
                  inputId = "perception_immigration",
                  label = "Statut d'immigrant",
                  choices = unique(as.character(perceptionDT$charact_fr)[perceptionDT$char_type_fr == "Statut d'immigrant"]),
                  # multiple = TRUE, # multi-select
                  # selected = unique(as.character(perceptionDT$charact_fr)[perceptionDT$char_type_fr == "Statut d'immigrant"])[1],
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                )
              ),
              ##### Confidence Interval ----
              pickerInput(
                inputId = "perception_interval",
                label = "Intervalles de confiance de 95%",
                choices = unique(as.character(perceptionDT$conf_fr)),
                multiple = TRUE,
                selected = unique(as.character(perceptionDT$conf_fr))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Désélectionner tout",
                  `select-all-text` = "Sélectionner tout"
                )
              ) 
            ),
            ##### 2.10.3. Discrimination and victimization (part 2) ----
            #'NOTE [polData]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Crimes haineux déclarés par la police motivés par la race ou l\\’origine ethnique, la religion et d\\’autres motifs'",
              ##### Year----
              pickerInput(
                inputId = "hate_year",
                label = "Année de référence",
                choices = unique(as.character(polData$Year)),
                selected = unique(as.character(polData$Year))[3:10], # I made it 15 to overcapture in case more years come in
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Désélectionner tout",
                  `select-all-text` = "Sélectionner tout"
                )),
              ##### Motivation----
              selectizeInput(
                inputId = "hate_motivation",
                label = "Motivation",
                choices = unique(as.character(polData$motivation_type_fr)),
                selected = unique(as.character(polData$motivation_type_fr))[1],              
              ),
             
              ##### Race or ethnicity ----
              conditionalPanel(
                condition = "input.hate_motivation == 'Race ou origine ethnique'",
                pickerInput(
                  inputId = "hate_race",
                  label = "Races ou origines ethniques",
                  choices = list ("Noir","Asiatique du Sud","Asiatique de l'Est ou du Sud-Est","Arabe ou Asiatique Occidentale",
                                  "Blanc","Autochtone","Races ou origines ethniques multiples","Autre race ou origine ethnique", "Race ou origine ethnique inconnue"),
                  selected = "Noir",
                  #choices = unique(as.character(polData$Motivation_fr)[polData$motivation_type == "Race ou origine ethnique"]),
                  #selected = unique(as.character(polData$Motivation_fr)[polData$motivation_type == "Race ou origine ethnique"]),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Désélectionner tout",
                    `select-all-text` = "Sélectionner tout"
                  )
                )
              ), 
              ##### Total police-reported hate crime -----
              conditionalPanel(
                condition = "input.hate_motivation == 'Total des crimes haineux déclarés par la police'",
                pickerInput(
                  inputId = "hate_police",
                  label = "Total des crimes haineux déclarés par la police",
                  choices = unique(as.character(polData$Motivation_fr)[polData$motivation_type_fr == "Total des crimes haineux déclarés par la police"]),
                  # selected = unique(as.character(polData$Motivation_fr)[polData$motivation_type_fr == "Total des crimes haineux déclarés par la police"]),
                  # multiple = TRUE,
                  # options = list(
                  #   `actions-box` = TRUE,
                  #   `deselect-all-text` = "Désélectionner tout",
                  #   `select-all-text` = "Sélectionner tout"
                  # )
                )
              ),
              # ##### Geography----
              # selectizeInput(
              #   inputId = "hate_geography",
              #   label = "Choisir une géographie",
              #   choices = unique(as.character(polData$geo_fr)),
              #   selected = unique(as.character(polData$geo_fr))[1],              
              # ),
              
            ),
          ),
          ### Main panel ----
          mainPanel(
            #### 10. Discrimination and victimization ----
            ##### 10.1. Experience(s) of discrimination ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Expérience(s) de discrimination'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_1",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            
            ##### 10.2. Experience(s) of discrimination based on ethnicity or culture ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Expérience(s) de discrimination en raison de l\\'origine ethnique ou de la culture'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination en raison de leur origine ethnique ou de leur culture</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_2",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            
            ##### 10.3. Experience(s) of discrimination based on race or colour ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Expérience(s) de discrimination en raison de la race ou de la couleur de la peau'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination en raison de leur race ou de la couleur de leur peau</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_3",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            
            ##### 10.4. Experience(s) of discrimination based on religion ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Expérience(s) de discrimination en raison de la religion'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination en raison de leur religion</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_4",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            
            ##### 10.5. Experience(s) of discrimination based on language ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Expérience(s) de discrimination en raison de la langue'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination en raison de leur langue</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_5",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            
            ##### 10.6. Discrimination at work or when applying for a job or promotion ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Discrimination au travail ou au moment de présenter une demande d\\'emploi ou d\\'avancement'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination au travail ou au moment de présenter une demande d'emploi ou d'avancement</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_6",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            
            ##### 10.7. Discrimination when dealing with the police ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Discrimination dans les rapports avec la police'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination dans les rapports avec la police</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_7",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            
            ##### 10.8. Discrimination when in a store, bank or restaurant ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Discrimination dans un magasin, une banque ou un restaurant'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination dans un magasin, une banque ou un restaurant</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_8",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            
            ##### 10.9. Discrimination when attending school or classes ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Discrimination à l\\'école ou en suivant des cours'",
              h3(HTML("<b>Proportion de personnes ayant été victimes de discrimination à l'école ou en suivant des cours</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_9",
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_fr)
            ),
            ##### 10.10.  Police-reported hate crimes motivated by race or ethnicity, religion and other motives ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Crimes haineux déclarés par la police motivés par la race ou l\\’origine ethnique, la religion et d’autres motifs'",
              h3(HTML("<b>Nombre de crimes haineux déclarés par la police motivés par la race ou l’origine ethnique, la religion et d’autres motifs</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_hate_crime",height = 700,
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_ucrs_fr)
              
            ),
            ##### 10.11. Est satisfait de sa sécurité personnelle par rapport à la criminalité ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Est satisfait de sa sécurité personnelle par rapport à la criminalité'",
              h3(HTML("<b>Proportion de personnes satisfaites de leur sécurité personnelle par rapport à la criminalité</b>")),
              br(),
              br(),
              plotlyOutput("plot_vm_perception",height = 500,
                           inline = TRUE),
              br(),
              h4(note_fr),
              h4(source_gss_old_fr)
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
            ind_2_fr == filter_var,
            vismin_fr %in% input$discrimination_vismin,
            #geo_fr == input$discrimination_geography,
            conf_fr %in% input$discrimination_conf_interval,
            char_type_fr == input$discrimination_sociodem,
            (charact_fr ==input$discrimination_age |
               charact_fr == input$discrimination_sex |
               charact_fr == input$discrimination_immigration |
               charact_fr == input$discrimination_generation |
               charact_fr == input$discrimination_language |
               charact_fr == input$discrimination_education
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
            x = before_since_fr,
            y = Value,
            colour = conf_fr,
            fill = conf_fr,
            text = paste0(
              "Minorité visible: ",
              vismin_fr,
              "<br>",
              "Intervalle de confiance de 95 %: ",
              conf_fr,
              "<br>",
              "Pourcentage: ",
              format(Value, big.mark = " ", small.mark =","),
              "<br>"
              # "Période de référence en relation avec la pandémie de COVID-19: ",
              # before_since_fr
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
          y = "pourcentage",
          colour = NULL,
          fill = NULL
        )
    }, 
    tooltip = "text") %>%
      layout(legend = list(orientation = 'h', # show entries horizontally
                           # xanchor = "center",  # use center of legend as anchor
                           x = 0.1,  
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
            #geo_fr == input$hate_geography,
            motivation_type_fr == input$hate_motivation,
            (
              Motivation_fr %in% input$hate_race |
                Motivation_fr == input$hate_police
            )
          )
      })
    
    renderPlotly(ggplotly({
      ggplot(filtered_data(), 
             aes(x = Year,
                 y = Value,
                 colour = Motivation_fr,
                 fill = Motivation_fr,
                 text = paste0(
                   "Année de référence: ",
                   Year,
                   "<br>",
                   "Motivé: ",
                   Motivation_fr,
                   "<br>",
                   "Nombre: ",
                   format(Value, big.mark = " ", small.mark =",")
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
          y = "nombre",
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
  # Public services and institutions (perceptionDT)----
  filtered_data <-
    reactive({
      perceptionDT %>%
        filter(
          ind_fr == filter_var,
          vismin_fr %in% input$perception_vismin_fr,
          #Year  == input$perception_year,
          #geo_fr == input$perception_geography,
          conf_fr %in% input$perception_interval,
          char_type_fr == input$perception_sociodem,
          (charact_fr == input$perception_age |
             charact_fr == input$perception_sex |
             charact_fr == input$perception_immigration
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
          x = factor(vismin_fr,c("Pas une minorité visible",
                                 "Asiatique du Sud-Est",
                                 "Arabe",
                                 "Latino-Américain",
                                 "Philippin",
                                 "Noir",
                                 "Chinois",
                                 "Sud-Asiatique",
                                 "Total de la population des minorités visibles",
                                 "Total – Minorités visibles")),
          y = Value,
          colour = conf_fr,
          fill = conf_fr,
          text = paste0(
            "Minorité visible: ",
            vismin_fr,
            "<br>",
            "Intervalle de confiance de 95 %: ",
            conf_fr,
            "<br>",
            "Pourcentage: ",
            format(Value, big.mark = " ", small.mark =","),
            "<br>",
            "Année de référence: ",
            Year
          )
        )
      ) +
      theme_minimal() + coord_flip() +
      theme(legend.position = "right",
            axis.text.x = element_text(color = "black"),
            axis.title.y = element_blank()) +
      scale_fill_manual(values = pal_2) +
      scale_color_manual(values = pal_2) +
      scale_y_continuous(labels = comma) +
      labs(
        y = "pourcentage",
        colour = NULL,
        fill = NULL
      )
  }, 
  tooltip = "text") 
  #%>%
    # layout(legend = list(orientation = 'h', # show entries horizontally
    #                      # xanchor = "center",  # use center of legend as anchor
    #                      x = 0.1,  
    #                      y = -1.5)) %>%
    # layout(legend = list(bgcolor = "transparent", bordercolor = "transparent")) %>%
    # layout(legend = list(font = list(size = 12)))
  )
}
  ### Plots ----
  ### 10. Discrimination and victimization ----
  ##### 10.1. Experience(s) of discrimination ----
  output$plot_vm_discrimination_1 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[1])
  
  # ##### 10.2. Experience(s) of discrimination based on ethnicity or culture ----
  output$plot_vm_discrimination_2 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[2])
  
  # ##### 10.3. Experience(s) of discrimination based on race or colour ----
  output$plot_vm_discrimination_3 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[3])
  
  # ##### 10.4. Experience(s) of discrimination based on religion ----
  output$plot_vm_discrimination_4 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[4])
  
  # ##### 10.5. Experience(s) of discrimination based on language ----
  output$plot_vm_discrimination_5 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[5])
  
  # ##### 10.6. Discrimination at work or when applying for a job or promotion ----
  output$plot_vm_discrimination_6 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[6])
  
  # ##### 10.7. Discrimination when dealing with the police ----
  output$plot_vm_discrimination_7<-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[7])
  
  # ##### 10.8. Discrimination when in a store, bank or restaurant ----
  output$plot_vm_discrimination_8 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[8])
  
  # ##### 10.9. Discrimination when attending school or classes ----
  output$plot_vm_discrimination_9 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind_2_fr))[9])
  ##### 10.10. Police-reported hate crimes motivated by race or ethnicity, religion and other motives----
  output$plot_vm_hate_crime <- func_plot_hate(filter_var = unique(as.character(polData$ind_2_fr))[1])
  ##### 10.11. Est satisfait de sa sécurité personnelle par rapport à la criminalité----
  output$plot_vm_perception <- func_plot_perception(filter_var = unique(as.character(perceptionDT$ind_fr))[7])
}

# All together now ----
shinyApp(ui, server)


