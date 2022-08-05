source(file = "1_example_data.R", encoding = "UTF-8")

dim_list <- 
  c(
    "Participation in the Labour Market",
    "Civic engagement and political participation",
    "Representation in decision-making positions",
    "Basic needs and housing",
    "Health and wellbeing",
    "Education, training and skills",
    "Income and wealth",
    "Social connections and personnal networks",
    "Local community",
    "Public services and institutions",
    "Discrimination and victimization"
  )

# User interface // create layout ----
ui <-
  fluidPage(
    titlePanel("Social Inclusion Data Visualization Tool"),
    # title of dashboard
    tabsetPanel(
      type = "pills",
      # type of navigation button
      
      ## Themes and Definitions of Indicators ----
      tabPanel(
        "Themes and Definitions of Indicators",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectizeInput(
              inputId = "theme_0",
              label = "Theme",
              choices = dim_list
            ),
          ),
          mainPanel(dataTableOutput("def_table"))
        )
      ), 
      
      ## 1. Theme: Groups designated as visible Minorities  ----
      tabPanel(
        "Groups designated as Visible Minorities",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            width = 3,
            
            ### Theme ----
            selectizeInput(
              inputId = "theme_1",
              label = "Theme",
              choices = dim_list
            ),
            
            ### 1. Conditional filtering for basicDT & healthDT ----
            conditionalPanel(
              condition = "input.theme_1  == 'Basic needs and housing'
              || input.theme_1 == 'Health and wellbeing'",
              #### Visible Minority ----
              selectizeInput(
                inputId = "vismin",
                label = "Choose a visible minority group",
                choices = unique(c(
                  as.character(basicDT$VisMin),
                  as.character(healthDT$VisMin)
                )),
                multiple = TRUE,
                selected = unique(c(
                  as.character(basicDT$VisMin),
                  as.character(healthDT$VisMin)
                ))[1]
              ),
              #### Year ----
              selectizeInput(
                inputId = "year",
                label = "Choose a year",
                choices = unique(c(
                  as.character(basicDT$Year),
                  as.character(healthDT$Year)
                ))
              ),
              #### Characteristic group ----
              selectizeInput(
                inputId = "char_type",
                label = "Choose a characteristic group",
                choices = unique(c(
                  as.character(basicDT$char_type),
                  as.character(healthDT$char_type)
                ))
              ),
              #### Characteristic ----
              selectizeInput(
                inputId = "characteristic",
                label = "Choose a characteristic",
                choices = unique(c(
                  as.character(basicDT$Characteristic),
                  as.character(healthDT$Characteristic)
                ))
              ),
              #### Indicator ----
              selectizeInput(
                inputId = "indicator",
                label = "Choose an indicator",
                choices = unique(c(
                  as.character(basicDT$Indicator),
                  as.character(healthDT$Indicator)
                ))
              ),
              #### Confidence ----
              selectizeInput(
                inputId = "confidence",
                label = "Choose a statistic",
                choices = unique(c(
                  as.character(basicDT$Confidence),
                  as.character(healthDT$Confidence)
                ))
              )
            ),

            ### 2. Conditional filtering for belongingDT, civicDT & confidenceDT ----
            conditionalPanel(
              condition = "input.theme_1  == 'Social connections and personnal networks'
              || input.theme_1 == 'Civic engagement and political participation'
              || input.theme_1 == 'Public services and institutions'",
              #### Visible Minority ----
              selectizeInput(
                inputId = "vismin",
                label = "Choose a visible minority group",
                choices = unique(c(
                  as.character(belongingDT$VisMin),
                  as.character(civicDT$VisMin),
                  as.character(confidenceDT$VisMin)
                )),
                multiple = TRUE,
                selected = unique(c(
                  as.character(belongingDT$VisMin),
                  as.character(civicDT$VisMin),
                  as.character(confidenceDT$VisMin)
                ))[1],
              ),
              #### Year ----
              selectizeInput(
                inputId = "year",
                label = "Choose a year",
                choices = unique(c(
                  as.character(belongingDT$Year),
                  as.character(civicDT$Year),
                  as.character(confidenceDT$Year)
                ))
              ),
              #### Geography ----
              selectizeInput(
                inputId = "geography",
                label = "Choose a year",
                choices = unique(c(
                  as.character(belongingDT$Geography),
                  as.character(civicDT$Geography),
                  as.character(confidenceDT$Geography)
                ))
              ),
              #### Characteristic group ----
              selectizeInput(
                inputId = "char_type",
                label = "Choose a characteristic group",
                choices = unique(c(
                  as.character(belongingDT$char_type),
                  as.character(civicDT$char_type),
                  as.character(confidenceDT$char_type)
                ))
              ),
              #### Characteristic ----
              selectizeInput(
                inputId = "characteristic",
                label = "Choose a characteristic",
                choices = unique(c(
                  as.character(belongingDT$Characteristic),
                  as.character(civicDT$Characteristic),
                  as.character(confidenceDT$Characteristic)
                ))
              ),
              #### Indicator ----
              selectizeInput(
                inputId = "indicator",
                label = "Choose an indicator",
                choices = unique(c(
                  as.character(belongingDT$Indicator),
                  as.character(civicDT$Indicator),
                  as.character(confidenceDT$Indicator)
                ))
              ),
              #### Confidence ----
              selectizeInput(
                inputId = "confidence",
                label = "Choose a statistic",
                choices = unique(c(
                  as.character(belongingDT$Confidence),
                  as.character(civicDT$Confidence),
                  as.character(confidenceDT$Confidence)
                ))
              )
            ),

            ### 3. Conditional filtering for group incomeDT & representationDT ----
            conditionalPanel(
              condition = "input.theme_1 == 'Income and wealth'
              || input.theme_1 == 'Representation in decision-making positions'",
              #### Visible Minority ----
              selectizeInput(
                inputId = "vismin",
                label = "Choose a visible minority group",
                choices = unique(c(
                  as.character(incomeDT$VisMin),
                  as.character(representationDT$VisMin)
                )),
                multiple = TRUE,
                selected = unique(c(
                  as.character(incomeDT$VisMin),
                  as.character(representationDT$VisMin)
                ))[1],
              ),
              #### Year ----
              selectizeInput(
                inputId = "year",
                label = "Choose a year",
                choices = unique(c(
                  as.character(incomeDT$Year),
                  as.character(representationDT$Year)
                ))
              ),
              #### Geography ----
              selectizeInput(
                inputId = "geography",
                label = "Choose a year",
                choices = unique(c(
                  as.character(incomeDT$Geography),
                  as.character(representationDT$Geography)
                ))
              ),
              #### Characteristic group ----
              selectizeInput(
                inputId = "char_type",
                label = "Choose a characteristic group",
                choices = unique(c(
                  as.character(incomeDT$char_type),
                  as.character(representationDT$char_type)
                ))
              ),
              #### Characteristic ----
              selectizeInput(
                inputId = "characteristic",
                label = "Choose a characteristic",
                choices = unique(c(
                  as.character(incomeDT$Characteristic),
                  as.character(representationDT$Characteristic)
                ))
              ),
              #### Indicator ----
              selectizeInput(
                inputId = "indicator",
                label = "Choose an indicator",
                choices = unique(c(
                  as.character(incomeDT$Indicator),
                  as.character(representationDT$Indicator)
                ))
              ),
              #### Confidence ----
              selectizeInput(
                inputId = "confidence",
                label = "Choose a statistic",
                choices = unique(c(
                  as.character(incomeDT$Confidence),
                  as.character(representationDT$Confidence)
                ))
              )
            ),

            ### 4. Conditional filtering for discriminationDT ----
            conditionalPanel(
              condition = "input.theme_1 == 'Discrimination and victimization'",
              #### Visible Minority ----
              selectizeInput(
                inputId = "vismin",
                label = "Choose a visible minority group",
                choices = unique(as.character(discriminationDT$VisMin)),
                multiple = TRUE,
                selected = unique(as.character(discriminationDT$VisMin))[1],
              ),
              #### Year ----
              selectizeInput(
                inputId = "year",
                label = "Choose a year",
                choices = unique(as.character(discriminationDT$Year))
              ),
              #### Geography ----
              selectizeInput(
                inputId = "geography",
                label = "Choose a year",
                choices = unique(as.character(discriminationDT$Geography))
              ),
              #### Characteristic group ----
              selectizeInput(
                inputId = "char_type",
                label = "Choose a characteristic group",
                choices = unique(as.character(discriminationDT$char_type))
              ),
              #### Characteristic ----
              selectizeInput(
                inputId = "characteristic",
                label = "Choose a characteristic",
                choices = unique(as.character(discriminationDT$Characteristic))
              ),
              #### Indicator ----
              selectizeInput(
                inputId = "indicator",
                label = "Choose an indicator",
                choices = unique(as.character(discriminationDT$Indicator))
              ),
              #### Confidence ----
              selectizeInput(
                inputId = "confidence",
                label = "Choose a statistic",
                choices = unique(as.character(discriminationDT$Confidence))
              ),
              #### Motivation ----
              selectizeInput(
                inputId = "motivation",
                label = "Choose a motivation",
                choices = unique(as.character(discriminationDT$Confidence))
              )
            ),

            ### 5. Conditional filtering educationDT ----
            conditionalPanel(
              condition = "input.theme_1 == 'Education, training and skills'",
              #### Visible Minority ----
              selectizeInput(
                inputId = "vismin",
                label = "Choose a visible minority group",
                choices = unique(as.character(educationDT$VisMin)),
                multiple = TRUE,
                selected = unique(as.character(educationDT$VisMin))[1],
              ),
              #### Year ----
              selectizeInput(
                inputId = "age",
                label = "Choose an age group",
                choices = unique(as.character(educationDT$Year))
              ),
              #### Age ----
              selectizeInput(
                inputId = "age",
                label = "Choose an age group",
                choices = unique(as.character(educationDT$Age))
              ),
              #### Sex ----
              selectizeInput(
                inputId = "sex",
                label = "Choose a sex",
                choices = unique(as.character(educationDT$Sex))
              ),
              #### Geography ----
              selectizeInput(
                inputId = "geography",
                label = "Choose a geography",
                choices = unique(as.character(educationDT$Geography))
              ),
              #### Indicator ----
              selectizeInput(
                inputId = "indicator",
                label = "Choose an indicator",
                choices = unique(as.character(educationDT$Indicator))
              ),
              #### Immigration ----
              selectizeInput(
                inputId = "immigration",
                label = "Choose an immigration status",
                choices = unique(as.character(educationDT$Immigration))
              ),
              #### Language ----
              selectizeInput(
                inputId = "language",
                label = "Choose a language",
                choices = unique(as.character(educationDT$Language))
              )
            ),

            ### 6. Conditional filtering rateDT ----
            conditionalPanel(
              condition = "input.theme_1 == 'Participation in the Labour Market'",
              #### Visible Minority ----
              selectizeInput(
                inputId = "vismin",
                label = "Choose a visible minority group",
                choices = unique(as.character(rateDT$VisMin)),
                multiple = TRUE,
                selected = unique(as.character(rateDT$VisMin))[1],
              ),
              #### Year ----
              selectizeInput(
                inputId = "age",
                label = "Choose an age group",
                choices = unique(as.character(rateDT$Year))
              ),
              #### Age ----
              selectizeInput(
                inputId = "age",
                label = "Choose an age group",
                choices = unique(as.character(rateDT$Age))
              ),
              #### Sex ----
              selectizeInput(
                inputId = "sex",
                label = "Choose a sex",
                choices = unique(as.character(rateDT$Sex))
              ),
              #### Geography ----
              selectizeInput(
                inputId = "geography",
                label = "Choose a geography",
                choices = unique(as.character(rateDT$Geography))
              ),
              #### Indicator ----
              selectizeInput(
                inputId = "indicator",
                label = "Choose an indicator",
                choices = unique(as.character(rateDT$Indicator))
              ),
              #### Immigration ----
              selectizeInput(
                inputId = "immigration",
                label = "Choose an immigration status",
                choices = unique(as.character(rateDT$Immigration))
              ),
              #### Language ----
              selectizeInput(
                inputId = "language",
                label = "Choose a language",
                choices = unique(as.character(rateDT$Language))
              ),
              #### Degree ----
              selectizeInput(
                inputId = "degree",
                label = "Choose a degree",
                choices = unique(as.character(rateDT$Degree))
              ),
              #### Location ----
              selectizeInput(
                inputId = "location",
                label = "Choose a location",
                choices = unique(as.character(rateDT$Location))
              )
            )
          ),
          
          ### Main panel ----
          mainPanel(plotlyOutput("plot_vismin", width = "100%"))
        )
      )
    )
  )

gc()
