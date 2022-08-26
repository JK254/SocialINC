source(file = "1_example_data.R", encoding = "UTF-8")

# Repeated lists ----
## VisMin lists ----
### 10 Values ----
#'NOTE [there's a specific order for vismin]
vm_10 <-
  c(
    "Total, by visible minority group",
    "Total visible minority population",
    "South Asian",
    "Chinese",
    "Black",
    "Filipino",
    "Latin American",
    "Arab",
    "Southeast Asian",
    "Not a visible minority"
  )

### 15 Values ----
#'NOTE [the order in the values with 15 are correct]

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
            #'NOTE [Used a radio button here to present all choices while making it a single selection]
            radioButtons(
              inputId = "theme_0",
              label = "Theme",
              choices =  unique(as.character(template$Theme))
            ),
          ),
          #'NOTE [Outputs a table that reacts to the user's selection]
          mainPanel(dataTableOutput("def_table"))
        )
      ), #'NOTE [END OF FIRST TAB]
      
      ## 1. Theme: Groups designated as visible Minorities  ----      
      tabPanel(
        "Groups designated as Visible Minorities",
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            width = 3,

            ### 1. Theme ----
            selectizeInput(
              inputId = "theme_1",
              label = "Theme",
              choices = unique(as.character(template$Theme))
            ),

            ### 2. Indicator ----
            selectizeInput(
              inputId = "indicator_1",
              label = "Indicator",
              choices = unique(as.character(template$Indicator))
            ),
            
            #### 2.1. Participation in the Labour Market ----
            ##### 2.1.1. Participation in the Labour Market (part 1) ----
            #'NOTE [rateDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Working-age population in the labour force (participation rate)'
              || input.indicator_1 == 'Working-age population in employment (employment rate)'
              || input.indicator_1 == 'Working-age population in unemployment (unemployment rate)'
              || input.indicator_1 == 'Workers working mainly full-time weeks in the previous year'",
              #'NOTE [indicators 1:4/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              selectizeInput(
                inputId = "lm_vismin",
                label = "Visible minority status",
                choices = unique(as.character(rateDT$VisMin)),
                multiple = TRUE,
                selected = unique(as.character(rateDT$VisMin))[1],
              ),
              ###### Degree ----
              selectizeInput(
                inputId = "lm_degree",
                label = "Highest certificate, diploma or degree",
                choices = unique(as.character(rateDT$Degree))
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_geography",
                label = "Geography",
                choices = unique(as.character(rateDT$Geography))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_immigration",
                label = "Immigrant and generation status",
                choices = unique(as.character(rateDT$Immigration))
              ),
              ###### Year ----
              selectizeInput(
                inputId = "lm_year",
                label = "Year",
                choices = unique(as.character(rateDT$Year))
              ),
              ###### Age ----
              selectizeInput(
                inputId = "lm_age",
                label = "Age group and first official language spoken",
                choices = unique(as.character(rateDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_sex",
                label = "Sex",
                choices = unique(as.character(rateDT$Sex))
              )
            ),

            ##### 2.1.2. Participation in the Labour Market (part 2) ----
            #'NOTE [representationDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Self-employed workers in the labour force (unincorporated)'",
              #'NOTE [indicators 5/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              selectizeInput(
                inputId = "lm_rep_vismin",
                label = "Visible minority status",
                choices = unique(as.character(representationDT$VisMin)),
                multiple = TRUE,
                selected = unique(as.character(representationDT$VisMin))[1],
              ),
              ###### Degree ----
              selectizeInput(
                inputId = "lm_rep_degree",
                label = "Highest certificate, diploma or degree",
                choices = unique(as.character(representationDT$Degree))
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_rep_geography",
                label = "Geography",
                choices = unique(as.character(representationDT$Geography))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_rep_immigration",
                label = "Immigrant and generation status",
                choices = unique(as.character(representationDT$Immigration))
              ),
              ###### Year ----
              selectizeInput(
                inputId = "lm_rep_year",
                label = "Year",
                choices = unique(as.character(representationDT$Year))
              ),
              ###### Age ----
              selectizeInput(
                inputId = "lm_rep_age",
                label = "Age group and first official language spoken",
                choices = unique(as.character(representationDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_rep_sex",
                label = "Sex",
                choices = unique(as.character(representationDT$Sex))
              )
            ),

            ##### 2.1.3. Participation in the Labour Market (part 3) ----
            #'NOTE [OverQualDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Overqualified workers with a university degree'",
              #'NOTE [indicators 6/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              selectizeInput(
                inputId = "lm_over_vismin",
                label = "Visible minority status",
                choices = unique(as.character(OverQualDT$VisMin)),
                multiple = TRUE,
                selected = unique(as.character(OverQualDT$VisMin))[1],
              ),
              ###### Location of Study ----
              selectizeInput(
                inputId = "lm_over_degree",
                label = "Location of Study",
                choices = unique(as.character(OverQualDT$Location))
              ),
              ###### Degree ----
              selectizeInput(
                inputId = "lm_over_degree",
                label = "Highest certificate, diploma or degree",
                choices = unique(as.character(OverQualDT$Degree))
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_over_geography",
                label = "Geography",
                choices = unique(as.character(OverQualDT$Geography))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_over_immigration",
                label = "Groups designated by Immigration and Generational Status",
                choices = unique(as.character(OverQualDT$Immigration))
              ),
              ###### Year ----
              selectizeInput(
                inputId = "lm_over_year",
                label = "Year",
                choices = unique(as.character(OverQualDT$Year))
              ),
              ###### Age ----
              selectizeInput(
                inputId = "lm_over_age",
                label = "Age group",
                choices = unique(as.character(OverQualDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_over_sex",
                label = "Sex",
                choices = unique(as.character(OverQualDT$Sex))
              ),
              ###### Language ----
              selectizeInput(
                inputId = "lm_over_language",
                label = "Choose a language",
                choices = unique(as.character(OverQualDT$Language))
              )
            ),

            ##### 2.1.4. Participation in the Labour Market (part 4) ----
            #'NOTE [youthDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Youth not in employment, education or training (NEET)'",
              #'NOTE [indicators 7/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              selectizeInput(
                inputId = "lm_youth_vismin",
                label = "Visible minority status",
                choices = unique(as.character(youthDT$VisMin)),
                multiple = TRUE,
                selected = unique(as.character(youthDT$VisMin))[1],
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_youth_geography",
                label = "Geography",
                choices = unique(as.character(youthDT$Geography))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_youth_immigration",
                label = "Immigrant and generation status",
                choices = unique(as.character(youthDT$Immigration))
              ),
              ###### Year ----
              selectizeInput(
                inputId = "lm_youth_year",
                label = "Year",
                choices = unique(as.character(youthDT$Year))
              ),
              ###### Age ----
              selectizeInput(
                inputId = "lm_youth_age",
                label = "Age group",
                choices = unique(as.character(youthDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_youth_sex",
                label = "Sex",
                choices = unique(as.character(youthDT$Sex))
              ),
              ###### Language ----
              selectizeInput(
                inputId = "lm_youth_language",
                label = "Choose a language",
                choices = unique(as.character(youthDT$Language))
              )
            ),

            ##### 2.1.5. Participation in the Labour Market (part 5) ----
            #'NOTE [incomeDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Average employment income of the population'
              || input.indicator_1 == 'Average weekly wage of paid employees'",
              #'NOTE [indicators 8:9/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              selectizeInput(
                inputId = "lm_income_vismin",
                label = "Visible minority status",
                choices = unique(as.character(incomeDT$VisMin)),
                multiple = TRUE,
                selected = unique(as.character(incomeDT$VisMin))[1],
              ),
              ###### Degree ----
              selectizeInput(
                inputId = "lm_income_degree",
                label = "Highest certificate, diploma or degree",
                choices = unique(as.character(incomeDT$Degree))
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_income_geography",
                label = "Geography",
                choices = unique(as.character(incomeDT$Geography))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_income_immigration",
                label = "Immigrant and generation status",
                choices = unique(as.character(incomeDT$Immigration))
              ),
              ###### Year ----
              selectizeInput(
                inputId = "lm_income_year",
                label = "Year",
                choices = unique(as.character(incomeDT$Year))
              ),
              ###### Age ----
              selectizeInput(
                inputId = "lm_income_age",
                label = "Age group and first official language spoken",
                choices = unique(as.character(incomeDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_income_sex",
                label = "Sex",
                choices = unique(as.character(incomeDT$Sex))
              )
            ),

            ##### 2.1.6. Participation in the Labour Market (part 6) ----
            #'NOTE [employmentDT]
            #' [THIS TABLE HAS NOT BEEN PUBLISHED YET]
            #'NOTE [indicators 10:16/22]

            #' conditionalPanel(
            #'   condition =
            #'     "input.ind_labour_market == 'Paid employees considering their current job good for career advancement'
            #'   || input.ind_labour_market == 'Paid employees considering their current job good for career advancement'
            #'   || input.ind_labour_market == 'Paid employees receiving at least one employment benefit in their current job'
            #'   || input.ind_labour_market == 'Paid employees having pension plan in their current job'
            #'   || input.ind_labour_market == 'Paid employees having paid sick leave in their current job'
            #'   || input.ind_labour_market == 'Paid employees having paid vacation leave in their current job'
            #'   || input.ind_labour_market == 'Paid employees having disability insurance in their current job'",
            #'
            #'   ###### Visible Minority ----
            #'   #'NOTE [this is the focal variable for this tab]
            #'   selectizeInput(
            #'     inputId = "lm_employment_vismin",
            #'     label = "Visible minority status",
            #'     choices = unique(as.character(employmentDT$VisMin)),
            #'     multiple = TRUE,
            #'     selected = unique(as.character(employmentDT$VisMin))[1],
            #'   ),
            #'   ###### Year ----
            #'   selectizeInput(
            #'     inputId = "lm_employmentyear",
            #'     label = "Year",
            #'     choices = unique(as.character(employmentDT$Year))
            #'   ),
            #'   ###### Geography ----
            #'   selectizeInput(
            #'     inputId = "lm_employmentgeography",
            #'     label = "Geography",
            #'     choices = unique(as.character(employmentDT$Geography))
            #'   ),
            #'   ###### Characteristic ----
            #'   selectizeInput(
            #'     inputId = "lm_employment_sex",
            #'     label = "Characteristic",
            #'     choices = unique(as.character(employmentDT$Characteristic))
            #'   ),
            #'   ###### Confidence ----
            #'   Confidence(
            #'     inputId = "lm_employment_language",
            #'     label = "Confidence",
            #'     choices = unique(as.character(employmentDT$Confidence))
            #'   )
            #' ),

            #'NOTE [end of Participation in the Labour Market section]
            #'NOTE [MISSING INDICATORS 17:22]
            #'[17 "Paid employees having supplemental medical care in their current job"]
            #'[18 "Paid employees having worker's compensation in their current job"]
            #'[19 "Paid employees having maternity, paternity or lay-off benefits in their current job"]
            #'[20 "Paid employees covered by union contract or collective agreement in their current job"]
            #'[21 "Paid employees receiving formal training in their current job"]
            #'[22 "Paid employees receiving informal training in their current job"]

            #### 2.2. Civic engagement and political participation ----
            ##### 2.2.1. Civic engagement and political participation (part 1) ----
            #'NOTE [civicDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Percent of the population members of at least one civic group or organization'
              || input.indicator_1 == 'Percent of the population members in a sports or recreational organization'
              || input.indicator_1 == 'Percent of the population members in a cultural, educational or hobby organization'
              || input.indicator_1 == 'Percent of the population members in union or professional association'
              || input.indicator_1 == 'Percent of the population members in a political party or group'
              || input.indicator_1 == 'Percent of the population members in a religious-affiliated group'
              || input.indicator_1 == 'Percent of the population members in a school group, neighbourhood, civic or community association'
              || input.indicator_1 == 'Percent of the population members in a humanitarian or charitable organization or service club'
              || input.indicator_1 == 'Percent of the population members in a seniors\\' group'
              || input.indicator_1 == 'Percent of the population members in a youth organization'
              || input.indicator_1 == 'Percent of the population members in an immigrant or ethnic association or club'
              || input.indicator_1 == 'Percent of the population members in an environmental group'
              || input.indicator_1 == 'Percent of the population engaged in political activities'",
              #'NOTE [indicators 1:13/16]
              #'#'NOTE [you need 2 backslashes to escape that single quotation used in "Percent of the population members in a seniors' group" because otherwise it thinks that's where the condition ends (AKA: "Percent of the population members in a seniors")]
              
              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              selectizeInput(
                inputId = "civic_vismin",
                label = "Visible minority status",
                choices = vm_10,
                multiple = TRUE,
                selected = vm_10[1],
              ),
              ###### Year ----
              selectizeInput(
                inputId = "civic_year",
                label = "Year",
                choices = unique(as.character(civicDT$Year))
              ),
              ###### Geography  ----
              selectizeInput(
                inputId = "civic_geography",
                label = "Geography",
                choices = unique(as.character(civicDT$Geography))
              ),
              ###### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "civic_sociodem",
                label = "Selected sociodemographic characteristics",
                choices = unique(as.character(civicDT$char_type))
              ),
              ####### Age ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Age'",
                selectizeInput(
                  inputId = "civic_age",
                  label = "Age group",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Age"])
                )
              ),
              ####### Gender ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "civic_sex",
                  label = "Gender",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Gender"])
                )
              ),
              ####### Immigration Status ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "civic_immigration",
                  label = "Immigration Status",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Immigration Status"])
                )
              ),
              ####### Generation Status ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Generation Status'",
                selectizeInput(
                  inputId = "civic_generation",
                  label = "Generation Status",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Generation Status"])
                )
              ),
              ####### Language Spoken ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Language Spoken'",
                selectizeInput(
                  inputId = "civic_language",
                  label = "Age group",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Language Spoken"])
                )
              ),
              ####### Education Status ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Education Status'",
                selectizeInput(
                  inputId = "civic_education",
                  label = "Education Status",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Education Status"])
                )
              ),
              ###### Confidence Interval ----
              selectizeInput(
                inputId = "civic_conf_interval",
                label = "Confidence Interval",
                choices = unique(as.character(civicDT$Confidence))
              )
            ),
            
            ##### 2.2.2. Civic engagement and political participation (part 2) ----
            #'NOTE [civicDT2]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Percent of the population voting in the last federal election'
              || input.indicator_1 == 'Percent of the population voting in the last provincial election'
              || input.indicator_1 == 'Percent of the population voting in the last municipal election'",
              #'NOTE [indicators 14:16/16]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              selectizeInput(
                inputId = "civic2_vismin",
                label = "Visible minority status",
                choices = vm_10,
                multiple = TRUE,
                selected = vm_10[1],
              ),
              ###### Year ----
              selectizeInput(
                inputId = "civic2_year",
                label = "Year",
                choices = unique(as.character(civicDT2$Year))
              ),
              ###### Geography  ----
              selectizeInput(
                inputId = "civic2_geography",
                label = "Geography",
                choices = unique(as.character(civicDT2$Geography))
              ),
              ###### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "civic2_sociodem",
                label = "Selected sociodemographic characteristics",
                choices = unique(as.character(civicDT2$char_type))
              ),
                ####### Age ----
                conditionalPanel(
                  condition = "input.civic2_sociodem == 'Age'",
                  selectizeInput(
                    inputId = "civic2_age",
                    label = "Age group",
                    choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Age"])
                  )
                ),
                ####### Gender ----
                conditionalPanel(
                  condition = "input.civic2_sociodem == 'Gender'",
                  selectizeInput(
                    inputId = "civic2_sex",
                    label = "Gender",
                    choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Gender"])
                  )
                ),
                ####### Immigration Status ----
                conditionalPanel(
                  condition = "input.civic2_sociodem == 'Immigration Status'",
                  selectizeInput(
                    inputId = "civic2_immigration",
                    label = "Immigration Status",
                    choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Immigration Status"])
                  )
                ),
                ####### Generation Status ----
                conditionalPanel(
                  condition = "input.civic2_sociodem == 'Generation Status'",
                  selectizeInput(
                    inputId = "civic2_generation",
                    label = "Generation Status",
                    choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Generation Status"])
                  )
                ),
                ####### Language Spoken ----
                conditionalPanel(
                  condition = "input.civic2_sociodem == 'Language Spoken'",
                  selectizeInput(
                    inputId = "civic2_language",
                    label = "Age group",
                    choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Language Spoken"])
                  )
                ),
                ####### Education Status ----
                conditionalPanel(
                  condition = "input.civic2_sociodem == 'Education Status'",
                  selectizeInput(
                    inputId = "civic2_education",
                    label = "Education Status",
                    choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Education Status"])
                  )
                ),
              ###### Confidence Interval ----
              selectizeInput(
                inputId = "civic2_conf_interval",
                label = "Confidence Interval",
                choices = unique(as.character(civicDT2$Confidence))
              )
            ),

          #### 2.3. Representation in decision-making positions ----
          #'NOTE [representationDT]
          conditionalPanel(
            condition =
              "input.theme_1 == 'Representation in decision-making positions'",
            #'NOTE [indicators 1:4/4]

            ##### Visible Minority ----
            #'NOTE [this is the focal variable for this tab]
            selectizeInput(
              inputId = "rep_vismin",
              label = "Visible minority status",
              choices = unique(as.character(representationDT$VisMin)),
              multiple = TRUE,
              selected = unique(as.character(representationDT$VisMin))[1],
            ),
            ##### Highest certificate, diploma or degree ----
            selectizeInput(
              inputId = "rep_year",
              label = "Highest certificate, diploma or degree",
              choices = unique(as.character(representationDT$Degree))
            ),
            ##### Geography ----
            selectizeInput(
              inputId = "rep_geography",
              label = "Geography",
              choices = unique(as.character(representationDT$Geography))
            ),
            ##### Immigrant and generation status ----
            selectizeInput(
              inputId = "rep_year",
              label = "Immigrant and generation status",
              choices = unique(as.character(representationDT$Immigration))
            ),
            ##### Year ----
            selectizeInput(
              inputId = "rep_year",
              label = "Immigrant and generation status",
              choices = unique(as.character(representationDT$Year))
            ),
            ##### Age group and first official language spoken ----
            selectizeInput(
              inputId = "rep_year",
              label = "Age group and first official language spoken",
              choices = unique(as.character(representationDT$Age))
            ),
            ##### Gender ----
            selectizeInput(
              inputId = "rep_year",
              label = "Gender",
              choices = unique(as.character(representationDT$Sex))
            )
          ),

          #### 2.4. Basic needs and housing ----
          #'NOTE [basicDT]
          conditionalPanel(
            condition =
              "input.theme_1 == 'Basic needs and housing'",
            #'NOTE [is there a reason why in the originaly code we don't see the following indicators:]
            #'[Percent of the population living in a dwelling owned by one member of the household]
            #'[Percent of the population living in core need household]
            #'[Percent of the population living in suitable housing]
            #'[Percent of the population living in an affordable housing]

            ##### Visible Minority ----
            #'NOTE [this is the focal variable for this tab]
            selectizeInput(
              inputId = "basic_vismin",
              label = "Visible minority status",
              choices = vm_10,
              multiple = TRUE,
              selected = vm_10[1],
            ),
            ##### Year ----
            selectizeInput(
              inputId = "basic_year",
              label = "Year",
              choices = unique(as.character(basicDT$Year))
            ),
            ##### Geography  ----
            selectizeInput(
              inputId = "basic_geography",
              label = "Geography",
              choices = unique(as.character(basicDT$Geography))
            ),
            ##### Selected sociodemographic characteristics ----
            selectizeInput(
              inputId = "basic_sociodem",
              label = "Selected sociodemographic characteristics",
              choices = unique(as.character(basicDT$char_type))
            ),
            ###### Age ----
            conditionalPanel(
              condition = "input.basic_sociodem == 'Age'",
              selectizeInput(
                inputId = "basic_age",
                label = "Age group",
                choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Age"])
              )
            ),
            ###### Gender ----
            conditionalPanel(
              condition = "input.basic_sociodem == 'Gender'",
              selectizeInput(
                inputId = "basic_sex",
                label = "Gender",
                choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Gender"])
              )
            ),
            ###### Immigration Status ----
            conditionalPanel(
              condition = "input.basic_sociodem == 'Immigration Status'",
              selectizeInput(
                inputId = "basic_immigration",
                label = "Immigration Status",
                choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Immigration Status"])
              )
            ),
            ##### Confidence Interval ----
            selectizeInput(
              inputId = "basic_conf_interval",
              label = "Confidence Interval",
              choices = unique(as.character(basicDT$Confidence))
            )
          ),

          #### 2.5. Local community ----
          #'NOTE [it doesn't look like there'a any conditions following this theme?]
          #'[from my notes it looks like it should take from incomeDT]

          #### 2.6. Health and wellbeing ----
          #'NOTE [it doesn't look like the health data comes from the basicDT???]

          #### 2.7. Public services and institutions ----
          #'NOTE [confidenceDT]
          conditionalPanel(
            condition =
              "input.theme_1 == 'Public services and institutions'",

            ##### Visible Minority ----
            #'NOTE [this is the focal variable for this tab]
            selectizeInput(
              inputId = "public_vismin",
              label = "Visible minority status",
              choices = vm_10,
              multiple = TRUE,
              selected = vm_10[1],
            ),
            ##### Year ----
            selectizeInput(
              inputId = "public_year",
              label = "Year",
              choices = unique(as.character(confidenceDT$Year))
            ),
            ##### Geography ----
            selectizeInput(
              inputId = "public_geography",
              label = "Geography",
              choices = unique(as.character(confidenceDT$Geography))
            ),
            ##### Selected sociodemographic characteristics ----
            selectizeInput(
              inputId = "public_sociodem",
              label = "Selected sociodemographic characteristics",
              choices = unique(as.character(confidenceDT$char_type))
            ),

            ###### Age ----
            conditionalPanel(
              condition = "input.public_sociodem == 'Age'",
              selectizeInput(
                inputId = "public_age",
                label = "Age group",
                choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Age"])
              )
            ),
            ###### Gender ----
            conditionalPanel(
              condition = "input.public_sociodem == 'Gender'",
              selectizeInput(
                inputId = "public_sex",
                label = "Gender",
                choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Gender"])
              )
            ),
            ###### Immigration Status ----
            conditionalPanel(
              condition = "input.public_sociodem == 'Immigration Status'",
              selectizeInput(
                inputId = "public_immigration",
                label = "Immigration Status",
                choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Immigration Status"])
              )
            ),
            ###### Generation Status ----
            conditionalPanel(
              condition = "input.public_sociodem == 'Generation Status'",
              selectizeInput(
                inputId = "public_generation",
                label = "Immigration Status",
                choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Generation Status"])
              )
            ),
            ###### Language Spoken ----
            conditionalPanel(
              condition = "input.public_sociodem == 'Language Spoken'",
              selectizeInput(
                inputId = "public_language",
                label = "Language Spoken",
                choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Language Spoken"])
              )
            ),
            ###### Education Status ----
            conditionalPanel(
              condition = "input.public_sociodem == 'Education Status'",
              selectizeInput(
                inputId = "public_education",
                label = "Education Status",
                choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Education Status"])
              )
            ),
            ##### Confidence Interval ----
            selectizeInput(
              inputId = "public_conf_interval",
              label = "Confidence Interval",
              choices = unique(as.character(confidenceDT$Confidence))
            )
          ),

          #### 2.8. Income and wealth ----
          #'NOTE [THIS IS ALL IN THE LABOUR MARKET SECTION ALREADY]
          #'NOTE [incomeDT]
          #' conditionalPanel(
          #'   condition =
          #'     "input.theme_1 == 'Income and wealth'",
          #' 
          #'   ##### Visible Minority ----
          #'   #'NOTE [this is the focal variable for this tab]
          #'   selectizeInput(
          #'     inputId = "income_vismin",
          #'     label = "Visible minority status",
          #'     choices = unique(as.character(incomeDT$VisMin)),
          #'     multiple = TRUE,
          #'     selected = unique(as.character(incomeDT$VisMin))[1],
          #'   ),
          #'   ##### Year ----
          #'   selectizeInput(
          #'     inputId = "income_year",
          #'     label = "Year",
          #'     choices = unique(as.character(incomeDT$Year))
          #'   ),
          #'   ##### Geography ----
          #'   selectizeInput(
          #'     inputId = "income_geography",
          #'     label = "Geography",
          #'     choices = unique(as.character(incomeDT$Geography))
          #'   ),
          #'   ##### Selected sociodemographic characteristics ----
          #'   selectizeInput(
          #'     inputId = "income_sociodem",
          #'     label = "Selected sociodemographic characteristics",
          #'     choices = unique(as.character(incomeDT$char_type))
          #'   ),
          #' 
          #'   ###### Age ----
          #'   conditionalPanel(
          #'     condition = "input.income_sociodem == 'Age'",
          #'     selectizeInput(
          #'       inputId = "income_age",
          #'       label = "Age group",
          #'       choices = unique(as.character(incomeDT$Characteristic)[incomeDT$char_type == "Age"])
          #'     )
          #'   ),
          #'   ###### Gender ----
          #'   conditionalPanel(
          #'     condition = "input.income_sociodem == 'Gender'",
          #'     selectizeInput(
          #'       inputId = "income_gender",
          #'       label = "Gender",
          #'       choices = unique(as.character(incomeDT$Characteristic)[incomeDT$char_type == "Gender"])
          #'     )
          #'   ),
          #'   ###### Immigration Status ----
          #'   conditionalPanel(
          #'     condition = "input.income_sociodem == 'Immigration Status'",
          #'     selectizeInput(
          #'       inputId = "income_immigration",
          #'       label = "Immigration Status",
          #'       choices = unique(as.character(incomeDT$Characteristic)[incomeDT$char_type == "Immigration Status"])
          #'     )
          #'   ),
          #'   ###### Generation Status ----
          #'   conditionalPanel(
          #'     condition = "input.income_sociodem == 'Generation Status'",
          #'     selectizeInput(
          #'       inputId = "income_generation",
          #'       label = "Immigration Status",
          #'       choices = unique(as.character(incomeDT$Characteristic)[incomeDT$char_type == "Generation Status"])
          #'     )
          #'   ),
          #'   ###### Language Spoken ----
          #'   conditionalPanel(
          #'     condition = "input.income_sociodem == 'Language Spoken'",
          #'     selectizeInput(
          #'       inputId = "income_language",
          #'       label = "Language Spoken",
          #'       choices = unique(as.character(incomeDT$Characteristic)[incomeDT$char_type == "Language Spoken"])
          #'     )
          #'   ),
          #'   ###### Education Status ----
          #'   conditionalPanel(
          #'     condition = "input.income_sociodem == 'Education Status'",
          #'     selectizeInput(
          #'       inputId = "income_education",
          #'       label = "Education Status",
          #'       choices = unique(as.character(incomeDT$Characteristic)[incomeDT$char_type == "Education Status"])
          #'     )
          #'   ),
          #'   ##### Confidence Interval ----
          #'   selectizeInput(
          #'     inputId = "income_conf_interval",
          #'     label = "Confidence Interval",
          #'     choices = unique(as.character(incomeDT$Confidence))
          #'   )
          #' ),

          #### 2.9. Social connections and personnal networks ----
          #'NOTE [belongingDT]
          conditionalPanel(
            condition =
              "input.theme_1 == 'Social connections and personnal networks'",

            ##### Visible Minority ----
            #'NOTE [this is the focal variable for this tab]
            selectizeInput(
              inputId = "belonging_vismin",
              label = "Visible minority status",
              choices = vm_10,
              multiple = TRUE,
              selected = vm_10[1],
            ),
            ##### Year ----
            selectizeInput(
              inputId = "belonging_year",
              label = "Year",
              choices = unique(as.character(belongingDT$Year))
            ),
            ##### Geography ----
            selectizeInput(
              inputId = "belonging_geography",
              label = "Geography",
              choices = unique(as.character(belongingDT$Geography))
            ),
            ##### Selected sociodemographic characteristics ----
            selectizeInput(
              inputId = "belonging_sociodem",
              label = "Selected sociodemographic characteristics",
              choices = unique(as.character(belongingDT$char_type))
            ),

            ###### Age ----
            conditionalPanel(
              condition = "input.belonging_sociodem == 'Age'",
              selectizeInput(
                inputId = "belongingage",
                label = "Age group",
                choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Age"])
              )
            ),
            ###### Gender ----
            conditionalPanel(
              condition = "input.belonging_sociodem == 'Gender'",
              selectizeInput(
                inputId = "public_income_social_gender",
                label = "Gender",
                choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Gender"])
              )
            ),
            ###### Immigration Status ----
            conditionalPanel(
              condition = "input.belonging_sociodem == 'Immigration Status'",
              selectizeInput(
                inputId = "belonging_immigration",
                label = "Immigration Status",
                choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Immigration Status"])
              )
            ),
            ###### Generation Status ----
            conditionalPanel(
              condition = "input.belonging_sociodem == 'Generation Status'",
              selectizeInput(
                inputId = "belonging_generation",
                label = "Immigration Status",
                choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Generation Status"])
              )
            ),
            ###### Language Spoken ----
            conditionalPanel(
              condition = "input.belonging_sociodem == 'Language Spoken'",
              selectizeInput(
                inputId = "belonging_language",
                label = "Language Spoken",
                choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Language Spoken"])
              )
            ),
            ###### Education Status ----
            conditionalPanel(
              condition = "input.belonging_sociodem == 'Education Status'",
              selectizeInput(
                inputId = "belonging_education",
                label = "Education Status",
                choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Education Status"])
              )
            ),
            ##### Confidence Interval ----
            selectizeInput(
              inputId = "belonging_conf_interval",
              label = "Confidence Interval",
              choices = unique(as.character(belongingDT$Confidence))
            )
          ),

          #### 2.10. Discrimination and victimization ----
          ##### 2.10.1. Discrimination and victimization (part 1) ----
          #'NOTE [discriminationDT]
          conditionalPanel(
            condition =
              "input.theme_1 == 'Discrimination and victimization'
              && input.indicator_1 != 'Hate Crime'",
            
            ##### Visible Minority ----
            #'NOTE [this is the focal variable for this tab]
            selectizeInput(
              inputId = "discrimination_vismin",
              label = "Visible minority status",
              choices = vm_10,
              multiple = TRUE,
              selected = vm_10[1],
            ),
            ##### Year ----
            selectizeInput(
              inputId = "discrimination_year",
              label = "Year",
              choices = unique(as.character(discriminationDT$Year))
            ),
            ##### Geography ----
            selectizeInput(
              inputId = "discrimination_geography",
              label = "Geography",
              choices = unique(as.character(discriminationDT$Geography))
            ),
            ##### Selected sociodemographic characteristics ----
            selectizeInput(
              inputId = "discrimination_sociodem",
              label = "Selected sociodemographic characteristics",
              choices = unique(as.character(discriminationDT$char_type))
            ),
            ###### Age ----
            conditionalPanel(
              condition = "input.discrimination_sociodem == 'Age'",
              selectizeInput(
                inputId = "discrimination_age",
                label = "Age group",
                choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age"])
              )
            ),
            ###### Gender ----
            conditionalPanel(
              condition = "input.discrimination_sociodem == 'Gender'",
              selectizeInput(
                inputId = "discrimination_sex",
                label = "Gender",
                choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age"])
              )
            ),
            ###### Immigration Status ----
            conditionalPanel(
              condition = "input.discrimination_sociodem == 'Immigration Status'",
              selectizeInput(
                inputId = "discrimination_immigration",
                label = "Immigration Status",
                choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age"])
              )
            ),
            ###### Generation Status ----
            conditionalPanel(
              condition = "input.discrimination_sociodem == 'Generation Status'",
              selectizeInput(
                inputId = "discrimination_generation",
                label = "Gender",
                choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age"])
              )
            ),
            ###### Language Spoken ----
            conditionalPanel(
              condition = "input.discrimination_sociodem == 'Language Spoken'",
              selectizeInput(
                inputId = "discrimination_language",
                label = "Language Spoken",
                choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age"])
              )
            ),
            ###### Education Status ----
            conditionalPanel(
              condition = "input.discrimination_sociodem == 'Education Status'",
              selectizeInput(
                inputId = "discrimination_education",
                label = "Education Status",
                choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age"])
              )
            ),
            ##### Confidence Interval ----
            selectizeInput(
              inputId = "discrimination_conf_interval",
              label = "Confidence Interval",
              choices = unique(as.character(discriminationDT$Confidence))
            )
          ),
          
          ##### 2.10.2. Discrimination and victimization (part 2) ----
          #'NOTE [discriminationDT]
          conditionalPanel(
            condition =
              "input.theme_1 == 'Discrimination and victimization'
              && input.indicator_1 == 'Hate Crime'",
            
            ##### Visible Minority ----
            #'NOTE [this is the focal variable for this tab]
            selectizeInput(
              inputId = "discrimination_vismin",
              label = "Visible minority status",
              choices = vm_10,
              multiple = TRUE,
              selected = vm_10[1],
            ),
            ##### Year ----
            selectizeInput(
              inputId = "discrimination2_year",
              label = "Year",
              choices = unique(as.character(polData$Year))
            ),
            ##### Motivation  ----
            selectizeInput(
              inputId = "discrimination2_motivation_type",
              label = "Motivation",
              choices = unique(as.character(polData$motivation_type))
            ),
            ###### Race or ethnicity and other characteristics ----
            conditionalPanel(
              condition = "input.discrimination2_motivation_type == 'Total police-reported hate crime'",
              selectizeInput(
                inputId = "discrimination_total",
                label = "Race or ethnicity and other characteristics",
                choices = unique(as.character(polData$Motivation)[polData$motivation_type == "Total police-reported hate crime"])
              )
            ),
            ###### Groups designated as Visible Minority ----
            conditionalPanel(
              condition = "input.discrimination2_motivation_type == 'Race or ethnicity'",
              selectizeInput(
                inputId = "discrimination_total",
                label = "Groups designated as Visible Minority",
                choices =  unique(as.character(polData$Motivation)[polData$motivation_type == "Race or ethnicity"])
              )
            )
          )

          ), # sidebarPanel closing bracket // should be blue
      
      ### Main panel ----
      mainPanel(
        h2("Groups Designated as Visible Minorities"),
        
        #### 1. Participation in the Labour Market ----
        ##### 1.1. Working-age population in the labour force (participation rate) ----
        #'NOTE [CONFIRM WHERE THE RATEDT COMES FROM FOR SOURCE]
        conditionalPanel(
          condition = "input.indicator_1 == 'Working-age population in the labour force (participation rate)'",
          br(),
          br(),
          plotlyOutput("plot_vm_lm_1",
                       inline = TRUE,
                       width = 700,
                       height = 500),
          br(),
          helpText(source_cchs)
        ),
        
        ##### 1.2. Working-age population in employment (employment rate) ----
        conditionalPanel(
          condition = "input.indicator_1 == 'Working-age population in employment (employment rate)'",
          br(),
          br(),
          plotlyOutput("plot_vm_lm_2",
                       inline = TRUE,
                       width = 700,
                       height = 500),
          br(),
          helpText(source_cchs)
        ),
        
        ##### 1.3. Working-age population in unemployment (unemployment rate) ----
        conditionalPanel(
          condition = "input.indicator_1 == 'Working-age population in unemployment (unemployment rate)'",
          br(),
          br(),
          plotlyOutput("plot_vm_lm_3",
                       inline = TRUE,
                       width = 700,
                       height = 500),
          br(),
          helpText(source_cchs)
        )

        #### 2. Civic engagement and political participation ----
        
        #### 3. Representation in decision-making positions ----
        
        #### 4. Basic needs and housing ----
        
        #### 5. Local community ----
        
        #### 6. Health and wellbeing ----
        
        #### 7. Public services and institutions ----
        
        #### 8. Income and wealth ----
        
        #### 9. Social connections and personnal networks ----
        
        #### 10. Discrimination and victimization ----
        
        
        # conditionalPanel(
        #   condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting very good or excellent mental health' & input.healthCharacteristics == 'Immigration Status'",
        #   br(),
        #   br(),
        #   plotlyOutput("sBarHealth3",
        #                inline = TRUE,
        #                width = 700,
        #                height = 500),
        #   br(),
        #   helpText("Source: Canadian Community Health Survey (CCHS), September to December 2020")
        # )
        
        ) # Main panel closing bracket // should be blue
      
        ) # sidebarLayout closing bracket // should be greenish-blue
      )
      
    )
  )

gc()
