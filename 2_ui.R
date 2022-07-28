# Load packages ----
library(shiny)
library(data.table)
library(readxl)
library(plotly)
library(leaflet)
library(htmltools)
library(RColorBrewer)
library(shinyjs)

# Repetitive code ----
## Variable lists ----
dim_list <- list(
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

vmstatus_list <- 
  list(
    "Total - Visible minority",
    "Visible minority population",
    "South Asian",
    "Chinese",
    "Black",
    "Filipino",
    "Latin American",
    "Arab",
    "Southeast Asian",
    "West Asian",
    "Korean",
    "Japanese",
    "Visible minority n.i.e",
    "Multiple visible minorities",
    "Not a visible minority"
  )
## Plot functions ----
func_cond_plot <- function(x) {
  conditionalPanel(
    condition = x,
    br(),
    br(),
    plotlyOutput(
      "sBarBasicAge4",
      inline = TRUE,
      width = 700,
      height = 500
    ),
    br(),
    helpText(
      "Source: Canadian Community Health Survey (CCHS), September to December 2020"
    )
  )
}

# Define the js method that resets the page ----
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# Define UI ----
ui <- fluidPage(
  #'NOTE [Include shinyjs in the UI]
  useShinyjs(),
  
  # Title of the app ----
  titlePanel("Social Inclusion Data Visualization Tool"),
  
  #'NOTE [Changes color of tabpanel from standard blue to black]
  tags$style(
    type = "text/css",
    "a{color: #000000;}",
    style = "font-size:70px",
    '* {font-family: "Arial"};'
  ),
  
  # Create the tabset panel ----
  tabsetPanel(
    
    # Commented on only the first tab, but follow this layout to create more
    # Bar Graphs by VisMin Tab
    
    ## 1. Tab for Geography ----
    tabPanel(
      "Theme & Definition of Indicators",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            "dim4",
            label = "Theme",
            choices = dim_list
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Participation in the Labour Market'",
            selectizeInput(
              "LM4",
              label = "Indicators",
              choices = list(
                "Working-age population in the labour force (participation rate)",
                "Working-age population in employment (employment rate)",
                "Working-age population in unemployment (unemployment rate)",
                "Workers working mainly full-time weeks in the previous year",
                "Self-employed workers in the labour force (unincorporated)",
                "Overqualified workers with a university degree",
                "Youth not in employment, education or training (NEET)",
                "Average employment income of the population",
                "Average weekly wage of paid employees",
                "Currently employed population considering their job related to their education",
                "Paid employees considering their current job good for career advancement",
                "Paid employees receiving at least one employment benefit in their current job",
                "Paid employees having pension plan in their current job",
                "Paid employees having paid sick leave in their current job",
                "Paid employees having paid vacation leave in their current job",
                "Paid employees having disability insurance in their current job",
                "Paid employees having supplemental medical care in their current job",
                "Paid employees having worker's compensation in their current job",
                "Paid employees having maternity, paternity or lay-off benefits in their current job",
                "Paid employees covered by union contract or collective agreement in their current job",
                "Paid employees receiving formal training in their current job",
                "Paid employees receiving informal training in their current job"
              ),
              selected = NULL
            ),
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Civic engagement and political participation'",
            selectizeInput(
              "dimCivilEngagement4",
              label = "Indicators",
              choices = list(
                "Percent of the population members of at least one civic group or organization",
                "Percent of the population members in a sports or recreational organization",
                "Percent of the population members in a cultural, educational or hobby organization",
                "Percent of the population members in union or professional association",
                "Percent of the population members in a political party or group",
                "Percent of the population members in a religious-affiliated group",
                "Percent of the population members in a school group, neighbourhood, civic or community association",
                "Percent of the population members in a humanitarian or charitable organization or service club",
                "Percent of the population members in a seniors' group",
                "Percent of the population members in a youth organization",
                "Percent of the population members in an immigrant or ethnic association or club",
                "Percent of the population members in an environmental group ",
                "Percent of the population engaged in political activities",
                "Percent of the population voting in the last federal election",
                "Percent of the population voting in the last provincial election",
                "Percent of the population voting in the last municipal election"
              )
            )
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Representation in decision-making positions'",
            selectizeInput(
              "Rep4",
              label = "Indicators",
              choices = list(
                "Percent of workers in all management occupations",
                "Percent of workers in senior management occupations",
                "Percent of workers in specialized middle management occupations",
                "Percent of workers in other middle management occupations"
              )
            ),
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Basic needs and housing'",
            selectizeInput(
              "dimBasicNeeds4",
              label = "Indicators",
              choices = list(
                "Percent of the population living in a dwelling owned by one member of the household ",
                "Percent of the population living in core need household",
                "Percent of the population living in suitable housing",
                "Percent of the population living in an affordable housing",
                "Percent of the population living in a food-secure household",
                "Percent of the population living in a household with marginal food security",
                "Percent of the population living in a food-insecure household, moderate or severe",
                "Percent of the population living in a household with moderate food insecurity",
                "Percent of the population living in a household with severe food insecurity"
              )
            )
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Local community'",
            selectizeInput(
              "dimCommunity4",
              label = "Indicators",
              choices = list(
                "Percent of the population satisfied with feeling part of their community",
                "Percent of the population satisfied with their neighbourhood",
                "Percent of the population satisfied with quality of local environment",
                "Percent of the population reporting feeling safe in their neighbourhood",
                "Percent of the population satisfied with personal safety from crime",
                "Violent victimization rate per 1,000 population",
                "Percent of the population reporting incident against them was a hate crime",
                "Percent of police reported hate crimes motivated by Race or ethnicity",
                "Percent of police reported hate crimes motivated by religion",
                "Percent of the population perceiving local police good in enforcing laws",
                "Percent of the population perceiving local police good in responding to calls",
                "Percent of the population perceiving local police good in being approachable",
                "Percent of the population perceiving local police good in supplying information",
                "Percent of the population perceiving local police good in ensuring safety in the area",
                "Percent of the population perceiving local police as treating people fairly"
              )
            )
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Health and wellbeing'",
            selectizeInput(
              "dimHealth4",
              label = "Indicators",
              choices = list(
                "Percent of the population reporting very good or excellent general health",
                "Percent of the population reporting fair or poor general health",
                "Percent of the population reporting very good or excellent mental health",
                "Percent of the population reporting fair or poor mental health",
                "Percent of the population reporting their life stressful",
                "Percent of the population satisfied with life as a whole",
                "Percent of the population predicting their life opportunities will improve in the next 5 years"
              )
            )
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Education, training and skills'",
            selectizeInput(
              "Edu4",
              label = "Education Indicators",
              choices = list(
                "Population with no certificate, diploma or degree",
                "Population with high school diploma or equivalency certificate",
                "Population with postsecondary certificate or diploma below bachelor level",
                "Population with bachelor’s degree or above",
                "Population with bachelor’s degree",
                "Population with master’s degree or earned doctorate",
                "Knowledge of official languages, English only",
                "Knowledge of official languages, French only",
                "Knowledge of official languages, English and French",
                "Knowledge of official languages, neither English nor French",
                "Received a formal training paid by the employer in the past 12 months",
                "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"
              )
            )
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Public services and institutions'",
            selectizeInput(
              "dimTrust4",
              label = "Indicators",
              choices = list(
                "Population expressing confidence in Federal Parliament",
                "Population expressing Confidence in the Canadian media",
                "Population expressing confidence in the school system",
                "Population expressing confidence in the justice system, courts",
                "Population expressing confidence in the police",
                "Population expressing confidence in major corporations",
                "Population expressing confidence in merchants and business people",
                "Population expressing confidence in banks"
              )
            )
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Income and wealth'",
            selectizeInput(
              "dimIncome4",
              label = "Indicators",
              choices = list(
                "Average total household income, adjusted for the number of persons",
                "Percent of the population living in poverty (low-income MBM)",
                "Percent of the population living in low income situation (before-tax)",
                "Percent of the population living in low income situation (after-tax)",
                "Percent of the population reporting difficulty in meeting financial needs of their household",
                "Percent of the population reporting ease in meeting financial needs of their household"
              )
            )
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Social connections and personnal networks'",
            selectizeInput(
              "dimSocial4",
              label = "Indicators",
              choices = list(
                "Percent of the population living alone",
                "Median size of a personal local network with close ties",
                "Average size of a local personal network with close ties",
                "Percent of the population with a personal close-ties network of 10 or more people",
                "Percent of the population with a personal close-ties network of 5 or more relatives",
                "Percent of the population with a personal close-ties network of 5 or more friends",
                "Percent of the population with no personal network with weak ties",
                "Percent of the population with a personal weak-ties network of 1 to 19 people",
                "Percent of the population with a personal weak-ties network of 20 or more people ",
                "Percent of the population with a personal ethnically-diverse network"
              )
            )
          ),
          conditionalPanel(
            condition = "input.dim4 == 'Discrimination and victimization'",
            selectizeInput(
              "disind4",
              label = "Discrimination and victimization Indicators",
              choices = list(
                "Experience(s) of discrimination",
                "Experience(s) of discrimination based on ethnicity or culture",
                "Experience(s) of discrimination based on race or colour",
                "Experience(s) of discrimination based on religion",
                "Experience(s) of discrimination based on language",
                "Discrimination at work or when applying for a job or promotion",
                "Discrimination when dealing with the police",
                "Discrimination when in a store, bank or restaurant",
                "Discrimination when attending school or classes",
                "Hate Crime"
              )
            )
          ),
          #'NOTE [Add the js code and button to the page]
          #extendShinyjs(text = jsResetCode, functions = "reset"),
          actionButton("reset_button", "Reset Page"),
        ),
        
        ## Main Panel for displaying graphs ----
        mainPanel(
          conditionalPanel(
            condition = "input.dim4 == 'Participation in the Labour Market' & input.LM4 == 'Overqualified workers with a university degree'",
            br(),
            h4("Overqualified workers with a university degree"),
            helpText(
              "Refers to people with a bachelor’s degree or above (at bachelor's level or above) who, during the current year or the year prior the census, held a position usually requiring a high school diploma or equivalency certificate or less."
            )
          )
        )
      )
    ),
    
    ## 2. Tab for ... ----
    tabPanel(
      "Groups Designated as Visible Minorities",
      fluid = TRUE,
      font = list(size = 10),
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            "dim",
            label = "Theme",
            choices = dim_list
          ),
          ### Participation in the Labour Market ----
          conditionalPanel(
            condition = "input.dim == 'Participation in the Labour Market'",
            selectizeInput(
              "LM",
              label = "Indicators",
              choices = list(
                "Working-age population in the labour force (participation rate)",
                "Working-age population in employment (employment rate)",
                "Working-age population in unemployment (unemployment rate)",
                "Workers working mainly full-time weeks in the previous year",
                "Self-employed workers in the labour force (unincorporated)",
                "Overqualified workers with a university degree",
                "Youth not in employment, education or training (NEET)",
                "Average employment income of the population",
                "Average weekly wage of paid employees",
                "Currently employed population considering their job related to their education",
                "Paid employees considering their current job good for career advancement",
                "Paid employees receiving at least one employment benefit in their current job",
                "Paid employees having pension plan in their current job",
                "Paid employees having paid sick leave in their current job",
                "Paid employees having paid vacation leave in their current job",
                "Paid employees having disability insurance in their current job",
                "Paid employees having supplemental medical care in their current job",
                "Paid employees having worker's compensation in their current job",
                "Paid employees having maternity, paternity or lay-off benefits in their current job",
                "Paid employees covered by union contract or collective agreement in their current job",
                "Paid employees receiving formal training in their current job",
                "Paid employees receiving informal training in their current job"
              ),
            ),
            ### Working-age population in the labour force (participation rate) ----
            conditionalPanel(
              condition = "input.LM == 'Working-age population in the labour force (participation rate)'
                                            ||input.LM == 'Working-age population in employment (employment rate)'
                                            ||input.LM == 'Working-age population in unemployment (unemployment rate)'
                                            ||input.LM == 'Workers working mainly full-time weeks in the previous year'",
              selectizeInput(
                "VM235",
                #' NOTE [why are there some hard-coded lists and some that are unique$df? why do some have default selections?]
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Chinese"),
                multiple = TRUE
              ),
              selectizeInput(
                "RateDegree",
                label = "Highest certificate, diploma or degree",
                choices = unique(rateDT$Degree),
              ),
              selectizeInput(
                "RateGeo",
                label = "Geography",
                choices = unique(rateDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "RateImm",
                label = "Immigrant and generation status",
                choices = unique(rateDT$Immigration),
                selected = "Canada"
              ),
              selectizeInput(
                "RateYear",
                label = "Year",
                choices = unique(rateDT$Year),
              ),
              selectizeInput(
                "RateAgeLang",
                label = "Age group and first official language spoken",
                choices = unique(rateDT$Age),
              ),
              selectizeInput(
                "RateSex",
                label = "Sex",
                choices = unique(rateDT$Sex),
                selected = "Total - Sex"
              ),
            ),
            ### Average employment income of the population ----
            conditionalPanel(
              condition = "input.LM == 'Average employment income of the population'
                                            ||input.LM == 'Average weekly wage of paid employees'",
              selectizeInput(
                "VM230",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Arab"),
                multiple = TRUE
              ),
              selectizeInput(
                "IncDegree1",
                label = "Highest certificate, diploma or degree",
                choices = unique(incomeDT$Degree),
              ),
              selectizeInput(
                "IncGeo1",
                label = "Geography",
                choices = unique(incomeDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "IncImm1",
                label = "Immigrant and generation status",
                choices = unique(incomeDT$Immigration),
                selected = "Canada"
              ),
              selectizeInput(
                "IncYear1",
                label = "Year",
                choices = unique(incomeDT$Year),
              ),
              selectizeInput(
                "IncAgeLang1",
                label = "Age group and first official language spoken",
                choices = unique(incomeDT$Age),
              ),
              selectizeInput(
                "IncSex1",
                label = "Gender",
                choices = unique(incomeDT$Sex),
                selected = "Total - Sex"
              ),
            ),
            ### Self-employed workers in the labour force (unincorporated) ----
            conditionalPanel(
              condition = "input.LM == 'Self-employed workers in the labour force (unincorporated)'",
              selectizeInput(
                "VM155",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("West Asian"),
                multiple = TRUE
              ),
              selectizeInput(
                "RepDegree1",
                label = "Highest certificate, diploma or degree",
                choices = unique(representationDT$Degree),
              ),
              selectizeInput(
                "RepGeo1",
                label = "Geography",
                choices = unique(representationDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "RepImm1",
                label = "Immigrant and generation status",
                choices = unique(representationDT$Immigration),
                selected = "Canada"
              ),
              selectizeInput(
                "RepYear1",
                label = "Year",
                choices = unique(representationDT$Year),
              ),
              selectizeInput(
                "RepAgeLang1",
                label = "Age group and first official language spoken",
                choices = unique(representationDT$Age),
              ),
              selectizeInput(
                "RepSex1",
                label = "Gender",
                choices = unique(representationDT$Sex),
                selected = "Total - Sex"
              ),
            ),
            #'NOTE [what is this chunk for?]
            conditionalPanel(
              condition = "input.LM == 'Overqualified workers with a university degree'",
              selectizeInput(
                "VM20",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "OverLocation",
                label = "Location of Study",
                choices = unique(OverQualDT$Location),
              ),
              selectizeInput(
                "OverDegree",
                label = "Highest certificate, diploma or degree",
                choices = unique(OverQualDT$Degree),
              ),
              selectizeInput(
                "OverGeo",
                label = "Geography",
                choices = unique(OverQualDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "OverImm",
                label = "Groups designated by Immigration and Generational Status",
                choices = unique(OverQualDT$'Immigration'),
                selected = "Canada"
              ),
              selectizeInput(
                "OverYear",
                label = "Year",
                choices = unique(OverQualDT$Year),
              ),
              selectizeInput(
                "OverAge",
                label = "Age Group",
                choices = unique(OverQualDT$Age),
                selected = "Total - Age"
              ),
              selectizeInput(
                "OverSex",
                label = "Sex",
                choices = sort(unique(OverQualDT$Sex), decreasing = TRUE),
                selected = "Total - Sex"
              ),
              selectizeInput(
                "OverLang",
                label = "Language",
                choices = unique(OverQualDT$'Language'),
              ),
            ),
            ### Youth not in employment, education or training (NEET) Tab 2 ----
            conditionalPanel(
              condition = "input.LM == 'Youth not in employment, education or training (NEET)'",
              selectizeInput(
                "VM140",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("South Asian"),
                multiple = TRUE
              ),
              selectizeInput(
                "YouthGeo",
                label = "Geography",
                choices = unique(youthDT$Geography),
              ),
              selectizeInput(
                "YouthImm",
                label = "Generation Status",
                choices = unique(youthDT$Immigration),
                selected = "Canada"
              ),
              selectizeInput(
                "YouthYear",
                label = "Year",
                choices = unique(youthDT$Year),
              ),
              selectizeInput(
                "YouthAge",
                label = "Age Group",
                choices = unique(youthDT$Age),
                selected = "Total - Age"
              ),
              selectizeInput(
                "YouthSex",
                label = "Sex",
                choices = sort(unique(youthDT$Sex), decreasing = TRUE),
                selected = "Total - Sex"
              ),
              selectizeInput(
                "YouthLang",
                label = "Language",
                choices = unique(youthDT$'Language'),
              ),
            ),
            ### Paid employees having disability insurance in their current job ----
            conditionalPanel(
              condition = "input.LM == 'Paid employees having disability insurance in their current job'",
              selectizeInput(
                "VM135",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Korean"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear7",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo7",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "EmploymentChar7",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf7",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
            ### Currently employed population considering their job related to their education ----
            conditionalPanel(
              condition = "input.LM == 'Currently employed population considering their job related to their education'",
              selectizeInput(
                "VM130",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear6",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo6",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "EmploymentChar6",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf6",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
            ### Paid employees having disability insurance in their current job ----
            conditionalPanel(
              condition = "input.LM == 'Paid employees having disability insurance in their current job'",
              selectizeInput(
                "VM135",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear7",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo7",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "EmploymentChar7",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf7",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
            ### Currently employed population considering their job related to their education ----
            conditionalPanel(
              condition = "input.LM == 'Currently employed population considering their job related to their education'",
              selectizeInput(
                "VM130",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear6",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo6",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "EmploymentChar6",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf6",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
            ### Paid employees having paid vacation leave in their current job ----
            conditionalPanel(
              condition = "input.LM == 'Paid employees having paid vacation leave in their current job'",
              selectizeInput(
                "VM125",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear5",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo5",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "EmploymentChar5",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf5",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
            ### Paid employees receiving at least one employment benefit in their current job ---- 
            conditionalPanel(
              condition = "input.LM == 'Paid employees receiving at least one employment benefit in their current job'",
              selectizeInput(
                "VM120",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear4",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo4",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "EmploymentChar4",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf4",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
            ### Paid employees having pension plan in their current job ---- 
            conditionalPanel(
              condition = "input.LM == 'Paid employees having pension plan in their current job'",
              selectizeInput(
                "VM115",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear3",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo3",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "EmploymentChar3",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf3",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
            conditionalPanel(
              condition = "input.LM == 'Paid employees considering their current job good for career advancement'",
              selectizeInput(
                "VM110",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear2",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo2",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "EmploymentChar2",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf2",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
            conditionalPanel(
              condition = "input.LM == 'Paid employees having paid sick leave in their current job'",
              selectizeInput(
                "VM100",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "EmploymentYear",
                label = "Year",
                choices = unique(employmentDT$Year),
              ),
              selectizeInput(
                "EmploymentGeo",
                label = "Geography",
                choices = unique(employmentDT$Geography),
                selected = "Canada",
              ),
              selectizeInput(
                "EmploymentChar",
                label = "Characteristic",
                choices = unique(employmentDT$Characteristic),
              ),
              selectizeInput(
                "EmploymentConf",
                label = "Confidence",
                choices = unique(employmentDT$Confidence),
              ),
            ),
          ),
          
          ## Public services and institutions ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions'",
            selectizeInput(
              "dimTrust",
              label = "Indicators",
              choices = list(
                "Population expressing confidence in Federal Parliament",
                "Population expressing Confidence in the Canadian media",
                "Population expressing confidence in the school system",
                "Population expressing confidence in the justice system and courts",
                "Population expressing confidence in the police service",
                "Population expressing confidence in major corporations",
                "Population expressing confidence in merchants and local business people",
                "Population expressing confidence in banks"
              ),
            ),
            ### Population expressing confidence in Federal Parliament ----
            conditionalPanel(
              condition = "input.dimTrust == 'Population expressing confidence in Federal Parliament'
                                 || input.dimTrust == 'Population expressing Confidence in the Canadian media'
                                 || input.dimTrust == 'Population expressing confidence in the school system'
                                 || input.dimTrust == 'Population expressing confidence in the justice system and courts'
                                 || input.dimTrust == 'Population expressing confidence in the police service'
                                 || input.dimTrust == 'Population expressing confidence in major corporations'
                                 || input.dimTrust == 'Population expressing confidence in merchants and local business people'
                                 || input.dimTrust == 'Population expressing confidence in banks'",
              selectizeInput(
                "confYear",
                label = "Year",
                choices = unique(confidenceDT$'Year')
              ),
              selectizeInput(
                "confGeo",
                label = "Geography",
                choices = unique(confidenceDT$'Geography')
              ),
              selectizeInput(
                "VM40",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Chinese"),
                multiple = TRUE
              ),
              selectizeInput(
                "confCharacteristicsP",
                label = "Selected sociodemographic characteristics",
                choices = list(
                  'Age',
                  'Gender',
                  'Immigration Status',
                  'Generation Status',
                  'Language Spoken',
                  'Education Status'
                ),
              ),
              #### Age ----
              conditionalPanel(
                condition = "input.confCharacteristicsP == 'Age'",
                selectizeInput(
                  "confCharSpecAgeP",
                  label = "Age",
                  choices = list(
                    'Total, 15 years and over',
                    '15 to 24 years',
                    '25 to 64 years',
                    '65 years and over'
                  ),
                ),
              ),
              #### Gender ----
              conditionalPanel(
                condition = "input.confCharacteristicsP == 'Gender'",
                selectizeInput(
                  "confCharSpecGenderP",
                  label = "Gender",
                  choices = list('Men', 'Women'),
                ),
              ),
              #### Immigration status ----
              conditionalPanel(
                condition = "input.confCharacteristicsP == 'Immigration Status'",
                selectizeInput(
                  "confCharSpecImmP",
                  label = "Immigration Status",
                  choices = list('Immigrants', 'Non-Immigrants'),
                ),
              ),
              #### Generation status ----
              conditionalPanel(
                condition = "input.confCharacteristicsP == 'Generation Status'",
                selectizeInput(
                  "confCharSpecGenP",
                  label = "Immigration Status",
                  choices = list(
                    'First generation',
                    'Second generation',
                    'Third generation or more'
                  ),
                ),
              ),
              #### Language Spoken' -----
              conditionalPanel(
                condition = "input.confCharacteristicsP == 'Language Spoken'",
                selectizeInput(
                  "confCharSpecLangP",
                  label = "Language Spoken",
                  choices = list(
                    'First official language spoken, English only',
                    'First official language spoken, French only'
                  ),
                ),
              ),
              #### Education status ----
              conditionalPanel(
                condition = "input.confCharacteristicsP == 'Education Status'",
                selectizeInput(
                  "confCharSpecEduP",
                  label = "Language Spoken",
                  choices = list(
                    'Secondary (high) school diploma or equivalency certificate or less',
                    'Postsecondary certificate or diploma (non-university)',
                    'University certificate or diploma'
                  ),
                ),
              ),
              selectizeInput(
                "confConfidenceP",
                label = "Confidence Interval",
                choices = unique(confidenceDT$'Confidence')
              ),
            ),
          ),

          ## Representation in decision-making positions ----
          conditionalPanel(
            condition = "input.dim == 'Representation in decision-making positions'",
            selectizeInput(
              "Rep",
              label = "Indicators",
              choices = list(
                "Percent of workers in all management occupations",
                "Percent of workers in senior management occupations",
                "Percent of workers in middle management occupations"
              ),
            ),
            selectizeInput(
              "VM150",
              label = "Visible minority status",
              choices = vmstatus_list,
              selected = list("West Asian"),
              multiple = TRUE
            ),
            selectizeInput(
              "RepDegree2",
              label = "Highest certificate, diploma or degree",
              choices = unique(representationDT$Degree),
            ),
            selectizeInput(
              "RepGeo2",
              label = "Geography",
              choices = unique(representationDT$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "RepImm2",
              label = "Immigrant and generation status",
              choices = unique(representationDT$Immigration),
              selected = "Canada"
            ),
            selectizeInput(
              "RepYear2",
              label = "Year",
              choices = unique(representationDT$Year),
            ),
            selectizeInput(
              "RepAgeLang2",
              label = "Age group and first official language spoken",
              choices = unique(representationDT$Age),
            ),
            selectizeInput(
              "RepSex2",
              label = "Gender",
              choices = unique(representationDT$Sex),
              selected = "Total - Sex"
            ),
          ),
          
          ## Discrimination and victimization Variables ----
          conditionalPanel(
            condition = "input.dim == 'Discrimination and victimization'",
            selectizeInput(
              "disind",
              label = "Discrimination and victimization Indicators",
              choices = list(
                "Experience(s) of discrimination",
                "Experience(s) of discrimination based on ethnicity or culture",
                "Experience(s) of discrimination based on race or colour",
                "Experience(s) of discrimination based on religion",
                "Experience(s) of discrimination based on language",
                "Discrimination at work or when applying for a job or promotion",
                "Discrimination when dealing with the police",
                "Discrimination when in a store, bank or restaurant",
                "Discrimination when attending school or classes",
                "Hate Crime"
              ),
            ),
            ### Experience(s) of discrimination based on ethnicity or culture ----
            conditionalPanel(
              condition = "input.disind == 'Experience(s) of discrimination based on ethnicity or culture'
                                 || input.disind == 'Experience(s) of discrimination'
                                 || input.disind == 'Experience(s) of discrimination based on race or colour'
                                 || input.disind == 'Experience(s) of discrimination based on religion'
                                 || input.disind == 'Experience(s) of discrimination based on language'
                                 || input.disind == 'Discrimination at work or when applying for a job or promotion'
                                 || input.disind == 'Discrimination when dealing with the police'
                                 || input.disind == 'Discrimination when in a store, bank or restaurant'
                                 || input.disind == 'Discrimination when attending school or classes'",
              selectizeInput(
                "covYear",
                label = "Year",
                choices = unique(discriminationDT$Year)
              ),
              selectizeInput(
                "covGeo",
                label = "Geography",
                choices = unique(discriminationDT$Geography)
              ),
              selectizeInput(
                "VM30",
                label = "Visible minority status",
                choices = vmstatus_list,
                selected = list("Black"),
                multiple = TRUE
              ),
              selectizeInput(
                "covCharacteristics",
                label = "Selected sociodemographic characteristics",
                choices = list(
                  'Age',
                  'Gender',
                  'Immigration Status',
                  'Generation Status',
                  'Language Spoken',
                  'Education Status'
                ),
              ),
              #### Age ----
              conditionalPanel(
                condition = "input.covCharacteristics == 'Age'",
                selectizeInput(
                  "covCharSpecAge",
                  label = "Age",
                  choices = list(
                    'Total, 15 years and over',
                    '15 to 24 years',
                    '25 to 64 years',
                    '65 years and over'
                  ),
                ),
              ),
              #### Gender ----
              conditionalPanel(
                condition = "input.covCharacteristics == 'Gender'",
                selectizeInput(
                  "covCharSex",
                  label = "Gender",
                  choices = list('Man', 'Woman'),
                ),
              ),
              #### Immigration ----
              conditionalPanel(
                condition = "input.covCharacteristics == 'Immigration Status'",
                selectizeInput(
                  "covCharSpecImm",
                  label = "Immigration Status",
                  choices = list('Immigrants', 'Non-Immigrants'),
                ),
              ),
              #### Generation status ----
              conditionalPanel(
                condition = "input.covCharacteristics == 'Generation Status'",
                selectizeInput(
                  "covCharSpecGen",
                  label = "Generation Status",
                  choices = list(
                    'First generation',
                    'Second generation',
                    'Third generation or more'
                  ),
                ),
              ),
              #### Language spoken ----
              conditionalPanel(
                condition = "input.covCharacteristics == 'Language Spoken'",
                selectizeInput(
                  "covCharSpecLang",
                  label = "Language Spoken",
                  choices = list(
                    'First official language spoken, English only',
                    'First official language spoken, French only'
                  ),
                ),
              ),
              #### Education status ----
              conditionalPanel(
                condition = "input.covCharacteristics == 'Education Status'",
                selectizeInput(
                  "covCharSpecEdu",
                  label = "Education Status",
                  choices = list(
                    'Secondary (high) school diploma or equivalency certificate or less',
                    'Postsecondary certificate or diploma (non-university)',
                    'University certificate or diploma'
                  ),
                ),
              ),
              selectizeInput(
                "covConfidence",
                label = "Confidence Interval",
                choices = unique(discriminationDT$Confidence)
              ),
            ),
            
            ### Hate crime ----
            conditionalPanel(
              condition = "input.disind == 'Hate Crime'",
              selectizeInput(
                "disYear",
                label = "Year",
                choices = list('2014', '2015', '2016', '2017', '2018', '2019', '2020')
              ),
              selectizeInput(
                "motivation",
                label = "Motivation",
                choices = list('Race or ethnicity', 'Total police-reported hate crime'),
              ),
              #### Total police-reported hate crime ----
              conditionalPanel(
                condition = "input.motivation == 'Total police-reported hate crime'",
                selectizeInput(
                  "VM8",
                  label = "Race or ethnicity and other characteristics",
                  choices = list(
                    'Race or ethnicity',
                    'Religion',
                    'Sexual orientation',
                    'Language',
                    'Disability',
                    'Gender',
                    'Age',
                    'Unknown motivation'
                  ),
                  selected = "Religion",
                  multiple = TRUE
                ),
              ),
              #### Race or ethnicity ----
              conditionalPanel(
                condition = "input.motivation == 'Race or ethnicity'",
                selectizeInput(
                  "VM7",
                  label = "Groups designated as Visible Minority",
                  choices = list(
                    'Black',
                    'South Asian',
                    'East or Southeast Asian',
                    'Arab or West Asian',
                    'White',
                    'Indigenous',
                    'Multiple races or ethnicities',
                    'Other Race or ethnicity',
                    'Unknown Race or ethnicity'
                  ),
                  selected = "Black",
                  multiple = TRUE
                ),
              ),
              selectizeInput(
                "geo_dis",
                label = "Geography",
                choices = list("Canada,selected police services")
              )
            ),
          ),
          ### Civic engagement and political participation - Visible Minority ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation'",
            selectizeInput(
              "dimCivilEngagement",
              label = "Indicators",
              choices = list(
                "Percent of the population members of at least one civic group or organization",
                "Percent of the population members in a sports or recreational organization",
                "Percent of the population members in a cultural, educational or hobby organization",
                "Percent of the population members in a union or professional association",
                "Percent of the population members in a political party or group",
                "Percent of the population members in a religious-affiliated group",
                "Percent of the population members in a school group, neighbourhood, civic or community association",
                "Percent of the population members in a humanitarian or charitable organization or service club",
                "Percent of the population members in a seniors' group",
                "Percent of the population members in a youth organization",
                "Percent of the population members in an immigrant or ethnic association or club",
                "Percent of the population members in an environmental group",
                "Percent of the population engaged in political activities",
                "Percent of the population voting in the last federal election",
                "Percent of the population voting in the last provincial election",
                "Percent of the population voting in the last municipal election"
              ),
            ),
            selectizeInput(
              "VM175",
              label = "Visible minority status",
              choices = vmstatus_list,
              selected = list("Filipino"),
              multiple = TRUE
            ),
            selectizeInput(
              "CivicYear3",
              label = "Year",
              choices = unique(civicDT$Year),
            ),
            selectizeInput(
              "CivicGeo3",
              label = "Geography",
              choices = unique(civicDT$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "CivicCharacteristics3",
              label = "Sociodemographic Characteristics",
              choices = list(
                'Age',
                'Gender',
                'Immigration Status',
                'Generation Status',
                'Language Spoken',
                'Education Status'
              ),
            ),
            #### Age ----
            conditionalPanel(
              condition = "input.CivicCharacteristics3 == 'Age'",
              selectizeInput(
                "CivicCharSpecAge3",
                label = "Age",
                choices = list(
                  'Total, 15 years and over',
                  '15 to 24 years',
                  '25 to 64 years',
                  '65 years and over'
                ),
              ),
            ),
            #### Gender ----
            conditionalPanel(
              condition = "input.CivicCharacteristics3 == 'Gender'",
              selectizeInput(
                "CivicCharSpecGend3",
                label = "Gender",
                choices = list('Man', 'Woman'),
              ),
            ),
            #### Immigration status ----
            conditionalPanel(
              condition = "input.CivicCharacteristics3 == 'Immigration Status'",
              selectizeInput(
                "CivicCharSpecImm3",
                label = "Immigration Status",
                choices = list('Immigrants', 'Non-Immigrants'),
              ),
            ),
            #### Generation status ----
            conditionalPanel(
              condition = "input.CivicCharacteristics3 == 'Generation Status'",
              selectizeInput(
                "CivicCharSpecGen3",
                label = "Immigration Status",
                choices = list(
                  'First generation',
                  'Second generation',
                  'Third generation or more'
                ),
              ),
            ),
            #### Language spoken ----
            conditionalPanel(
              condition = "input.CivicCharacteristics3 == 'Language Spoken'",
              selectizeInput(
                "CivicCharSpecLang3",
                label = "Language Spoken",
                choices = list(
                  'First official language spoken, English only',
                  'First official language spoken, French only'
                ),
              ),
            ),
            #### Education status ----
            conditionalPanel(
              condition = "input.CivicCharacteristics3 == 'Education Status'",
              selectizeInput(
                "CivicCharSpecEdu3",
                label = "Language Spoken",
                choices = list(
                  'Secondary (high) school diploma or equivalency certificate or less',
                  'Postsecondary certicate or diploma (non-university)',
                  'University certificate or diploma'
                ),
              ),
            ),
            selectizeInput(
              "CivicConf3",
              label = "Confidence",
              choices = unique(civicDT$Confidence),
            ),
          ),
          ### Civil engagement ----
          conditionalPanel(
            condition = "input.dimCivilEngagement == 'Percent of the population voting in the last municipal election'
                                 ||input.dimCivilEngagement == 'Percent of the population voting in the last provincial election'
                                 ||input.dimCivilEngagement == 'Percent of the population voting in the last federal election'",
            selectizeInput(
              "VM170",
              label = "Visible minority status",
              choices = vmstatus_list,
              selected = list("South Asian"),
              multiple = TRUE
            ),
            selectizeInput(
              "CivicYear2",
              label = "Year",
              choices = unique(civicDT2$Year),
            ),
            selectizeInput(
              "CivicGeo2",
              label = "Geography",
              choices = unique(civicDT2$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "CivicCharacteristics2",
              label = "Sociodemographic Characteristics",
              choices = list(
                'Age',
                'Gender',
                'Immigration Status',
                'Generation Status',
                'Language Spoken',
                'Education Status'
              ),
            ),
            #### Age ----
            conditionalPanel(
              condition = "input.CivicCharacteristics2 == 'Age'",
              selectizeInput(
                "CivicCharSpecAge2",
                label = "Age",
                choices = list(
                  'Total, 18 years and over',
                  '18 to 24 years',
                  '25 to 64 years',
                  '65 years and over'
                ),
              ),
            ),
            #### Gender ----
            conditionalPanel(
              condition = "input.CivicCharacteristics2 == 'Gender'",
              selectizeInput(
                "CivicCharSpecSex2",
                label = "Gender",
                choices = list('Man', 'Woman'),
              ),
            ),
            #### Immigration status ----
            conditionalPanel(
              condition = "input.CivicCharacteristics2 == 'Immigration Status'",
              selectizeInput(
                "CivicCharSpecImm2",
                label = "Immigration Status",
                choices = list('Immigrants', 'Non-Immigrants'),
              ),
            ),
            #### Generation status ----
            conditionalPanel(
              condition = "input.CivicCharacteristics2 == 'Generation Status'",
              selectizeInput(
                "CivicCharSpecGen2",
                label = "Immigration Status",
                choices = list(
                  'First generation',
                  'Second generation',
                  'Third generation or more'
                ),
              ),
            ),
            #### Language spoken ----
            conditionalPanel(
              condition = "input.CivicCharacteristics2 == 'Language Spoken'",
              selectizeInput(
                "CivicCharSpecLang2",
                label = "Language Spoken",
                choices = list(
                  'First official language spoken, English only',
                  'First official language spoken, French only'
                ),
              ),
            ),
            #### Education status ----
            conditionalPanel(
              condition = "input.CivicCharacteristics2 == 'Education Status'",
              selectizeInput(
                "CivicCharSpecEdu2",
                label = "Language Spoken",
                choices = list(
                  'Secondary (high) school diploma or equivalency certificate or less',
                  'Postsecondary certicate or diploma (non-university)',
                  'University certificate or diploma'
                ),
              ),
            ),
            selectizeInput(
              "CivicConf2",
              label = "Confidence",
              choices = unique(civicDT2$Confidence),
            ),
          ),
          ### Basic needs and housing ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing'",
            selectizeInput(
              "dimBasicNeeds",
              label = "Indicators",
              choices = list(
                "Percent of the population living in a food-secure household",
                "Percent of the population living in a household with marginal food security",
                "Percent of the population living in a food-insecure household, moderate or severe",
                "Percent of the population living in a household with moderate food insecurity",
                "Percent of the population living in a household with severe food insecurity"
              ),
            ),
            selectizeInput(
              "basicYear",
              label = "Year",
              choices = unique(basicDT$'Year')
            ),
            selectizeInput(
              "basicGeo",
              label = "Geography",
              choices = unique(basicDT$'Geography')
            ),
            selectizeInput(
              "VM185",
              label = "Visible minority status",
              choices = vmstatus_list,
              selected = list("Latin American"),
              multiple = TRUE
            ),
            selectizeInput(
              "basicCharacteristics",
              label = "Selected sociodemographic characteristics",
              choices = list('Age', 'Gender', 'Immigration Status'),
            ),
            #### Age ----
            conditionalPanel(
              condition = "input.basicCharacteristics == 'Age'",
              selectizeInput(
                "basicCharSpecAge",
                label = "Age",
                choices = list(
                  'Total, 12 years and over',
                  '12 to 17 years',
                  '18 to 64 years',
                  '65 years and over'
                ),
              ),
            ),
            #### Gender ----
            conditionalPanel(
              condition = "input.basicCharacteristics == 'Gender'",
              selectizeInput(
                "basicCharSpecSex",
                label = "Gender",
                choices = list('Total, by gender of person', 'Men', 'Women'),
              ),
            ),
            #### Immigration status ----
            conditionalPanel(
              condition = "input.basicCharacteristics == 'Immigration Status'",
              selectizeInput(
                "basicCharSpecImm",
                label = "Immigration Status",
                choices = list(
                  'Total, by immigration status',
                  'Landed immigrants',
                  'Immigrant, less than 10 years in Canada',
                  'Immigrant, 10 or more years in Canada',
                  'Born in Canada'
                ),
              ),
            ),
            selectizeInput(
              "basicConfidence",
              label = "Confidence Interval",
              choices = unique(basicDT$'Confidence')
            ),
          ),
          ### Health and wellbeing ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing'",
            selectizeInput(
              "dimHealth",
              label = "Indicators",
              choices = list(
                "Percent of the population reporting very good or excellent general health",
                "Percent of the population reporting fair or poor general health",
                "Percent of the population reporting very good or excellent mental health",
                "Percent of the population reporting fair or poor mental health",
                "Percent of the population reporting their life stressful",
                "Percent of the population reporting life satisfaction, satisfied or very satisfied",
                "Percent of the population reporting having a regular healthcare provider",
                "Percent of the population reporting no need for mental health care",
                "Percent of the population reporting all needs met for mental health care",
                "Percent of the population reporting needs partially met for mental health care",
                "Percent of the population reporting needs partially met or needs not met for mental health care",
                "Percent of the population reporting needs not met for mental health care",
                "Percent of the population reporting unmet health care needs"
              ),
            ),
            selectizeInput(
              "healthYear",
              label = "Year",
              choices = unique(healthDT$'Year')
            ),
            selectizeInput(
              "healthGeo",
              label = "Geography",
              choices = unique(healthDT$'Geography')
            ),
            selectizeInput(
              "VM180",
              label = "Visible minority status",
              choices = vmstatus_list,
              selected = list("Latin American"),
              multiple = TRUE
            ),
            selectizeInput(
              "healthCharacteristics",
              label = "Selected sociodemographic characteristics",
              choices = list('Age', 'Gender', 'Immigration Status'),
            ),
            #### Age ----
            conditionalPanel(
              condition = "input.healthCharacteristics == 'Age'",
              selectizeInput(
                "healthCharSpecAge",
                label = "Age",
                choices = list(
                  'Total, 12 years and over',
                  '12 to 17 years',
                  '18 to 64 years',
                  '65 years and over'
                ),
              ),
            ),
            #### Gender ----
            conditionalPanel(
              condition = "input.healthCharacteristics == 'Gender'",
              selectizeInput(
                "healthCharSpecSex",
                label = "Gender",
                choices = list('Total, by gender of person', 'Men', 'Women'),
              ),
            ),
            #### Immigration status ----
            conditionalPanel(
              condition = "input.healthCharacteristics == 'Immigration Status'",
              selectizeInput(
                "healthCharSpecImm",
                label = "Immigration Status",
                choices = list(
                  'Total, by immigration status',
                  'Landed immigrants',
                  'Immigrant, less than 10 years in Canada',
                  'Immigrant, 10 or more years in Canada',
                  'Born in Canada'
                ),
              ),
            ),
            selectizeInput(
              "healthConfidence",
              label = "Confidence Interval",
              choices = unique(healthDT$'Confidence')
            ),
          ),
          ### Income and wealth ----
          conditionalPanel(
            condition = "input.dim == 'Income and wealth'",
            selectizeInput(
              "dimIncome",
              label = "Indicators",
              choices = list(
                "Average total household income, adjusted for the number of persons",
                "Percent of the population living in poverty (low-income MBM)",
                "Percent of the population living in low income situation (before-tax)",
                "Percent of the population living in low income situation (after-tax)",
                "Percent of the population reporting difficulty in meeting financial needs of their household",
                "Percent of the population reporting ease in meeting financial needs of their household"
              )
            )
          ),
          #### Social connections and personnal networks ----
          conditionalPanel(
            condition = "input.dim == 'Social connections and personnal networks'",
            selectizeInput(
              "dimSocial",
              label = "Indicators",
              choices = list(
                "Population reporting that most people can be trusted",
                "Population reporting strong sense of belonging to their local community",
                "Population reporting strong sense of belonging to their town or city",
                "Population reporting strong sense of belonging to their province",
                "Population reporting strong sense of belonging to Canada",
                "Percent of the population living alone",
                "Median size of a personal local network with close ties",
                "Average size of a local personal network with close ties",
                "Percent of the population with a personal close-ties network of 10 or more people",
                "Percent of the population with a personal close-ties network of 5 or more relatives",
                "Percent of the population with a personal close-ties network of 5 or more friends",
                "Percent of the population with no personal network with weak ties",
                "Percent of the population with a personal weak-ties network of 1 to 19 people",
                "Percent of the population with a personal weak-ties network of 20 or more people ",
                "Percent of the population with a personal ethnically-diverse network"
              ),
            ),
            selectizeInput(
              "VM401",
              label = "Visible minority status",
              choices = vmstatus_list,
              selected = list("Black"),
              multiple = TRUE
            ),
            selectizeInput(
              "confYearS",
              label = "Year",
              choices = unique(confidenceDT$'Year')
            ),
            selectizeInput(
              "confGeoS",
              label = "Geography",
              choices = unique(confidenceDT$'Geography')
            ),
            selectizeInput(
              "confCharacteristics",
              label = "Selected sociodemographic characteristics",
              choices = list(
                'Age',
                'Gender',
                'Immigration Status',
                'Generation Status',
                'Language Spoken',
                'Education Status'
              ),
            ),
            #### Age ----
            conditionalPanel(
              condition = "input.confCharacteristics == 'Age'",
              selectizeInput(
                "confCharSpecAge",
                label = "Age",
                choices = list(
                  'Total, 15 years and over',
                  '15 to 24 years',
                  '25 to 64 years',
                  '65 years and over'
                ),
              ),
            ),
            #### Gender ----
            conditionalPanel(
              condition = "input.confCharacteristics == 'Gender'",
              selectizeInput(
                "confCharSpecSex",
                label = "Gender",
                choices = list('Men', 'Women'),
              ),
            ),
            #### Immigration status ----
            conditionalPanel(
              condition = "input.confCharacteristics == 'Immigration Status'",
              selectizeInput(
                "confCharSpecImm",
                label = "Immigration Status",
                choices = list('Immigrants', 'Non-Immigrants'),
              ),
            ),
            #### Generation status ----
            conditionalPanel(
              condition = "input.confCharacteristics == 'Generation Status'",
              selectizeInput(
                "confCharSpecGen",
                label = "Immigration Status",
                choices = list(
                  'First generation',
                  'Second generation',
                  'Third generation or more'
                ),
              ),
            ),
            #### Language spoken ----
            conditionalPanel(
              condition = "input.confCharacteristics == 'Language Spoken'",
              selectizeInput(
                "confCharSpecLang",
                label = "Language Spoken",
                choices = list(
                  'First official language spoken, English only',
                  'First official language spoken, French only'
                ),
              ),
            ),
            #### Education status ----
            conditionalPanel(
              condition = "input.confCharacteristics == 'Education Status'",
              selectizeInput(
                "confCharSpecEdu",
                label = "Language Spoken",
                choices = list(
                  'Secondary (high) school diploma or equivalency certificate or less',
                  'Postsecondary certicate or diploma (non-university)',
                  'University certificate or diploma'
                ),
              ),
            ),
            selectizeInput(
              "confConfidence",
              label = "Confidence Interval",
              choices = unique(confidenceDT$'Confidence')
            ),
          ),
          ### Education,training and skills ----
          conditionalPanel(
            condition = "input.dim == 'Education, training and skills'",
            selectizeInput(
              "dimEducation4",
              label = "Indicators",
              choices = list(
                "Population with no certificate, diploma or degree",
                "Population with high school diploma or equivalency certificate",
                "Population with postsecondary certificate or diploma below bachelor level",
                "Population with bachelor’s degree or above",
                "Population with bachelor’s degree",
                "Population with master’s degree or earned doctorate"
              ),
            ),
            #### Population with bachelor’s degree ----
            conditionalPanel(
              condition = "input.dimEducation4 == 'Population with bachelor’s degree'
                                 ||input.dimEducation4 =='Population with no certificate, diploma or degree'
                                 ||input.dimEducation4 == 'Population with high school diploma or equivalency certificate'
                                 ||input.dimEducation4 =='Population with postsecondary certificate or diploma below bachelor level'
                                 ||input.dimEducation4 == 'Population with bachelor’s degree or above'
                                 ||input.dimEducation4 == 'Population with master’s degree or earned doctorate' ",
              selectizeInput(
                "VM10",
                label = "Visible Minority Group",
                choices = unique(educationDT$VisMin),
                selected = list("Total - Visible minority", "Not a visible minority"),
                multiple = TRUE
              ),
              selectizeInput(
                "eduGeo2",
                label = "Geography",
                choices = unique(educationDT$Geography),
                selected = "Canada"
              ),
              selectizeInput(
                "eduVisMin2",
                label = "Groups designated by Immigration and Generational Status",
                choices = unique(educationDT$'Immigration'),
                selected = "Canada"
              ),
              selectizeInput(
                "eduYear2",
                label = "Year",
                choices = unique(educationDT$Year),
              ),
              selectizeInput(
                "eduAge2",
                label = "Age Group",
                choices = unique(educationDT$Age),
                selected = "Total - Age"
              ),
              selectizeInput(
                "eduSex2",
                label = "Gender",
                choices = sort(unique(educationDT$Sex), decreasing = TRUE),
                selected = "Total - Sex"
              ),
              selectizeInput(
                "eduLang2",
                label = "Language",
                choices = unique(educationDT$'Language'),
              )
            ),
          ),
          ### Local Community ----
          conditionalPanel(
            condition = "input.dim == 'Local community'",
            selectizeInput(
              "dimCommunity",
              label = "Indicators",
              choices = list(
                "Percent of the population satisfied with feeling part of their community",
                "Percent of the population satisfied with their neighbourhood",
                "Percent of the population satisfied with quality of local environment",
                "Percent of the population reporting feeling safe in their neighbourhood",
                "Percent of the population satisfied with personal safety from crime",
                "Violent victimization rate per 1,000 population",
                "Percent of the population reporting incident against them was a hate crime",
                "Percent of police reported hate crimes motivated by Race or ethnicity",
                "Percent of police reported hate crimes motivated by religion",
                "Percent of the population perceiving local police good in enforcing laws",
                "Percent of the population perceiving local police good in responding to calls",
                "Percent of the population perceiving local police good in being approachable",
                "Percent of the population perceiving local police good in supplying information",
                "Percent of the population perceiving local police good in ensuring safety in the area",
                "Percent of the population perceiving local police as treating people fairly"
              )
            )
          ),
          #### Not Included ----
          #'NOTE [why is this not included/should it be commented out?]
          conditionalPanel(
            condition = "input.dim == 'Access to public services and institutions'",
            selectizeInput(
              "dimPublicServices",
              label = "Indicators",
              choices = list(
                "Percent of the population reporting having regular health care provider",
                "Percent of the population reporting unmet healthcare needs",
                "Percent of the population reporting no need for mental health care",
                "Percent of the population reporting needs for mental health care not met or partially met",
                "Percent of the population reporting needs for mental health care not met",
                "Percent of the population reporting needs for mental health partially met",
                "Percent of the population reporting all needs for mental health care met"
              )
            )
          ),
          # Add the js code and button to the page
          # extendShinyjs(text = jsResetCode, functions = "reset"),
          actionButton("reset_button", "Reset Page"),
        ),
        
        ## Main panel ----
        # Visuals will be displayed here
        # You can also add text, images, etc. here
        mainPanel(
          h2("Groups Designated as Visible Minorities"),
          ### Basic needs and housing ----
          #### Severe food insecurity ----
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with severe food insecurity' & input.basicCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasic5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with severe food insecurity' & input.basicCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicAge5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with severe food insecurity' & input.basicCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicSex5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Moderate food insecurity ----
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with moderate food insecurity' & input.basicCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasic4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with moderate food insecurity' & input.basicCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicAge4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with moderate food insecurity' & input.basicCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicSex4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Food-insecure household, moderate or severe ----
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a food-insecure household, moderate or severe' & input.basicCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasic3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a food-insecure household, moderate or severe' & input.basicCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicAge3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a food-insecure household, moderate or severe' & input.basicCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicSex3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Marginal food security ----
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with marginal food security' & input.basicCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasic2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with marginal food security' & input.basicCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicAge2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a household with marginal food security' & input.basicCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicSex2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Food-secure household ----
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a food-secure household' & input.basicCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasic1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a food-secure household' & input.basicCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicAge1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Basic needs and housing' & input.dimBasicNeeds == 'Percent of the population living in a food-secure household' & input.basicCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarBasicSex1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ### Health and wellbeing ----
          #### Unmet health care needs ----
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting unmet health care needs' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth13",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting unmet health care needs' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge13",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting unmet health care needs' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex13",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting needs not met for mental health care' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Needs not met for mental health care ----
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting needs not met for mental health care' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting needs not met for mental health care' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Needs partially met or needs not met for mental health care ----
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting needs partially met or needs not met for mental health care' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting needs partially met or needs not met for mental health care' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting needs partially met or needs not met for mental health care' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### All needs met for mental health care ----
          ##### Immigration status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting all needs met for mental health care' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting all needs met for mental health care' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting all needs met for mental health care' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### No need for mental health care ----
          ##### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting no need for mental health care' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting no need for mental health care' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting no need for mental health care' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Regular healthcare provider ----
          ##### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting having a regular healthcare provider' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting having a regular healthcare provider' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting having a regular healthcare provider' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Life satisfaction, satisfied or very satisfied ----
          ##### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting life satisfaction, satisfied or very satisfied' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting life satisfaction, satisfied or very satisfied' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting life satisfaction, satisfied or very satisfied' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Reporting their life stressful ----
          ##### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting their life stressful' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting their life stressful' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting their life stressful' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Fair or poor mental health ----
          ##### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting fair or poor mental health' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting fair or poor mental health' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting fair or poor mental health' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### vVery good or excellent mental health ----
          ##### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting very good or excellent mental health' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting very good or excellent mental health' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting very good or excellent mental health' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Fair or poor general health ----
          ##### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting fair or poor general health' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting fair or poor general health' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting fair or poor general health' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Very good or excellent general health ----
          ##### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting very good or excellent general health' & input.healthCharacteristics == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealth1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Age ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting very good or excellent general health' & input.healthCharacteristics == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthAge1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          ##### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Percent of the population reporting very good or excellent general health' & input.healthCharacteristics == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarHealthSex1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Canadian Community Health Survey (CCHS), September to December 2020"
            )
          ),
          #### Public services and institutions ----
          ##### Confidence in banks ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConf7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfAge7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfSex7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfGen7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfLang7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfEdu7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Confidence in the justice system and courts ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConf4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfAge4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfSex4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfGen4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfLang4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfEdu4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Confidence in merchants and local business people ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConf6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfAge6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfSex6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfGen6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfLang6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfEdu6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Confidence in major corporations ---- 
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConf5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfAge5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfSex5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfGen5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfLang5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfEdu5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Confidence in the school system ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConf3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfAge3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfSex3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfGen3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfLang3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfEdu3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Confidence in the police service ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConf",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfAge",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfSex",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfGen",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfLang",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Statu ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfEdu",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Confidence in Federal Parliament ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConf1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfAge1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfSex1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfGen1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfLang1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfEdu1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Confidence in the Canadian media ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConf2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfAge2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfSex2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfGen2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfLang2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarConfEdu2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          #### Civic engagement and political participation ----
          ##### Engaged in political activities ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population engaged in political activities' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic17",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population engaged in political activities' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge17",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population engaged in political activities' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex17",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population engaged in political activities' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen17",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population engaged in political activities' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang17",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population engaged in political activities' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu17",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in an environmental group ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an environmental group' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic16",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an environmental group' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge16",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an environmental group' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex16",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an environmental group' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen16",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an environmental group' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang16",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an environmental group' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu16",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an immigrant or ethnic association or club' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic15",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an immigrant or ethnic association or club' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge15",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an immigrant or ethnic association or club' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex15",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an immigrant or ethnic association or club' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen15",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an immigrant or ethnic association or club' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang15",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in an immigrant or ethnic association or club' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu15",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in a youth organization ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a youth organization' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic14",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a youth organization' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge14",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a youth organization' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex14",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a youth organization' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen14",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a youth organization' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang14",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a youth organization' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu14",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in a humanitarian or charitable organization or service club -----
          ###### Immigration Status ----
          # FIXME: Number 13 has an error and slows down the program
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a humanitarian or charitable organization or service club' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a humanitarian or charitable organization or service club' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a humanitarian or charitable organization or service club' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a humanitarian or charitable organization or service club' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoke ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a humanitarian or charitable organization or service club' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a humanitarian or charitable organization or service club' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu12",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in a school group, neighbourhood, civic or community association ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a school group, neighbourhood, civic or community association' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a school group, neighbourhood, civic or community association' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a school group, neighbourhood, civic or community association' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a school group, neighbourhood, civic or community association' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a school group, neighbourhood, civic or community association' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a school group, neighbourhood, civic or community association' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu11",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in a religious-affiliated group ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a religious-affiliated group' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a religious-affiliated group' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a religious-affiliated group' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a religious-affiliated group' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a religious-affiliated group' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a religious-affiliated group' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu10",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in a political party or group ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a political party or group' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a political party or group' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a political party or group' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a political party or group' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a political party or group' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a political party or group' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu9",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in a union or professional association ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a union or professional association' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a union or professional association' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a union or professional association' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a union or professional association' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a union or professional association' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a union or professional association' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu8",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in a cultural, educational or hobby organization ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a cultural, educational or hobby organization' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a cultural, educational or hobby organization' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a cultural, educational or hobby organization' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a cultural, educational or hobby organization' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a cultural, educational or hobby organization' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a cultural, educational or hobby organization' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu7",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members of at least one civic group or organization ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu6",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Members in a sports or recreational organization ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a sports or recreational organization' & input.CivicCharacteristics3 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a sports or recreational organization' & input.CivicCharacteristics3 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a sports or recreational organization' & input.CivicCharacteristics3 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a sports or recreational organization' & input.CivicCharacteristics3 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken' ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a sports or recreational organization' & input.CivicCharacteristics3 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members in a sports or recreational organization' & input.CivicCharacteristics3 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu5",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Voting in the last provincial election ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Voting in the last federal election ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ##### Voting in the last municipal election ----
          ###### Immigration Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Immigration Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivic2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Age ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Age'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicAge2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Gender ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Gender'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicSex2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Generation Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Generation Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicGen2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Language Spoken ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Language Spoken'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicLang2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          ###### Education Status ----
          conditionalPanel(
            condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Education Status'",
            br(),
            br(),
            plotlyOutput(
              "sBarCivicEdu2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
            )
          ),
          #### Participation in the Labour Market ----
          ##### Mainly full-time weeks in the previous year ----
          conditionalPanel(
            condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Workers working mainly full-time weeks in the previous year'",
            br(),
            br(),
            plotlyOutput(
              "sBarRate4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
            )
          ),
          ##### Unemployment ----
          conditionalPanel(
            condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Working-age population in unemployment (unemployment rate)'",
            br(),
            br(),
            plotlyOutput(
              "sBarRate3",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
            )
          ),
          ##### Employment ----
          conditionalPanel(
            condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Working-age population in employment (employment rate)'",
            br(),
            br(),
            plotlyOutput(
              "sBarRate2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
            )
          ),
          ##### In the labour force ----
          conditionalPanel(
            condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Working-age population in the labour force (participation rate)'",
            br(),
            br(),
            plotlyOutput(
              "sBarRate1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
            )
          ),
          ##### Average weekly wage of paid employees ----
          conditionalPanel(
            condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Average weekly wage of paid employees'",
            br(),
            br(),
            plotlyOutput(
              "sBarInc2",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Censuses of population, 2006 and 2016; National Household Survey, 2011"
            )
          ),
          ##### Average employment income of the population ----
          conditionalPanel(
            condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Average employment income of the population'",
            br(),
            br(),
            plotlyOutput(
              "sBarInc1",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Censuses of population, 2006 and 2016; National Household Survey, 2011"
            )
          ),
          ##### Self-employed workers in the labour force ----
          conditionalPanel(
            condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Self-employed workers in the labour force (unincorporated)'",
            br(),
            br(),
            plotlyOutput(
              "sBarRep4",
              inline = TRUE,
              width = 700,
              height = 500
            ),
            br(),
            helpText(
              "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
            )
          ),
          condition = "input.dim == 'Representation in decision-making positions' & input.Rep == 'Percent of workers in middle management occupations'",
          br(),
          br(),
          plotlyOutput(
            "sBarRep3",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
          )
        ),
        #### Representation in decision-making positions ----
        ##### Senior management occupations ----
        conditionalPanel(
          condition = "input.dim == 'Representation in decision-making positions' & input.Rep == 'Percent of workers in senior management occupations'",
          br(),
          br(),
          plotlyOutput(
            "sBarRep2",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
          )
        ),
        ##### All management occupations ----
        conditionalPanel(
          condition = "input.dim == 'Representation in decision-making positions' & input.Rep == 'Percent of workers in all management occupations'",
          br(),
          br(),
          plotlyOutput(
            "sBarRep1",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
          )
        ),
        #### Participation in the Labour Market ----
        ##### Youth not in employment, education or training ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Youth not in employment, education or training (NEET)'",
          br(),
          br(),
          plotlyOutput(
            "sBarYouth",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
          )
        ),
        ##### Paid employees having disability insurance in their current job ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees having disability insurance in their current job'",
          br(),
          br(),
          plotlyOutput(
            "employmentPlot7",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText("Note: Source.")
        ),
        ##### Currently employed population considering their job related to their education ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Currently employed population considering their job related to their education'",
          br(),
          br(),
          plotlyOutput(
            "employmentPlot6",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText("Note: Source.")
        ),
        ##### Paid employees having paid vacation leave in their current job ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees having paid vacation leave in their current job'",
          br(),
          br(),
          plotlyOutput(
            "employmentPlot5",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText("Note: Source.")
        ),
        ##### Paid employees receiving at least one employment benefit in their current job ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees receiving at least one employment benefit in their current job'",
          br(),
          br(),
          plotlyOutput(
            "employmentPlot4",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText("Note: Source.")
        ),
        ##### Paid employees having pension plan in their current job ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees having pension plan in their current job'",
          br(),
          br(),
          plotlyOutput(
            "employmentPlot3",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText("Note: Source.")
        ),
        ##### Paid employees considering their current job good for career advancement ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees considering their current job good for career advancement'",
          br(),
          br(),
          plotlyOutput(
            "employmentPlot2",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText("Note: Source.")
        ),
        ##### Paid employees having paid sick leave in their current job ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees having paid sick leave in their current job'",
          br(),
          br(),
          plotlyOutput(
            "employmentPlot",
            inline = TRUE,
            width = 00,
            height = 500
          ),
          br(),
          helpText("Note: Source.")
        ),
        #### Discrimination and victimization ----
        ##### Discrimination when attending school or classes ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Discrimination when attending school or classes"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarClass",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarClass2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Discrimination when attending school or classes"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarClassAge",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarClassAge2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Discrimination when attending school or classes"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarClassGender",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarClassGender2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Discrimination when attending school or classes"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarClassGen",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarClassGen2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Discrimination when attending school or classes"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarClassLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarClassLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Discrimination when attending school or classes"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarClassEdu",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarClassEdu2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Discrimination when in a store, bank or restaurant ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Discrimination when in a store, bank or restaurant"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarBan",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarBan2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Discrimination when in a store, bank or restaurant"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarBanAge",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarBanAge2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Discrimination when in a store, bank or restaurant"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarBanSex",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarBanSex2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Discrimination when in a store, bank or restaurant"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarBanGen",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarBanGen2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Discrimination when in a store, bank or restaurant"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarBanLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarBanLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Discrimination when in a store, bank or restaurant"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarBanEdu",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarBanEdu2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Discrimination when dealing with the police ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Discrimination when dealing with the police"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarPol",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarPol2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Discrimination when dealing with the police"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarPolAge",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarPolAge2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Discrimination when dealing with the police"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarPolSex",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarPolSex2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Discrimination when dealing with the police"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarPolGen",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarPolGen2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Discrimination when dealing with the police"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarPolLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarPolLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Discrimination when dealing with the police"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarPolEdu",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarPolEdu2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
          
        ),
        ##### Discrimination at work or when applying for a job or promotion ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Discrimination at work or when applying for a job or promotion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarJob",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarJob2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Discrimination at work or when applying for a job or promotion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarJobAge",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarJobAge2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Discrimination at work or when applying for a job or promotion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarJobSex",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarJobSex2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Discrimination at work or when applying for a job or promotion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarJobGen",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarJobGen2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Discrimination at work or when applying for a job or promotion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarJobLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarJobLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Discrimination at work or when applying for a job or promotion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarJobEdu",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarJobEdu2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Experience(s) of discrimination based on language ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Experience(s) of discrimination based on language"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Experience(s) of discrimination based on language"),
          br(),
          
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarLangAge",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarLangAge2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Experience(s) of discrimination based on language"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarLangSex",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarLangSex2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Experience(s) of discrimination based on language"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarLangGen",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarLangGen2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Experience(s) of discrimination based on language"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarLangLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarLangLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Experience(s) of discrimination based on language"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarLangEdu",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarLangEdu2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Age ----
        ###### Experience(s) of discrimination based on religion Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Experience(s) of discrimination based on religion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarRel",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarRel2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Experience(s) of discrimination based on religion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarRelAge",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarRelAge2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Experience(s) of discrimination based on religion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarRelSex",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarRelSex2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Experience(s) of discrimination based on religion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarRelGen",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarRelGen2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Experience(s) of discrimination based on religion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarRelLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarRelLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Experience(s) of discrimination based on religion"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarRelEdu",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarRelEdu2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Experience(s) of discrimination based on race or colour ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Experience(s) of discrimination based on race or colour"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCol",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCol2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Experience(s) of discrimination based on race or colour"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarColAge",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarColAge2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Experience(s) of discrimination based on race or colour"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarColSex",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarColSex2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Experience(s) of discrimination based on race or colour"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarColGen",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarColGen2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Experience(s) of discrimination based on race or colour"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarColLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarColLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Experience(s) of discrimination based on race or colour"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarColEdu",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarColEdu2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Experience(s) of discrimination based on ethnicity or cultur ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Experience(s) of discrimination based on ethnicity or culture"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCov",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCov2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Experience(s) of discrimination based on ethnicity or culture"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovAge",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovAge2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Experience(s) of discrimination based on ethnicity or culture"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovSex",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovSex2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Experience(s) of discrimination based on ethnicity or culture"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovGen",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovGen2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Experience(s) of discrimination based on ethnicity or culture"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovLang",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovLang2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Experience(s) of discrimination based on ethnicity or culture"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovEdu",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovEdu2",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Experience(s) of discrimination ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Immigration Status'" ,
          br(),
          h3("Experience(s) of discrimination"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCov1",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCov21",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Age'" ,
          br(),
          h3("Experience(s) of discrimination"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovAge1",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovAge21",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Gender'" ,
          br(),
          h3("Experience(s) of discrimination"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovSex1",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovSex21",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Generation Status'" ,
          br(),
          h3("Experience(s) of discrimination"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovGen1",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovGen21",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Language Spoken'" ,
          br(),
          h3("Experience(s) of discrimination"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovLang1",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovLang21",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Education Status'" ,
          br(),
          h3("Experience(s) of discrimination"),
          br(),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotlyOutput(
              "sBarCovEdu1",
              inline = TRUE,
              width = 400,
              height = 500
            ),
            plotlyOutput(
              "sBarCovEdu21",
              inline = TRUE,
              width = 400,
              height = 500
            )
          )),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Hate crime ----
        ###### Race or ethnicity ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Hate Crime' & input.motivation == 'Race or ethnicity'",
          br(),
          br(),
          plotlyOutput(
            "immdisPlot",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Note: Uniform Crime Reporting Survey (UCR) data are collected
                            directly from survey respondents (Police Services) and extracted
                            from administrative files. The categories that appear in this chart
                            are those used by police services when reporting on hate crime incidences."
          )
          
        ),
        #### Social connections and personnal networks ----
        ##### Strong sense of belonging to Canada ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Immigration Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConf12",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Age'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfAge12",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Gender'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfSex12",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Generation Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfGen12",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Language Spoken'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfLang12",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Education Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfEdu12",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Strong sense of belonging to their province ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Immigration Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConf11",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Age'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfAge11",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Gender'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfSex11",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Generation Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfGen11",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Language Spoken'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfLang11",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Education Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfEdu11",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Strong sense of belonging to their town or city ----
        ###### Education Status ----
        ####### I THINK THIS IS A MISTAKE/I think it must be immigration status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Education Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConf10",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Age'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfAge10",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Gender'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfSex10",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Generation Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfGen10",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Language Spoken'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfLang10",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Education Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfEdu10",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Strong sense of belonging to their local community ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Immigration Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConf9",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Age'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfAge9",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Gender'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfSex9",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Generation Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfGen9",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Language Spoken'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfLang9",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Education Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfEdu9",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ##### Most people can be trusted ----
        ###### Immigration Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Immigration Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConf8",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Age ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Age'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfAge8",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Gender ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Gender'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfSex8",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Generation Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Generation Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfGen8",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Language Spoken ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Language Spoken'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfLang8",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        ###### Education Status ----
        conditionalPanel(
          condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Education Status'",
          br(),
          br(),
          plotlyOutput(
            "sBarConfEdu8",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
          )
        ),
        #### Participation in the Labour Market ----
        ##### Overqualified workers with a university degree ----
        conditionalPanel(
          condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Overqualified workers with a university degree'",
          br(),
          br(),
          plotlyOutput(
            "sBarOver",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011."
          )
        ),
        #### Education, training and skills ----
        ##### Population with bachelor’s degree ----
        conditionalPanel(
          condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with bachelor’s degree'",
          br(),
          br(),
          plotlyOutput(
            "sBarEduVM",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        ##### Population with no certificate, diploma or degree ----
        conditionalPanel(
          condition = "input.dim == 'Education, training and skills' && input.dimEducation4 == 'Population with no certificate, diploma or degree'",
          br(),
          br(),
          plotlyOutput(
            "sBarEduVM1",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        ##### Population with high school diploma or equivalency certificate ----
        conditionalPanel(
          condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with high school diploma or equivalency certificate'",
          br(),
          br(),
          plotlyOutput(
            "sBarEduVM2",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        ##### Population with postsecondary certificate or diploma below bachelor level ----
        conditionalPanel(
          condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with postsecondary certificate or diploma below bachelor level'",
          br(),
          br(),
          plotlyOutput(
            "sBarEduVM3",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        ##### Population with bachelor’s degree or above ----
        conditionalPanel(
          condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with bachelor’s degree or above'",
          br(),
          br(),
          plotlyOutput(
            "sBarEduVM4",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        ##### Population with master’s degree or earned doctorate ----
        conditionalPanel(
          condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with master’s degree or earned doctorate'",
          br(),
          br(),
          plotlyOutput(
            "sBarEduVM5",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        #### Discrimination and victimization ----
        ##### Hate Crime ----
        ###### Total police-reported hate crime ----
        conditionalPanel(
          condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Hate Crime' && input.motivation == 'Total police-reported hate crime'",
          br(),
          br(),
          plotlyOutput(
            "immdis2Plot",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          helpText(
            "Note: Uniform Crime Reporting Survey (UCR) data are collected
                            directly from survey respondents (Police Services) and extracted
                            from administrative files. The categories that appear in this chart
                            are those used by police services when reporting on hate crime incidences."
          )
        )
      )
    )
  ),
  
  ## 3. Tab for Immigrant Status Analysis ----
  #'NOTE [why is the vm list different?]
  tabPanel(
    "Immigration Status",
    fluid = TRUE,
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "dim2",
          label = "Theme",
          choices = list(
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
        ),
        ### Participation in the Labour Market ----
        conditionalPanel(
          condition = "input.dim2 == 'Participation in the Labour Market'",
          selectizeInput(
            "LM2",
            label = "Indicators",
            choices = list(
              "Working-age population in the labour force (participation rate)",
              "Working-age population in employment (employment rate)",
              "Working-age population in unemployment (unemployment rate)",
              "Workers working mainly full-time weeks in the previous year",
              "Self-employed workers in the labour force (unincorporated)",
              "Overqualified workers with a university degree",
              "Youth not in employment, education or training (NEET)",
              "Average employment income of the population",
              "Average weekly wage of paid employees",
              "Currently employed population considering their job related to their education",
              "Paid employees considering their current job good for career advancement",
              "Paid employees receiving at least one employment benefit in their current job",
              "Paid employees having pension plan in their current job",
              "Paid employees having paid sick leave in their current job",
              "Paid employees having disability insurance in their current job",
              "Paid employees having supplemental medical care in their current job",
              "Paid employees having worker's compensation in their current job",
              "Paid employees having maternity, paternity or lay-off benefits in their current job",
              "Paid employees covered by union contract or collective agreement in their current job",
              "Paid employees receiving formal training in their current job",
              "Paid employees receiving informal training in their current job"
            ),
            selected = NULL
          ),
          ### Self-employed workers in the labour force ----
          conditionalPanel(
            condition = "input.LM2 == 'Self-employed workers in the labour force (unincorporated)'",
            selectizeInput(
              "RepImm2",
              label = "Immigrant and generation status",
              choices = unique(representationDT$Immigration),
              selected = ("Immigrants"),
              multiple = TRUE
            ),
            selectizeInput(
              "VM190",
              label = "Visible minority status",
              choices = list(
                "Total - Visible minority",
                "Visible minority population",
                "South Asian",
                "Chinese",
                "Black",
                "Filipino",
                "Latin American",
                "Arab",
                "Southeast Asian",
                "West Asian",
                "Korean",
                "Japanese",
                "Visible minority n.i.e",
                "Multiple visible minorities",
                "Not a visible minority"
              ),
              selected = list("West Asian"),
            ),
            selectizeInput(
              "RepDegree2",
              label = "Highest certificate, diploma or degree",
              choices = unique(representationDT$Degree),
            ),
            selectizeInput(
              "RepGeo2",
              label = "Geography",
              choices = unique(representationDT$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "RepYear2",
              label = "Year",
              choices = unique(representationDT$Year),
            ),
            selectizeInput(
              "RepAgeLang2",
              label = "Age group and first official language spoken",
              choices = unique(representationDT$Age),
            ),
            selectizeInput(
              "RepSex2",
              label = "Gender",
              choices = unique(representationDT$Sex),
              selected = "Total - Sex"
            ),
          ),
          ### Overqualified workers with a university degree ----
          conditionalPanel(
            condition = "input.LM2 == 'Overqualified workers with a university degree'",
            selectizeInput(
              "VM21",
              label = "Groups designated by Immigration and Generational Status",
              choices = unique(OverQualDT$Immigration),
              multiple = TRUE
            ),
            selectizeInput(
              "OverVMIS",
              label = "Visible minority status",
              choices = list(
                "Total - Visible minority",
                "Visible minority population",
                "South Asian",
                "Chinese",
                "Black",
                "Filipino",
                "Latin American",
                "Arab",
                "Southeast Asian",
                "West Asian",
                "Korean",
                "Japanese",
                "Visible minority n.i.e",
                "Multiple visible minorities",
                "Not a visible minority"
              ),
              selected = list("Filipino")
            ),
            selectizeInput(
              "OverLocationIS",
              label = "Location of Study",
              choices = unique(OverQualDT$Location),
            ),
            selectizeInput(
              "OverDegreeIS",
              label = "Highest certificate, diploma or degree",
              choices = unique(OverQualDT$Degree),
            ),
            selectizeInput(
              "OverGeoIS",
              label = "Geography",
              choices = unique(OverQualDT$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "OverYearIS",
              label = "Year",
              choices = unique(OverQualDT$Year),
            ),
            selectizeInput(
              "OverAgeIS",
              label = "Age Group",
              choices = unique(OverQualDT$Age),
              selected = "Total - Age"
            ),
            selectizeInput(
              "OverSexIS",
              label = "Sex",
              choices = sort(unique(OverQualDT$Sex), decreasing = TRUE),
              selected = "Total - Sex"
            ),
            selectizeInput(
              "OverLangIS",
              label = "Language",
              choices = unique(OverQualDT$'Language'),
            )
          ),
          ### Youth not in employment, education or training (NEET) ----
          conditionalPanel(
            condition = "input.LM2 == 'Youth not in employment, education or training (NEET)'",
            selectizeInput(
              "YouthImm2",
              label = "Generation Status",
              choices = unique(youthDT$Immigration),
              selected = ("First generation"),
              multiple = TRUE
            ),
            selectizeInput(
              "YouthGeo2",
              label = "Geography",
              choices = unique(youthDT$Geography),
            ),
            selectizeInput(
              "VM195",
              label = "Visible minority status",
              choices = list(
                "Total - Visible minority",
                "Visible minority population",
                "South Asian",
                "Chinese",
                "Black",
                "Filipino",
                "Latin American",
                "Arab",
                "Southeast Asian",
                "West Asian",
                "Korean",
                "Japanese",
                "Visible minority n.i.e",
                "Multiple visible minorities",
                "Not a visible minority"
              ),
              selected = list("South Asian"),
            ),
            selectizeInput(
              "YouthYear2",
              label = "Year",
              choices = unique(youthDT$Year),
            ),
            selectizeInput(
              "YouthAge2",
              label = "Age Group",
              choices = unique(youthDT$Age),
              selected = "Total - Age"
            ),
            selectizeInput(
              "YouthSex2",
              label = "Sex",
              choices = sort(unique(youthDT$Sex), decreasing = TRUE),
              selected = "Total - Sex"
            ),
            selectizeInput(
              "YouthLang2",
              label = "Language",
              choices = unique(youthDT$'Language'),
              
            ),
          ),
          ### Working-age population in the labour force (participation rate) ----
          conditionalPanel(
            condition = "input.LM2 == 'Working-age population in the labour force (participation rate)'
                                    ||input.LM2 == 'Working-age population in employment (employment rate)'
                                    ||input.LM2 == 'Working-age population in unemployment (unemployment rate)'
                                    ||input.LM2 == 'Workers working mainly full-time weeks in the previous year'",
            selectizeInput(
              "RateImm2",
              label = "Immigrant and generation status",
              choices = unique(rateDT$Immigration),
              selected = list("Immigrants"),
              multiple = TRUE
              
            ),
            selectizeInput(
              "VM245",
              label = "Visible minority status",
              choices = list(
                "Total - Visible minority",
                "Visible minority population",
                "South Asian",
                "Chinese",
                "Black",
                "Filipino",
                "Latin American",
                "Arab",
                "Southeast Asian",
                "West Asian",
                "Korean",
                "Japanese",
                "Visible minority n.i.e",
                "Multiple visible minorities",
                "Not a visible minority"
              ),
              selected = list("Chinese"),
            ),
            selectizeInput(
              "RateDegree2",
              label = "Highest certificate, diploma or degree",
              choices = unique(rateDT$Degree),
            ),
            selectizeInput(
              "RateGeo2",
              label = "Geography",
              choices = unique(rateDT$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "RateYear2",
              label = "Year",
              choices = unique(rateDT$Year),
            ),
            selectizeInput(
              "RateAgeLang2",
              label = "Age group and first official language spoken",
              choices = unique(rateDT$Age),
            ),
            selectizeInput(
              "RateSex2",
              label = "Sex",
              choices = unique(rateDT$Sex),
              selected = "Total - Sex"
            ),
          ),
          ### Average employment income of the population ----
          conditionalPanel(
            condition = "input.LM2 == 'Average employment income of the population'
                                    ||input.LM2 == 'Average weekly wage of paid employees'",
            selectizeInput(
              "IncImm2",
              label = "Immigrant and generation status",
              choices = unique(incomeDT$Immigration),
              multiple = TRUE
            ),
            selectizeInput(
              "IncDegree2",
              label = "Highest certificate, diploma or degree",
              choices = unique(incomeDT$Degree),
            ),
            selectizeInput(
              "IncGeo2",
              label = "Geography",
              choices = unique(incomeDT$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "VM240",
              label = "Visible minority status",
              choices = list(
                "Total - Visible minority",
                "Visible minority population",
                "South Asian",
                "Chinese",
                "Black",
                "Filipino",
                "Latin American",
                "Arab",
                "Southeast Asian",
                "West Asian",
                "Korean",
                "Japanese",
                "Visible minority n.i.e",
                "Multiple visible minorities",
                "Not a visible minority"
              ),
              selected = list("Arab"),
            ),
            selectizeInput(
              "IncYear2",
              label = "Year",
              choices = unique(incomeDT$Year),
            ),
            selectizeInput(
              "IncAgeLang2",
              label = "Age group and first official language spoken",
              choices = unique(incomeDT$Age),
            ),
            selectizeInput(
              "IncSex2",
              label = "Gender",
              choices = unique(incomeDT$Sex),
              selected = "Total - Sex"
            ),
          ),
          ### Paid employees having disability insurance in their current job ----
          conditionalPanel(
            condition = "input.LM2 == 'Paid employees having disability insurance in their current job'",
            selectizeInput(
              "VM200",
              label = "Visible minority status",
              choices = list(
                "Total - Visible minority",
                "Visible minority population",
                "South Asian",
                "Chinese",
                "Black",
                "Filipino",
                "Latin American",
                "Arab",
                "Southeast Asian",
                "West Asian",
                "Korean",
                "Japanese",
                "Visible minority n.i.e",
                "Multiple visible minorities",
                "Not a visible minority"
              ),
              selected = list("Korean"),
              multiple = TRUE
            ),
            selectizeInput(
              "EmploymentYear8",
              label = "Year",
              choices = unique(employmentDT$Year),
            ),
            selectizeInput(
              "EmploymentGeo8",
              label = "Geography",
              choices = unique(employmentDT$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "EmploymentChar8",
              label = "Characteristic",
              choices = unique(employmentDT$Characteristic),
            ),
            selectizeInput(
              "EmploymentConf8",
              label = "Confidence",
              choices = unique(employmentDT$Confidence),
            ),
          ),
        ),
        ### Education, training and skills ----
        conditionalPanel(
          condition = "input.dim2 == 'Education, training and skills'",
          selectizeInput(
            "Edu",
            label = "Education Indicators",
            choices = list(
              "Population with no certificate, diploma or degree",
              "Population with high school diploma or equivalency certificate",
              "Population with postsecondary certificate or diploma below bachelor level",
              "Population with bachelor’s degree or above",
              "Population with bachelor’s degree",
              "Population with master’s degree or earned doctorate"
            ),
            selected = NULL
          ),
          ### Population with bachelor’s degree ----
          conditionalPanel(
            condition = "input.Edu == 'Population with bachelor’s degree'
                                 ||input.Edu ==  'Population with no certificate, diploma or degree'
                                 ||input.Edu ==  'Population with high school diploma or equivalency certificate'
                                 ||input.Edu ==  'Population with postsecondary certificate or diploma below bachelor level'
                                 ||input.Edu ==  'Population with bachelor’s degree or above'
                                 ||input.Edu ==  'Population with master’s degree or earned doctorate' ",
            selectizeInput(
              "VM9",
              label = "Groups designated by Immigration and Generational Status",
              choices = unique(educationDT$'Immigration'),
              selected = list("Total - Visible minority", "Not a visible minority"),
              multiple = TRUE
            ),
            selectizeInput(
              "eduGeo",
              label = "Geography",
              choices = unique(educationDT$Geography),
              selected = "Canada"
            ),
            selectizeInput(
              "eduVisMin",
              label = "Visible Minority Group",
              choices = unique(educationDT$VisMin),
              selected = "Canada"
            ),
            selectizeInput(
              "eduYear",
              label = "Year",
              choices = unique(educationDT$Year),
            ),
            selectizeInput(
              "eduAge",
              label = "Age Group",
              choices = unique(educationDT$Age),
              selected = "Total - Age"
            ),
            selectizeInput(
              "eduSex",
              label = "Gender",
              choices = sort(unique(educationDT$Sex), decreasing = TRUE),
              selected = "Total - Sex"
            ),
            selectizeInput(
              "eduLang",
              label = "Language",
              choices = unique(educationDT$'Language'),
            )
          )
        ),
        ### Civic engagement and political participation ----
        #'NOTE [is this supposed to be set for immigration as well? It's not in the condition]
        conditionalPanel(
          condition = "input.dim2 == 'Civic engagement and political participation'",
          selectizeInput(
            "dimCivilEngagement2",
            label = "Indicators",
            choices = list(
              "Percent of the population members of at least one civic group or organization",
              "Percent of the population members in a sports or recreational organization",
              "Percent of the population members in a cultural, educational or hobby organization",
              "Percent of the population members in a union or professional association",
              "Percent of the population members in a political party or group",
              "Percent of the population members in a religious-affiliated group",
              "Percent of the population members in a school group, neighbourhood, civic or community association",
              "Percent of the population members in a humanitarian or charitable organization or service club",
              "Percent of the population members in a seniors' group",
              "Percent of the population members in a youth organization",
              "Percent of the population members in an immigrant or ethnic association or club",
              "Percent of the population members in an environmental group",
              "Percent of the population engaged in political activities",
              "Percent of the population voting in the last federal election",
              "Percent of the population voting in the last provincial election",
              "Percent of the population voting in the last municipal election"
            ),
          ),
          selectizeInput(
            "VM260",
            label = "Visible minority status",
            choices = list(
              "Total, by visible minority group",
              "Total - Visible minority",
              "South Asian",
              "Chinese",
              "Black",
              "Filipino",
              "Latin American",
              "Arab",
              "Southeast Asian",
              "Not a visible minority"
            ),
            selected = list("Filipino"),
            multiple = TRUE
          ),
          selectizeInput(
            "CivicYear4",
            label = "Year",
            choices = unique(civicDT$Year),
          ),
          selectizeInput(
            "CivicGeo4",
            label = "Geography",
            choices = unique(civicDT$Geography),
            selected = "Canada"
          ),
          selectizeInput(
            "CivicCharacteristics4",
            label = "Sociodemographic Characteristics",
            choices = list(
              'Age',
              'Gender',
              'Immigration Status',
              'Generation Status',
              'Language Spoken',
              'Education Status'
            ),
          ),
          #### Age ----
          conditionalPanel(
            condition = "input.CivicCharacteristics4 == 'Age'",
            selectizeInput(
              "CivicCharSpecAge4",
              label = "Age",
              choices = list(
                'Total, 15 years and over',
                '15 to 24 years',
                '25 to 64 years',
                '65 years and over'
              ),
            ),
          ),
          #### Gender ----
          conditionalPanel(
            condition = "input.CivicCharacteristics4 == 'Gender'",
            selectizeInput(
              "CivicCharSpecGend4",
              label = "Gender",
              choices = list('Man', 'Woman'),
            ),
          ),
          #### Immigration Status ----
          conditionalPanel(
            condition = "input.CivicCharacteristics4 == 'Immigration Status'",
            selectizeInput(
              "CivicCharSpecImm4",
              label = "Immigration Status",
              choices = list('Immigrants', 'Non-Immigrants'),
            ),
          ),
          #### Generation Status ----
          conditionalPanel(
            condition = "input.CivicCharacteristics4 == 'Generation Status'",
            selectizeInput(
              "CivicCharSpecGen4",
              label = "Immigration Status",
              choices = list(
                'First generation',
                'Second generation',
                'Third generation or more'
              ),
            ),
          ),
          #### Language Spoken ----
          conditionalPanel(
            condition = "input.CivicCharacteristics4 == 'Language Spoken'",
            selectizeInput(
              "CivicCharSpecLang4",
              label = "Language Spoken",
              choices = list(
                'First official language spoken, English only',
                'First official language spoken, French only'
              ),
            ),
          ),
          #### Education Status ----
          condition = "input.CivicCharacteristics4 == 'Education Status'",
          selectizeInput(
            "CivicCharSpecEdu4",
            label = "Language Spoken",
            choices = list(
              'Secondary (high) school diploma or equivalency certificate or less',
              'Postsecondary certicate or diploma (non-university)',
              'University certificate or diploma'
            ),
          ),
        ),
        selectizeInput(
          "CivicConf4",
          label = "Confidence",
          choices = unique(civicDT$Confidence),
        ),
        ### Percent of the population voting in the last municipal election ----
        conditionalPanel(
          condition = "input.dimCivilEngagement2 == 'Percent of the population voting in the last municipal election'
                                                ||input.dimCivilEngagement2 == 'Percent of the population voting in the last provincial election'
                                                ||input.dimCivilEngagement2 == 'Percent of the population voting in the last federal election'",
          selectizeInput(
            "VM250",
            label = "Visible minority status",
            choices = list(
              "Total, by visible minority group",
              "Total - Visible minority",
              "South Asian",
              "Chinese",
              "Black",
              "Filipino",
              "Latin American",
              "Arab",
              "Southeast Asian",
              "Not a visible minority"
            ),
            selected = list("South Asian"),
            multiple = TRUE
          ),
          selectizeInput(
            "CivicYear5",
            label = "Year",
            choices = unique(civicDT2$Year),
          ),
          selectizeInput(
            "CivicGeo5",
            label = "Geography",
            choices = unique(civicDT2$Geography),
            selected = "Canada"
          ),
          selectizeInput(
            "CivicCharacteristics5",
            label = "Sociodemographic Characteristics",
            choices = list(
              'Immigration Status',
              'Generation Status',
              'Age',
              'Gender',
              'Language Spoken',
              'Education Status'
            ),
          ),
          #### Immigration Status ----
          conditionalPanel(
            condition = "input.CivicCharacteristics5 == 'Immigration Status'",
            selectizeInput(
              "CivicCharSpecImm5",
              label = "Immigration Status",
              choices = list('Immigrants', 'Non-Immigrants'),
              multiple = TRUE
            ),
          ),
          #### Generation Status ----
          conditionalPanel(
            condition = "input.CivicCharacteristics5 == 'Generation Status'",
            selectizeInput(
              "CivicCharSpecGen5",
              label = "Immigration Status",
              choices = list(
                'First generation',
                'Second generation',
                'Third generation or more'
              ),
              multiple = TRUE
            ),
          ),
          #### Age ----
          conditionalPanel(
            condition = "input.CivicCharacteristics5 == 'Age'",
            selectizeInput(
              "CivicCharSpecAge5",
              label = "Age",
              choices = list(
                'Total, 18 years and over',
                '18 to 24 years',
                '25 to 64 years',
                '65 years and over'
              ),
            ),
          ),
          #### Gender ----
          conditionalPanel(
            condition = "input.CivicCharacteristics5 == 'Gender'",
            selectizeInput(
              "CivicCharSpecSex5",
              label = "Gender",
              choices = list('Man', 'Woman'),
            ),
          ),
          #### Language Spoken ----
          conditionalPanel(
            condition = "input.CivicCharacteristics5 == 'Language Spoken'",
            selectizeInput(
              "CivicCharSpecLang5",
              label = "Language Spoken",
              choices = list(
                'First official language spoken, English only',
                'First official language spoken, French only'
              ),
            ),
          ),
          #### Education Status ----
          conditionalPanel(
            condition = "input.CivicCharacteristics5 == 'Education Status'",
            selectizeInput(
              "CivicCharSpecEdu5",
              label = "Language Spoken",
              choices = list(
                'Secondary (high) school diploma or equivalency certificate or less',
                'Postsecondary certicate or diploma (non-university)',
                'University certificate or diploma'
              ),
            ),
          ),
          selectizeInput(
            "CivicConf5",
            label = "Confidence",
            choices = unique(civicDT2$Confidence),
          ),
        ),
      ),
      ### Representation in decision-making positions ----
      conditionalPanel(
        condition = "input.dim2 == 'Representation in decision-making positions'",
        selectizeInput(
          "Rep2",
          label = "Indicators",
          choices = list(
            "Percent of workers in all management occupations",
            "Percent of workers in senior management occupations",
            "Percent of workers in middle management occupations"
          ),
        ),
        selectizeInput(
          "RepImm3",
          label = "Immigrant and generation status",
          choices = unique(representationDT$Immigration),
          selected = "Immigrants",
          multiple = TRUE
        ),
        selectizeInput(
          "VM210",
          label = "Visible minority status",
          choices = list(
            "Total - Visible minority",
            "Visible minority population",
            "South Asian",
            "Chinese",
            "Black",
            "Filipino",
            "Latin American",
            "Arab",
            "Southeast Asian",
            "West Asian",
            "Korean",
            "Japanese",
            "Visible minority n.i.e",
            "Multiple visible minorities",
            "Not a visible minority"
          ),
          selected = list("West Asian"),
        ),
        selectizeInput(
          "RepDegree3",
          label = "Highest certificate, diploma or degree",
          choices = unique(representationDT$Degree),
        ),
        selectizeInput(
          "RepGeo3",
          label = "Geography",
          choices = unique(representationDT$Geography),
          selected = "Canada"
        ),
        selectizeInput(
          "RepYear3",
          label = "Year",
          choices = unique(representationDT$Year),
        ),
        selectizeInput(
          "RepAgeLang3",
          label = "Age group and first official language spoken",
          choices = unique(representationDT$Age),
        ),
        selectizeInput(
          "RepSex3",
          label = "Gender",
          choices = unique(representationDT$Sex),
          selected = "Total - Sex"
        ),
      ),
      ### Basic needs and housing ----
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing'",
        selectizeInput(
          "dimBasicNeeds2",
          label = "Indicators",
          choices = list(
            "Percent of the population living in a food-secure household",
            "Percent of the population living in a household with marginal food security",
            "Percent of the population living in a food-insecure household, moderate or severe",
            "Percent of the population living in a household with moderate food insecurity",
            "Percent of the population living in a household with severe food insecurity"
          ),
        ),
        #### Characteristics? ----
        conditionalPanel(
          condition = "input.basicCharacteristics2",
          selectizeInput(
            "basicCharacteristics2",
            label = "Selected sociodemographic characteristics",
            choices = list('Immigration Status', 'Age', 'Gender'),
          ),
        ),
        #### Immigration Status ----
        conditionalPanel(
          condition = "input.basicCharacteristics2 == 'Immigration Status'",
          selectizeInput(
            "basicCharSpecImm2",
            label = "Immigration Status",
            choices = list(
              'Total, by immigration status',
              'Landed immigrants',
              'Immigrant, less than 10 years in Canada',
              'Immigrant, 10 or more years in Canada',
              'Born in Canada'
            ),
          ),
        ),
        #### Age ----
        conditionalPanel(
          condition = "input.basicCharacteristics2 == 'Age'",
          selectizeInput(
            "basicCharSpecAge2",
            label = "Age",
            choices = list(
              'Total, 12 years and over',
              '12 to 17 years',
              '18 to 64 years',
              '65 years and over'
            ),
            selected = NULL,
            multiple =  TRUE
          ),
        ),
        #### Gender ----
        conditionalPanel(
          condition = "input.basicCharacteristics2 == 'Gender'",
          selectizeInput(
            "basicCharSpecSex2",
            label = "Gender",
            choices = list('Total, by gender of person', 'Men', 'Women'),
          ),
        ),
        selectizeInput(
          "VM215",
          label = "Visible minority status",
          choices = list(
            "Total, by visible minority group",
            "Total - Visible minority",
            "South Asian",
            "Chinese",
            "Black",
            "Filipino",
            "Latin American",
            "Arab",
            "Southeast Asian",
            "Not a visible minority"
          ),
          selected = list("Latin American"),
        ),
        selectizeInput(
          "basicYear2",
          label = "Year",
          choices = unique(basicDT$'Year')
        ),
        selectizeInput(
          "basicGeo2",
          label = "Geography",
          choices = unique(basicDT$'Geography')
        ),
        selectizeInput(
          "basicConfidence2",
          label = "Confidence Interval",
          choices = unique(basicDT$'Confidence')
        ),
      ),
      ### Local community ----
      conditionalPanel(
        condition = "input.dim2 == 'Local community'",
        selectizeInput(
          "dimCommunity2",
          label = "Indicators",
          choices = list(
            "Percent of the population satisfied with feeling part of their community",
            "Percent of the population satisfied with their neighbourhood",
            "Percent of the population satisfied with quality of local environment",
            "Percent of the population reporting feeling safe in their neighbourhood",
            "Percent of the population satisfied with personal safety from crime",
            "Violent victimization rate per 1,000 population",
            "Percent of the population reporting incident against them was a hate crime",
            "Percent of police reported hate crimes motivated by Race or ethnicity",
            "Percent of police reported hate crimes motivated by religion",
            "Percent of the population perceiving local police good in enforcing laws",
            "Percent of the population perceiving local police good in responding to calls",
            "Percent of the population perceiving local police good in being approachable",
            "Percent of the population perceiving local police good in supplying information",
            "Percent of the population perceiving local police good in ensuring safety in the area",
            "Percent of the population perceiving local police as treating people fairly"
          )
        )
      ),
      ### Health and wellbeing ----
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing'",
        selectizeInput(
          "dimHealth2",
          label = "Indicators",
          choices = list(
            "Percent of the population reporting very good or excellent general health",
            "Percent of the population reporting fair or poor general health",
            "Percent of the population reporting very good or excellent mental health",
            "Percent of the population reporting fair or poor mental health",
            "Percent of the population reporting their life stressful",
            "Percent of the population reporting life satisfaction, satisfied or very satisfied",
            "Percent of the population reporting having a regular healthcare provider",
            "Percent of the population reporting no need for mental health care",
            "Percent of the population reporting all needs met for mental health care",
            "Percent of the population reporting needs partially met for mental health care",
            "Percent of the population reporting needs partially met or needs not met for mental health care",
            "Percent of the population reporting needs not met for mental health care",
            "Percent of the population reporting unmet health care needs"
          ),
        ),
        selectizeInput(
          "healthCharacteristics2",
          label = "Selected sociodemographic characteristics",
          choices = list('Immigration Status', 'Age', 'Gender'),
        ),
        ### Health characteristics ----
        conditionalPanel(
          condition = "input.healthCharacteristics2",
          selectizeInput(
            "healthCharSpecImm2",
            label = "Immigration Status",
            choices = list(
              'Total, by immigration status',
              'Landed immigrants',
              'Immigrant, less than 10 years in Canada',
              'Immigrant, 10 or more years in Canada',
              'Born in Canada'
            ),
            multiple = TRUE
          ),
        ),
        #### Age ----
        conditionalPanel(
          condition = "input.healthCharacteristics2 == 'Age'",
          selectizeInput(
            "healthCharSpecAge2",
            label = "Age",
            choices = list(
              'Total, 12 years and over',
              '12 to 17 years',
              '18 to 64 years',
              '65 years and over'
            ),
            multiple = TRUE
          ),
        ),
        #### Gender ----
        conditionalPanel(
          condition = "input.healthCharacteristics2 == 'Gender'",
          selectizeInput(
            "healthCharSpecSex2",
            label = "Gender",
            choices = list('Total, by gender of person', 'Men', 'Women'),
          ),
        ),
        selectizeInput(
          "VM220",
          label = "Visible minority status",
          choices = list(
            "Total, by visible minority group",
            "Total - Visible minority",
            "South Asian",
            "Chinese",
            "Black",
            "Filipino",
            "Latin American",
            "Arab",
            "Southeast Asian",
            "Not a visible minority"
          ),
          selected = list("Latin American"),
        ),
        selectizeInput(
          "healthYear2",
          label = "Year",
          choices = unique(healthDT$'Year')
        ),
        selectizeInput(
          "healthGeo2",
          label = "Geography",
          choices = unique(healthDT$'Geography')
        ),
        selectizeInput(
          "healthConfidence2",
          label = "Confidence Interval",
          choices = unique(healthDT$'Confidence')
        ),
      ),
      ### Public services and institutions ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions'",
        selectizeInput(
          "dimTrust2",
          label = "Indicators",
          choices = list(
            "Population expressing confidence in Federal Parliament",
            "Population expressing Confidence in the Canadian media",
            "Population expressing confidence in the school system",
            "Population expressing Confidence in the justice system and courts",
            "Population expressing confidence in the police",
            "Population expressing confidence in major corporations",
            "Population expressing Confidence in merchants and local business people",
            "Population expressing confidence in banks"
          ),
        ),
        selectizeInput(
          "confCharacteristicsIM",
          label = "Selected sociodemographic characteristics",
          choices = list(
            'Immigration Status',
            'Generation Status',
            'Age',
            'Gender',
            'Language Spoken',
            'Education Status'
          ),
        ),
        #### Immigration Status ---- 
        conditionalPanel(
          condition = "input.confCharacteristicsIM == 'Immigration Status'",
          selectizeInput(
            "confCharSpecImmIM",
            label = "Immigration Status",
            choices = list('Immigrants', 'Non-Immigrants'),
          ),
        ),
        #### Generation Status ---- 
        conditionalPanel(
          condition = "input.confCharacteristicsIM == 'Generation Status'",
          selectizeInput(
            "confCharSpecGenIM",
            label = "Immigration Status",
            choices = list(
              'First generation',
              'Second generation',
              'Third generation or more'
            ),
          ),
        ),
        #### Age ----
        conditionalPanel(
          condition = "input.confCharacteristicsIM == 'Age'",
          selectizeInput(
            "confCharSpecAgeIM",
            label = "Age",
            choices = list(
              'Total, 15 years and over',
              '15 to 24 years',
              '25 to 64 years',
              '65 years and over'
            ),
          ),
        ),
        #### Gender ----
        conditionalPanel(
          condition = "input.confCharacteristicsIM == 'Gender'",
          selectizeInput(
            "confCharSpecGenderIM",
            label = "Gender",
            choices = list('Men', 'Women'),
          ),
        ),
        #### Language Spoken ----
        conditionalPanel(
          condition = "input.confCharacteristicsIM == 'Language Spoken'",
          selectizeInput(
            "confCharSpecLangIM",
            label = "Language Spoken",
            choices = list(
              'First official language spoken, English only',
              'First official language spoken, French only'
            ),
          ),
        ),
        #### Education Status ----
        conditionalPanel(
          condition = "input.confCharacteristicsIM == 'Education Status'",
          selectizeInput(
            "confCharSpecEduIM",
            label = "Language Spoken",
            choices = list(
              'Secondary (high) school diploma or equivalency certificate or less',
              'Postsecondary certificate or diploma (non-university)',
              'University certificate or diploma'
            ),
          ),
        ),
        selectizeInput(
          "confYearIM",
          label = "Year",
          choices = unique(confidenceDT$'Year')
        ),
        selectizeInput(
          "confGeoIM",
          label = "Geography",
          choices = unique(confidenceDT$'Geography')
        ),
        selectizeInput(
          "VM205",
          label = "Visible minority status",
          choices = list(
            "Total, by visible minority group",
            "Total - Visible minority",
            "South Asian",
            "Chinese",
            "Black",
            "Filipino",
            "Latin American",
            "Arab",
            "Southeast Asian",
            "Not a visible minority"
          ),
          selected = list("Chinese"),
          multiple = TRUE
        ),
        selectizeInput(
          "confConfidenceIM",
          label = "Confidence Interval",
          choices = unique(confidenceDT$'Confidence')
        ),
      ),
      ### Income and wealth Tab Immigration ----
      conditionalPanel(
        condition = "input.dim2 == 'Income and wealth'",
        selectizeInput(
          "dimIncome2",
          label = "Indicators",
          choices = list(
            "Average total household income, adjusted for the number of persons",
            "Percent of the population living in poverty (low-income MBM)",
            "Percent of the population living in low income situation (before-tax)",
            "Percent of the population living in low income situation (after-tax)",
            "Percent of the population reporting difficulty in meeting financial needs of their household",
            "Percent of the population reporting ease in meeting financial needs of their household"
          )
        )
      ),
      #### Social connections and personnal networks ----
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks'",
        selectizeInput(
          "dimSocial2",
          label = "Indicators",
          choices = list(
            "Population reporting that most people can be trusted",
            "Population reporting strong sense of belonging to their local community",
            "Population reporting strong sense of belonging to their town or city",
            "Population reporting strong sense of belonging to their province",
            "Population reporting strong sense of belonging to Canada",
            "Percent of the population living alone",
            "Median size of a personal local network with close ties",
            "Average size of a local personal network with close ties",
            "Percent of the population with a personal close-ties network of 10 or more people",
            "Percent of the population with a personal close-ties network of 5 or more relatives",
            "Percent of the population with a personal close-ties network of 5 or more friends",
            "Percent of the population with no personal network with weak ties",
            "Percent of the population with a personal weak-ties network of 1 to 19 people",
            "Percent of the population with a personal weak-ties network of 20 or more people ",
            "Percent of the population with a personal ethnically-diverse network"
          ),
        ),
        selectizeInput(
          "confCharacteristics2",
          label = "Selected sociodemographic characteristics",
          choices = list(
            'Immigration Status',
            'Generation Status',
            'Age',
            'Gender',
            'Language Spoken',
            'Education Status'
          ),
        ),
        #### Immigration Status ----
        conditionalPanel(
          condition = "input.confCharacteristics2 == 'Immigration Status'",
          selectizeInput(
            "confCharSpecImm2",
            label = "Immigration Status",
            choices = list('Immigrants', 'Non-Immigrants'),
            multiple = TRUE
          ),
        ),
        #### Generation Status ----
        conditionalPanel(
          condition = "input.confCharacteristics2 == 'Generation Status'",
          selectizeInput(
            "confCharSpecGen2",
            label = "Immigration Status",
            choices = list(
              'First generation',
              'Second generation',
              'Third generation or more'
            ),
            multiple = TRUE
          ),
        ),
        #### Age ----
        conditionalPanel(
          condition = "input.confCharacteristics2 == 'Age'",
          selectizeInput(
            "confCharSpecAge2",
            label = "Age",
            choices = list(
              'Total, 15 years and over',
              '15 to 24 years',
              '25 to 64 years',
              '65 years and over'
            ),
          ),
        ),
        #### Gender ----
        conditionalPanel(
          condition = "input.confCharacteristics2 == 'Gender'",
          selectizeInput(
            "confCharSpecSex2",
            label = "Gender",
            choices = list('Men', 'Women'),
          ),
        ),
        #### Language Spoken ----
        conditionalPanel(
          condition = "input.confCharacteristics2 == 'Language Spoken'",
          selectizeInput(
            "confCharSpecLang2",
            label = "Language Spoken",
            choices = list(
              'First official language spoken, English only',
              'First official language spoken, French only'
            ),
          ),
        ),
        #### Education Status ----
        conditionalPanel(
          condition = "input.confCharacteristics2 == 'Education Status'",
          selectizeInput(
            "confCharSpecEdu2",
            label = "Language Spoken",
            choices = list(
              'Secondary (high) school diploma or equivalency certificate or less',
              'Postsecondary certicate or diploma (non-university)',
              'University certificate or diploma'
            ),
          ),
        ),
        selectizeInput(
          "confYearIM",
          label = "Year",
          choices = unique(confidenceDT$'Year')
        ),
        selectizeInput(
          "confGeoIM",
          label = "Geography",
          choices = unique(confidenceDT$'Geography')
        ),
        selectizeInput(
          "VM225",
          label = "Visible minority status",
          choices = list(
            "Total, by visible minority group",
            "Total - Visible minority",
            "South Asian",
            "Chinese",
            "Black",
            "Filipino",
            "Latin American",
            "Arab",
            "Southeast Asian",
            "Not a visible minority"
          ),
          selected = list("Black"),
        ),
        selectizeInput(
          "confConfidence2",
          label = "Confidence Interval",
          choices = unique(confidenceDT$'Confidence')
        ),
      ),
      ### Discrimination and victimization ----
      condition = "input.dim2 == 'Discrimination and victimization'",
      selectizeInput(
        "disind2",
        label = "Discrimination and victimization Indicators",
        choices = list(
          "Experience(s) of discrimination",
          "Experience(s) of discrimination based on ethnicity or culture",
          "Experience(s) of discrimination based on race or colour",
          "Experience(s) of discrimination based on religion",
          "Experience(s) of discrimination based on language",
          "Discrimination at work or when applying for a job or promotion",
          "Discrimination when dealing with the police",
          "Discrimination when in a store, bank or restaurant",
          "Discrimination when attending school or classes",
          "Hate Crime"
        ),
      ),
    ),
    
    ## Tab 3 main panel ----
    mainPanel(
      h2("Immigration Status"),
      ### Public services and institutions ----
      #### Population expressing confidence in banks ----
      ##### Immigration Status ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      #### Age ----
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      #### Age ----
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      #### Age ----
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      #### Age ----
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ##### Age ----
      conditionalPanel(
        condition = "input.dim2 == 'Public services and institutions' & input.dimTrust2 == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ### Social connections and personnal networks ----
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics2 == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics2 == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics2 == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics2 == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics2 == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics2 == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics2 == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics2 == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics2 == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics2 == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics2 == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics2 == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics2 == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics2 == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics2 == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics2 == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics2 == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics2 == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics2 == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics2 == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics2 == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics2 == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics2 == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics2 == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting that most people can be trusted' & input.confCharacteristics2 == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Conf8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting that most people can be trusted' & input.confCharacteristics2 == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfAge8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting that most people can be trusted' & input.confCharacteristics2 == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfSex8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting that most people can be trusted' & input.confCharacteristics2 == 'Generation Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfGen8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting that most people can be trusted' & input.confCharacteristics2 == 'Language Spoken'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfLang8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Social connections and personnal networks' & input.dimSocial2 == 'Population reporting that most people can be trusted' & input.confCharacteristics2 == 'Education Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2ConfEdu8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
        )
      ),
      ### Representation in decision-making positions ----
      conditionalPanel(
        condition = "input.dim2 == 'Representation in decision-making positions' & input.Rep2 == 'Percent of workers in middle management occupations'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Rep3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Representation in decision-making positions' & input.Rep2 == 'Percent of workers in senior management occupations'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Rep2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Representation in decision-making positions' & input.Rep2 == 'Percent of workers in all management occupations'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Rep1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      ### Basic Needs and Housing ----
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with severe food insecurity' & input.basicCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Basic5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with severe food insecurity' & input.basicCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicAge5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with severe food insecurity' & input.basicCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicSex5",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with moderate food insecurity' & input.basicCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Basic4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with moderate food insecurity' & input.basicCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicAge4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with moderate food insecurity' & input.basicCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicSex4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a food-insecure household, moderate or severe' & input.basicCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Basic3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a food-insecure household, moderate or severe' & input.basicCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicAge3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a food-insecure household, moderate or severe' & input.basicCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicSex3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with marginal food security' & input.basicCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Basic2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with marginal food security' & input.basicCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicAge2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a household with marginal food security' & input.basicCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicSex2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a food-secure household' & input.basicCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Basic1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a food-secure household' & input.basicCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicAge1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Basic needs and housing' & input.dimBasicNeeds2 == 'Percent of the population living in a food-secure household' & input.basicCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2BasicSex1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      # Visuals related to Health and well being - Immigration Status ----
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs partially met for mental health care' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health14",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs partially met for mental health care' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge14",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs partially met for mental health care' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex14",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting unmet health care needs' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health13",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting unmet health care needs' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge13",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting unmet health care needs' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex13",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs not met for mental health care' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs not met for mental health care' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs not met for mental health care' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex12",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs partially met or needs not met for mental health care' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs partially met or needs not met for mental health care' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting needs partially met or needs not met for mental health care' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex11",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting all needs met for mental health care' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting all needs met for mental health care' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting all needs met for mental health care' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex10",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting no need for mental health care' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting no need for mental health care' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting no need for mental health care' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex9",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting having a regular healthcare provider' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting having a regular healthcare provider' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting having a regular healthcare provider' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex8",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting life satisfaction, satisfied or very satisfied' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting life satisfaction, satisfied or very satisfied' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting life satisfaction, satisfied or very satisfied' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex7",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting their life stressful' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting their life stressful' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting their life stressful' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex6",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting fair or poor mental health' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting fair or poor mental health' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting fair or poor mental health' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting very good or excellent mental health' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting very good or excellent mental health' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting very good or excellent mental health' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting fair or poor general health' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting fair or poor general health' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting fair or poor general health' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting very good or excellent general health' & input.healthCharacteristics == 'Immigration Status'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Health1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting very good or excellent general health' & input.healthCharacteristics == 'Age'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthAge1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Health and wellbeing' & input.dimHealth2 == 'Percent of the population reporting very good or excellent general health' & input.healthCharacteristics == 'Gender'",
        br(),
        br(),
        plotlyOutput(
          "sBar2HealthSex1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Canadian Community Health Survey (CCHS), September to December 2020"
        )
      ),
      ### Participation in the Labour Market ----
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Youth not in employment, education or training (NEET)'",
        br(),
        br(),
        plotlyOutput(
          "sBarYouth2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Paid employees having disability insurance in their current job'",
        br(),
        br(),
        plotlyOutput(
          "employment2Plot1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText("Note: Source.")
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Workers working mainly full-time weeks in the previous year'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Rate4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Working-age population in unemployment (unemployment rate)'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Rate3",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Working-age population in employment (employment rate)'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Rate2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Working-age population in the labour force (participation rate)'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Rate1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Average weekly wage of paid employees'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Inc2",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Censuses of population, 2006 and 2016; National Household Survey, 2011"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Average employment income of the population'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Inc1",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Censuses of population, 2006 and 2016; National Household Survey, 2011"
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Self-employed workers in the labour force (unincorporated)'",
        br(),
        br(),
        plotlyOutput(
          "sBar2Rep4",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Education, training and skills'",
        conditionalPanel(
          condition = "input.Edu == 'Population with bachelor’s degree'",
          br(),
          br(),
          plotlyOutput(
            "sBarEdu",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        conditionalPanel(
          condition = "input.Edu == 'Population with no certificate, diploma or degree'",
          br(),
          br(),
          plotlyOutput(
            "sBarEdu1",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        conditionalPanel(
          condition = "input.Edu == 'Population with high school diploma or equivalency certificate'",
          br(),
          br(),
          plotlyOutput(
            "sBarEdu2",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        conditionalPanel(
          condition = "input.Edu == 'Population with postsecondary certificate or diploma below bachelor level'",
          br(),
          br(),
          plotlyOutput(
            "sBarEdu3",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        conditionalPanel(
          condition = "input.Edu == 'Population with bachelor’s degree or above'",
          br(),
          br(),
          plotlyOutput(
            "sBarEdu4",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        conditionalPanel(
          condition = "input.Edu == 'Population with master’s degree or earned doctorate'",
          br(),
          br(),
          plotlyOutput(
            "sBarEdu5",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          br(),
          helpText(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        )
      ),
      conditionalPanel(
        condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Overqualified workers with a university degree'",
        br(),
        br(),
        plotlyOutput(
          "sBarOverIS",
          inline = TRUE,
          width = 700,
          height = 500
        ),
        br(),
        helpText(
          "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011."
        )
      )
    )
  ),
  
  ## 4. Tab for ... ----
  # Bar graphs by Sex Tab 
  tabPanel("Gender", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               selectizeInput(
                 "dim3",
                 label = "Theme",
                 choices = list(
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
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Participation in the Labour Market'",
                 selectizeInput(
                   "LM3",
                   label = "Indicators",
                   choices = list(
                     "Working-age population in the labour force (participation rate)",
                     "Working-age population in employment (employment rate)",
                     "Working-age population in unemployment (unemployment rate)",
                     "Workers working mainly full-time weeks in the previous year",
                     "Self-employed workers in the labour force (unincorporated)",
                     "Overqualified workers with a university degree",
                     "Youth not in employment, education or training (NEET)",
                     "Average employment income of the population",
                     "Average weekly wage of paid employees",
                     "Currently employed population considering their job related to their education",
                     "Paid employees considering their current job good for career advancement",
                     "Paid employees receiving at least one employment benefit in their current job",
                     "Paid employees having pension plan in their current job",
                     "Paid employees having paid sick leave in their current job",
                     "Paid employees having paid vacation leave in their current job",
                     "Paid employees having disability insurance in their current job",
                     "Paid employees having supplemental medical care in their current job",
                     "Paid employees having worker's compensation in their current job",
                     "Paid employees having maternity, paternity or lay-off benefits in their current job",
                     "Paid employees covered by union contract or collective agreement in their current job",
                     "Paid employees receiving formal training in their current job",
                     "Paid employees receiving informal training in their current job"
                   ),
                   selected = NULL
                 ),
                 conditionalPanel(
                   condition = "input.LM3 == 'Overqualified workers with a university degree'",
                   selectizeInput(
                     "VM22",
                     label = "Sex",
                     choices = unique(OverQualDT$Sex),
                     selected = list("Male"),
                     multiple = TRUE
                   ),
                   selectizeInput(
                     "OverLocationSX",
                     label = "Location of Study",
                     choices = unique(OverQualDT$Location),
                   ),
                   selectizeInput(
                     "OverDegreeSX",
                     label = "Highest certificate, diploma or degree",
                     choices = unique(OverQualDT$Degree),
                   ),
                   selectizeInput(
                     "OverGeoSX",
                     label = "Geography",
                     choices = unique(OverQualDT$Geography),
                     selected = "Canada"
                   ),
                   selectizeInput(
                     "OverVMSX",
                     label = "Visible minority status",
                     choices = list(
                       "Total - Visible minority",
                       "Visible minority population",
                       "South Asian",
                       "Chinese",
                       "Black",
                       "Filipino",
                       "Latin American",
                       "Arab",
                       "Southeast Asian",
                       "West Asian",
                       "Korean",
                       "Japanese",
                       "Visible minority n.i.e",
                       "Multiple visible minorities",
                       "Not a visible minority"
                     ),
                   ),
                   selectizeInput(
                     "OverYearSX",
                     label = "Year",
                     choices = unique(OverQualDT$Year),
                   ),
                   selectizeInput(
                     "OverAgeSX",
                     label = "Age Group",
                     choices = unique(OverQualDT$Age),
                     selected = "Total - Age"
                   ),
                   selectizeInput(
                     "OverImmSX",
                     label = "Groups designated by Immigration and Generational Status",
                     choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
                   ),
                   selectizeInput(
                     "OverLangSX",
                     label = "Language",
                     choices = unique(OverQualDT$'Language'),
                   )
                 ),
               ),
               conditionalPanel(condition = "input.dim3 == 'Civic engagement and political participation'", ),
               # Widgets for Immigration - Representation in decision-making positions ----
               conditionalPanel(
                 condition = "input.dim3 == 'Representation in decision-making positions'",
                 selectizeInput(
                   "Rep3",
                   label = "Indicators",
                   choices = list(
                     "Percent of workers in all management occupations",
                     "Percent of workers in senior management occupations",
                     "Percent of workers in specialized middle management occupations",
                     "Percent of workers in other middle management occupations"
                   )
                 ),
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Basic needs and housing'",
                 selectizeInput(
                   "dimBasicNeeds3",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population living in a dwelling owned by one member of the household ",
                     "Percent of the population living in core need household",
                     "Percent of the population living in suitable housing",
                     "Percent of the population living in an affordable housing",
                     "Percent of the population living in a food-secure household",
                     "Percent of the population living in a household with marginal food security",
                     "Percent of the population living in a food-insecure household, moderate or severe",
                     "Percent of the population living in a household with moderate food insecurity",
                     "Percent of the population living in a household with severe food insecurity"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Local community'",
                 selectizeInput(
                   "dimCommunity3",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population satisfied with feeling part of their community",
                     "Percent of the population satisfied with their neighbourhood",
                     "Percent of the population satisfied with quality of local environment",
                     "Percent of the population reporting feeling safe in their neighbourhood",
                     "Percent of the population satisfied with personal safety from crime",
                     "Violent victimization rate per 1,000 population",
                     "Percent of the population reporting incident against them was a hate crime",
                     "Percent of police reported hate crimes motivated by Race or ethnicity",
                     "Percent of police reported hate crimes motivated by religion",
                     "Percent of the population perceiving local police good in enforcing laws",
                     "Percent of the population perceiving local police good in responding to calls",
                     "Percent of the population perceiving local police good in being approachable",
                     "Percent of the population perceiving local police good in supplying information",
                     "Percent of the population perceiving local police good in ensuring safety in the area",
                     "Percent of the population perceiving local police as treating people fairly"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Health and wellbeing'",
                 selectizeInput(
                   "dimHealth3",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population reporting very good or excellent general health",
                     "Percent of the population reporting fair or poor general health",
                     "Percent of the population reporting very good or excellent mental health",
                     "Percent of the population reporting fair or poor mental health",
                     "Percent of the population reporting their life stressful",
                     "Percent of the population satisfied with life as a whole",
                     "Percent of the population predicting their life opportunities will improve in the next 5 years"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Public services and institutions'",
                 selectizeInput(
                   "dimTrust3",
                   label = "Indicators",
                   choices = list(
                     "Population expressing confidence in Federal Parliament",
                     "Population expressing Confidence in the Canadian media",
                     "Population expressing confidence in the school system",
                     "Population expressing Confidence in the justice system and courts",
                     "Population expressing confidence in the police",
                     "Population expressing confidence in major corporations",
                     "Population expressing Confidence in merchants and local business people",
                     "Population expressing confidence in banks"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Income and wealth'",
                 selectizeInput(
                   "dimIncome3",
                   label = "Indicators",
                   choices = list(
                     "Average total household income, adjusted for the number of persons",
                     "Percent of the population living in poverty (low-income MBM)",
                     "Percent of the population living in low income situation (before-tax)",
                     "Percent of the population living in low income situation (after-tax)",
                     "Percent of the population reporting difficulty in meeting financial needs of their household",
                     "Percent of the population reporting ease in meeting financial needs of their household"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Social connections and personnal networks'",
                 selectizeInput(
                   "dimSocial3",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population living alone",
                     "Median size of a personal local network with close ties",
                     "Average size of a local personal network with close ties",
                     "Percent of the population with a personal close-ties network of 10 or more people",
                     "Percent of the population with a personal close-ties network of 5 or more relatives",
                     "Percent of the population with a personal close-ties network of 5 or more friends",
                     "Percent of the population with no personal network with weak ties",
                     "Percent of the population with a personal weak-ties network of 1 to 19 people",
                     "Percent of the population with a personal weak-ties network of 20 or more people ",
                     "Percent of the population with a personal ethnically-diverse network"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Discrimination and victimization'",
                 selectizeInput(
                   "disind3",
                   label = "Discrimination and victimization Indicators",
                   choices = list(
                     "Experience(s) of discrimination",
                     "Experience(s) of discrimination based on ethnicity or culture",
                     "Experience(s) of discrimination based on race or colour",
                     "Experience(s) of discrimination based on religion",
                     "Experience(s) of discrimination based on language",
                     "Discrimination at work or when applying for a job or promotion",
                     "Discrimination when dealing with the police",
                     "Discrimination when in a store, bank or restaurant",
                     "Discrimination when attending school or classes",
                     "Hate Crime"
                   )
                 ),
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Education, training and skills'",
                 # These are the drop down menus ----
                 selectizeInput(
                   "Edu3",
                   label = "Education, training and skills Indicators",
                   choices = list(
                     "Population with no certificate, diploma or degree",
                     "Population with high school diploma or equivalency certificate",
                     "Population with postsecondary certificate or diploma below bachelor level",
                     "Population with bachelor’s degree or above",
                     "Population with bachelor’s degree",
                     "Population with master’s degree or earned doctorate",
                     "Knowledge of official languages, English only",
                     "Knowledge of official languages, French only",
                     "Knowledge of official languages, English and French",
                     "Knowledge of official languages, neither English nor French",
                     "Received a formal training paid by the employer in the past 12 months",
                     "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"
                   ),
                   selected = NULL
                 ),
                 conditionalPanel(
                   condition = "input.dim3 == 'Education, training and skills' && input.Edu3 == 'Population with bachelor’s degree' || 'Population with no certificate, diploma or degree' || 'Population with high school diploma or equivalency certificate' || 'Population with postsecondary certificate or diploma below bachelor level' || 'Population with bachelor’s degree or above' || 'Population with master’s degree or earned doctorate' ",
                   selectizeInput(
                     "VM11",
                     label = "Sex",
                     choices = unique(educationDT$Sex),
                     selected = list(),
                     multiple = TRUE
                   ),
                   selectizeInput(
                     "eduGeo3",
                     label = "Geography",
                     choices = unique(educationDT$Geography),
                     selected = "Canada"
                   ),
                   selectizeInput(
                     "eduVisMin3",
                     label = "Visible Minority Group",
                     choices = unique(educationDT$VisMin),
                     selected = "Canada"
                   ),
                   selectizeInput(
                     "eduYear3",
                     label = "Year",
                     choices = unique(educationDT$Year),
                   ),
                   selectizeInput(
                     "eduAge3",
                     label = "Age Group",
                     choices = unique(educationDT$Age),
                     selected = "Total - Age"
                   ),
                   selectizeInput(
                     "eduImm3",
                     label = "Groups designated by Immigration and Generational Status",
                     choices = sort(unique(educationDT$'Immigration'), decreasing = TRUE),
                   ),
                   selectizeInput(
                     "eduLang3",
                     label = "Language",
                     choices = unique(educationDT$'Language'),
                   )
                 ),
               ),
               # Add the js code and button to the page ----
               #extendShinyjs(text = jsResetCode, functions = "reset"),
               actionButton("reset_button", "Reset Page"),
             ),
             
             # Tab ----
             mainPanel(
               h2("Gender"),
               # Visuals related to Labour Market ----
               conditionalPanel(
                 condition = "input.dim3 == 'Education, training and skills'",
                 conditionalPanel(
                   condition = "input.Edu3 == 'Population with bachelor’s degree'",
                   br(),
                   br(),
                   plotlyOutput(
                     "sBarEduSX",
                     inline = TRUE,
                     width = 700,
                     height = 500
                   ),
                   br(),
                   helpText(
                     "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.Edu3 == 'Population with no certificate, diploma or degree'",
                   br(),
                   br(),
                   plotlyOutput(
                     "sBarEduSX1",
                     inline = TRUE,
                     width = 700,
                     height = 500
                   ),
                   br(),
                   helpText(
                     "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.Edu3 == 'Population with high school diploma or equivalency certificate'",
                   br(),
                   br(),
                   plotlyOutput(
                     "sBarEduSX2",
                     inline = TRUE,
                     width = 700,
                     height = 500
                   ),
                   br(),
                   helpText(
                     "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.Edu3 == 'Population with postsecondary certificate or diploma below bachelor level'",
                   br(),
                   br(),
                   plotlyOutput(
                     "sBarEduSX3",
                     inline = TRUE,
                     width = 700,
                     height = 500
                   ),
                   br(),
                   helpText(
                     "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.Edu3 == 'Population with bachelor’s degree or above'",
                   br(),
                   br(),
                   plotlyOutput(
                     "sBarEduSX4",
                     inline = TRUE,
                     width = 700,
                     height = 500
                   ),
                   br(),
                   helpText(
                     "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
                   )
                 ),
                 conditionalPanel(
                   condition = "input.Edu3 == 'Population with master’s degree or earned doctorate'",
                   br(),
                   br(),
                   plotlyOutput(
                     "sBarEduSX5",
                     inline = TRUE,
                     width = 700,
                     height = 500
                   ),
                   br(),
                   helpText(
                     "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim3 == 'Participation in the Labour Market' & input.LM3 == 'Overqualified workers with a university degree'",
                 br(),
                 br(),
                 plotlyOutput(
                   "sBarOverSX",
                   inline = TRUE,
                   width = 700,
                   height = 500
                 ),
                 br(),
                 helpText(
                   "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011."
                 )
               )
             )
           )
  ),
  
  ## 5. Tab for Geography ----
  tabPanel("Geography", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               selectizeInput(
                 "dim5",
                 label = "Theme",
                 choices = list(
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
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Participation in the Labour Market'",
                 selectizeInput(
                   "LM5",
                   label = "Indicators",
                   choices = list(
                     "Working-age population in the labour force (participation rate)",
                     "Working-age population in employment (employment rate)",
                     "Working-age population in unemployment (unemployment rate)",
                     "Workers working mainly full-time weeks in the previous year",
                     "Self-employed workers in the labour force (unincorporated)",
                     "Overqualified workers with a university degree",
                     "Youth not in employment, education or training (NEET)",
                     "Average employment income of the population",
                     "Average weekly wage of paid employees",
                     "Currently employed population considering their job related to their education",
                     "Paid employees considering their current job good for career advancement",
                     "Paid employees receiving at least one employment benefit in their current job",
                     "Paid employees having pension plan in their current job",
                     "Paid employees having paid sick leave in their current job",
                     "Paid employees having paid vacation leave in their current job",
                     "Paid employees having disability insurance in their current job",
                     "Paid employees having supplemental medical care in their current job",
                     "Paid employees having worker's compensation in their current job",
                     "Paid employees having maternity, paternity or lay-off benefits in their current job",
                     "Paid employees covered by union contract or collective agreement in their current job",
                     "Paid employees receiving formal training in their current job",
                     "Paid employees receiving informal training in their current job"
                   ),
                   selected = NULL
                 ),
                 selectizeInput(
                   "VM23",
                   label = "Geography",
                   choices = unique(OverQualDT$Geography),
                   selected = list("Male"),
                   multiple = TRUE
                 ),
                 selectizeInput(
                   "OverLocationGEO",
                   label = "Location of Study",
                   choices = unique(OverQualDT$Location),
                 ),
                 selectizeInput(
                   "OverDegreeGEO",
                   label = "Highest certificate, diploma or degree",
                   choices = unique(OverQualDT$Degree),
                 ),
                 selectizeInput(
                   "OverSexGEO",
                   label = "Sex",
                   choices = unique(OverQualDT$Sex),
                   selected = "Males"
                 ),
                 selectizeInput(
                   "OverVMGEO",
                   label = "Visible minority status",
                   choices = list(
                     "Total - Visible minority",
                     "Visible minority population",
                     "South Asian",
                     "Chinese",
                     "Black",
                     "Filipino",
                     "Latin American",
                     "Arab",
                     "Southeast Asian",
                     "West Asian",
                     "Korean",
                     "Japanese",
                     "Visible minority n.i.e",
                     "Multiple visible minorities",
                     "Not a visible minority"
                   ),
                 ),
                 selectizeInput(
                   "OverYearGEO",
                   label = "Year",
                   choices = unique(OverQualDT$Year),
                 ),
                 selectizeInput(
                   "OverAgeGEO",
                   label = "Age Group",
                   choices = unique(OverQualDT$Age),
                   selected = "Total - Age"
                 ),
                 selectizeInput(
                   "OverImmGEO",
                   label = "Groups designated by Immigration and Generational Status",
                   choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
                 ),
                 selectizeInput(
                   "OverLangGEO",
                   label = "Language",
                   choices = unique(OverQualDT$'Language'),
                 )
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Civic engagement and political participation'",
                 selectizeInput(
                   "dimCivilEngagement5",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population members of at least one civic group or organization",
                     "Percent of the population members in a sports or recreational organization",
                     "Percent of the population members in a cultural, educational or hobby organization",
                     "Percent of the population members in union or professional association",
                     "Percent of the population members in a political party or group",
                     "Percent of the population members in a religious-affiliated group",
                     "Percent of the population members in a school group, neighbourhood, civic or community association",
                     "Percent of the population members in a humanitarian or charitable organization or service club",
                     "Percent of the population members in a seniors' group",
                     "Percent of the population members in a youth organization",
                     "Percent of the population members in an immigrant or ethnic association or club",
                     "Percent of the population members in an environmental group ",
                     "Percent of the population engaged in political activities",
                     "Percent of the population voting in the last federal election",
                     "Percent of the population voting in the last provincial election",
                     "Percent of the population voting in the last municipal election"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Representation in decision-making positions'",
                 selectizeInput(
                   "Rep5",
                   label = "Indicators",
                   choices = list(
                     "Percent of workers in all management occupations",
                     "Percent of workers in senior management occupations",
                     "Percent of workers in specialized middle management occupations",
                     "Percent of workers in other middle management occupations"
                   )
                 ),
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Basic needs and housing'",
                 selectizeInput(
                   "dimBasicNeeds5",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population living in a dwelling owned by one member of the household ",
                     "Percent of the population living in core need household",
                     "Percent of the population living in suitable housing",
                     "Percent of the population living in an affordable housing",
                     "Percent of the population living in a food-secure household",
                     "Percent of the population living in a household with marginal food security",
                     "Percent of the population living in a food-insecure household, moderate or severe",
                     "Percent of the population living in a household with moderate food insecurity",
                     "Percent of the population living in a household with severe food insecurity"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Local community'",
                 selectizeInput(
                   "dimCommunity5",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population satisfied with feeling part of their community",
                     "Percent of the population satisfied with their neighbourhood",
                     "Percent of the population satisfied with quality of local environment",
                     "Percent of the population reporting feeling safe in their neighbourhood",
                     "Percent of the population satisfied with personal safety from crime",
                     "Violent victimization rate per 1,000 population",
                     "Percent of police reported hate crimes motivated by Race or ethnicity",
                     "Percent of police reported hate crimes motivated by religion",
                     "Percent of the population perceiving local police good in enforcing laws",
                     "Percent of the population perceiving local police good in responding to calls",
                     "Percent of the population perceiving local police good in being approachable",
                     "Percent of the population perceiving local police good in supplying information",
                     "Percent of the population perceiving local police good in ensuring safety in the area",
                     "Percent of the population perceiving local police as treating people fairly"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Education, training and skills'",
                 selectizeInput(
                   "Edu5",
                   label = "Education, training and skills Indicators",
                   choices = list(
                     "Population with no certificate, diploma or degree",
                     "Population with high school diploma or equivalency certificate",
                     "Population with postsecondary certificate or diploma below bachelor level",
                     "Population with bachelor’s degree or above",
                     "Population with bachelor’s degree",
                     "Population with master’s degree or earned doctorate",
                     "Knowledge of official languages, English only",
                     "Knowledge of official languages, French only",
                     "Knowledge of official languages, English and French",
                     "Knowledge of official languages, neither English nor French",
                     "Received a formal training paid by the employer in the past 12 months",
                     "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"
                   ),
                   selected = NULL
                 ),
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Health and wellbeing'",
                 selectizeInput(
                   "dimHealth5",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population reporting very good or excellent general health",
                     "Percent of the population reporting fair or poor general health",
                     "Percent of the population reporting very good or excellent mental health",
                     "Percent of the population reporting fair or poor mental health",
                     "Percent of the population reporting their life stressful",
                     "Percent of the population satisfied with life as a whole",
                     "Percent of the population predicting their life opportunities will improve in the next 5 years"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Public services and institutions'",
                 selectizeInput(
                   "dimTrust5",
                   label = "Indicators",
                   choices = list(
                     "Population expressing confidence in Federal Parliament",
                     "Population expressing Confidence in the Canadian media",
                     "Population expressing confidence in the school system",
                     "Population expressing Confidence in the justice system and courts",
                     "Population expressing confidence in the police",
                     "Population expressing confidence in major corporations",
                     "Population expressing Confidence in merchants and local business people",
                     "Population expressing confidence in banks"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Income and wealth'",
                 selectizeInput(
                   "dimIncome5",
                   label = "Indicators",
                   choices = list(
                     "Average total household income, adjusted for the number of persons",
                     "Percent of the population living in poverty (low-income MBM)",
                     "Percent of the population living in low income situation (before-tax)",
                     "Percent of the population living in low income situation (after-tax)",
                     "Percent of the population reporting difficulty in meeting financial needs of their household",
                     "Percent of the population reporting ease in meeting financial needs of their household"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Social connections and personnal networks'",
                 selectizeInput(
                   "dimSocial5",
                   label = "Indicators",
                   choices = list(
                     "Percent of the population living alone",
                     "Median size of a personal local network with close ties",
                     "Average size of a local personal network with close ties",
                     "Percent of the population with a personal close-ties network of 10 or more people",
                     "Percent of the population with a personal close-ties network of 5 or more relatives",
                     "Percent of the population with a personal close-ties network of 5 or more friends",
                     "Percent of the population with no personal network with weak ties",
                     "Percent of the population with a personal weak-ties network of 1 to 19 people",
                     "Percent of the population with a personal weak-ties network of 20 or more people ",
                     "Percent of the population with a personal ethnically-diverse network"
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.dim5 == 'Discrimination and victimization'",
                 selectizeInput(
                   "disind5",
                   label = "Discrimination and victimization Indicators",
                   choices = list(
                     "Experience(s) of discrimination",
                     "Experience(s) of discrimination based on ethnicity or culture",
                     "Experience(s) of discrimination based on race or colour",
                     "Experience(s) of discrimination based on religion",
                     "Experience(s) of discrimination based on language",
                     "Discrimination at work or when applying for a job or promotion",
                     "Discrimination when dealing with the police",
                     "Discrimination when in a store, bank or restaurant",
                     "Discrimination when attending school or classes",
                     "Hate Crime"
                   )
                 ),
               ),
               # Add the js code and button to the page ----
               #extendShinyjs(text = jsResetCode, functions = "reset"),
               actionButton("reset_button", "Reset Page"),
             ),
             # Main Panel for displaying graphs ----
             # FIXME: this indent doesn't look right
             mainPanel(
               conditionalPanel(
                 condition = "input.dim5 == 'Participation in the Labour Market'",
                 br(),
                 br(),
                 plotlyOutput(
                   "sBarOverGEO",
                   inline = TRUE,
                   width = 700,
                   height = 500
                 ),
                 br(),
                 helpText(
                   "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011."
                 )
               )
             )
           )),
  # Scatter plot and Line Graph Tab ----
  tabPanel(
    "Time Series Analysis",
    fluid = TRUE,
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "spdim6",
          label = "Theme",
          choices = list(
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
          ),
          selected = "Discrimination and victimization",
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Participation in the Labour Market'",
          selectizeInput(
            "LM6",
            label = "Indicators",
            choices = list(
              "Working-age population in the labour force (participation rate)",
              "Working-age population in employment (employment rate)",
              "Working-age population in unemployment (unemployment rate)",
              "Workers working mainly full-time weeks in the previous year",
              "Self-employed workers in the labour force (unincorporated)",
              "Overqualified workers with a university degree",
              "Youth not in employment, education or training (NEET)",
              "Average employment income of the population",
              "Average weekly wage of paid employees",
              "Currently employed population considering their job related to their education",
              "Paid employees considering their current job good for career advancement",
              "Paid employees receiving at least one employment benefit in their current job",
              "Paid employees having pension plan in their current job",
              "Paid employees having paid sick leave in their current job",
              "Paid employees having paid vacation leave in their current job",
              "Paid employees having disability insurance in their current job",
              "Paid employees having supplemental medical care in their current job",
              "Paid employees having worker's compensation in their current job",
              "Paid employees having maternity, paternity or lay-off benefits in their current job",
              "Paid employees covered by union contract or collective agreement in their current job",
              "Paid employees receiving formal training in their current job",
              "Paid employees receiving informal training in their current job"
            ),
            selected = NULL
          ),
          selectizeInput(
            "OverGeoLINE",
            label = "Geography",
            choices = unique(OverQualDT$Geography),
          ),
          selectizeInput(
            "OverLocationLINE",
            label = "Location of Study",
            choices = unique(OverQualDT$Location),
          ),
          selectizeInput(
            "OverDegreeLINE",
            label = "Highest certificate, diploma or degree",
            choices = unique(OverQualDT$Degree),
          ),
          selectizeInput(
            "OverSexLINE",
            label = "Sex",
            choices = unique(OverQualDT$Sex),
            selected = "Males"
          ),
          selectizeInput(
            "OverVMLINE",
            label = "Visible minority status",
            choices = list(
              "Total - Visible minority",
              "Visible minority population",
              "South Asian",
              "Chinese",
              "Black",
              "Filipino",
              "Latin American",
              "Arab",
              "Southeast Asian",
              "West Asian",
              "Korean",
              "Japanese",
              "Visible minority n.i.e",
              "Multiple visible minorities",
              "Not a visible minority"
            ),
          ),
          selectizeInput(
            "OverAgeLINE",
            label = "Age Group",
            choices = unique(OverQualDT$Age),
            selected = "Total - Age"
          ),
          selectizeInput(
            "OverImmLINE",
            label = "Groups designated by Immigration and Generational Status",
            choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
          ),
          selectizeInput(
            "OverLangLINE",
            label = "Language",
            choices = unique(OverQualDT$'Language'),
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Discrimination and victimization'",
          selectizeInput(
            "LM6",
            label = "Indicators",
            choices = list("Hate Crime"),
            selected = "Hate Crime",
          ),
          selectizeInput(
            "motivation2",
            label = "Motivation",
            choices = list('Race or ethnicity', 'Total police-reported hate crime'),
            selected = "Race or ethnicity",
          ),
          conditionalPanel(
            condition = "input.motivation2 == 'Race or ethnicity'",
            selectizeInput(
              "VisMi2",
              label = "Race or ethnicity and other characteristics",
              choices = list(
                'Black',
                'South Asian',
                'East or Southeast Asian',
                'Arab or West Asian',
                'White',
                'Indigenous',
                'Multiple races or ethnicities',
                'Other Race or ethnicity',
                'Unknown Race or ethnicity'
              ),
              selected = "Black",
              multiple = TRUE
            ),
          ),
          conditionalPanel(
            condition = "input.motivation2 == 'Total police-reported hate crime'",
            selectizeInput(
              "VisMi",
              label = "Police Reported Hate Crimes",
              choices = list(
                'Race or ethnicity',
                'Religion',
                'Sexual orientation',
                'Language',
                'Disabilitiy',
                'Sex',
                'Age',
                'Unknown motivation'
              ),
              selected = "Religion",
              multiple = TRUE
            ),
          ),
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Civic engagement and political participation'",
          selectizeInput(
            "dimCivilEngagement6",
            label = "Indicators",
            choices = list(
              "Percent of the population members of at least one civic group or organization",
              "Percent of the population members in a sports or recreational organization",
              "Percent of the population members in a cultural, educational or hobby organization",
              "Percent of the population members in union or professional association",
              "Percent of the population members in a political party or group",
              "Percent of the population members in a religious-affiliated group",
              "Percent of the population members in a school group, neighbourhood, civic or community association",
              "Percent of the population members in a humanitarian or charitable organization or service club",
              "Percent of the population members in a seniors' group",
              "Percent of the population members in a youth organization",
              "Percent of the population members in an immigrant or ethnic association or club",
              "Percent of the population members in an environmental group ",
              "Percent of the population engaged in political activities",
              "Percent of the population voting in the last federal election",
              "Percent of the population voting in the last provincial election",
              "Percent of the population voting in the last municipal election"
            )
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Representation in decision-making positions'",
          selectizeInput(
            "Rep6",
            label = "Indicators",
            choices = list(
              "Percent of workers in all management occupations",
              "Percent of workers in senior management occupations",
              "Percent of workers in specialized middle management occupations",
              "Percent of workers in other middle management occupations"
            )
          ),
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Basic needs and housing'",
          selectizeInput(
            "dimBasicNeeds6",
            label = "Indicators",
            choices = list(
              "Percent of the population living in a dwelling owned by one member of the household ",
              "Percent of the population living in core need household",
              "Percent of the population living in suitable housing",
              "Percent of the population living in an affordable housing",
              "Percent of the population living in a food-secure household",
              "Percent of the population living in a household with marginal food security",
              "Percent of the population living in a food-insecure household, moderate or severe",
              "Percent of the population living in a household with moderate food insecurity",
              "Percent of the population living in a household with severe food insecurity"
            )
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Local community'",
          selectizeInput(
            "dimCommunity4",
            label = "Indicators",
            choices = list(
              "Percent of the population satisfied with feeling part of their community",
              "Percent of the population satisfied with their neighbourhood",
              "Percent of the population satisfied with quality of local environment",
              "Percent of the population reporting feeling safe in their neighbourhood",
              "Percent of the population satisfied with personal safety from crime",
              "Violent victimization rate per 1,000 population",
              "Percent of the population reporting incident against them was a hate crime",
              "Percent of police reported hate crimes motivated by Race or ethnicity",
              "Percent of police reported hate crimes motivated by religion",
              "Percent of the population perceiving local police good in enforcing laws",
              "Percent of the population perceiving local police good in responding to calls",
              "Percent of the population perceiving local police good in being approachable",
              "Percent of the population perceiving local police good in supplying information",
              "Percent of the population perceiving local police good in ensuring safety in the area",
              "Percent of the population perceiving local police as treating people fairly"
            )
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Health and wellbeing'",
          selectizeInput(
            "dimHealth6",
            label = "Indicators",
            choices = list(
              "Percent of the population reporting very good or excellent general health",
              "Percent of the population reporting fair or poor general health",
              "Percent of the population reporting very good or excellent mental health",
              "Percent of the population reporting fair or poor mental health",
              "Percent of the population reporting their life stressful",
              "Percent of the population satisfied with life as a whole",
              "Percent of the population predicting their life opportunities will improve in the next 5 years"
            )
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Education, training and skills'",
          selectizeInput(
            "Edu6",
            label = "Education Indicators",
            choices = list(
              "Population with no certificate, diploma or degree",
              "Population with high school diploma or equivalency certificate",
              "Population with postsecondary certificate or diploma below bachelor level",
              "Population with bachelor’s degree or above",
              "Population with bachelor’s degree",
              "Population with master’s degree or earned doctorate",
              "Knowledge of official languages, English only",
              "Knowledge of official languages, French only",
              "Knowledge of official languages, English and French",
              "Knowledge of official languages, neither English nor French",
              "Received a formal training paid by the employer in the past 12 months",
              "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"
            )
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Public services and institutions'",
          selectizeInput(
            "dimTrust6",
            label = "Indicators",
            choices = list(
              "Population expressing confidence in Federal Parliament",
              "Population expressing Confidence in the Canadian media",
              "Population expressing confidence in the school system",
              "Population expressing Confidence in the justice system and courts",
              "Population expressing confidence in the police",
              "Population expressing confidence in major corporations",
              "Population expressing Confidence in merchants and local business people",
              "Population expressing confidence in banks"
            )
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Income and wealth'",
          selectizeInput(
            "dimIncome6",
            label = "Indicators",
            choices = list(
              "Average total household income, adjusted for the number of persons",
              "Percent of the population living in poverty (low-income MBM)",
              "Percent of the population living in low income situation (before-tax)",
              "Percent of the population living in low income situation (after-tax)",
              "Percent of the population reporting difficulty in meeting financial needs of their household",
              "Percent of the population reporting ease in meeting financial needs of their household"
            )
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Social connections and personnal networks'",
          selectizeInput(
            "dimSocial6",
            label = "Indicators",
            choices = list(
              "Percent of the population living alone",
              "Median size of a personal local network with close ties",
              "Average size of a local personal network with close ties",
              "Percent of the population with a personal close-ties network of 10 or more people",
              "Percent of the population with a personal close-ties network of 5 or more relatives",
              "Percent of the population with a personal close-ties network of 5 or more friends",
              "Percent of the population with no personal network with weak ties",
              "Percent of the population with a personal weak-ties network of 1 to 19 people",
              "Percent of the population with a personal weak-ties network of 20 or more people ",
              "Percent of the population with a personal ethnically-diverse network"
            )
          )
        ),
        # Add the js code and button to the page ----
        # extendShinyjs(text = jsResetCode, functions = "reset"),
        actionButton("reset_button", "Reset Page"),
      ),
      # Tab ----
      mainPanel(
        conditionalPanel(
          condition = "input.spdim6 == 'Discrimination and victimization' & input.motivation2 == 'Total police-reported hate crime'",
          h2("Time Series Analysis"),
          br(),
          br(),
          plotlyOutput(
            "ltwograph",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          p(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Discrimination and victimization' & input.motivation2 == 'Race or ethnicity'",
          h2("Time Series Analysis"),
          br(),
          br(),
          plotlyOutput(
            "lgraph",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          p(
            "Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006"
          )
        ),
        conditionalPanel(
          condition = "input.spdim6 == 'Participation in the Labour Market'",
          h2("Time Series Analysis"),
          br(),
          br(),
          plotlyOutput(
            "lthreegraph",
            inline = TRUE,
            width = 700,
            height = 500
          ),
          p("Source: ----")
        ),
      )
    )
  )
)