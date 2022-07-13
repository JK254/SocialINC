# Task: Create a data visualization for SII-IIS
# Authors: Suhayl Sayed, Jecinta Kemboi
# Date: 2022-04-13

# Notes
# Here is a tutorial for RShiny: https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/
# For examples of selectizeInput, go to https://shiny.rstudio.com/gallery/selectize-examples.html (ui.R tab)
# To create a hierarchy of buttons, use conditional panels (https://shiny.rstudio.com/reference/shiny/1.4.0/conditionalPanel.html)
# For graphing help, refer to: https://plotly.com/r/. Note that traces, values to be plotted x, have to be columns.
# For choropleth/leaflet help, refer to: https://rstudio.github.io/leaflet/json.html

#---------------------------------------------------------------------
# Install packages
#install.packages("plotly")
#install.packages("leaflet")
#install.packages("shinyjs")
#---------------------------------------------------------------------


# Load packages
library(shiny)
library(tidyverse)
library(data.table)
library(readxl)
library(plotly)
library(leaflet)
library(htmltools)
library(RColorBrewer)
library(shinyjs)

# Define the js method that resets the page
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" 


# Data loading and pre-processing------------------------------------------------

#####################################################
# Read and rename data for civic engagement data
civicDT2 <- read_csv("4310006601_databaseLoadingData.csv") %>% 
    select(c("REF_DATE", "GEO", "Visible minority", "Selected sociodemographic characteristics", 
             "Indicators", "Statistics", "VALUE"))

setnames(civicDT2, colnames(civicDT2), c("Year", "Geography", "VisMin", "Characteristic", 
                                       "Indicator", "Confidence","Value"))

#########################################################
# Read and rename data for civic engagement data
civicDT <- read_csv("4310006501_databaseLoadingData.csv") %>% 
    select(c("REF_DATE", "GEO", "Visible minority", "Selected sociodemographic characteristics", 
             "Indicators", "Statistics", "VALUE"))

setnames(civicDT, colnames(civicDT), c("Year", "Geography", "VisMin", "Characteristic", 
                                       "Indicator", "Confidence","Value"))

#############################################################
# Read and rename data for representation data
representationDT <- read_csv("4310007001_databaseLoadingData.csv") %>% 
    select(c("REF_DATE", "GEO","Sex","Age group and first officiel language spoken","Immigrant and generation status","Visible minority status","Highest certificate, diploma or degree", "Indicators","VALUE"))

setnames(representationDT, colnames(representationDT), c("Year", "Geography","Sex", "Age","Immigration", "VisMin","Degree", "Indicator", "Value"))


############################################################
# Read and rename data for youth not in employment data
youthDT <- read_csv("4310007201_databaseLoadingData.csv") %>% 
    select(c("REF_DATE", "GEO","Sex","Age group","First official language spoken","Generation status","Visible minority status", "Indicators","VALUE"))

setnames(youthDT, colnames(youthDT), c("Year", "Geography","Sex", "Age", "Language", "Immigration", "VisMin",  "Indicator", "Value"))


#############################################################
# Read and rename data for sense of belonging data
belongingDT <- read_csv("4310006401_databaseLoadingData (1).csv") %>% 
    select(c("REF_DATE", "GEO", "Visible minority", "Selected sociodemographic characteristics", "Indicators", "Statistics", "VALUE"))

setnames(belongingDT, colnames(belongingDT), c("Year", "Geography", "VisMin", "Characteristic", "Indicator", "Confidence","Value"))

############################################################
# Read and rename data for employment data
employmentDT <- read_csv("4310005901_databaseLoadingData.csv") %>% 
    select(c("REF_DATE", "GEO", "Visible minority", "Selected sociodemographic characteristics", 
             "Indicators", "Statistics", "VALUE"))

setnames(employmentDT, colnames(employmentDT), c("Year", "Geography", "VisMin", "Characteristic", "Indicator", "Confidence","Value"))


##########################################################
# Read and rename data for confidence data
confidenceDT <- read_csv("4310006201_databaseLoadingData (1).csv") %>% 
    select(c("REF_DATE", "GEO", "Visible minority", "Selected sociodemographic characteristics", 
             "Indicators", "Statistics", "VALUE"))

setnames(confidenceDT, colnames(confidenceDT), c("Year", "Geography", "VisMin", "Characteristic", "Indicator", "Confidence","Value"))


##########################################################
# Read and rename data for discrimination data
discriminationDT <- read_csv("4310006101_databaseLoadingData.csv") %>% 
    select(c("REF_DATE", "GEO", "Visible minority", "Selected sociodemographic characteristics", 
             "Indicators","Statistics","VALUE"))

setnames(discriminationDT, colnames(discriminationDT), c("Year", "Geography", "VisMin", "Characteristic", "Indicator", "Confidence","Value"))


#########################################################
# Read and rename data for Education data
educationDT <- read_csv("4310006701_databaseLoadingData (6).csv") %>% 
    select(c("REF_DATE", "GEO", "Sex", "Age group", 
             "First official language spoken", "Immigrant and generation status", "Visible minority status", "Indicators", 
             "VALUE"))

setnames(educationDT, colnames(educationDT), c("Year", "Geography", "Sex", "Age", "Language", "Immigration", "VisMin", "Indicators", "Value"))


##############################################################
# Read and rename data for over qualification data
OverQualDT <- read_csv("4310007101_databaseLoadingData.csv") %>% 
    select(c("REF_DATE", "GEO", "Sex", "Age group", 
             "First official language spoken", "Immigrant and generation status", "Visible minority status", "Location of study","Highest certificate, diploma or degree", "Indicators", 
             "VALUE"))

setnames(OverQualDT, colnames(OverQualDT), c("Year", "Geography", "Sex", "Age", "Language", "Immigration", "VisMin", "Location", "Degree", "Indicators", "Value"))


####################################################
#Load Dataset for Police-reported hate crime
polData <- read.csv("3510006601_databaseLoadingData (1).csv") %>%
    select(c("REF_DATE", "GEO",4,"VALUE"))
setnames(polData, colnames(polData), c("Year", "Geography", "Motivation", "Value"))




# Define UI --------------------------------------------------------------------
ui <- fluidPage(
    
    # Include shinyjs in the UI
    useShinyjs(),
    
    # Title of the app
    titlePanel("Social Inclusion Data Visualization Tool"),
    
    #Changes color of tabpanel from standard blue to black
    tags$style(type = "text/css", "a{color: #000000;}", style = "font-size:70px",'* {font-family: "Arial"};'),
    
    
    
    
    
    # Create the tab panel
    tabsetPanel(
        
        
        # Commented on only the first tab, but follow this layout to create more
        # Bar Graphs by VisMin Tab
        #Tab for Geography
        tabPanel("Theme & Definition of Indicators", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         
                         selectizeInput("dim4",
                                        label = "Theme",
                                        choices = list("Participation in the Labour Market",
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
                             condition = "input.dim4 == 'Participation in the Labour Market'",
                             
                             
                             selectizeInput("LM4",
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
                             
                             selectizeInput("dimCivilEngagement4",
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
                             
                             selectizeInput("Rep4",
                                            label = "Indicators",
                                            choices = list("Percent of workers in all management occupations",
                                                           
                                                           "Percent of workers in senior management occupations",
                                                           
                                                           "Percent of workers in specialized middle management occupations",
                                                           
                                                           "Percent of workers in other middle management occupations"
                                            )
                             ),
                             
                         ),
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim4 == 'Basic needs and housing'",
                             
                             selectizeInput("dimBasicNeeds4",
                                            label = "Indicators",
                                            choices = list(
                                                "Percent of the population living in a dwelling owned by one member of the household ",
                                                
                                                "Percent of the population living in core need household",
                                                
                                                "Percent of the population living in suitable housing",
                                                
                                                "Percent of the population living in an affordable housing",
                                                
                                                "Percent of the population living in a food-secure household",
                                                
                                                " Percent of the population living in a household with marginal food security",
                                                
                                                "Percent of the population living in a food-insecure household, moderate or severe",
                                                
                                                "Percent of the population living in a household with moderate food insecurity",
                                                
                                                " Percent of the population living in a household with severe food insecurity"
                                                
                                                
                                            )
                             )
                             
                         ),
                         
                         
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim4 == 'Local community'",
                             
                             selectizeInput("dimCommunity4",
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
                             
                             selectizeInput("dimHealth4",
                                            label = "Indicators",
                                            choices = list(
                                                " Percent of the population reporting very good or excellent general health",
                                                
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
                             
                             
                             selectizeInput("Edu4",
                                            label = "Education Indicators",
                                            choices = list( "Population with no certificate, diploma or degree",
                                                            
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
                             
                             selectizeInput("dimTrust4",
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
                             
                             selectizeInput("dimIncome4",
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
                             
                             selectizeInput("dimSocial4",
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
                             
                             
                             selectizeInput("disind4",
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
                         
                         
                         
                         
                         
                         
                         # Add the js code and button to the page
                         #extendShinyjs(text = jsResetCode, functions = "reset"),
                         actionButton("reset_button", "Reset Page"),
                     ),
                     
                     #Main Panel for displaying graphs
                     mainPanel(
                         conditionalPanel(
                             
                             condition = "input.dim4 == 'Participation in the Labour Market' & input.LM4 == 'Overqualified workers with a university degree'",
                             
                             br(),
                             h4("Overqualified workers with a university degree"),
                             
                             helpText("Refers to people with a bachelor’s degree or above (at bachelor's level or above) who, during the current year or the year prior the census, held a position usually requiring a high school diploma or equivalency certificate or less.")
                             
                         )
                         
                         
                     )
                 )
                 
        ),
                 
                 
        
        
        tabPanel("Groups Designated as Visible Minorities", fluid = TRUE, font = list(size = 10),
                 sidebarLayout(
                     
                     # Specify your widgets here
                     sidebarPanel(
                         
                         selectizeInput("dim",
                                        label = "Theme",
                                        choices = list("Participation in the Labour Market",
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
                         # Widgets for use with datasets with Labour Market Variables
                         conditionalPanel(
                             condition = "input.dim == 'Participation in the Labour Market'",
                             
                             
                             selectizeInput("LM",
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
                                 condition = "input.LM == 'Overqualified workers with a university degree'",
                                 
                                 
                                 
                                 
                                 selectizeInput("VM20",
                                                label = "Visible minority status",
                                                choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
		
                                                selected = list("Total - Visible minority", "Not a visible minority"),
                                                multiple = TRUE
                                 ),
                                 
                                 
                                 selectizeInput("OverLocation",
                                                label = "Location of Study",
                                                choices = unique(OverQualDT$Location),
                                                
                                                
                                 ),
                                 
                                 selectizeInput("OverDegree",
                                                label = "Highest certificate, diploma or degree",
                                                choices = unique(OverQualDT$Degree),
                                                
                                                
                                 ),
                                 
                                 
                                 selectizeInput("OverGeo",
                                                label = "Geography",
                                                choices = unique(OverQualDT$Geography),
                                                selected = "Canada"
                                                
                                 ),
                                 
                                 selectizeInput("OverImm",
                                                label = "Groups designated by Immigration and Generational Status",
                                                choices = unique(OverQualDT$'Immigration'),
                                                selected = "Canada"
                                                
                                 ),
                                 
                                 
                                 selectizeInput("OverYear",
                                                label = "Year",
                                                choices = unique(OverQualDT$Year),
                                                
                                 ),
                                 
                                 selectizeInput("OverAge",
                                                label = "Age Group",
                                                choices = unique(OverQualDT$Age),
                                                selected = "Total - Age"
                                 ),
                                 
                                 selectizeInput("OverSex",
                                                label = "Sex",
                                                choices = sort(unique(OverQualDT$Sex), decreasing = TRUE),
                                                selected = "Total - Sex"
                                 ),
                                 
                                 selectizeInput("OverLang",
                                                label = "Language",
                                                choices = unique(OverQualDT$'Language'),
                                                
                                 ),
                             ),
                             
                       
                         
                         
                         
                         
                        # Done 
                       
                        
                        conditionalPanel(
                            condition = "input.LM == 'Youth not in employment, education or training (NEET)'",
                            
                            
                            
                            
                            selectizeInput("VM140",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("YouthGeo",
                                           label = "Geography",
                                           choices = unique(youthDT$Geography),
                                           
                         
                            ),
                            
                            
                          
                            
                            selectizeInput("YouthImm",
                                           label = "Groups designated by Immigration and Generational Status",
                                           choices = unique(youthDT$Immigration),
                                           selected = "Canada"
                                           
                            ),
                            
                            
                            selectizeInput("YouthYear",
                                           label = "Year",
                                           choices = unique(youthDT$Year),
                                           
                            ),
                            
                            selectizeInput("YouthAge",
                                           label = "Age Group",
                                           choices = unique(youthDT$Age),
                                           selected = "Total - Age"
                            ),
                            
                            selectizeInput("YouthSex",
                                           label = "Sex",
                                           choices = sort(unique(youthDT$Sex), decreasing = TRUE),
                                           selected = "Total - Sex"
                            ),
                            
                            selectizeInput("YouthLang",
                                           label = "Language",
                                           choices = unique(youthDT$'Language'),
                                           
                            ),
                        ),
                        
                   
                     
                        
                        #End
                        conditionalPanel(
                            condition = "input.LM == 'Paid employees having disability insurance in their current job'",
                            
                            
                            
                            selectizeInput("VM135",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("EmploymentYear7",
                                           label = "Year",
                                           choices = unique(employmentDT$Year),
                                           
                            ),      
                            
                            selectizeInput("EmploymentGeo7",
                                           label = "Geography",
                                           choices = unique(employmentDT$Geography),
                                           selected = "Canada"
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("EmploymentChar7",
                                           label = "Characteristic",
                                           choices = unique(employmentDT$Characteristic),
                                           
                                           
                            ),
                            
                            selectizeInput("EmploymentConf7",
                                           label = "Confidence",
                                           choices = unique(employmentDT$Confidence),
                                           
                                           
                            ),
                            
                        ),
                        
                        
                        conditionalPanel(
                            condition = "input.LM == 'Currently employed population considering their job related to their education'",
                            
                            
                            
                            selectizeInput("VM130",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("EmploymentYear6",
                                           label = "Year",
                                           choices = unique(employmentDT$Year),
                                           
                            ),      
                            
                            selectizeInput("EmploymentGeo6",
                                           label = "Geography",
                                           choices = unique(employmentDT$Geography),
                                           selected = "Canada"
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("EmploymentChar6",
                                           label = "Characteristic",
                                           choices = unique(employmentDT$Characteristic),
                                           
                                           
                            ),
                            
                            selectizeInput("EmploymentConf6",
                                           label = "Confidence",
                                           choices = unique(employmentDT$Confidence),
                                           
                                           
                            ),
                            
                        ),
                        
                        
                        
                        conditionalPanel(
                            condition = "input.LM == 'Paid employees having disability insurance in their current job'",
                            
                            
                            
                            selectizeInput("VM135",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("EmploymentYear7",
                                           label = "Year",
                                           choices = unique(employmentDT$Year),
                                           
                            ),      
                            
                            selectizeInput("EmploymentGeo7",
                                           label = "Geography",
                                           choices = unique(employmentDT$Geography),
                                           selected = "Canada"
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("EmploymentChar7",
                                           label = "Characteristic",
                                           choices = unique(employmentDT$Characteristic),
                                           
                                           
                            ),
                            
                            selectizeInput("EmploymentConf7",
                                           label = "Confidence",
                                           choices = unique(employmentDT$Confidence),
                                           
                                           
                            ),
                            
                        ),
                        
                        
                        conditionalPanel(
                            condition = "input.LM == 'Currently employed population considering their job related to their education'",
                            
                            
                            
                            selectizeInput("VM130",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("EmploymentYear6",
                                           label = "Year",
                                           choices = unique(employmentDT$Year),
                                           
                            ),      
                            
                            selectizeInput("EmploymentGeo6",
                                           label = "Geography",
                                           choices = unique(employmentDT$Geography),
                                           selected = "Canada"
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("EmploymentChar6",
                                           label = "Characteristic",
                                           choices = unique(employmentDT$Characteristic),
                                           
                                           
                            ),
                            
                            selectizeInput("EmploymentConf6",
                                           label = "Confidence",
                                           choices = unique(employmentDT$Confidence),
                                           
                                           
                            ),
                            
                        ),
                        
                        
                         
                        
                        conditionalPanel(
                            condition = "input.LM == 'Paid employees having paid vacation leave in their current job'",
                            
                            
                            
                            selectizeInput("VM125",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("EmploymentYear5",
                                           label = "Year",
                                           choices = unique(employmentDT$Year),
                                           
                            ),      
                            
                            selectizeInput("EmploymentGeo5",
                                           label = "Geography",
                                           choices = unique(employmentDT$Geography),
                                           selected = "Canada"
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("EmploymentChar5",
                                           label = "Characteristic",
                                           choices = unique(employmentDT$Characteristic),
                                           
                                           
                            ),
                            
                            selectizeInput("EmploymentConf5",
                                           label = "Confidence",
                                           choices = unique(employmentDT$Confidence),
                                           
                                           
                            ),
                            
                        ),
                        
                        
                        
                        
                        
                        
                        conditionalPanel(
                            condition = "input.LM == 'Paid employees receiving at least one employment benefit in their current job'",
                            
                            
                            
                            selectizeInput("VM120",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("EmploymentYear4",
                                           label = "Year",
                                           choices = unique(employmentDT$Year),
                                           
                            ),      
                            
                            selectizeInput("EmploymentGeo4",
                                           label = "Geography",
                                           choices = unique(employmentDT$Geography),
                                           selected = "Canada"
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("EmploymentChar4",
                                           label = "Characteristic",
                                           choices = unique(employmentDT$Characteristic),
                                           
                                           
                            ),
                            
                            selectizeInput("EmploymentConf4",
                                           label = "Confidence",
                                           choices = unique(employmentDT$Confidence),
                                           
                                           
                            ),
                            
                        ),
                        
                        
                        
                        
                        
                        conditionalPanel(
                            condition = "input.LM == 'Paid employees having pension plan in their current job'",
                            
                            
                            
                            selectizeInput("VM115",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("EmploymentYear3",
                                           label = "Year",
                                           choices = unique(employmentDT$Year),
                                           
                            ),      
                            
                            selectizeInput("EmploymentGeo3",
                                           label = "Geography",
                                           choices = unique(employmentDT$Geography),
                                           selected = "Canada"
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("EmploymentChar3",
                                           label = "Characteristic",
                                           choices = unique(employmentDT$Characteristic),
                                           
                                           
                            ),
                            
                            selectizeInput("EmploymentConf3",
                                           label = "Confidence",
                                           choices = unique(employmentDT$Confidence),
                                           
                                           
                            ),
                            
                        ),
                        
                        
                        
                        
                        conditionalPanel(
                            condition = "input.LM == 'Paid employees considering their current job good for career advancement'",
                            
                            
                            
                            selectizeInput("VM110",
                                           label = "Visible minority status",
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                            ),
                            
                            
                            selectizeInput("EmploymentYear2",
                                           label = "Year",
                                           choices = unique(employmentDT$Year),
                                           
                            ),      
                            
                            selectizeInput("EmploymentGeo2",
                                           label = "Geography",
                                           choices = unique(employmentDT$Geography),
                                           selected = "Canada"
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("EmploymentChar2",
                                           label = "Characteristic",
                                           choices = unique(employmentDT$Characteristic),
                                           
                                           
                            ),
                            
                            selectizeInput("EmploymentConf2",
                                           label = "Confidence",
                                           choices = unique(employmentDT$Confidence),
                                           
                                           
                            ),
                            
                        ),
                        
                        
                        
                        
                        
                        
                        conditionalPanel(
                             condition = "input.LM == 'Paid employees having paid sick leave in their current job'",
                             
                             
                             
                             selectizeInput("VM100",
                                            label = "Visible minority status",
                                            choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                            selected = list("Total - Visible minority", "Not a visible minority"),
                                            multiple = TRUE
                             ),
                             
                             
                             selectizeInput("EmploymentYear",
                                            label = "Year",
                                            choices = unique(employmentDT$Year),
                                            
                             ),      
                            
                             selectizeInput("EmploymentGeo",
                                             label = "Geography",
                                             choices = unique(employmentDT$Geography),
                                             selected = "Canada"
                                                           
       
                                            
                             ),
                             
                             
                             selectizeInput("EmploymentChar",
                                            label = "Characteristic",
                                            choices = unique(employmentDT$Characteristic),
                                           
                                            
                             ),
                             
                             selectizeInput("EmploymentConf",
                                            label = "Confidence",
                                            choices = unique(employmentDT$Confidence),
                                            
                                            
                             ),
                             
                         ),
                             
                     ),   
                     
                     
                     
                     
                     #Public services and institutions widgets Tab 2
                     
                     conditionalPanel(
                         
                         condition = "input.dim == 'Public services and institutions'",
                         
                         selectizeInput("dimTrust",
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
                                            
                                            
                                            
                                        )
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dimTrust == 'Population expressing confidence in Federal Parliament' 
                                 || input.dimTrust == 'Population expressing Confidence in the Canadian media' 
                                 || input.dimTrust == 'Population expressing confidence in the school system'
                                 || input.dimTrust == 'Population expressing confidence in the justice system and courts'
                                 || input.dimTrust == 'Population expressing confidence in the police service'
                                 || input.dimTrust == 'Population expressing confidence in major corporations'
                                 || input.dimTrust == 'Population expressing confidence in merchants and local business people'
                                 || input.dimTrust == 'Population expressing confidence in banks'",
                             
                             
                             selectizeInput("confYear",
                                            label = "Year",
                                            choices = unique(confidenceDT$'Year')
                             ),
                             
                             selectizeInput("confGeo",
                                            label = "Geography",
                                            choices = unique(confidenceDT$'Geography')
                             ),
                             
                             
                             
                             selectizeInput("VM40",
                                            label = "Visible minority status",
                                            choices = list("Total, by visible minority group","Total - Visible minority","South Asian","Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","Not a visible minority"),
                                            selected = list("Chinese"),
                                            multiple = TRUE
                             ),
                             
                             selectizeInput("confCharacteristicsP",
                                            label = "Selected sociodemographic characteristics",
                                            choices = list('Age', 'Gender', 'Immigration Status', 'Generation Status', 'Language Spoken', 'Education Status'),
                                            
                             ),
                             
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristicsP == 'Age'",
                                 
                                 selectizeInput("confCharSpecAgeP",
                                                label = "Age",
                                                choices = list('Total, 15 years and over', '15 to 24 years', '25 to 64 years', '65 years and over'),
                                                
                                 ),
                                 
                             ),
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristicsP == 'Gender'",
                                 
                                 selectizeInput("confCharSpecGenderP",
                                                label = "Gender",
                                                choices = list('Men', 'Women'),
                                                
                                 ),
                                 
                             ),
                             
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristicsP == 'Immigration Status'",
                                 
                                 selectizeInput("confCharSpecImmP",
                                                label = "Immigration Status",
                                                choices = list('Immigrants', 'Non-Immigrants'),
                                                
                                 ),
                                 
                             ),
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristicsP == 'Generation Status'",
                                 
                                 selectizeInput("confCharSpecGenP",
                                                label = "Immigration Status",
                                                choices = list( 'First generation', 'Second generation', 'Third generation or more'),
                                                
                                 ),
                                 
                             ),
                             
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristicsP == 'Language Spoken'",
                                 
                                 selectizeInput("confCharSpecLangP",
                                                label = "Language Spoken",
                                                choices = list('First official language spoken, English only', 'First official language spoken, French only'),
                                                
                                 ),
                                 
                             ),
                             
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristicsP == 'Education Status'",
                                 
                                 selectizeInput("confCharSpecEduP",
                                                label = "Language Spoken",
                                                choices = list('Secondary (high) school diploma or equivalency certificate or less', 'Postsecondary certificate or diploma (non-university)', 'University certificate or diploma'),
                                                
                                 ),
                                 
                             ),
                             
                             selectizeInput("confConfidenceP",
                                            label = "Confidence Interval",
                                            choices = unique(confidenceDT$'Confidence')
                             ),
                             
                             
                         ),
                         
                         
                     ),
                     
                     
                     
                             
                         #TodayRep
                         
                         #Widgets for Representation Variables
                         conditionalPanel(
                             
                             condition = "input.dim == 'Representation in decision-making positions'",
                             
                             selectizeInput("Rep",
                                            label = "Indicators",
                                            choices = list("Percent of workers in all management occupations",
                                                           
                                                           "Percent of workers in senior management occupations",
                                                           
                                                           "Percent of workers in specialized middle management occupations",
                                                           
                                                           "Percent of workers in other middle management occupations"
                                            )
                             ),
                             
                         
                        #Today1
                        
                        
                        
                        
                        
            
                        
                        conditionalPanel(
                            condition = "input.Rep == 'Percent of workers in specialized middle management occupations'",
                            
                            
                            
                            
                            selectizeInput("VM155",
                                           label = "Visible minority status",
                                           #choices =unique(representationDT$VisMin),
                                           
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           
                                           selected = list("Total - Visible minority"),
                                           multiple = TRUE
                                           
                            ),
                            
                            selectizeInput("RepDegree3",
                                           label = "Highest certificate, diploma or degree",
                                           choices = unique(representationDT$Degree),
                                           
                                           
                            ),
                            
                            
                            selectizeInput("RepGeo3",
                                           label = "Geography",
                                           choices = unique(representationDT$Geography),
                                           selected = "Canada"
                                           
                            ),
                            
                            selectizeInput("RepImm3",
                                           label = "Immigrant and generation status",
                                           choices = unique(representationDT$Immigration),
                                           selected = "Canada"
                                           
                            ),
                            
                            
                            selectizeInput("RepYear3",
                                           label = "Year",
                                           choices = unique(representationDT$Year),
                                           
                            ),
                            
                            selectizeInput("RepAgeLang3",
                                           label = "Age group and first official language spoken",
                                           choices = unique(representationDT$Age),
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("RepSex3",
                                           label = "Sex",
                                           choices = sort(unique(representationDT$Sex), decreasing = TRUE),
                                           selected = "Total - Sex"
                            ),
                            
                            
                        ),
                        
                        
                        
                        #End
                        conditionalPanel(
                            condition = "input.Rep == 'Percent of workers in senior management occupations'",
                            
                            
                            
                            
                            selectizeInput("VM150",
                                           label = "Visible minority status",
                                         
                                           
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           
                                           selected = list("Total - Visible minority"),
                                           multiple = TRUE
                                           
                            ),
                            
                            selectizeInput("RepDegree2",
                                           label = "Highest certificate, diploma or degree",
                                           choices = unique(representationDT$Degree),
                                           
                                           
                            ),
                            
                            
                            selectizeInput("RepGeo2",
                                           label = "Geography",
                                           choices = unique(representationDT$Geography),
                                           selected = "Canada"
                                           
                            ),
                            
                            selectizeInput("RepImm2",
                                           label = "Immigrant and generation status",
                                           choices = unique(representationDT$Immigration),
                                           selected = "Canada"
                                           
                            ),
                            
                            
                            selectizeInput("RepYear2",
                                           label = "Year",
                                           choices = unique(representationDT$Year),
                                           
                            ),
                            
                            selectizeInput("RepAgeLang2",
                                           label = "Age group and first official language spoken",
                                           choices = unique(representationDT$Age),
                                           
                                           
                                           
                            ),
                            
                            
                            selectizeInput("RepSex2",
                                           label = "Sex",
                                           choices = unique(representationDT$Sex),
                                           selected = "Total - Sex"
                            ),
                            
                            
                        ),
                        
                        
                        #End
                        conditionalPanel(
                            condition = "input.Rep == 'Percent of workers in all management occupations'",
                            
                            
                            
                            
                            selectizeInput("VM145",
                                           label = "Visible minority status",
                                           #choices =unique(representationDT$VisMin),
                                           
                                           choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                           
                                           selected = list("Total - Visible minority", "Not a visible minority"),
                                           multiple = TRUE
                              
                            ),
                            
                            selectizeInput("RepDegree",
                                           label = "Highest certificate, diploma or degree",
                                           choices = unique(representationDT$Degree),
                                           
                                           
                            ),
                            
                            
                            selectizeInput("RepGeo",
                                           label = "Geography",
                                           choices = unique(representationDT$Geography),
                                           selected = "Canada"
                                           
                            ),
                            
                            selectizeInput("RepImm",
                                           label = "Immigrant and generation status",
                                           choices = unique(representationDT$Immigration),
                                           selected = "Canada"
                                           
                            ),
                            
                            
                            selectizeInput("RepYear",
                                           label = "Year",
                                           choices = unique(representationDT$Year),
                                           
                            ),
                            
                            selectizeInput("RepAgeLang",
                                           label = "Age group and first official language spoken",
                                           choices = unique(representationDT$Age),
                                          
                                           
                                           
                            ),
                            
                         
                            selectizeInput("RepSex",
                                           label = "Sex",
                                           choices = sort(unique(representationDT$Sex), decreasing = TRUE),
                                           selected = "Total - Sex"
                            ),
                            
                           
                        ),
                        
                     
                         ),
                     
                     
                         
      # Widgets for use with datasets with Discrimination and victimization Variables
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization'",
                             
                             
                             selectizeInput("disind",
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
                             
                             
                             #new
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
                                 
                                 selectizeInput("covYear",
                                                label = "Year",
                                                choices = unique(discriminationDT$Year)
                                 ),
                                 
                                 selectizeInput("covGeo",
                                                label = "Geography",
                                                choices = unique(discriminationDT$Geography)
                                 ),
                                 
                  
                                 selectizeInput("VM30",
                                                label = "Visible minority status",
                                                choices = list("Total, by visible minority group","Total - Visible minority","South Asian",
                                                               "Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","Not a visible minority"),
                                                selected = list("Black"),
                                                multiple = TRUE
                                 ),
                                 
                                 selectizeInput("covCharacteristics",
                                                label = "Selected sociodemographic characteristics",
                                                choices = list('Age', 'Gender', 'Immigration Status', 'Generation Status', 'Language Spoken', 'Education Status'),
                                                
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.covCharacteristics == 'Age'",
                                     
                                     selectizeInput("covCharSpecAge",
                                                    label = "Age",
                                                    choices = list('Total, 15 years and over', '15 to 24 years', '25 to 64 years', '65 years and over'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.covCharacteristics == 'Gender'",
                                     
                                     selectizeInput("covCharSpecGender",
                                                    label = "Gender",
                                                    choices = list('Man', 'Woman'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.covCharacteristics == 'Immigration Status'",
                                     
                                     selectizeInput("covCharSpecImm",
                                                    label = "Immigration Status",
                                                    choices = list('Immigrants', 'Non-Immigrants'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.covCharacteristics == 'Generation Status'",
                                     
                                     selectizeInput("covCharSpecGen",
                                                    label = "Generation Status",
                                                    choices = list( 'First generation', 'Second generation', 'Third generation or more'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.covCharacteristics == 'Language Spoken'",
                                     
                                     selectizeInput("covCharSpecLang",
                                                    label = "Language Spoken",
                                                    choices = list('First official language spoken, English only', 'First official language spoken, French only'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.covCharacteristics == 'Education Status'",
                                     
                                     selectizeInput("covCharSpecEdu",
                                                    label = "Education Status",
                                                    choices = list('Secondary (high) school diploma or equivalency certificate or less','Postsecondary certificate or diploma (non-university)', 'University certificate or diploma'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 selectizeInput("covConfidence",
                                                label = "Confidence Interval",
                                                choices = unique(discriminationDT$Confidence)
                                 ),
                                 
                                 
                            
                                 
                                 
                             ),
                             
                             
                             conditionalPanel(
                                 
                                 condition = "input.disind == 'Hate Crime'",
                                 selectizeInput("disYear",
                                                label = "Year",
                                                choices = list('2014','2015','2016','2017','2018','2019','2020')
                                 ),
                                 
                                 selectizeInput("motivation",
                                                label = "Motivation",
                                                choices = list('Race or ethnicity', 'Total police-reported hate crime'),
                                               
                                                
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     condition = "input.motivation == 'Total police-reported hate crime'",
                                     
                                     selectizeInput("VM8",
                                                    label = "Race or ethnicity and other characteristics",
                                                    choices = list('Race or ethnicity', 'Religion', 'Sexual orientation', 'Language', 'Disability', 'Gender', 'Age', 'Unknown motivation'),
                                                    selected = "Religion",
                                                    multiple = TRUE
                                     ),
                                     
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     condition = "input.motivation == 'Race or ethnicity'",
                                     
                                     selectizeInput("VM7",
                                                    label = "Groups designated as Visible Minority",
                                                    choices = list('Black', 'South Asian', 'East or Southeast Asian', 'Arab or West Asian', 'White', 'Indigenous', 'Multiple races or ethnicities', 'Other Race or ethnicity', 'Unknown Race or ethnicity' ),
                                                    selected = "Black",
                                                    multiple = TRUE
                                     ),
                                     
                                 ),
                     
                                 selectizeInput("geo_dis",
                                                label = "Geography",
                                                choices = list("Canada,selected police services")
                                 )
                                 
                             ),
                             
                         ),
                     
                     
                     
                     
                     
                     
            #Civic engagement and political participation Tab 2 
                     
                         conditionalPanel(
                             
                             condition = "input.dim == 'Civic engagement and political participation'",
                             
                             selectizeInput("dimCivilEngagement",
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
                             ),
                             
                             #Leo
                             conditionalPanel(
                                 condition = "input.dimCivilEngagement == 'Percent of the population voting in the last municipal election'
                                 ||input.dimCivilEngagement == 'Percent of the population voting in the last provincial election'
                                 ||input.dimCivilEngagement == 'Percent of the population voting in the last federal election'",
                                 
                                 
                                 
                                 selectizeInput("VM170",
                                                label = "Visible minority status",
                                                choices = list("Total, by visible minority group","Total - Visible minority","South Asian","Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","Not a visible minority"),
                                                selected = list("Total - Visible minority"),
                                                multiple = TRUE
                                 ),
                                 
                                 
                                 selectizeInput("CivicYear2",
                                                label = "Year",
                                                choices = unique(civicDT2$Year),
                                                
                                 ),      
                                 
                                 selectizeInput("CivicGeo2",
                                                label = "Geography",
                                                choices = unique(civicDT2$Geography),
                                                selected = "Canada"
                                                
                                                
                                                
                                 ),
                                 
                                 selectizeInput("CivicCharacteristics2",
                                                label = "Sociodemographic Characteristics",
                                                choices = list('Age', 'Gender', 'Immigration Status', 'Generation Status', 'Language Spoken', 'Education Status'),
                                                
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Age'",
                                     
                                     selectizeInput("CivicCharSpecAge2",
                                                    label = "Age",
                                                    choices = list('Total, 15 years and over', '15 to 24 years', '25 to 64 years', '65 years and over'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Gender'",
                                     
                                     selectizeInput("CivicCharSpecSex2",
                                                    label = "Gender",
                                                    choices = list('Man', 'Woman'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Immigration Status'",
                                     
                                     selectizeInput("CivicCharSpecImm2",
                                                    label = "Immigration Status",
                                                    choices = list('Immigrants', 'Non-Immigrants'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Generation Status'",
                                     
                                     selectizeInput("CivicCharSpecGen2",
                                                    label = "Immigration Status",
                                                    choices = list( 'First generation', 'Second generation', 'Third generation or more'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Language Spoken'",
                                     
                                     selectizeInput("CivicCharSpecLang2",
                                                    label = "Language Spoken",
                                                    choices = list('First official language spoken, English only', 'First official language spoken, French only'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Education Status'",
                                     
                                     selectizeInput("CivicCharSpecEdu2",
                                                    label = "Language Spoken",
                                                    choices = list('Secondary (high) school diploma or equivalency certificate or less', 'Postsecondary certicate or diploma (non-university)', 'University certificate or diploma'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 selectizeInput("CivicConf2",
                                                label = "Confidence",
                                                choices = unique(civicDT$Confidence),
                                                
                                                
                                 ),
                                 
                             ),
                             
                             
                             
                             
                             
                             
                             
                             
                             #####
                             conditionalPanel(
                                 condition = "input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization'",
                                
                                 
                                 
                                 selectizeInput("VM160",
                                                label = "Visible minority status",
                                                choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean",
                                                               "Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                                selected = list("Total - Visible minority"),
                                                multiple = TRUE
                                 ),
                                 
                                 
                                 selectizeInput("CivicYear1",
                                                label = "Year",
                                                choices = unique(civicDT$Year),
                                                
                                 ),      
                                 
                                 selectizeInput("CivicGeo1",
                                                label = "Geography",
                                                choices = unique(civicDT$Geography),
                                                selected = "Canada"
                                                
                                                
                                                
                                 ),
                                 
                                 selectizeInput("CivicCharacteristics1",
                                                label = "Sociodemographic Characteristics",
                                                choices = list('Age', 'Sex', 'Immigration Status', 'Generation Status', 'Language Spoken', 'Education Status'),
                                                
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics1 == 'Age'",
                                     
                                     selectizeInput("CivicCharSpecAge1",
                                                    label = "Age",
                                                    choices = list('Total, 15 years and over', '15 to 24 years', '25 to 64 years', '65 years and over'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics1 == 'Sex'",
                                     
                                     selectizeInput("CivicCharSpecSex1",
                                                    label = "Sex",
                                                    choices = list('Man', 'Woman'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics1 == 'Immigration Status'",
                                     
                                     selectizeInput("CivicCharSpecImm1",
                                                    label = "Immigration Status",
                                                    choices = list('Immigrants', 'Non-Immigrants'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics1 == 'Generation Status'",
                                     
                                     selectizeInput("CivicCharSpecGen1",
                                                    label = "Immigration Status",
                                                    choices = list( 'First generation', 'Second generation', 'Third-plus generation'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics1 == 'Language Spoken'",
                                     
                                     selectizeInput("CivicCharSpecLang1",
                                                    label = "Language Spoken",
                                                    choices = list('First official language spoken, English only', 'First official language spoken, French only'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics1 == 'Education Status'",
                                     
                                     selectizeInput("CivicCharSpecEdu1",
                                                    label = "Language Spoken",
                                                    choices = list('High school diploma or a high school equivalency certificate or less', 'Postsecondary certicate or diploma (non-university)', 'University certificate or diploma'),
                                                    
                                     ),
                                     
                                 ),
                                 
                             
                                 selectizeInput("CivicConf1",
                                                label = "Confidence",
                                                choices = unique(civicDT$Confidence),
                                                
                                                
                                 ),
                                 
                             ),
                           
                             
                             
                             
                             
                             conditionalPanel(
                                 condition = "input.dimCivilEngagement == 'Percent of the population members in a sports or recreational organization'",
                                 
                                 
                                 
                                 selectizeInput("VM161",
                                                label = "Visible minority status",
                                                choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                                selected = list("Total - Visible minority"),
                                                multiple = TRUE
                                 ),
                                 
                                 
                                 selectizeInput("CivicYear2",
                                                label = "Year",
                                                choices = unique(civicDT$Year),
                                                
                                 ),      
                                 
                                 selectizeInput("CivicGeo2",
                                                label = "Geography",
                                                choices = unique(civicDT$Geography),
                                                selected = "Canada"
                                                
                                                
                                 ),              
                                
                                 
                                 
                                 selectizeInput("CivicCharacteristics2",
                                                label = "Sociodemographic Characteristics",
                                                choices = list('Age', 'Sex', 'Immigration Status', 'Generation Status', 'Language Spoken', 'Education Status'),
                                                
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Age'",
                                     
                                     selectizeInput("CivicCharSpecAge2",
                                                    label = "Age",
                                                    choices = list('Total, 15 years and over', '15 to 24 years', '25 to 64 years', '65 years and over'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Sex'",
                                     
                                     selectizeInput("CivicCharSpecSex2",
                                                    label = "Sex",
                                                    choices = list('Man', 'Woman'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Immigration Status'",
                                     
                                     selectizeInput("CivicCharSpecImm2",
                                                    label = "Immigration Status",
                                                    choices = list('Immigrants', 'Non-Immigrants'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Generation Status'",
                                     
                                     selectizeInput("CivicCharSpecGen2",
                                                    label = "Immigration Status",
                                                    choices = list( 'First generation', 'Second generation', 'Third-plus generation'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Language Spoken'",
                                     
                                     selectizeInput("CivicCharSpecLang2",
                                                    label = "Language Spoken",
                                                    choices = list('First official language spoken, English only', 'First official language spoken, French only'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.CivicCharacteristics2 == 'Education Status'",
                                     
                                     selectizeInput("CivicCharSpecEdu2",
                                                    label = "Language Spoken",
                                                    choices = list('High school diploma or a high school equivalency certificate or less', 'Postsecondary certicate or diploma (non-university)', 'University certificate or diploma'),
                                                    
                                     ),
                                     
                                 ),
                                 
                                
                                 selectizeInput("CivicConf2",
                                                label = "Confidence",
                                                choices = unique(civicDT$Confidence),
                                                
                                                
                                 ),
                                 
                             ),
                             
                             
                             
                              
                         ),
                         
                     
                     
                     
                    #Basic needs and housing Tab 2  
                         conditionalPanel(
                             
                             condition = "input.dim == 'Basic needs and housing'",
                             
                             selectizeInput("dimBasicNeeds",
                                            label = "Indicators",
                                            choices = list(
                                                "Percent of the population living in a dwelling owned by one member of the household ",
                                                
                                                "Percent of the population living in core need household",
                                                
                                                "Percent of the population living in suitable housing",
                                                
                                                "Percent of the population living in an affordable housing",
                                                
                                                "Percent of the population living in a food-secure household",
                                                
                                                " Percent of the population living in a household with marginal food security",
                                                
                                                "Percent of the population living in a food-insecure household, moderate or severe",
                                                
                                                "Percent of the population living in a household with moderate food insecurity",
                                                
                                                " Percent of the population living in a household with severe food insecurity"
                                                
                                                
                                            )
                             )
                             
                         ),
                         
                         
                     
                     
                     #Health and wellbeing Tab 2
                         conditionalPanel(
                             
                             condition = "input.dim == 'Health and wellbeing'",
                             
                             selectizeInput("dimHealth",
                                            label = "Indicators",
                                            choices = list(
                                                " Percent of the population reporting very good or excellent general health",
                                                
                                                "Percent of the population reporting fair or poor general health",
                                                
                                                "Percent of the population reporting very good or excellent mental health",
                                                
                                                "Percent of the population reporting fair or poor mental health",
                                                
                                                "Percent of the population reporting their life stressful",
                                                
                                                "Percent of the population satisfied with life as a whole",
                                                
                                                "Percent of the population predicting their life opportunities will improve in the next 5 years"
                                                
                                                
                                                
                                            )
                             )
                             
                         ),
                         
                         
                     
                         
                 # Income and wealth Tab 2 
                         conditionalPanel(
                             
                             condition = "input.dim == 'Income and wealth'",
                             
                             selectizeInput("dimIncome",
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
                         
                         
                     
                     
                     #Social connections and personnal networks Tab 2
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks'",
                             
                             selectizeInput("dimSocial",
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
                                                
                                                "Percent of the population with a personal ethnically-diverse network",
                                                
                                                "Population reporting that most people can be trusted",
                                                
                                                "Population reporting strong sense of belonging to their local community",
                                                
                                                "Population reporting strong sense of belonging to their town or city",
                                                
                                                "Population reporting strong sense of belonging to their province",
                                                
                                                "Population reporting strong sense of belonging to Canada"
                                                
                                                
                                                
                                                
                                                
                                            )
                             ),
                             
                             selectizeInput("confYearS",
                                            label = "Year",
                                            choices = unique(confidenceDT$'Year')
                             ),
                             
                             selectizeInput("confGeoS",
                                            label = "Geography",
                                            choices = unique(confidenceDT$'Geography')
                             ),
                             
                             
                             
                             
                             selectizeInput("VM401",
                                            label = "Visible minority status",
                                            choices = list("Total, by visible minority group","Total - Visible minority","South Asian","Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","Not a visible minority"),
                                            selected = list("Black"),
                                            multiple = TRUE
                             ),
                             
                             selectizeInput("confCharacteristics",
                                            label = "Selected sociodemographic characteristics",
                                            choices = list('Age', 'Gender', 'Immigration Status', 'Generation Status', 'Language Spoken', 'Education Status'),
                                            
                             ),
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristics == 'Age'",
                                 
                                 selectizeInput("confCharSpecAge",
                                                label = "Age",
                                                choices = list('Total, 15 years and over', '15 to 24 years', '25 to 64 years', '65 years and over'),
                                                
                                 ),
                                 
                             ),
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristics == 'Gender'",
                                 
                                 selectizeInput("confCharSpecSex",
                                                label = "Gender",
                                                choices = list('Men', 'Women'),
                                                
                                 ),
                                 
                             ),
                             
                             
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristics == 'Immigration Status'",
                                 
                                 selectizeInput("confCharSpecImm",
                                                label = "Immigration Status",
                                                choices = list('Immigrants', 'Non-Immigrants'),
                                                
                                 ),
                                 
                             ),
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristics == 'Generation Status'",
                                 
                                 selectizeInput("confCharSpecGen",
                                                label = "Immigration Status",
                                                choices = list( 'First generation', 'Second generation', 'Third generation or more'),
                                                
                                 ),
                                 
                             ),
                             
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristics == 'Language Spoken'",
                                 
                                 selectizeInput("confCharSpecLang",
                                                label = "Language Spoken",
                                                choices = list('First official language spoken, English only', 'First official language spoken, French only'),
                                                
                                 ),
                                 
                             ),
                             
                             
                             conditionalPanel(
                                 
                                 condition = "input.confCharacteristics == 'Education Status'",
                                 
                                 selectizeInput("confCharSpecEdu",
                                                label = "Language Spoken",
                                                choices = list('Secondary (high) school diploma or equivalency certificate or less', 'Postsecondary certicate or diploma (non-university)', 'University certificate or diploma'),
                                                
                                 ),
                                 
                             ),
                             
                             selectizeInput("confConfidence",
                                            label = "Confidence Interval",
                                            choices = unique(confidenceDT$'Confidence')
                             ),
                             
                             
                             
                             
                             
                             
                         ),
                             
           
                        
                         
                         
            #Education,training and skills indicator Tab 2
                         conditionalPanel(
                             
                             condition = "input.dim == 'Education, training and skills'",
                             
                             selectizeInput("dimEducation4",
                                            label = "Indicators",
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
                             ),
                             
                             
                             conditionalPanel(
                                 condition = "input.dimEducation4 == 'Population with bachelor’s degree' || 'Population with no certificate, diploma or degree' || 'Population with high school diploma or equivalency certificate' || 'Population with postsecondary certificate or diploma below bachelor level' || 'Population with bachelor’s degree or above' || 'Population with master’s degree or earned doctorate' ",
                                 
                                 
                                 
                                 
                                 selectizeInput("VM10",
                                                label = "Visible Minority Group",
                                                choices = unique(educationDT$VisMin),
                                                selected = list("Total - Visible minority", "Not a visible minority"),
                                                multiple = TRUE
                                 ),
                                 
                                 
                                 
                                 selectizeInput("eduGeo2",
                                                label = "Geography",
                                                choices = unique(educationDT$Geography),
                                                selected = "Canada"
                                                
                                 ),
                                 
                                 selectizeInput("eduVisMin2",
                                                label = "Groups designated by Immigration and Generational Status",
                                                choices = unique(educationDT$'Immigration'),
                                                selected = "Canada"
                                                
                                 ),
                                 
                                 
                                 selectizeInput("eduYear2",
                                                label = "Year",
                                                choices = unique(educationDT$Year),
                                                
                                 ),
                                 
                                 selectizeInput("eduAge2",
                                                label = "Age Group",
                                                choices = unique(educationDT$Age),
                                                selected = "Total - Age"
                                 ),
                                 
                                 selectizeInput("eduSex2",
                                                label = "Sex",
                                                choices = sort(unique(educationDT$Sex), decreasing = TRUE),
                                                selected = "Total - Sex"
                                 ),
                                 
                                 selectizeInput("eduLang2",
                                                label = "Language",
                                                choices = unique(educationDT$'Language'),
                                                
                                 )
                             ),
                             
                         ),
                         
                         
                         
                         
                         
                    
           #Local Community widgets Tab 2              
                         conditionalPanel(
                             
                             condition = "input.dim == 'Local community'",
                             
                             selectizeInput("dimCommunity",
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
                         
                         #Not Included
                         conditionalPanel(
                             
                             condition = "input.dim == 'Access to public services and institutions'",
                             
                             selectizeInput("dimPublicServices",
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
           
           
                     # This is the main panel
                     # Visuals will be displayed here
                     # You can also add text, images, etc. here
         mainPanel(
                         
         h2("Groups Designated as Visible Minorities"),
                       
           
        #Leo
        
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Immigration Status'",
            
            br(),
            br(),
            plotlyOutput("sBarConf7", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        
        
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Age'",
            
            br(),
            br(),
            plotlyOutput("sBarConfAge7", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        
        
        
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Gender'",
            
            br(),
            br(),
            plotlyOutput("sBarConfSex7", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Generation Status'",
            
            br(),
            br(),
            plotlyOutput("sBarConfGen7", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Language Spoken'",
            
            br(),
            br(),
            plotlyOutput("sBarConfLang7", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in banks' & input.confCharacteristicsP == 'Education Status'",
            
            br(),
            br(),
            plotlyOutput("sBarConfEdu7", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        
        
        
        
        ####
        
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Immigration Status'",
            
            br(),
            br(),
            plotlyOutput("sBarConf4", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        
         
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Age'",
            
            br(),
            br(),
            plotlyOutput("sBarConfAge4", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        
        
        
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Gender'",
            
            br(),
            br(),
            plotlyOutput("sBarConfSex4", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Generation Status'",
            
            br(),
            br(),
            plotlyOutput("sBarConfGen4", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Language Spoken'",
            
            br(),
            br(),
            plotlyOutput("sBarConfLang4", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        conditionalPanel(
            
            condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the justice system and courts' & input.confCharacteristicsP == 'Education Status'",
            
            br(),
            br(),
            plotlyOutput("sBarConfEdu4", inline = TRUE, width = 700, height = 500),
            br(),
            helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
            
        ),
        
         
    ##########
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Immigration Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConf6", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Age'",
             
             br(),
             br(),
             plotlyOutput("sBarConfAge6", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Gender'",
             
             br(),
             br(),
             plotlyOutput("sBarConfSex6", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Generation Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfGen6", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Language Spoken'",
             
             br(),
             br(),
             plotlyOutput("sBarConfLang6", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in merchants and local business people' & input.confCharacteristicsP == 'Education Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfEdu6", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         
         ##
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Immigration Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConf5", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Age'",
             
             br(),
             br(),
             plotlyOutput("sBarConfAge5", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Gender'",
             
             br(),
             br(),
             plotlyOutput("sBarConfSex5", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Generation Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfGen5", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Language Spoken'",
             
             br(),
             br(),
             plotlyOutput("sBarConfLang5", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristicsP == 'Education Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfEdu5", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         
         ##
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Immigration Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConf3", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Age'",
             
             br(),
             br(),
             plotlyOutput("sBarConfAge3", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Gender'",
             
             br(),
             br(),
             plotlyOutput("sBarConfSex3", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Generation Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfGen3", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Language Spoken'",
             
             br(),
             br(),
             plotlyOutput("sBarConfLang3", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristicsP == 'Education Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfEdu3", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         
         
         ##
             conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Immigration Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConf", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Age'",
             
             br(),
             br(),
             plotlyOutput("sBarConfAge", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Gender'",
             
             br(),
             br(),
             plotlyOutput("sBarConfSex", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Generation Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfGen", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Language Spoken'",
             
             br(),
             br(),
             plotlyOutput("sBarConfLang", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in the police service' & input.confCharacteristicsP == 'Education Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfEdu", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         ##
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Immigration Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConf1", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Age'",
             
             br(),
             br(),
             plotlyOutput("sBarConfAge1", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Gender'",
             
             br(),
             br(),
             plotlyOutput("sBarConfSex1", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Generation Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfGen1", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Language Spoken'",
             
             br(),
             br(),
             plotlyOutput("sBarConfLang1", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristicsP == 'Education Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfEdu1", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
            ##   
         
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Immigration Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConf2", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Age'",
             
             br(),
             br(),
             plotlyOutput("sBarConfAge2", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Gender'",
             
             br(),
             br(),
             plotlyOutput("sBarConfSex2", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Generation Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfGen2", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Language Spoken'",
             
             br(),
             br(),
             plotlyOutput("sBarConfLang2", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         conditionalPanel(
             
             condition = "input.dim == 'Public services and institutions' & input.dimTrust == 'Population expressing Confidence in the Canadian media' & input.confCharacteristicsP == 'Education Status'",
             
             br(),
             br(),
             plotlyOutput("sBarConfEdu2", inline = TRUE, width = 700, height = 500),
             br(),
             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
             
         ),
         
        #Leo
        
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Immigration Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivic4", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Age'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicAge4", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Gender'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicSex4", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provinciall election' & input.CivicCharacteristics2 == 'Generation Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicGen4", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Language Spoken'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicLang4", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last provincial election' & input.CivicCharacteristics2 == 'Education Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicEdu4", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    
    
    
 
        ###
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Immigration Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivic3", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Age'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicAge3", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Gender'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicSex3", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Generation Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicGen3", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Language Spoken'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicLang3", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last federal election' & input.CivicCharacteristics2 == 'Education Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicEdu3", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    
    
    


        ####
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Immigration Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivic2", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Age'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicAge2", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Gender'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicSex2", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Generation Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicGen2", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Language Spoken'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicLang2", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    conditionalPanel(
        
        condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population voting in the last municipal election' & input.CivicCharacteristics2 == 'Education Status'",
        
        br(),
        br(),
        plotlyOutput("sBarCivicEdu2", inline = TRUE, width = 700, height = 500),
        br(),
        helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
        
    ),
    
         
                         
                         ###
                         conditionalPanel(
                             
                             condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics1 == 'Immigration Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarCivic1", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics1 == 'Age'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarCivicAge1", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics1 == 'Gender'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarCivicSex1", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics1 == 'Generation Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarCivicGen1", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics1 == 'Language Spoken'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarCivicLang1", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Civic engagement and political participation' & input.dimCivilEngagement == 'Percent of the population members of at least one civic group or organization' & input.CivicCharacteristics1 == 'Education Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarCivicEdu1", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         #end
                         
                         
                         
                         
                         
                    
                        
                         conditionalPanel(
                             
                             condition = "input.dim == 'Representation in decision-making positions' & input.Rep == 'Percent of workers in specialized middle management occupations'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarRep3", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Representation in decision-making positions' & input.Rep == 'Percent of workers in senior management occupations'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarRep2", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Representation in decision-making positions' & input.Rep == 'Percent of workers in all management occupations'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarRep1", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Youth not in employment, education or training (NEET)'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarYouth", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         #end
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees having disability insurance in their current job'",
                             
                             br(),
                             br(),
                             plotlyOutput("employmentPlot7", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Currently employed population considering their job related to their education'",
                             
                             br(),
                             br(),
                             plotlyOutput("employmentPlot6", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees having paid vacation leave in their current job'",
                             
                             br(),
                             br(),
                             plotlyOutput("employmentPlot5", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees receiving at least one employment benefit in their current job'",
                             
                             br(),
                             br(),
                             plotlyOutput("employmentPlot4", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees having pension plan in their current job'",
                             
                             br(),
                             br(),
                             plotlyOutput("employmentPlot3", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees considering their current job good for career advancement'",
                             
                             br(),
                             br(),
                             plotlyOutput("employmentPlot2", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Paid employees having paid sick leave in their current job'",
                             
                             br(),
                             br(),
                             plotlyOutput("employmentPlot", inline = TRUE, width = 00, height = 500),
                             br(),
                             helpText("Note: Source.")
                             
                         ),
                         
                         #Today
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Discrimination when attending school or classes"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarClass", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarClass2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Discrimination when attending school or classes"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarClassAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarClassAge2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Discrimination when attending school or classes"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarClassGender", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarClassGender2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Discrimination when attending school or classes"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarClassGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarClassGen2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Discrimination when attending school or classes"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarClassLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarClassLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when attending school or classes' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Discrimination when attending school or classes"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarClassEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarClassEdu2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         
                         
                         
                         #end
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Discrimination when in a store, bank or restaurant"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarBan", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarBan2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Discrimination when in a store, bank or restaurant"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarBanAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarBanAge2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Discrimination when in a store, bank or restaurant"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarBanSex", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarBanSex2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Discrimination when in a store, bank or restaurant"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarBanGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarBanGen2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Discrimination when in a store, bank or restaurant"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarBanLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarBanLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when in a store, bank or restaurant' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Discrimination when in a store, bank or restaurant"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarBanEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarBanEdu2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         #end
                         
                         
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Discrimination when dealing with the police"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarPol", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarPol2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Discrimination when dealing with the police"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarPolAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarPolAge2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Discrimination when dealing with the police"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarPolSex", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarPolSex2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Discrimination when dealing with the police"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarPolGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarPolGen2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Discrimination when dealing with the police"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarPolLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarPolLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination when dealing with the police' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Discrimination when dealing with the police"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarPolEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarPolEdu2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         
                         
                         
                         
                         #end
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Discrimination at work or when applying for a job or promotion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarJob", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarJob2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Discrimination at work or when applying for a job or promotion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarJobAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarJobAge2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Discrimination at work or when applying for a job or promotion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarJobSex", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarJobSex2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Discrimination at work or when applying for a job or promotion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarJobGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarJobGen2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Discrimination at work or when applying for a job or promotion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarJobLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarJobLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Discrimination at work or when applying for a job or promotion' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Discrimination at work or when applying for a job or promotion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarJobEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarJobEdu2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         
                         
                         
                         #end
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on language"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on language"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarLangAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarLangAge2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on language"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarLangSex", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarLangSex2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on language"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarLangGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarLangGen2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on language"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarLangLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarLangLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on language' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on language"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarLangEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarLangEdu2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         
                         #end
                         
                         
                        
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on religion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarRel", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarRel2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on religion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarRelAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarRelAge2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on religion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarRelSex", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarRelSex2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on religion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarRelGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarRelGen2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on religion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarRelLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarRelLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on religion' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on religion"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarRelEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarRelEdu2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         
                         #end
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on race or colour"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCol", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCol2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on race or colour"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarColAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarColAge2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on race or colour"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarColSex", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarColSex2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on race or colour"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarColGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarColGen2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on race or colour"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarColLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarColLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on race or colour' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on race or colour"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarColEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarColEdu2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         #end
                         
                        
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on ethnicity or culture"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCov", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCov2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on ethnicity or culture"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovAge2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on ethnicity or culture"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovSex", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovSex2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on ethnicity or culture"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovGen2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on ethnicity or culture"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovLang2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination based on ethnicity or culture"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovEdu2", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         ###
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Immigration Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCov1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCov21", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Age'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovAge1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovAge21", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Gender'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovSex1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovSex21", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Generation Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovGen1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovGen21", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Language Spoken'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovLang1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovLang21", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Education Status'" ,
                             br(),
                             
                             h3("Experience(s) of discrimination"),
                             br(),
                             
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovEdu1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovEdu21", inline = TRUE, width = 400, height = 500))
                             ),
                             
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         ###
                         
                         
                #Hate crime 
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Hate Crime' & input.motivation == 'Race or ethnicity'",
                             
                             br(),
                             br(),
                             plotlyOutput("immdisPlot", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Note: Uniform Crime Reporting Survey (UCR) data are collected 
                            directly from survey respondents (Police Services) and extracted 
                            from administrative files. The categories that appear in this chart 
                            are those used by police services when reporting on hate crime incidences.")
                             
                         ),
                         
                      #end
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Immigration Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConf12", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Age'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfAge12", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Gender'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfSex12", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Generation Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfGen12", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Language Spoken'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfLang12", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to Canada' & input.confCharacteristics == 'Education Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfEdu12", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         
                         
                         #End
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Immigration Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConf11", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Age'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfAge11", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Gender'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfSex11", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Generation Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfGen11", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Language Spoken'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfLang11", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their province' & input.confCharacteristics == 'Education Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfEdu11", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         
                         
                         #End
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Immigration Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConf10", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Age'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfAge10", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Gender'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfSex10", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Generation Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfGen10", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Language Spoken'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfLang10", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their town or city' & input.confCharacteristics == 'Education Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfEdu10", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         
                         
                         #End
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Immigration Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConf9", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Age'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfAge9", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Gender'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfSex9", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Generation Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfGen9", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Language Spoken'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfLang9", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting strong sense of belonging to their local community' & input.confCharacteristics == 'Education Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfEdu9", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         #End
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Immigration Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConf8", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Age'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfAge8", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Gender'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfSex8", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Generation Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfGen8", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Language Spoken'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfLang8", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         conditionalPanel(
                             
                             condition = "input.dim == 'Social connections and personnal networks' & input.dimSocial == 'Population reporting that most people can be trusted' & input.confCharacteristics == 'Education Status'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarConfEdu8", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, General Social Survey – Social Identity, 2020.")
                             
                         ),
                         
                         
                         ###
                         
                        
                         ##
                      
                         conditionalPanel(
                             
                             condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Overqualified workers with a university degree'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarOver", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011.")
                             
                         ),
                         
                         
                       
                         conditionalPanel(
                             
                             condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with bachelor’s degree'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarEduVM", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Education, training and skills' && input.dimEducation4 == 'Population with no certificate, diploma or degree'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarEduVM1", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with high school diploma or equivalency certificate'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarEduVM2", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with postsecondary certificate or diploma below bachelor level'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarEduVM3", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with bachelor’s degree or above'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarEduVM4", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             
                         ),
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with master’s degree or earned doctorate'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarEduVM5", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             
                         ),
                         
                         
                         
                         
                         
                         
                         #Visuals for Discrimination and victimization Variables
                         conditionalPanel(
                             
                             condition = "input.dim == 'Discrimination and victimization' & input.disind == 'Hate Crime' && input.motivation == 'Total police-reported hate crime'",
                             
                             br(),
                             br(),
                             plotlyOutput("immdis2Plot", inline = TRUE, width = 700, height = 500),
                             helpText("Note: Uniform Crime Reporting Survey (UCR) data are collected 
                            directly from survey respondents (Police Services) and extracted 
                            from administrative files. The categories that appear in this chart 
                            are those used by police services when reporting on hate crime incidences.")
                         )
                     )
                 )
        ),
        
        
        
        #Tab for Immigrant Status Analysis
        
        tabPanel("Immigration Status", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         
                         
                         selectizeInput("dim2",
                                        label = "Theme",
                                        choices = list("Participation in the Labour Market",
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
                         
                         
                         # Widgets for use with datasets with Labour Market Variables
                         conditionalPanel(
                             condition = "input.dim2 == 'Participation in the Labour Market'",
                             
                             
                             selectizeInput("LM2",
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
                                 condition = "input.LM2 == 'Overqualified workers with a university degree'",
                                 
                                 
                                 
                                 
                                 selectizeInput("VM21",
                                                label = "Groups designated by Immigration and Generational Status",
                                                choices = unique(OverQualDT$Immigration),
                                                selected = list("Black"),
                                                multiple = TRUE
                                 ),
                                 
                                 
                                 selectizeInput("OverLocationIS",
                                                label = "Location of Study",
                                                choices = unique(OverQualDT$Location),
                                                
                                                
                                 ),
                                 
                                 selectizeInput("OverDegreeIS",
                                                label = "Highest certificate, diploma or degree",
                                                choices = unique(OverQualDT$Degree),
                                                
                                                
                                 ),
                                 
                                 
                                 selectizeInput("OverGeoIS",
                                                label = "Geography",
                                                choices = unique(OverQualDT$Geography),
                                                selected = "Canada"
                                                
                                 ),
                                 
                                 selectizeInput("OverVMIS",
                                                label = "Visible minority status",
                                                choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                                
                                                
                                 ),
                                 
                                 
                                 selectizeInput("OverYearIS",
                                                label = "Year",
                                                choices = unique(OverQualDT$Year),
                                                
                                 ),
                                 
                                 selectizeInput("OverAgeIS",
                                                label = "Age Group",
                                                choices = unique(OverQualDT$Age),
                                                selected = "Total - Age"
                                 ),
                                 
                                 selectizeInput("OverSexIS",
                                                label = "Sex",
                                                choices = sort(unique(OverQualDT$Sex), decreasing = TRUE),
                                                selected = "Total - Sex"
                                 ),
                                 
                                 selectizeInput("OverLangIS",
                                                label = "Language",
                                                choices = unique(OverQualDT$'Language'),
                                                
                                 )
                             ),
                             
                         ),
                         
                         # Widgets for use with datasets with Labour Market Variables
                         conditionalPanel(
                             condition = "input.dim2 == 'Education, training and skills'",
                             
                             
                             selectizeInput("Edu",
                                            label = "Education Indicators",
                                            choices = list( "Population with no certificate, diploma or degree",
                                                            
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
                                 condition = "input.Edu == 'Population with bachelor’s degree' || 'Population with no certificate, diploma or degree' || 'Population with high school diploma or equivalency certificate' || 'Population with postsecondary certificate or diploma below bachelor level' || 'Population with bachelor’s degree or above' || 'Population with master’s degree or earned doctorate' ",
                                 
                                 
                                 
                                 
                                 selectizeInput("VM9",
                                                label = "Groups designated by Immigration and Generational Status",
                                                choices = unique(educationDT$'Immigration'),
                                                selected = list("Total - Visible minority", "Not a visible minority"),
                                                multiple = TRUE
                                 ),
                                 
                                 
                                 
                                 selectizeInput("eduGeo",
                                                label = "Geography",
                                                choices = unique(educationDT$Geography),
                                                selected = "Canada"
                                                
                                 ),
                                 
                                 selectizeInput("eduVisMin",
                                                label = "Visible Minority Group",
                                                choices = unique(educationDT$VisMin),
                                                selected = "Canada"
                                                
                                 ),
                                 
                                 
                                 selectizeInput("eduYear",
                                                label = "Year",
                                                choices = unique(educationDT$Year),
                                                
                                 ),
                                 
                                 selectizeInput("eduAge",
                                                label = "Age Group",
                                                choices = unique(educationDT$Age),
                                                selected = "Total - Age"
                                 ),
                                 
                                 selectizeInput("eduSex",
                                                label = "Sex",
                                                choices = sort(unique(educationDT$Sex), decreasing = TRUE),
                                                selected = "Total - Sex"
                                 ),
                                 
                                 selectizeInput("eduLang",
                                                label = "Language",
                                                choices = unique(educationDT$'Language'),
                                                
                                 )
                             )
                             
                             
                             
                             
                             
                             
                         ),
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim2 == 'Civic engagement and political participation'",
                             
                             selectizeInput("dimCivilEngagement2",
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
                             
                             condition = "input.dim2 == 'Representation in decision-making positions'",
                             
                             selectizeInput("Rep2",
                                            label = "Indicators",
                                            choices = list("Percent of workers in all management occupations",
                                                           
                                                           "Percent of workers in senior management occupations",
                                                           
                                                           "Percent of workers in specialized middle management occupations",
                                                           
                                                           "Percent of workers in other middle management occupations"
                                            )
                             ),
                             
                         ),
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim2 == 'Basic needs and housing'",
                             
                             selectizeInput("dimBasicNeeds2",
                                            label = "Indicators",
                                            choices = list(
                                                "Percent of the population living in a dwelling owned by one member of the household ",
                                                
                                                "Percent of the population living in core need household",
                                                
                                                "Percent of the population living in suitable housing",
                                                
                                                "Percent of the population living in an affordable housing",
                                                
                                                "Percent of the population living in a food-secure household",
                                                
                                                " Percent of the population living in a household with marginal food security",
                                                
                                                "Percent of the population living in a food-insecure household, moderate or severe",
                                                
                                                "Percent of the population living in a household with moderate food insecurity",
                                                
                                                " Percent of the population living in a household with severe food insecurity"
                                                
                                                
                                            )
                             )
                             
                         ),
                         
                         
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim2 == 'Local community'",
                             
                             selectizeInput("dimCommunity2",
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
                             
                             condition = "input.dim2 == 'Health and wellbeing'",
                             
                             selectizeInput("dimHealth2",
                                            label = "Indicators",
                                            choices = list(
                                                " Percent of the population reporting very good or excellent general health",
                                                
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
                             
                             condition = "input.dim2 == 'Public services and institutions'",
                             
                             selectizeInput("dimTrust2",
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
                                 
                                 condition = "input.dim2 == 'Income and wealth'",
                                 
                                 selectizeInput("dimIncome2",
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
                                 
                                 condition = "input.dim2 == 'Social connections and personnal networks'",
                                 
                                 selectizeInput("dimSocial2",
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
                                 
                                 condition = "input.dim2 == 'Discrimination and victimization'",
                                 
                                 
                                 selectizeInput("disind2",
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
                             
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                     ),
                     mainPanel(
                         
                         h2("Immigration Status"),
                         
                         #Visuals related to Labour Market
                         conditionalPanel(
                             
                             condition = "input.dim2 == 'Education, training and skills'",
                             
                             
                             conditionalPanel(
                                 condition = "input.Edu == 'Population with bachelor’s degree'",
                                 
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEdu", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             ),
                             
                             
                             conditionalPanel(
                                 condition = "input.Edu == 'Population with no certificate, diploma or degree'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEdu1", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.Edu == 'Population with high school diploma or equivalency certificate'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEdu2", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.Edu == 'Population with postsecondary certificate or diploma below bachelor level'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEdu3", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.Edu == 'Population with bachelor’s degree or above'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEdu4", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.Edu == 'Population with master’s degree or earned doctorate'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEdu5", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             )
                             
                             
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Overqualified workers with a university degree'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarOverIS", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011.")
                             
                         )
                     )
                     
                 )
        ),
        
        #Bar graphs by Sex Tab
        tabPanel("Gender", fluid = TRUE,
                 sidebarLayout(
                     
                     sidebarPanel(
                         
                         selectizeInput("dim3",
                                        label = "Theme",
                                        choices = list("Participation in the Labour Market",
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
                             
                             
                             selectizeInput("LM3",
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
                                 
                                 
                                 
                                 
                                 selectizeInput("VM22",
                                                label = "Sex",
                                                choices = unique(OverQualDT$Sex),
                                                selected = list("Male"),
                                                multiple = TRUE
                                 ),
                                 
                                 
                                 selectizeInput("OverLocationSX",
                                                label = "Location of Study",
                                                choices = unique(OverQualDT$Location),
                                                
                                                
                                 ),
                                 
                                 selectizeInput("OverDegreeSX",
                                                label = "Highest certificate, diploma or degree",
                                                choices = unique(OverQualDT$Degree),
                                                
                                                
                                 ),
                                 
                                 
                                 selectizeInput("OverGeoSX",
                                                label = "Geography",
                                                choices = unique(OverQualDT$Geography),
                                                selected = "Canada"
                                                
                                 ),
                                 
                                 selectizeInput("OverVMSX",
                                                label = "Visible minority status",
                                                choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                                
                                                
                                 ),
                                 
                                 
                                 selectizeInput("OverYearSX",
                                                label = "Year",
                                                choices = unique(OverQualDT$Year),
                                                
                                 ),
                                 
                                 selectizeInput("OverAgeSX",
                                                label = "Age Group",
                                                choices = unique(OverQualDT$Age),
                                                selected = "Total - Age"
                                 ),
                                 
                                 selectizeInput("OverImmSX",
                                                label = "Groups designated by Immigration and Generational Status",
                                                choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
                                                
                                 ),
                                 
                                 selectizeInput("OverLangSX",
                                                label = "Language",
                                                choices = unique(OverQualDT$'Language'),
                                                
                                 )
                             ),
                             
                         ),
                         
                      #Done 5/9/22
                      conditionalPanel(
                          
                          condition = "input.dim3 == 'Civic engagement and political participation'",
                          
                          selectizeInput("dimCivilEngagement3",
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
                          
                          condition = "input.dim3 == 'Representation in decision-making positions'",
                          
                          selectizeInput("Rep3",
                                         label = "Indicators",
                                         choices = list("Percent of workers in all management occupations",
                                                        
                                                        "Percent of workers in senior management occupations",
                                                        
                                                        "Percent of workers in specialized middle management occupations",
                                                        
                                                        "Percent of workers in other middle management occupations"
                                         )
                          ),
                          
                      ),
                      
                      
                      
                      conditionalPanel(
                          
                          condition = "input.dim3 == 'Basic needs and housing'",
                          
                          selectizeInput("dimBasicNeeds3",
                                         label = "Indicators",
                                         choices = list(
                                             "Percent of the population living in a dwelling owned by one member of the household ",
                                             
                                             "Percent of the population living in core need household",
                                             
                                             "Percent of the population living in suitable housing",
                                             
                                             "Percent of the population living in an affordable housing",
                                             
                                             "Percent of the population living in a food-secure household",
                                             
                                             " Percent of the population living in a household with marginal food security",
                                             
                                             "Percent of the population living in a food-insecure household, moderate or severe",
                                             
                                             "Percent of the population living in a household with moderate food insecurity",
                                             
                                             " Percent of the population living in a household with severe food insecurity"
                                             
                                             
                                         )
                          )
                          
                      ),
                      
                      
                      
                      
                      
                      conditionalPanel(
                          
                          condition = "input.dim3 == 'Local community'",
                          
                          selectizeInput("dimCommunity3",
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
                          
                          selectizeInput("dimHealth3",
                                         label = "Indicators",
                                         choices = list(
                                             " Percent of the population reporting very good or excellent general health",
                                             
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
                          
                          selectizeInput("dimTrust3",
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
                          
                          selectizeInput("dimIncome3",
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
                          
                          selectizeInput("dimSocial3",
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
                          
                          
                          selectizeInput("disind3",
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
                             
                             # These are the drop down menus
                             
                             
                             selectizeInput("Edu3",
                                            label = "Education, training and skills Indicators",
                                            choices = list("Population with no certificate, diploma or degree",
                                                           
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
                                                           
                                                           "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"),
                                            selected = NULL
                             ),
                             
                             
                             
                             
                    
                         
                         
                         conditionalPanel(
                             condition = "input.dim3 == 'Education, training and skills' && input.Edu3 == 'Population with bachelor’s degree' || 'Population with no certificate, diploma or degree' || 'Population with high school diploma or equivalency certificate' || 'Population with postsecondary certificate or diploma below bachelor level' || 'Population with bachelor’s degree or above' || 'Population with master’s degree or earned doctorate' ",
                             
                             
                             
                             
                             selectizeInput("VM11",
                                            label = "Sex",
                                            choices = unique(educationDT$Sex),
                                            selected = list(),
                                            multiple = TRUE
                             ),
                             
                             
                             
                             selectizeInput("eduGeo3",
                                            label = "Geography",
                                            choices = unique(educationDT$Geography),
                                            selected = "Canada"
                                            
                             ),
                             
                             selectizeInput("eduVisMin3",
                                            label = "Visible Minority Group",
                                            choices = unique(educationDT$VisMin),
                                            selected = "Canada"
                                            
                             ),
                             
                             
                             selectizeInput("eduYear3",
                                            label = "Year",
                                            choices = unique(educationDT$Year),
                                            
                             ),
                             
                             selectizeInput("eduAge3",
                                            label = "Age Group",
                                            choices = unique(educationDT$Age),
                                            selected = "Total - Age"
                             ),
                             
                             selectizeInput("eduImm3",
                                            label = "Groups designated by Immigration and Generational Status",
                                            choices = sort(unique(educationDT$'Immigration'), decreasing = TRUE),
                                            
                             ),
                             
                             selectizeInput("eduLang3",
                                            label = "Language",
                                            choices = unique(educationDT$'Language'),
                                            
                             )
                         ),
                         ),
                         
                         # Add the js code and button to the page
                         #extendShinyjs(text = jsResetCode, functions = "reset"),
                         actionButton("reset_button", "Reset Page"),
                         
                     ),
                     mainPanel(
                         
                         
                         h2("Gender"),
                         
                         #Visuals related to Labour Market
                         conditionalPanel(
                             
                             condition = "input.dim3 == 'Education, training and skills'",
                             
                             
                             conditionalPanel(
                                 condition = "input.Edu3 == 'Population with bachelor’s degree'",
                                 
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEduSX", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             ),
                             
                             
                             conditionalPanel(
                                 condition = "input.Edu3 == 'Population with no certificate, diploma or degree'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEduSX1", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.Edu3 == 'Population with high school diploma or equivalency certificate'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEduSX2", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.Edu3 == 'Population with postsecondary certificate or diploma below bachelor level'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEduSX3", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.Edu3 == 'Population with bachelor’s degree or above'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEduSX4", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.Edu3 == 'Population with master’s degree or earned doctorate'",
                                 br(),
                                 br(),
                                 plotlyOutput("sBarEduSX5", inline = TRUE, width = 700, height = 500),
                                 br(),
                                 helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                                 
                                 
                                 
                             )
                             
                             
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim3 == 'Participation in the Labour Market' & input.LM3 == 'Overqualified workers with a university degree'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarOverSX", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011.")
                             
                         )
                         
                         
                         
                         
                     )
                 )
        ),
        #Tab for Geography
        tabPanel("Geography", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         
                         selectizeInput("dim5",
                                        label = "Theme",
                                        choices = list("Participation in the Labour Market",
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
                             
                             
                             selectizeInput("LM5",
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
                             
                             
                             
                             
                             
                             
                             
                             selectizeInput("VM23",
                                            label = "Geography",
                                            choices = unique(OverQualDT$Geography),
                                            selected = list("Male"),
                                            multiple = TRUE
                             ),
                             
                             
                             selectizeInput("OverLocationGEO",
                                            label = "Location of Study",
                                            choices = unique(OverQualDT$Location),
                                            
                                            
                             ),
                             
                             selectizeInput("OverDegreeGEO",
                                            label = "Highest certificate, diploma or degree",
                                            choices = unique(OverQualDT$Degree),
                                            
                                            
                             ),
                             
                             
                             selectizeInput("OverSexGEO",
                                            label = "Sex",
                                            choices = unique(OverQualDT$Sex),
                                            selected = "Males"
                                            
                             ),
                             
                             selectizeInput("OverVMGEO",
                                            label = "Visible minority status",
                                            choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                            
                                            
                             ),
                             
                             
                             selectizeInput("OverYearGEO",
                                            label = "Year",
                                            choices = unique(OverQualDT$Year),
                                            
                             ),
                             
                             selectizeInput("OverAgeGEO",
                                            label = "Age Group",
                                            choices = unique(OverQualDT$Age),
                                            selected = "Total - Age"
                             ),
                             
                             selectizeInput("OverImmGEO",
                                            label = "Groups designated by Immigration and Generational Status",
                                            choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
                                            
                             ),
                             
                             selectizeInput("OverLangGEO",
                                            label = "Language",
                                            choices = unique(OverQualDT$'Language'),
                                            
                             )
                             
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim5 == 'Civic engagement and political participation'",
                             
                             selectizeInput("dimCivilEngagement5",
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
                             
                             selectizeInput("Rep5",
                                            label = "Indicators",
                                            choices = list("Percent of workers in all management occupations",
                                                           
                                                           "Percent of workers in senior management occupations",
                                                           
                                                           "Percent of workers in specialized middle management occupations",
                                                           
                                                           "Percent of workers in other middle management occupations"
                                            )
                             ),
                             
                         ),
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim5 == 'Basic needs and housing'",
                             
                             selectizeInput("dimBasicNeeds5",
                                            label = "Indicators",
                                            choices = list(
                                                "Percent of the population living in a dwelling owned by one member of the household ",
                                                
                                                "Percent of the population living in core need household",
                                                
                                                "Percent of the population living in suitable housing",
                                                
                                                "Percent of the population living in an affordable housing",
                                                
                                                "Percent of the population living in a food-secure household",
                                                
                                                " Percent of the population living in a household with marginal food security",
                                                
                                                "Percent of the population living in a food-insecure household, moderate or severe",
                                                
                                                "Percent of the population living in a household with moderate food insecurity",
                                                
                                                " Percent of the population living in a household with severe food insecurity"
                                                
                                                
                                            )
                             )
                             
                         ),
                         
                         
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.dim5 == 'Local community'",
                             
                             selectizeInput("dimCommunity5",
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
                             condition = "input.dim5 == 'Education, training and skills'",
                             
                          
                             selectizeInput("Edu5",
                                            label = "Education, training and skills Indicators",
                                            choices = list("Population with no certificate, diploma or degree",
                                                           
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
                                                           
                                                           "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"),
                                            selected = NULL
                             ),
                             
                             
                             
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.dim5 == 'Health and wellbeing'",
                             
                             selectizeInput("dimHealth5",
                                            label = "Indicators",
                                            choices = list(
                                                " Percent of the population reporting very good or excellent general health",
                                                
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
                             
                             selectizeInput("dimTrust5",
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
                             
                             selectizeInput("dimIncome5",
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
                             
                             selectizeInput("dimSocial5",
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
                             
                             
                             selectizeInput("disind5",
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
                         
                         
                         
                         
                         
                         # Add the js code and button to the page
                         #extendShinyjs(text = jsResetCode, functions = "reset"),
                         actionButton("reset_button", "Reset Page"),
                     ),
                     
                     #Main Panel for displaying graphs
                     mainPanel(
                         conditionalPanel(
                             
                             condition = "input.dim5 == 'Participation in the Labour Market'",
                             
                             br(),
                             br(),
                             plotlyOutput("sBarOverGEO", inline = TRUE, width = 700, height = 500),
                             br(),
                             helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011.")
                             
                         )
                         
                         
                     )
                 )
                 
        ),
        
        # Scatter plot and Line Graph Tab
        tabPanel("Time Series Analysis", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         
                         
                         
                         selectizeInput("spdim6",
                                        label = "Theme",
                                        choices = list("Participation in the Labour Market", 
                                                       "Civic engagement and political participation",
                                                       "Representation in decision-making positions",
                                                       "Basic needs and housing",
                                                       
                                                       "Health and wellbeing",
                                                       "Education, training and skills",
                                                       "Income and wealth",
                                                       "Social connections and personnal networks",
                                                       
                                                       "Local community",
                                                       "Public services and institutions",
                                                       "Discrimination and victimization"),
                                        selected = "Discrimination and victimization",
                         ),
                         
                         
                         
                         conditionalPanel(
                             condition = "input.spdim6 == 'Participation in the Labour Market'",
                             
                             
                             selectizeInput("LM6",
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
                             
                             
                             
                             
                             
                             
                             
                             selectizeInput("OverGeoLINE",
                                            label = "Geography",
                                            choices = unique(OverQualDT$Geography),
                                            
                             ),
                             
                             
                             selectizeInput("OverLocationLINE",
                                            label = "Location of Study",
                                            choices = unique(OverQualDT$Location),
                                            
                                            
                             ),
                             
                             selectizeInput("OverDegreeLINE",
                                            label = "Highest certificate, diploma or degree",
                                            choices = unique(OverQualDT$Degree),
                                            
                                            
                             ),
                             
                             
                             selectizeInput("OverSexLINE",
                                            label = "Sex",
                                            choices = unique(OverQualDT$Sex),
                                            selected = "Males"
                                            
                             ),
                             
                             selectizeInput("OverVMLINE",
                                            label = "Visible minority status",
                                            choices = list("Total - Visible minority","Visible minority population","South Asian,Chinese","Black","Filipino","Latin American","Arab","Southeast Asian","West Asian","Korean","Japanese","Visible minority n.i.e","Multiple visible minorities","Not a visible minority"),
                                            
                                            
                             ),
                             
                             
                             
                             
                             selectizeInput("OverAgeLINE",
                                            label = "Age Group",
                                            choices = unique(OverQualDT$Age),
                                            selected = "Total - Age"
                             ),
                             
                             selectizeInput("OverImmLINE",
                                            label = "Groups designated by Immigration and Generational Status",
                                            choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
                                            
                             ),
                             
                             selectizeInput("OverLangLINE",
                                            label = "Language",
                                            choices = unique(OverQualDT$'Language'),
                                            
                             )
                             
                             
                         ),
                         
                         
                         
                         
                         conditionalPanel(
                             condition = "input.spdim6 == 'Discrimination and victimization'",
                             selectizeInput("LM6",
                                            label = "Indicators",
                                            choices = list("Hate Crime"),
                                            selected = "Hate Crime",
                             ),
                             
                             
                             selectizeInput("motivation2",
                                            label = "Motivation",
                                            choices = list('Race or ethnicity', 'Total police-reported hate crime'),
                                            selected = "Race or ethnicity",
                                            
                             ),
                             
                             
                             
                             
                             conditionalPanel(
                                 condition = "input.motivation2 == 'Race or ethnicity'",
                                 
                                 selectizeInput("VisMi2",
                                                label = "Race or ethnicity and other characteristics",
                                                choices = list('Black', 'South Asian', 'East or Southeast Asian', 'Arab or West Asian', 'White', 'Indigenous', 'Multiple races or ethnicities', 'Other Race or ethnicity', 'Unknown Race or ethnicity' ),
                                                selected = "Black",
                                                multiple = TRUE
                                 ),
                                 
                                 
                             ),
                             
                             conditionalPanel(
                                 condition = "input.motivation2 == 'Total police-reported hate crime'",
                                 
                                 selectizeInput("VisMi",
                                                label = "Police Reported Hate Crimes",
                                                choices = list('Race or ethnicity', 'Religion', 'Sexual orientation', 'Language', 'Disabilitiy', 'Sex', 'Age', 'Unknown motivation'),
                                                selected = "Religion",
                                                multiple = TRUE
                                 ),
                                 
                                 
                             ),
                             
                             
                             
                         ),
                         
                         conditionalPanel(
                             
                             condition = "input.spdim6 == 'Civic engagement and political participation'",
                             
                             selectizeInput("dimCivilEngagement6",
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
                             
                             selectizeInput("Rep6",
                                            label = "Indicators",
                                            choices = list("Percent of workers in all management occupations",
                                                           
                                                           "Percent of workers in senior management occupations",
                                                           
                                                           "Percent of workers in specialized middle management occupations",
                                                           
                                                           "Percent of workers in other middle management occupations"
                                            )
                             ),
                             
                         ),
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.spdim6 == 'Basic needs and housing'",
                             
                             selectizeInput("dimBasicNeeds6",
                                            label = "Indicators",
                                            choices = list(
                                                "Percent of the population living in a dwelling owned by one member of the household ",
                                                
                                                "Percent of the population living in core need household",
                                                
                                                "Percent of the population living in suitable housing",
                                                
                                                "Percent of the population living in an affordable housing",
                                                
                                                "Percent of the population living in a food-secure household",
                                                
                                                " Percent of the population living in a household with marginal food security",
                                                
                                                "Percent of the population living in a food-insecure household, moderate or severe",
                                                
                                                "Percent of the population living in a household with moderate food insecurity",
                                                
                                                " Percent of the population living in a household with severe food insecurity"
                                                
                                                
                                            )
                             )
                             
                         ),
                         
                         
                         
                         
                         
                         conditionalPanel(
                             
                             condition = "input.spdim6 == 'Local community'",
                             
                             selectizeInput("dimCommunity4",
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
                             
                             selectizeInput("dimHealth6",
                                            label = "Indicators",
                                            choices = list(
                                                " Percent of the population reporting very good or excellent general health",
                                                
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
                             
                             
                             selectizeInput("Edu6",
                                            label = "Education Indicators",
                                            choices = list( "Population with no certificate, diploma or degree",
                                                            
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
                             
                             selectizeInput("dimTrust6",
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
                             
                             selectizeInput("dimIncome6",
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
                             
                             selectizeInput("dimSocial6",
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
                         
                         
                         
                         
                         
                         
                         
                         # Add the js code and button to the page
                         # extendShinyjs(text = jsResetCode, functions = "reset"),
                         actionButton("reset_button", "Reset Page"),
                     ),
                     
                     mainPanel(
                         
                         conditionalPanel(
                             condition = "input.spdim6 == 'Discrimination and victimization' & input.motivation2 == 'Total police-reported hate crime'",
                             h2("Time Series Analysis"),
                             br(),
                             br(),
                             plotlyOutput("ltwograph", inline = TRUE, width = 700, height = 500),
                             p("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             
                         ),
                         
                         
                         conditionalPanel(
                             condition = "input.spdim6 == 'Discrimination and victimization' & input.motivation2 == 'Race or ethnicity'",
                             h2("Time Series Analysis"),
                             br(),
                             br(),
                             plotlyOutput("lgraph", inline = TRUE, width = 700, height = 500),
                             p("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                             
                         ),
                         
                         conditionalPanel(
                             condition = "input.spdim6 == 'Participation in the Labour Market'",
                             h2("Time Series Analysis"),
                             br(),
                             br(),
                             plotlyOutput("lthreegraph", inline = TRUE, width = 700, height = 500),
                             p("Source: ----")
                             
                         ),
                         
                         
                         
                     )
                 )
        )
    )
)


# Define server logic ---------------------------------------------------------------
server <- function(input, output) {
    
    # Method to call the reset function
    observeEvent(input$reset_button, {js$reset()})
    
    # Reactive values ----------------------------------------------------------
    # This reactive filters for sex to build the first column graph on the first tab

     #Leo
    
    filtered_civic4 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecImm2,`Indicator` == 'Voted in last provincial election' )
        
        
        return(newDT)
    })
    
    filtered_civicAge4 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecAge2,`Indicator` == 'Voted in last provincial election')
        
        
        return(newDT)
    })
    
    filtered_civicSex4 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecSex2,`Indicator` == 'Voted in last provincial election')
        
        
        return(newDT)
    })
    
    filtered_civicGen4 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecGen2,`Indicator` == 'Voted in last provincial election')
        
        
        return(newDT)
    })
    
    
    filtered_civicLang4 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecLang2,`Indicator` == 'Voted in last provincial election')
        
        
        return(newDT)
    })
    
    filtered_civicEdu4 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecEdu2,`Indicator` == 'Voted in last provincial election')
        
        
        return(newDT)
    })
    
    
   
    
    ####
    filtered_civic3 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecImm2,`Indicator` == 'Voted in last municipal election' )
        
        
        return(newDT)
    })
    
    filtered_civicAge3 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecAge2,`Indicator` == 'Voted in last municipal election')
        
        
        return(newDT)
    })
    
    filtered_civicSex3 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecSex2,`Indicator` == 'Voted in last municipal election')
        
        
        return(newDT)
    })
    
    filtered_civicGen3 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecGen2,`Indicator` == 'Voted in last municipal election')
        
        
        return(newDT)
    })
    
    
    filtered_civicLang3 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecLang2,`Indicator` == 'Voted in last municipal election')
        
        
        return(newDT)
    })
    
    filtered_civicEdu3 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecEdu2,`Indicator` == 'Voted in last municipal election')
        
        
        return(newDT)
    })
    
    ####
    filtered_civic2 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecImm2,`Indicator` == 'Voted in last federal election' )
        
        
        return(newDT)
    })
    
    filtered_civicAge2 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecAge2,`Indicator` == 'Voted in last federal election')
        
        
        return(newDT)
    })
    
    filtered_civicSex2 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecSex2,`Indicator` == 'Voted in last federal election')
        
        
        return(newDT)
    })
    
    filtered_civicGen2 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecGen2,`Indicator` == 'Voted in last federal election')
        
        
        return(newDT)
    })
    
    
    filtered_civicLang2 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecLang2,`Indicator` == 'Voted in last federal election')
        
        
        return(newDT)
    })
    
    filtered_civicEdu2 <- reactive({
        
        newDT <- civicDT2 %>%
            
            filter(`VisMin` %in% input$VM170, `Year` %in% input$CivicYear2, `Geography` %in% input$CivicGeo2, `Confidence` %in% input$CivicConfidence2,`Characteristic` %in% input$civicCharSpecEdu2,`Indicator` == 'Voted in last federal election')
        
        
        return(newDT)
    })
    
        ####
    filtered_civic1 <- reactive({
        
        newDT <- civicDT %>%
            
            filter(`VisMin` %in% input$VM160, `Year` %in% input$CivicYear1, `Geography` %in% input$CivicGeo1, `Confidence` %in% input$CivicConfidence1,`Characteristic` %in% input$civicCharSpecImm1,`Indicator` == 'Member or participant of at least one group, organization or association')
        
        
        return(newDT)
    })
    
    filtered_civicAge1 <- reactive({
        
        newDT <- civicDT %>%
            
            filter(`VisMin` %in% input$VM160, `Year` %in% input$CivicYear1, `Geography` %in% input$CivicGeo1, `Confidence` %in% input$CivicConfidence1,`Characteristic` %in% input$civicCharSpecAge1,`Indicator` == 'Member or participant of at least one group, organization or association')
        
        
        return(newDT)
    })
    
    filtered_civicSex1 <- reactive({
        
        newDT <- civicDT %>%
            
            filter(`VisMin` %in% input$VM160, `Year` %in% input$CivicYear1, `Geography` %in% input$CivicGeo1, `Confidence` %in% input$CivicConfidence1,`Characteristic` %in% input$civicCharSpecSex1,`Indicator` == 'Member or participant of at least one group, organization or association')
        
        
        return(newDT)
    })
    
    filtered_civicGen1 <- reactive({
        
        newDT <- civicDT %>%
            
            filter(`VisMin` %in% input$VM160, `Year` %in% input$CivicYear1, `Geography` %in% input$CivicGeo1, `Confidence` %in% input$CivicConfidence1,`Characteristic` %in% input$civicCharSpecGen1,`Indicator` == 'Member or participant of at least one group, organization or association')
        
        
        return(newDT)
    })
    
    
    filtered_civicLang1 <- reactive({
        
        newDT <- civicDT %>%
            
            filter(`VisMin` %in% input$VM160, `Year` %in% input$CivicYear1, `Geography` %in% input$CivicGeo1, `Confidence` %in% input$CivicConfidence1,`Characteristic` %in% input$civicCharSpecLang1,`Indicator` == 'Member or participant of at least one group, organization or association')
        
        
        return(newDT)
    })
    
    filtered_civicEdu1 <- reactive({
        
        newDT <- civicDT %>%
            
            filter(`VisMin` %in% input$VM160, `Year` %in% input$CivicYear1, `Geography` %in% input$CivicGeo1, `Confidence` %in% input$CivicConfidence1,`Characteristic` %in% input$civicCharSpecEdu1,`Indicator` == 'Member or participant of at least one group, organization or association')
        
        
        return(newDT)
    })
    
    
    
    
 
    
    #End #####
    filtered_Conf12 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Strong sense of belonging to Canada' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge12 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Strong sense of belonging to Canada' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex12 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Strong sense of belonging to Canada' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen12 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Strong sense of belonging to Canada' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang12 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Strong sense of belonging to Canada' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu12 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Strong sense of belonging to Canada' )
        
        
        return(newDT)
    })
    
    
    
     #End
    filtered_Conf11 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Strong sense of belonging to the province' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge11 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Strong sense of belonging to the province' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex11 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Strong sense of belonging to the province' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen11 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Strong sense of belonging to the province' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang11 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Strong sense of belonging to the province' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu11 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Strong sense of belonging to the province' )
        
        
        return(newDT)
    })
    
    
     #End
    
    filtered_Conf10 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Strong sense of belonging to the town or city' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge10 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Strong sense of belonging to the town or city' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex10 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Strong sense of belonging to the town or city' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen10 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Strong sense of belonging to the town or city' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang10 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Strong sense of belonging to the town or city' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu10 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Strong sense of belonging to the town or city' )
        
        
        return(newDT)
    })
    
    
     #End
    
    
    filtered_Conf9 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Strong sense of belonging to the local community' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge9 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Strong sense of belonging to the local community' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex9 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Strong sense of belonging to the local community' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen9 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Strong sense of belonging to the local community' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang9 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Strong sense of belonging to the local community' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu9 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Strong sense of belonging to the local community' )
        
        
        return(newDT)
    })
    
    
    
    #End
    
    filtered_Conf8 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Reported that most people can be trusted in general' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge8 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Reported that most people can be trusted in general' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex8 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Reported that most people can be trusted in general' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen8 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Reported that most people can be trusted in general' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang8 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Reported that most people can be trusted in general' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu8 <- reactive({
        
        newDT <- belongingDT %>%
            
            filter(`VisMin` %in% input$VM401, `Year` %in% input$confYearS, `Geography` %in% input$confGeoS, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Reported that most people can be trusted in general' )
        
        
        return(newDT)
    })
    
    
    #End
   
    
  
    
    filtered_Employ7 <- reactive({
        
        newDT <- employmentDT%>%
            
            filter(`VisMin` %in% input$VM135, `Year` %in% input$EmploymentYear7, `Geography` %in% input$EmploymentGeo7, `Confidence` %in% input$EmploymentConf7,`Characteristic` %in% input$EmploymentChar7,`Indicator` == 'Have access to disability insurance under employment contract' )
        
        
        return(newDT)
    })
    
    filtered_Employ5 <- reactive({
        
        newDT <- employmentDT%>%
            
            filter(`VisMin` %in% input$VM125, `Year` %in% input$EmploymentYear5, `Geography` %in% input$EmploymentGeo5, `Confidence` %in% input$EmploymentConf5,`Characteristic` %in% input$EmploymentChar5,`Indicator` == 'Have access to paid vacation leave under employment contract' )
        
        
        return(newDT)
    })
    
    
    filtered_Employ4 <- reactive({
        
        newDT <- employmentDT%>%
            
            filter(`VisMin` %in% input$VM120, `Year` %in% input$EmploymentYear4, `Geography` %in% input$EmploymentGeo4, `Confidence` %in% input$EmploymentConf4,`Characteristic` %in% input$EmploymentChar4,`Indicator` == 'Employment contract includes at least one type of employment benefits' )
        
        
        return(newDT)
    })
    
    filtered_Employ3 <- reactive({
        
        newDT <- employmentDT%>%
            
            filter(`VisMin` %in% input$VM115, `Year` %in% input$EmploymentYear3, `Geography` %in% input$EmploymentGeo3, `Confidence` %in% input$EmploymentConf3,`Characteristic` %in% input$EmploymentChar3,`Indicator` == 'Have a workplace pension plan' )
        
        
        return(newDT)
    })
    
    
    filtered_Employ2 <- reactive({
        
        newDT <- employmentDT%>%
            
            filter(`VisMin` %in% input$VM110, `Year` %in% input$EmploymentYear2, `Geography` %in% input$EmploymentGeo2, `Confidence` %in% input$EmploymentConf2,`Characteristic` %in% input$EmploymentChar2,`Indicator` == 'Job offers good prospects for career advancement' )
        
        
        return(newDT)
    })
    
    
    filtered_Employ <- reactive({
        
        newDT <- employmentDT%>%
            
            filter(`VisMin` %in% input$VM100, `Year` %in% input$EmploymentYear, `Geography` %in% input$EmploymentGeo, `Confidence` %in% input$EmploymentConf,`Characteristic` %in% input$EmploymentChar,`Indicator` == 'Have access to paid sick leave under employment contract' )
        
        
        return(newDT)
    })
    
    
    
    filtered_Conf <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecImmP,`Indicator` == 'Confidence in the police service' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecAgeP,`Indicator` == 'Confidence in the police service' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenderP,`Indicator` == 'Confidence in the police service' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenP,`Indicator` == 'Confidence in the police service' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecLangP,`Indicator` == 'Confidence in the police service' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecEduP,`Indicator` == 'Confidence in the police service' )
        
        
        return(newDT)
    })
    
    
    ##
    
    
    filtered_Conf1 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecImmP,`Indicator` == 'Confidence in Federal Parliament' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge1 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecAgeP,`Indicator` == 'Confidence in Federal Parliament' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex1 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenderP,`Indicator` == 'Confidence in Federal Parliament' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen1 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenP,`Indicator` == 'Confidence in Federal Parliament' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang1 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecLangP,`Indicator` == 'Confidence in Federal Parliament' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu1 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecEduP,`Indicator` == 'Confidence in Federal Parliament' )
        
        
        return(newDT)
    })
    
    
    ##
    
    
    filtered_Conf2 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecImmP,`Indicator` == 'Confidence in the Canadian media' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge2 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecAgeP,`Indicator` == 'Confidence in the Canadian media' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex2 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenderP,`Indicator` == 'Confidence in the Canadian media' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen2 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenP,`Indicator` == 'Confidence in the Canadian media' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang2 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecLangP,`Indicator` == 'Confidence in the Canadian media' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu2 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecEduP,`Indicator` == 'Confidence in the Canadian media' )
        
        
        return(newDT)
    })
    
    
    ##
    
    
    filtered_Conf3 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecImmP,`Indicator` == 'Confidence in the school system' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge3 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecAgeP,`Indicator` == 'Confidence in the school system' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex3 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenderP,`Indicator` == 'Confidence in the school system' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen3 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenP,`Indicator` == 'Confidence in the school system' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang3 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecLangP,`Indicator` == 'Confidence in the school system' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu3 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecEduP,`Indicator` == 'Confidence in the school system' )
        
        
        return(newDT)
    })
    
    
    ##
    
    
    filtered_Conf4 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecImmP,`Indicator` == 'Confidence in the justice system and courts' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge4 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecAgeP,`Indicator` == 'Confidence in the justice system and courts' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex4 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenderP,`Indicator` == 'Confidence in the justice system and courts' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen4 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenP,`Indicator` == 'Confidence in the justice system and courts' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang4 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecLangP,`Indicator` == 'Confidence in the justice system and courts' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu4 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecEduP,`Indicator` == 'Confidence in the justice system and courts' )
        
        
        return(newDT)
    })
    
    
    ##
    
    
    
    filtered_Conf5 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecImmP,`Indicator` == 'Confidence in major corporations' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge5 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecAgeP,`Indicator` == 'Confidence in major corporations' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex5 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenderP,`Indicator` == 'Confidence in major corporations' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen5 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenP,`Indicator` == 'Confidence in major corporations' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang5 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecLangP,`Indicator` == 'Confidence in major corporations' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu5 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecEduP,`Indicator` == 'Confidence in major corporations' )
        
        
        return(newDT)
    })
    
    
    ##
    
    
    
    
    filtered_Conf6 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecImmP,`Indicator` == 'Confidence in merchants and local business people' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge6 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecAgeP,`Indicator` == 'Confidence in merchants and local business people' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex6 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenderP,`Indicator` == 'Confidence in merchants and local business people' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen6 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenP,`Indicator` == 'Confidence in merchants and local business people' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang6 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecLangP,`Indicator` == 'Confidence in merchants and local business people' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu6 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecEduP,`Indicator` == 'Confidence in merchants and local business people' )
        
        
        return(newDT)
    })
    
    ####
    filtered_Conf7 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecImmP,`Indicator` == 'Confidence in banks' )
        
        
        return(newDT)
    })
    
    filtered_ConfAge7 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecAgeP,`Indicator` == 'Confidence in banks' )
        
        
        return(newDT)
    })
    
    filtered_ConfSex7 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenderP,`Indicator` == 'Confidence in banks' )
        
        
        return(newDT)
    })
    
    filtered_ConfGen7 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecGenP,`Indicator` == 'Confidence in banks' )
        
        
        return(newDT)
    })
    
    
    filtered_ConfLang7 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecLangP,`Indicator` == 'Confidence in banks' )
        
        
        return(newDT)
    })
    
    filtered_ConfEdu7 <- reactive({
        
        newDT <- confidenceDT %>%
            
            filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidenceP,`Characteristic` %in% input$confCharSpecEduP,`Indicator` == 'Confidence in banks' )
        
        
        return(newDT)
    })
    
    
    
    ##
    
    #Thursday
    
    filtered_Work <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Work2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    filtered_WorkAge <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkAge2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkGender <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkGender2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkGen <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkGen2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkLang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkLang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkEdu <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_WorkEdu2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    
    #end
    
    filtered_Bank <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Bank2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    filtered_BankAge <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankAge2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankSex <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankSex2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankGen <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankGen2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankLang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankLang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankEdu <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_BankEdu2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    
    
    
    #end
    filtered_Class <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Class2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    filtered_ClassAge <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassAge2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassGender <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassGender2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassGen <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassGen2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassLang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassLang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassEdu <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ClassEdu2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    #end
    
    filtered_Pol <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Pol2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    filtered_PolAge <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolAge2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolSex <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolSex2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolGen <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolGen2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolLang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolLang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolEdu <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_PolEdu2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    
    #end
    
    filtered_Lang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Lang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    filtered_LangAge <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangAge2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangSex <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangSex2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangGen <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangGen2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangLang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangLang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangEdu <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_LangEdu2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    
    
    #end
    
    filtered_Rel <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Rel2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    filtered_RelAge <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelAge2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelSex <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelSex2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelGen <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelGen2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelLang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelLang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelEdu <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_RelEdu2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    
    
    #end
    
    filtered_Col <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Col2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    filtered_ColAge <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ColAge2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ColSex <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ColSex2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ColGen <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })

    
    filtered_ColGen2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ColLang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ColLang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ColEdu <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_ColEdu2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    #end
    filtered_Cov <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Cov2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    
    filtered_CovAge <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovAge2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovSex <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovSex2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovGen <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovGen2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovLang <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovLang2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovEdu <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovEdu21 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    ##
    
    
    
    
    
    filtered_Cov1 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_Cov21 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic'  )
        
        
        return(newDT)
    })
    
    
    
    filtered_CovAge1 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovAge21 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovSex1 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender,`Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovSex21 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGender, `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovGen1 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovGen21 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovLang1 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovLang21 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovEdu1 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    filtered_CovEdu2 <- reactive({
        
        newDT <- discriminationDT %>%
            
            filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic' )
        
        
        return(newDT)
    })
    
    
    ###
    #Today
    
    filtered_rep3VM <- reactive({
        
        newDT <- representationDT %>%
            filter(`Immigration` %in% input$RepImm,`Degree` %in% input$RepDegree,`VisMin` %in% input$VM145, `Geography` %in% input$RepGeo,`Year` %in% input$RepYear,`Age` %in% input$RepAgeLang,`Sex` %in% input$RepSex,`Indicator` == 'Workers in all management occupations' )
        
        
        return(newDT)
    })
    
    
    filtered_rep2VM <- reactive({
        
        newDT <- representationDT %>%
            filter(`Immigration` %in% input$RepImm,`Degree` %in% input$RepDegree,`VisMin` %in% input$VM145, `Geography` %in% input$RepGeo,`Year` %in% input$RepYear,`Age` %in% input$RepAgeLang,`Sex` %in% input$RepSex, `Indicator` == 'Workers in senior management occupations' )
        
        
        return(newDT)
    })
    
    
    filtered_rep1VM <- reactive({
        
        newDT <- representationDT %>%
            filter(`Immigration` %in% input$RepImm,`Degree` %in% input$RepDegree,`VisMin` %in% input$VM145, `Geography` %in% input$RepGeo,`Year` %in% input$RepYear,`Age` %in% input$RepAgeLang,`Sex` %in% input$RepSexc,`Indicator` == 'Workers in specialized middle management occupations' )
        
        
        return(newDT)
    })
    
    
    #End
    filtered_youthVM <- reactive({
        
        newDT <- youthDT %>%
            filter(`Immigration` %in% input$youthImm,`Language` %in% input$youthLang,`VisMin` %in% input$VM140, `Geography` %in% input$youthGeo,`Year` %in% input$youthYear,`Age` %in% input$youthAge,`Sex` %in% input$youthSex)
        
        
        return(newDT)
    })
    
    
    #End
    filtered_OverVM <- reactive({
        
        newDT <- OverQualDT %>%
            filter(`Immigration` %in% input$OverImm,`Degree` %in% input$OverDegree, `Language` %in% input$OverLang,`Location` %in% input$OverLocation, `VisMin` %in% input$VM20, `Geography` %in% input$OverGeo,`Year` %in% input$OverYear,`Age` %in% input$OverAge,`Sex` %in% input$OverSex)
        
        
        return(newDT)
    })
    
    
    filtered_OverGEO <- reactive({
        
        newDT <- OverQualDT %>%
            filter(`Immigration` %in% input$OverImmGEO,`Degree` %in% input$OverDegreeGEO, `Language` %in% input$OverLangGEO,`Location` %in% input$OverLocationGEO, `VisMin` %in% input$OverVMGEO, `Geography` %in% input$VM23,`Year` %in% input$OverYearGEO,`Age` %in% input$OverAgeGEO,`Sex` %in% input$OverSexGEO)
        
        
        return(newDT)
    })
    
    filtered_OverIS <- reactive({
        
        newDT <- OverQualDT %>%
            filter(`Immigration` %in% input$VM21,`Degree` %in% input$OverDegreeIS, `Language` %in% input$OverLangIS,`Location` %in% input$OverLocationIS, `VisMin` %in% input$OverVMIS, `Geography` %in% input$OverGeoIS,`Year` %in% input$OverYearIS,`Age` %in% input$OverAgeIS,`Sex` %in% input$OverSexIS)
        
        
        return(newDT)
    })
    
    
    filtered_OverSX <- reactive({
        
        newDT <- OverQualDT %>%
            filter(`Immigration` %in% input$OverImmSX,`Degree` %in% input$OverDegreeSX, `Language` %in% input$OverLangSX,`Location` %in% input$OverLocationSX, `VisMin` %in% input$OverVMSX, `Geography` %in% input$OverGeoSX,`Year` %in% input$OverYearSX,`Age` %in% input$OverAgeSX,`Sex` %in% input$VM22)
        
        
        return(newDT)
    })
    
    
    filtered_educationSX1 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11, `Indicators` == "Population with bachelor’s degree")
        
        
        return(newDT)
    })
    
    filtered_educationSX2 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11, `Indicators` == "Population with no certificate, diploma or degree")
        
        
        return(newDT)
    })
    
    filtered_educationSX3 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11,`Indicators` == "Population with high school diploma or equivalency certificate")
        
        
        return(newDT)
    })
    
    
    filtered_educationSX4 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11, `Indicators` == "Population with postsecondary certificate or diploma below bachelor level")
        
        
        return(newDT)
    })
    
    
    filtered_educationSX5 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11, `Indicators` == "Population with bachelor’s degree or above")
        
        
        return(newDT)
    })
    
    
    filtered_educationSX6 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11,`Indicators` == "Population with master’s degree or earned doctorate")
        
        
        return(newDT)
    })
    filtered_educationVM1 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with bachelor’s degree")
        
        
        return(newDT)
    })
    
    filtered_educationVM2 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with no certificate, diploma or degree")
        
        
        return(newDT)
    })
    
    filtered_educationVM3 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with high school diploma or equivalency certificate")
        
        
        return(newDT)
    })
    
    
    filtered_educationVM4 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with postsecondary certificate or diploma below bachelor level")
        
        
        return(newDT)
    })
    
    
    filtered_educationVM5 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with bachelor’s degree or above")
        
        
        return(newDT)
    })
    
    
    filtered_educationVM6 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with master’s degree or earned doctorate")
        
        
        return(newDT)
    })
    
    filtered_education1 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with bachelor’s degree")
        
        
        return(newDT)
    })
    
    filtered_education2 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with no certificate, diploma or degree")
        
        
        return(newDT)
    })
    
    
    filtered_education3 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with high school diploma or equivalency certificate")
        
        
        return(newDT)
    })
    
    filtered_education4 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with postsecondary certificate or diploma below bachelor level")
        
        
        return(newDT)
    })
    filtered_education5 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with bachelor’s degree or above")
        
        
        return(newDT)
    })
    filtered_education6 <- reactive({
        
        newDT <- educationDT %>%
            filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with master’s degree or earned doctorate")
        
        
        return(newDT)
    })
    
    
    #This is a reactive for  police reported hate crime by VisMin
    filtered_immdisc <- reactive({
        
        req(input$VM7)
        
        newDT <- polData %>%
            #pivot_wider(names_from = "Year", names_prefix = "yr", values_from = "Value") %>% 
            
            filter(Motivation %in% input$VM7, Year %in% input$disYear)
        
        return(newDT)
        
    })
    
    
    filtered_imm2disc <- reactive({
        
        req(input$VM8)
        
        newDT <- polData %>%
            #pivot_wider(names_from = "Year", names_prefix = "yr", values_from = "Value") %>% 
            
            filter(Motivation %in% input$VM8, Year %in% input$disYear)
        
        return(newDT)
        
    })
    
    
    
    
    
    
    
    
    
    
    # This reactive filters the line plot data
    filtered_lineData <- reactive({
        
        # Require Sex, Age, Generation Status, VisMin
        req(input$VisMi2)
        
        # Filter values
        newDT <- polData %>%
            filter(
                `Motivation` %in% input$VisMi2)
        
        
        # Pivot by VisMin
        newDT <- pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
        
        return(newDT)
    })
    
    
    filtered_linethreeData <- reactive({
        
        # Require Sex, Age, Generation Status, VisMin
        req(input$VisMi2)
        
        # Filter values
        newDT <- polData %>%
            filter(`Immigration` %in% input$OverImmLINE,`Degree` %in% input$OverDegreeLINE, `Language` %in% input$OverLangLINE,`Location` %in% input$OverLocationLINE, `VisMin` %in% input$OverVMLINE, `Geography` %in% input$OverGeoLINE,`Age` %in% input$OverAgeLINE,`Sex` %in% input$OverSexLINE)
        
        
        
        # Pivot by VisMin
        newDT <- pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
        
        return(newDT)
    })
    
    
    
    
    # This reactive filters the line plot data
    filtered_linetwoData <- reactive({
        
        # Require Sex, Age, Generation Status, VisMin
        req(input$VisMi)
        
        # Filter values
        newDT <- polData %>%
            filter(
                `Motivation` %in% input$VisMi)
        
        
        # Pivot by VisMin
        newDT <- pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
        
        return(newDT)
    })
    
    
    
    # Output ---------------------------------------------------
    
    #Leo
    
    output$sBarCivic3 <- renderPlotly({
        
        req(filtered_civic2())
        
        
        fig <- plot_ly(filtered_civic2(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Voted in last federal election ",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarCivicAge3<- renderPlotly({
        
        req(filtered_civicAge2())
        
        
        fig <- plot_ly(filtered_civicAge2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Voted in last federal election ",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarCivicSex3 <- renderPlotly({
        
        req(filtered_civicSex2())
        
        
        fig <- plot_ly(filtered_civicSex2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Voted in last federal election ",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCivicGen3 <- renderPlotly({
        
        req(filtered_civicGen2())
        
        
        fig <- plot_ly(filtered_civicGen2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Voted in last federal election ",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarCivicLang3 <- renderPlotly({
        
        req(filtered_civicLang2())
        
        
        fig <- plot_ly(filtered_civicLang2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Voted in last federal election ",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCivicEdu3 <- renderPlotly({
        
        req(filtered_civicEdu2())
        
        
        fig <- plot_ly(filtered_civicEdu2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Voted in last federal election ",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    
    #####
    output$sBarCivic1 <- renderPlotly({
        
        req(filtered_civic1())
        
        
        fig <- plot_ly(filtered_civic1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Member or participant of at least one group, organization or association",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarCivicAge1<- renderPlotly({
        
        req(filtered_civicAge1())
        
        
        fig <- plot_ly(filtered_civicAge1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Member or participant of at least one group, organization or association",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarCivicSex1 <- renderPlotly({
        
        req(filtered_civicSex1())
        
        
        fig <- plot_ly(filtered_civicSex1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Member or participant of at least one group, organization or association",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCivicGen1 <- renderPlotly({
        
        req(filtered_civicGen1())
        
        
        fig <- plot_ly(filtered_civicGen1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Member or participant of at least one group, organization or association",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarCivicLang1 <- renderPlotly({
        
        req(filtered_civicLang1())
        
        
        fig <- plot_ly(filtered_civicLang1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Member or participant of at least one group, organization or association",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCivicEdu1 <- renderPlotly({
        
        req(filtered_civicEdu1())
        
        
        fig <- plot_ly(filtered_civicEdu1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Member or participant of at least one group, organization or association",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
   
    
    
    
    #end
    output$sBarConf12 <- renderPlotly({
        
        req(filtered_Conf12())
        
        
        fig <- plot_ly(filtered_Conf12(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to Canada",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge12<- renderPlotly({
        
        req(filtered_ConfAge12())
        
        
        fig <- plot_ly(filtered_ConfAge12(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to Canada",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex12 <- renderPlotly({
        
        req(filtered_ConfSex12())
        
        
        fig <- plot_ly(filtered_ConfSex12(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to Canada",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen12 <- renderPlotly({
        
        req(filtered_ConfGen12())
        
        
        fig <- plot_ly(filtered_ConfGen12(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to Canada",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang12 <- renderPlotly({
        
        req(filtered_ConfLang12())
        
        
        fig <- plot_ly(filtered_ConfLang12(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to Canada",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu12 <- renderPlotly({
        
        req(filtered_ConfEdu12())
        
        
        fig <- plot_ly(filtered_ConfEdu12(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to Canada",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    #End
    output$sBarConf11 <- renderPlotly({
        
        req(filtered_Conf11())
        
        
        fig <- plot_ly(filtered_Conf11(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the province",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge11<- renderPlotly({
        
        req(filtered_ConfAge11())
        
        
        fig <- plot_ly(filtered_ConfAge11(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the province",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex11 <- renderPlotly({
        
        req(filtered_ConfSex11())
        
        
        fig <- plot_ly(filtered_ConfSex11(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the province",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen11 <- renderPlotly({
        
        req(filtered_ConfGen11())
        
        
        fig <- plot_ly(filtered_ConfGen11(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the province",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang11 <- renderPlotly({
        
        req(filtered_ConfLang11())
        
        
        fig <- plot_ly(filtered_ConfLang11(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the province",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu11 <- renderPlotly({
        
        req(filtered_ConfEdu11())
        
        
        fig <- plot_ly(filtered_ConfEdu11(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the province",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    #End
    output$sBarConf10 <- renderPlotly({
        
        req(filtered_Conf10())
        
        
        fig <- plot_ly(filtered_Conf10(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the town or city",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge10 <- renderPlotly({
        
        req(filtered_ConfAge10())
        
        
        fig <- plot_ly(filtered_ConfAge10(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the town or city",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex10 <- renderPlotly({
        
        req(filtered_ConfSex10())
        
        
        fig <- plot_ly(filtered_ConfSex10(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the town or city",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen10 <- renderPlotly({
        
        req(filtered_ConfGen10())
        
        
        fig <- plot_ly(filtered_ConfGen10(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the town or city",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang10 <- renderPlotly({
        
        req(filtered_ConfLang10())
        
        
        fig <- plot_ly(filtered_ConfLang10(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the town or city",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu10 <- renderPlotly({
        
        req(filtered_ConfEdu10())
        
        
        fig <- plot_ly(filtered_ConfEdu10(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the town or city",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    #End
    
    
    output$sBarConf9 <- renderPlotly({
        
        req(filtered_Conf9())
        
        
        fig <- plot_ly(filtered_Conf9(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the local community",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge9 <- renderPlotly({
        
        req(filtered_ConfAge9())
        
        
        fig <- plot_ly(filtered_ConfAge9(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the local community",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex9 <- renderPlotly({
        
        req(filtered_ConfSex9())
        
        
        fig <- plot_ly(filtered_ConfSex9(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the local community",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen9 <- renderPlotly({
        
        req(filtered_ConfGen9())
        
        
        fig <- plot_ly(filtered_ConfGen9(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the local community",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang9 <- renderPlotly({
        
        req(filtered_ConfLang9())
        
        
        fig <- plot_ly(filtered_ConfLang9(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the local community",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu9 <- renderPlotly({
        
        req(filtered_ConfEdu9())
        
        
        fig <- plot_ly(filtered_ConfEdu9(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Strong sense of belonging to the local community",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    #End
    
    output$sBarConf8 <- renderPlotly({
        
        req(filtered_Conf8())
        
        
        fig <- plot_ly(filtered_Conf8(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Reported that most people can be trusted in general",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge8 <- renderPlotly({
        
        req(filtered_ConfAge8())
        
        
        fig <- plot_ly(filtered_ConfAge8(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Reported that most people can be trusted in general",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex8 <- renderPlotly({
        
        req(filtered_ConfSex8())
        
        
        fig <- plot_ly(filtered_ConfSex8(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Reported that most people can be trusted in general",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen8 <- renderPlotly({
        
        req(filtered_ConfGen8())
        
        
        fig <- plot_ly(filtered_ConfGen8(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Reported that most people can be trusted in general",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang8 <- renderPlotly({
        
        req(filtered_ConfLang8())
        
        
        fig <- plot_ly(filtered_ConfLang8(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Reported that most people can be trusted in general",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu8 <- renderPlotly({
        
        req(filtered_ConfEdu8())
        
        
        fig <- plot_ly(filtered_ConfEdu8(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Reported that most people can be trusted in general",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    #
    output$sBarConf1 <- renderPlotly({
        
        req(filtered_Conf1())
        
        
        fig <- plot_ly(filtered_Conf1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge1 <- renderPlotly({
        
        req(filtered_ConfAge1())
        
        
        fig <- plot_ly(filtered_ConfAge1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex1 <- renderPlotly({
        
        req(filtered_ConfSex1())
        
        
        fig <- plot_ly(filtered_ConfSex1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen1 <- renderPlotly({
        
        req(filtered_ConfGen1())
        
        
        fig <- plot_ly(filtered_ConfGen1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang1 <- renderPlotly({
        
        req(filtered_ConfLang1())
        
        
        fig <- plot_ly(filtered_ConfLang1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu1 <- renderPlotly({
        
        req(filtered_ConfEdu1())
        
        
        fig <- plot_ly(filtered_ConfEdu1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    #
    output$employmentPlot7<- renderPlotly({
        
        req(filtered_Employ7())
        
        
        fig <- plot_ly(filtered_Employ7(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Have access to disability insurance under employment contract",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$employmentPlot6<- renderPlotly({
        
        req(filtered_Employ6())
        
        
        fig <- plot_ly(filtered_Employ6(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Match between education and employment",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$employmentPlot5<- renderPlotly({
        
        req(filtered_Employ5())
        
        
        fig <- plot_ly(filtered_Employ5(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Have access to paid vacation leave under employment contract",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$employmentPlot4<- renderPlotly({
        
        req(filtered_Employ4())
        
        
        fig <- plot_ly(filtered_Employ4(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Employment contract includes at least one type of employment benefits",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$employmentPlot3<- renderPlotly({
        
        req(filtered_Employ3())
        
        
        fig <- plot_ly(filtered_Employ3(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Have a workplace pension plan",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$employmentPlot2<- renderPlotly({
        
        req(filtered_Employ2())
        
        
        fig <- plot_ly(filtered_Employ2(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Job offers good prospects for career advancement",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$employmentPlot<- renderPlotly({
        
        req(filtered_Employ())
        
        
        fig <- plot_ly(filtered_Employ(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "Have access to paid sick leave under employment contract",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    output$sBarConf <- renderPlotly({
        
        req(filtered_Conf())
        
        
        fig <- plot_ly(filtered_Conf(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge <- renderPlotly({
        
        req(filtered_ConfAge())
        
        
        fig <- plot_ly(filtered_ConfAge(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex <- renderPlotly({
        
        req(filtered_ConfSex())
        
        
        fig <- plot_ly(filtered_ConfSex(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen <- renderPlotly({
        
        req(filtered_ConfGen())
        
        
        fig <- plot_ly(filtered_ConfGen(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang <- renderPlotly({
        
        req(filtered_ConfLang())
        
        
        fig <- plot_ly(filtered_ConfLang(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu <- renderPlotly({
        
        req(filtered_ConfEdu())
        
        
        fig <- plot_ly(filtered_ConfEdu(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConf1 <- renderPlotly({
        
        req(filtered_Conf1())
        
        
        fig <- plot_ly(filtered_Conf1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge1 <- renderPlotly({
        
        req(filtered_ConfAge1())
        
        
        fig <- plot_ly(filtered_ConfAge1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex1 <- renderPlotly({
        
        req(filtered_ConfSex1())
        
        
        fig <- plot_ly(filtered_ConfSex1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen1 <- renderPlotly({
        
        req(filtered_ConfGen1())
        
        
        fig <- plot_ly(filtered_ConfGen1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang1 <- renderPlotly({
        
        req(filtered_ConfLang1())
        
        
        fig <- plot_ly(filtered_ConfLang1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu1 <- renderPlotly({
        
        req(filtered_ConfEdu1())
        
        
        fig <- plot_ly(filtered_ConfEdu1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    ##
    
    output$sBarConf2 <- renderPlotly({
        
        req(filtered_Conf2())
        
        
        fig <- plot_ly(filtered_Conf2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the Canadian media",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge2 <- renderPlotly({
        
        req(filtered_ConfAge2())
        
        
        fig <- plot_ly(filtered_ConfAge2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the Canadian media",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex2 <- renderPlotly({
        
        req(filtered_ConfSex2())
        
        
        fig <- plot_ly(filtered_ConfSex2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the Canadian media",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen2 <- renderPlotly({
        
        req(filtered_ConfGen2())
        
        
        fig <- plot_ly(filtered_ConfGen2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the Canadian media",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang2 <- renderPlotly({
        
        req(filtered_ConfLang2())
        
        
        fig <- plot_ly(filtered_ConfLang2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the Canadian media",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu2 <- renderPlotly({
        
        req(filtered_ConfEdu2())
        
        
        fig <- plot_ly(filtered_ConfEdu2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the Canadian media",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    ##
    
    output$sBarConf3 <- renderPlotly({
        
        req(filtered_Conf3())
        
        
        fig <- plot_ly(filtered_Conf3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge3 <- renderPlotly({
        
        req(filtered_ConfAge3())
        
        
        fig <- plot_ly(filtered_ConfAge3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex3 <- renderPlotly({
        
        req(filtered_ConfSex3())
        
        
        fig <- plot_ly(filtered_ConfSex3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen3 <- renderPlotly({
        
        req(filtered_ConfGen3())
        
        
        fig <- plot_ly(filtered_ConfGen3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang3 <- renderPlotly({
        
        req(filtered_ConfLang3())
        
        
        fig <- plot_ly(filtered_ConfLang3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu3 <- renderPlotly({
        
        req(filtered_ConfEdu3())
        
        
        fig <- plot_ly(filtered_ConfEdu3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    ##
    
    
    output$sBarConf4 <- renderPlotly({
        
        req(filtered_Conf4())
        
        
        fig <- plot_ly(filtered_Conf4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the justice system and courts",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge4 <- renderPlotly({
        
        req(filtered_ConfAge4())
        
        
        fig <- plot_ly(filtered_ConfAge4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the justice system and courts",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex4 <- renderPlotly({
        
        req(filtered_ConfSex4())
        
        
        fig <- plot_ly(filtered_ConfSex4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the justice system and courts",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen4 <- renderPlotly({
        
        req(filtered_ConfGen4())
        
        
        fig <- plot_ly(filtered_ConfGen4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the justice system and courts",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang4 <- renderPlotly({
        
        req(filtered_ConfLang4())
        
        
        fig <- plot_ly(filtered_ConfLang4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the justice system and courts",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu4 <- renderPlotly({
        
        req(filtered_ConfEdu4())
        
        
        fig <- plot_ly(filtered_ConfEdu4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in the justice system and courts",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    ##
    
    
    output$sBarConf5 <- renderPlotly({
        
        req(filtered_Conf5())
        
        
        fig <- plot_ly(filtered_Conf5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge5 <- renderPlotly({
        
        req(filtered_ConfAge5())
        
        
        fig <- plot_ly(filtered_ConfAge5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex5 <- renderPlotly({
        
        req(filtered_ConfSex5())
        
        
        fig <- plot_ly(filtered_ConfSex5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen5 <- renderPlotly({
        
        req(filtered_ConfGen5())
        
        
        fig <- plot_ly(filtered_ConfGen5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang5 <- renderPlotly({
        
        req(filtered_ConfLang5())
        
        
        fig <- plot_ly(filtered_ConfLang5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu5 <- renderPlotly({
        
        req(filtered_ConfEdu5())
        
        
        fig <- plot_ly(filtered_ConfEdu4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    ##
    
    output$sBarConf5 <- renderPlotly({
        
        req(filtered_Conf5())
        
        
        fig <- plot_ly(filtered_Conf5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge5 <- renderPlotly({
        
        req(filtered_ConfAge5())
        
        
        fig <- plot_ly(filtered_ConfAge5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex5 <- renderPlotly({
        
        req(filtered_ConfSex5())
        
        
        fig <- plot_ly(filtered_ConfSex5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen5 <- renderPlotly({
        
        req(filtered_ConfGen5())
        
        
        fig <- plot_ly(filtered_ConfGen5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang5 <- renderPlotly({
        
        req(filtered_ConfLang5())
        
        
        fig <- plot_ly(filtered_ConfLang5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu5 <- renderPlotly({
        
        req(filtered_ConfEdu5())
        
        
        fig <- plot_ly(filtered_ConfEdu4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    ##
    
    output$sBarConf6 <- renderPlotly({
        
        req(filtered_Conf6())
        
        
        fig <- plot_ly(filtered_Conf6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in merchants and local business people",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge6 <- renderPlotly({
        
        req(filtered_ConfAge6())
        
        
        fig <- plot_ly(filtered_ConfAge6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in merchants and local business people",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex6 <- renderPlotly({
        
        req(filtered_ConfSex6())
        
        
        fig <- plot_ly(filtered_ConfSex6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in merchants and local business people",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen6 <- renderPlotly({
        
        req(filtered_ConfGen6())
        
        
        fig <- plot_ly(filtered_ConfGen6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in merchants and local business people",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang6 <- renderPlotly({
        
        req(filtered_ConfLang6())
        
        
        fig <- plot_ly(filtered_ConfLang6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in merchants and local business people",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu6 <- renderPlotly({
        
        req(filtered_ConfEdu6())
        
        
        fig <- plot_ly(filtered_ConfEdu6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in merchants and local business people",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    ##
    
    output$sBarConf7 <- renderPlotly({
        
        req(filtered_Conf7())
        
        
        fig <- plot_ly(filtered_Conf7(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in banks",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfAge7 <- renderPlotly({
        
        req(filtered_ConfAge7())
        
        
        fig <- plot_ly(filtered_ConfAge7(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in banks",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfSex7 <- renderPlotly({
        
        req(filtered_ConfSex7())
        
        
        fig <- plot_ly(filtered_ConfSex7(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in banks",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfGen7 <- renderPlotly({
        
        req(filtered_ConfGen7())
        
        
        fig <- plot_ly(filtered_ConfGen7(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in banks",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarConfLang7 <- renderPlotly({
        
        req(filtered_ConfLang7())
        
        
        fig <- plot_ly(filtered_ConfLang7(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in banks",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarConfEdu7 <- renderPlotly({
        
        req(filtered_ConfEdu7())
        
        
        fig <- plot_ly(filtered_ConfEdu7(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "Confidence in banks",font = list(size = 18)),  yaxis = list( title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    ##
    #Discrimination
    
    
    #Today
    
    output$sBarBan <- renderPlotly({
        
        req(filtered_Bank())
        
        
        fig <- plot_ly(filtered_Bank(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBan2 <- renderPlotly({
        
        req(filtered_Bank2())
        
        
        fig <- plot_ly(filtered_Bank2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBanAge <- renderPlotly({
        
        req(filtered_BankAge())
        
        
        fig <- plot_ly(filtered_BankAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBanAge2 <- renderPlotly({
        
        req(filtered_BankAge2())
        
        
        fig <- plot_ly(filtered_BankAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBanSex <- renderPlotly({
        
        req(filtered_BankSex())
        
        
        fig <- plot_ly(filtered_BankSex(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBanSex2 <- renderPlotly({
        
        req(filtered_BankSex2())
        
        
        fig <- plot_ly(filtered_BankSex2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)), 
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBanGen <- renderPlotly({
        
        req(filtered_BankGen())
        
        
        fig <- plot_ly(filtered_BankGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), 
                                                                                                                 title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBanGen2 <- renderPlotly({
        
        req(filtered_BankGen2())
        
        
        fig <- plot_ly(filtered_BankGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarBanLang <- renderPlotly({
        
        req(filtered_BankLang())
        
        
        fig <- plot_ly(filtered_BankLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), 
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBanLang2 <- renderPlotly({
        
        req(filtered_BankLang2())
        
        
        fig <- plot_ly(filtered_BankLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarBanEdu <- renderPlotly({
        
        req(filtered_BankEdu())
        
        
        fig <- plot_ly(filtered_BankEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), 
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarBanEdu2 <- renderPlotly({
        
        req(filtered_BankEdu2())
        
        
        fig <- plot_ly(filtered_BankEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    
    
    #end
    
    output$sBarClass <- renderPlotly({
        
        req(filtered_Class())
        
        
        fig <- plot_ly(filtered_Class(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClass2 <- renderPlotly({
        
        req(filtered_Class2())
        
        
        fig <- plot_ly(filtered_Class2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClassAge <- renderPlotly({
        
        req(filtered_ClassAge())
        
        
        fig <- plot_ly(filtered_ClassAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClassAge2 <- renderPlotly({
        
        req(filtered_ClassAge2())
        
        
        fig <- plot_ly(filtered_ClassAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClassGender <- renderPlotly({
        
        req(filtered_ClassGender())
        
        
        fig <- plot_ly(filtered_ClassGender(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClassGender2 <- renderPlotly({
        
        req(filtered_ClassGender2())
        
        
        fig <- plot_ly(filtered_ClassGender2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)), 
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClassGen <- renderPlotly({
        
        req(filtered_ClassGen())
        
        
        fig <- plot_ly(filtered_ClassGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), 
          title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClassGen2 <- renderPlotly({
        
        req(filtered_ClassGen2())
        
        
        fig <- plot_ly(filtered_ClassGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarClassLang <- renderPlotly({
        
        req(filtered_ClassLang())
        
        
        fig <- plot_ly(filtered_ClassLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), 
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClassLang2 <- renderPlotly({
        
        req(filtered_ClassLang2())
        
        
        fig <- plot_ly(filtered_ClassLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarClassEdu <- renderPlotly({
        
        req(filtered_ClassEdu())
        
        
        fig <- plot_ly(filtered_ClassEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), 
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarClassEdu2 <- renderPlotly({
        
        req(filtered_ClassEdu2())
        
        
        fig <- plot_ly(filtered_ClassEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    
    
    #end
    output$sBarJob <- renderPlotly({
        
        req(filtered_Work())
        
        
        fig <- plot_ly(filtered_Work(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJob2 <- renderPlotly({
        
        req(filtered_Work2())
        
        
        fig <- plot_ly(filtered_Work2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJobAge <- renderPlotly({
        
        req(filtered_WorkAge())
        
        
        fig <- plot_ly(filtered_WorkAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJobAge2 <- renderPlotly({
        
        req(filtered_WorkAge2())
        
        
        fig <- plot_ly(filtered_WorkAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJobSex <- renderPlotly({
        
        req(filtered_WorkGender())
        
        
        fig <- plot_ly(filtered_WorkGender(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJobSex2 <- renderPlotly({
        
        req(filtered_WorkGender2())
        
        
        fig <- plot_ly(filtered_WorkGender2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)), 
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJobGen <- renderPlotly({
        
        req(filtered_WorkGen())
        
        
        fig <- plot_ly(filtered_WorkGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), 
                                                                                                                 title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJobGen2 <- renderPlotly({
        
        req(filtered_WorkGen2())
        
        
        fig <- plot_ly(filtered_WorkGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarJobLang <- renderPlotly({
        
        req(filtered_WorkLang())
        
        
        fig <- plot_ly(filtered_WorkLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), 
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJobLang2 <- renderPlotly({
        
        req(filtered_WorkLang2())
        
        
        fig <- plot_ly(filtered_WorkLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarJobEdu <- renderPlotly({
        
        req(filtered_WorkEdu())
        
        
        fig <- plot_ly(filtered_WorkEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), 
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarJobEdu2 <- renderPlotly({
        
        req(filtered_WorkEdu2())
        
        
        fig <- plot_ly(filtered_WorkEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    #end
    
    output$sBarPol <- renderPlotly({
        
        req(filtered_Pol())
        
        
        fig <- plot_ly(filtered_Pol(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPol2 <- renderPlotly({
        
        req(filtered_Pol2())
        
        
        fig <- plot_ly(filtered_Pol2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPolAge <- renderPlotly({
        
        req(filtered_PolAge())
        
        
        fig <- plot_ly(filtered_PolAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPolAge2 <- renderPlotly({
        
        req(filtered_PolAge2())
        
        
        fig <- plot_ly(filtered_PolAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPolSex <- renderPlotly({
        
        req(filtered_PolSex())
        
        
        fig <- plot_ly(filtered_PolSex(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPolSex2 <- renderPlotly({
        
        req(filtered_PolSex2())
        
        
        fig <- plot_ly(filtered_PolSex2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)), 
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPolGen <- renderPlotly({
        
        req(filtered_PolGen())
        
        
        fig <- plot_ly(filtered_PolGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), 
                                                                                                                 title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPolGen2 <- renderPlotly({
        
        req(filtered_PolGen2())
        
        
        fig <- plot_ly(filtered_PolGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarPolLang <- renderPlotly({
        
        req(filtered_PolLang())
        
        
        fig <- plot_ly(filtered_PolLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), 
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPolLang2 <- renderPlotly({
        
        req(filtered_PolLang2())
        
        
        fig <- plot_ly(filtered_PolLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarPolEdu <- renderPlotly({
        
        req(filtered_PolEdu())
        
        
        fig <- plot_ly(filtered_PolEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), 
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarPolEdu2 <- renderPlotly({
        
        req(filtered_PolEdu2())
        
        
        fig <- plot_ly(filtered_PolEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    #end
    
    output$sBarLang <- renderPlotly({
        
        req(filtered_Lang())
        
        
        fig <- plot_ly(filtered_Lang(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLang2 <- renderPlotly({
        
        req(filtered_Lang2())
        
        
        fig <- plot_ly(filtered_Lang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLangAge <- renderPlotly({
        
        req(filtered_LangAge())
        
        
        fig <- plot_ly(filtered_LangAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLangAge2 <- renderPlotly({
        
        req(filtered_LangAge2())
        
        
        fig <- plot_ly(filtered_LangAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLangSex <- renderPlotly({
        
        req(filtered_LangSex())
        
        
        fig <- plot_ly(filtered_LangSex(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLangSex2 <- renderPlotly({
        
        req(filtered_LangSex2())
        
        
        fig <- plot_ly(filtered_LangSex2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)), 
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLangGen <- renderPlotly({
        
        req(filtered_LangGen())
        
        
        fig <- plot_ly(filtered_LangGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), 
           title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLangGen2 <- renderPlotly({
        
        req(filtered_LangGen2())
        
        
        fig <- plot_ly(filtered_LangGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarLangLang <- renderPlotly({
        
        req(filtered_LangLang())
        
        
        fig <- plot_ly(filtered_LangLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), 
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLangLang2 <- renderPlotly({
        
        req(filtered_LangLang2())
        
        
        fig <- plot_ly(filtered_LangLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarLangEdu <- renderPlotly({
        
        req(filtered_LangEdu())
        
        
        fig <- plot_ly(filtered_LangEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), 
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarLangEdu2 <- renderPlotly({
        
        req(filtered_LangEdu2())
        
        
        fig <- plot_ly(filtered_LangEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    #end
    
    output$sBarRel <- renderPlotly({
        
        req(filtered_Rel())
        
        
        fig <- plot_ly(filtered_Rel(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRel2 <- renderPlotly({
        
        req(filtered_Rel2())
        
        
        fig <- plot_ly(filtered_Rel2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRelAge <- renderPlotly({
        
        req(filtered_RelAge())
        
        
        fig <- plot_ly(filtered_RelAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRelAge2 <- renderPlotly({
        
        req(filtered_RelAge2())
        
        
        fig <- plot_ly(filtered_RelAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRelSex <- renderPlotly({
        
        req(filtered_RelSex())
        
        
        fig <- plot_ly(filtered_RelSex(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRelSex2 <- renderPlotly({
        
        req(filtered_RelSex2())
        
        
        fig <- plot_ly(filtered_RelSex2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)), 
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRelGen <- renderPlotly({
        
        req(filtered_RelGen())
        
        
        fig <- plot_ly(filtered_RelGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), 
                                                                                                                 title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRelGen2 <- renderPlotly({
        
        req(filtered_RelGen2())
        
        
        fig <- plot_ly(filtered_RelGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarRelLang <- renderPlotly({
        
        req(filtered_RelLang())
        
        
        fig <- plot_ly(filtered_RelLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRelLang2 <- renderPlotly({
        
        req(filtered_RelLang2())
        
        
        fig <- plot_ly(filtered_RelLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarRelEdu <- renderPlotly({
        
        req(filtered_RelEdu())
        
        
        fig <- plot_ly(filtered_RelEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), 
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRelEdu2 <- renderPlotly({
        
        req(filtered_RelEdu2())
        
        
        fig <- plot_ly(filtered_RelEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    #end
    output$sBarCol <- renderPlotly({
        
        req(filtered_Col())
        
        
        fig <- plot_ly(filtered_Col(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCol2 <- renderPlotly({
        
        req(filtered_Cov2())
        
        
        fig <- plot_ly(filtered_Col2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarColAge <- renderPlotly({
        
        req(filtered_ColAge())
        
        
        fig <- plot_ly(filtered_ColAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarColAge2 <- renderPlotly({
        
        req(filtered_ColAge2())
        
        
        fig <- plot_ly(filtered_ColAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarColSex <- renderPlotly({
        
        req(filtered_ColSex())
        
        
        fig <- plot_ly(filtered_ColSex(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarColSex2 <- renderPlotly({
        
        req(filtered_ColSex2())
        
        
        fig <- plot_ly(filtered_ColSex2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)), 
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarColGen <- renderPlotly({
        
        req(filtered_ColGen())
        
        
        fig <- plot_ly(filtered_ColGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), 
            title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarColGen2 <- renderPlotly({
        
        req(filtered_ColGen2())
        
        
        fig <- plot_ly(filtered_ColGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarColLang <- renderPlotly({
        
        req(filtered_ColLang())
        
        
        fig <- plot_ly(filtered_ColLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarColLang2 <- renderPlotly({
        
        req(filtered_ColLang2())
        
        
        fig <- plot_ly(filtered_ColLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarColEdu <- renderPlotly({
        
        req(filtered_ColEdu())
        
        
        fig <- plot_ly(filtered_ColEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), 
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarColEdu2 <- renderPlotly({
        
        req(filtered_ColEdu2())
        
        
        fig <- plot_ly(filtered_ColEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    #end
    output$sBarCov <- renderPlotly({
        
        req(filtered_Cov())
        
        
        fig <- plot_ly(filtered_Cov(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCov2 <- renderPlotly({
        
        req(filtered_Cov2())
        
        
        fig <- plot_ly(filtered_Cov2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovAge <- renderPlotly({
        
        req(filtered_CovAge())
        
        
        fig <- plot_ly(filtered_CovAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovAge2 <- renderPlotly({
        
        req(filtered_CovAge2())
        
        
        fig <- plot_ly(filtered_CovAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovSex <- renderPlotly({
        
        req(filtered_CovSex())
        
        
        fig <- plot_ly(filtered_CovSex(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
            yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovSex2 <- renderPlotly({
        
        req(filtered_CovSex2())
        
        
        fig <- plot_ly(filtered_CovSex2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)), 
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovGen <- renderPlotly({
        
        req(filtered_CovGen())
        
        
        fig <- plot_ly(filtered_CovGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), 
            title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovGen2 <- renderPlotly({
        
        req(filtered_CovGen2())
        
        
        fig <- plot_ly(filtered_CovGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarCovLang <- renderPlotly({
        
        req(filtered_CovLang())
        
        
        fig <- plot_ly(filtered_CovLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
           yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovLang2 <- renderPlotly({
        
        req(filtered_CovLang2())
        
        
        fig <- plot_ly(filtered_CovLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarCovEdu <- renderPlotly({
        
        req(filtered_CovEdu())
        
        
        fig <- plot_ly(filtered_CovEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  
                yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), 
                xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovEdu2 <- renderPlotly({
        
        req(filtered_CovEdu2())
        
        
        fig <- plot_ly(filtered_CovEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  
                   yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    ####
    
    output$sBarCov1 <- renderPlotly({
        
        req(filtered_Cov1())
        
        
        fig <- plot_ly(filtered_Cov1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCov21 <- renderPlotly({
        
        req(filtered_Cov21())
        
        
        fig <- plot_ly(filtered_Cov21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovAge1 <- renderPlotly({
        
        req(filtered_CovAge1())
        
        
        fig <- plot_ly(filtered_CovAge1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovAge21 <- renderPlotly({
        
        req(filtered_CovAge21())
        
        
        fig <- plot_ly(filtered_CovAge21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovSex1 <- renderPlotly({
        
        req(filtered_CovSex1())
        
        
        fig <- plot_ly(filtered_CovSex1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovSex21 <- renderPlotly({
        
        req(filtered_CovSex21())
        
        
        fig <- plot_ly(filtered_CovSex21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovGen1 <- renderPlotly({
        
        req(filtered_CovGen1())
        
        
        fig <- plot_ly(filtered_CovGen1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovGen21 <- renderPlotly({
        
        req(filtered_CovGen21())
        
        
        fig <- plot_ly(filtered_CovGen21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarCovLang1 <- renderPlotly({
        
        req(filtered_CovLang1())
        
        
        fig <- plot_ly(filtered_CovLang1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovLang21 <- renderPlotly({
        
        req(filtered_CovLang21())
        
        
        fig <- plot_ly(filtered_CovLang21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    output$sBarCovEdu1 <- renderPlotly({
        
        req(filtered_CovEdu1())
        
        
        fig <- plot_ly(filtered_CovEdu1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            
            layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'), yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarCovEdu21 <- renderPlotly({
        
        req(filtered_CovEdu21())
        
        
        fig <- plot_ly(filtered_CovEdu21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
            
            
            
            
            layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'Percent (%)'),
                   xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    ###
    
    #Graph for Hate Crime by VisMin
    output$immdisPlot <- renderPlotly({
        
        req(filtered_immdisc())
        
        fig <- plot_ly(filtered_immdisc(), x = ~Motivation, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Police reported hate crimes by groups designated as visible minority",font = list(size = 18)),
                   yaxis = list(title = 'Number'), xaxis = list(title = 'Race or ethnicity'))
        
        fig
        
    })
    
    
    #Graph for Hate Crime by VisMin
    output$immdis2Plot <- renderPlotly({
        
        req(filtered_imm2disc())
        
        fig <- plot_ly(filtered_imm2disc(), x = ~Motivation, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Police reported hate crimes by groups designated as visible minority",font = list(size = 18)),
                   yaxis = list(title = 'Number'), xaxis = list(title = 'Motive'))
        
        fig
        
    })
    
    
    #Graph for Hate Crime by VisMin
    output$sBarOver <- renderPlotly({
        
        req(filtered_OverVM())
        
        fig <- plot_ly(filtered_OverVM(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
   
    
    #Today
    
    output$sBarRep3 <- renderPlotly({
        
        req(filtered_rep3VM())
        
        fig <- plot_ly(filtered_rep3VM(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            layout(title = list(text = "Workers in middle management occupations",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    
    
    output$sBarRep2 <- renderPlotly({
        
        req(filtered_rep2VM())
        
        fig <- plot_ly(filtered_rep2VM(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            layout(title = list(text = "Workers in senior management occupations",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarRep1 <- renderPlotly({
        
        req(filtered_rep1VM())
        
        fig <- plot_ly(filtered_rep1VM(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            layout(title = list(text = "Workers in all management occupations",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    #end
    output$sBarYouth <- renderPlotly({
        
        req(filtered_youthVM())
        
        fig <- plot_ly(filtered_youthVM(), x = ~VisMin, y = ~Value, type = 'bar') %>%
            
            layout(title = list(text = "Youth not in employment, education or training",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    
    #End
    
    
    #Graph for Hate Crime by VisMin
    output$sBarOver <- renderPlotly({
        
        req(filtered_OverVM())
        
        fig <- plot_ly(filtered_OverVM(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    
    #Graph for Hate Crime by VisMin
    output$sBarOverIS <- renderPlotly({
        
        req(filtered_OverIS())
        
        fig <- plot_ly(filtered_OverIS(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
        
        fig
        
    })
    
    
    #Graph for Hate Crime by VisMin
    output$sBarOverGEO <- renderPlotly({
        
        req(filtered_OverGEO())
        
        fig <- plot_ly(filtered_OverGEO(), x = ~Geography, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Geography'))
        
        fig
        
    })
    
    output$sBarOverSX <- renderPlotly({
        
        req(filtered_OverIS())
        
        fig <- plot_ly(filtered_OverSX(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Sex'))
        
        fig
        
    })
  
   
    
    #Graph for Hate Crime by VisMin
    output$sBarEduSX <- renderPlotly({
        
        req(filtered_educationSX1())
        
        fig <- plot_ly(filtered_educationSX1(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with bachelor’s degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Sex'))
        
        fig
        
    })
    
    output$sBarEduSX1 <- renderPlotly({
        
        req(filtered_educationSX2())
        
        fig <- plot_ly(filtered_educationSX2(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with no certificate, diploma or degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Sex'))
        
        fig
        
    })
    
    output$sBarEduSX2 <- renderPlotly({
        
        req(filtered_educationSX3())
        
        fig <- plot_ly(filtered_educationSX3(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with high school diploma or equivalency certificate",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Sex'))
        
        fig
        
    })
    
    
    output$sBarEduSX3 <- renderPlotly({
        
        req(filtered_educationSX4())
        
        fig <- plot_ly(filtered_educationSX4(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with postsecondary certificate or diploma below bachelor level",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Sex'))
        
        fig
        
    })
    
    
    output$sBarEduSX4 <- renderPlotly({
        
        req(filtered_educationSX5())
        
        fig <- plot_ly(filtered_educationSX5(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with bachelor’s degree or above",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Sex'))
        
        fig
        
    })
    
    
    output$sBarEduSX5 <- renderPlotly({
        
        req(filtered_educationSX6())
        
        fig <- plot_ly(filtered_educationSX6(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with master’s degree or earned doctorate",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Sex'))
        
        fig
        
    })
    
    
    #Graph for Hate Crime by VisMin
    output$sBarEduVM <- renderPlotly({
        
        req(filtered_educationVM1())
        
        fig <- plot_ly(filtered_educationVM1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with bachelor’s degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarEduVM1 <- renderPlotly({
        
        req(filtered_educationVM2())
        
        fig <- plot_ly(filtered_educationVM2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with no certificate, diploma or degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    output$sBarEduVM2 <- renderPlotly({
        
        req(filtered_educationVM3())
        
        fig <- plot_ly(filtered_educationVM3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with high school diploma or equivalency certificate",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarEduVM3 <- renderPlotly({
        
        req(filtered_educationVM4())
        
        fig <- plot_ly(filtered_educationVM4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with postsecondary certificate or diploma below bachelor level",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarEduVM4 <- renderPlotly({
        
        req(filtered_educationVM5())
        
        fig <- plot_ly(filtered_educationVM5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with bachelor’s degree or above",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    output$sBarEduVM5 <- renderPlotly({
        
        req(filtered_educationVM6())
        
        fig <- plot_ly(filtered_educationVM6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with master’s degree or earned doctorate",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Visible minority status'))
        
        fig
        
    })
    
    
    #Graph for Hate Crime by VisMin
    output$sBarEdu <- renderPlotly({
        
        req(filtered_education1())
        
        fig <- plot_ly(filtered_education1(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with bachelor’s degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
        
        fig
        
    })
    
    #Graph for Hate Crime by VisMin
    output$sBarEdu1 <- renderPlotly({
        
        req(filtered_education2())
        
        fig <- plot_ly(filtered_education2(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with no certificate, diploma or degree",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
        
        fig
        
    })
    
    
    output$sBarEdu2 <- renderPlotly({
        
        req(filtered_education3())
        
        fig <- plot_ly(filtered_education3(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with high school diploma or equivalency certificate",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
        
        fig
        
    })
    
    
    output$sBarEdu3 <- renderPlotly({
        
        req(filtered_education4())
        
        fig <- plot_ly(filtered_education4(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with postsecondary certificate or diploma below bachelor level",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
        
        fig
        
    })
    
    
    output$sBarEdu4 <- renderPlotly({
        
        req(filtered_education5())
        
        fig <- plot_ly(filtered_education5(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with bachelor’s degree or above",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
        
        fig
        
    })
    
    
    output$sBarEdu5 <- renderPlotly({
        
        req(filtered_education6())
        
        fig <- plot_ly(filtered_education6(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
            
            layout(title = list(text = "Population with master’s degree or earned doctorate",font = list(size = 18)),
                   yaxis = list(title = 'Percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
        
        fig
        
    })
    
    
    
    
    
    
    
    
    
    
    # Line graph
    output$lgraph <- renderPlotly({
        
        # Require filtered_lineData
        req(filtered_lineData())
        
        # Create the base graph
        lp <- plot_ly(data = filtered_lineData(), x = ~Year)
        
        # Add each series one-by-one as new traces
        for (i in 3:length(colnames(filtered_lineData()))) {
            lp <- lp %>%
                add_trace(x = filtered_lineData()$Year, y = filtered_lineData()[[i]],
                          type = "scatter", mode = "lines+markers",
                          name = colnames(filtered_lineData())[i])
        }
        
        
        
        
        # Note hovermode = "x unified" is not working as it is supposed to
        # Best work-around was used in xaxis with spike layout
        lp <- lp %>%
            layout(title = "Police Reported Hate Crime Time Series Analysis",
                   hovermode = "Police Reported Hate Crime Time Series Analysis",
                   xaxis = list(title = "Year",
                                showspikes = TRUE,
                                spikecolor = "black",
                                spikethickness = 2,
                                spikemode  = 'toaxis+across',
                                spikesnap = 'data',
                                showline=TRUE),
                   yaxis = list(title = "Number")
            )
        
        lp
    })
    
    
    
    # Line graph
    output$ltwograph <- renderPlotly({
        
        # Require filtered_lineData
        req(filtered_linetwoData())
        
        # Create the base graph
        lp <- plot_ly(data = filtered_linetwoData(), x = ~Year)
        
        # Add each series one-by-one as new traces
        for (i in 3:length(colnames(filtered_linetwoData()))) {
            lp <- lp %>%
                add_trace(x = filtered_linetwoData()$Year, y = filtered_linetwoData()[[i]],
                          type = "scatter", mode = "lines+markers",
                          name = colnames(filtered_linetwoData())[i])
        }
        
        
        
        
        # Note hovermode = "x unified" is not working as it is supposed to
        # Best work-around was used in xaxis with spike layout
        lp <- lp %>%
            layout(title = "Police Reported Hate Crime Time Series Analysis",
                   hovermode = "Police Reported Hate Crime Time Series Analysis",
                   xaxis = list(title = "Year",
                                showspikes = TRUE,
                                spikecolor = "black",
                                spikethickness = 2,
                                spikemode  = 'toaxis+across',
                                spikesnap = 'data',
                                showline=TRUE),
                   yaxis = list(title = "Number")
            )
        
        lp
    })
}
# Run the app -------------------------------------------------------
shinyApp(ui = ui, server = server)
