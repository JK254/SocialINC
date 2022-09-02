# Load libraries ----
library(arrow)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(shiny)
library(shinyWidgets)

# Import data ----
df_list <-
  list.files("./_tempdata/", ".*\\.parquet$", ignore.case = TRUE) %>%
  as.data.frame() %>%
  rename(df_list = ".") %>%
  mutate(df_list = sub(
    pattern = "\\.parquet",
    replacement = "",
    x = df_list
  )) %>% 
  unlist()

#'NOTE [make sure the working directory is pointing to the right location]
for (i in df_list) {
  assign(i, read_parquet(file = paste0("./_tempdata/", i, ".parquet")))
}
gc()

# Functions ----
add_date_charvar <- function(x) {
  x %>%
    mutate(
      Year = as.Date(paste0(Year, "-01-01")), 
      # adding "01-01" so R recognizes and treats "Year" as a date column
      char_type = case_when(
        Characteristic %in% c(
          "Total, 15 years and over",
          "15 to 24 years",
          "25 to 64 years",
          "65 years and over"
        ) ~ "Age",
        Characteristic %in% c("Men", "Women") ~ "Sex",
        Characteristic %in% c("Immigrants", "Non-Immigrants") ~ "Immigration status",
        Characteristic %in% c(
          "First generation",
          "Second generation",
          "Third generation or more"
        ) ~ "Generation",
        Characteristic %in% c(
          "First official language spoken, English only",
          "First official language spoken, French only"
        ) ~ "Language",
        Characteristic %in% c(
          "Secondary (high) school diploma or equivalency certificate or less",
          "Postsecondary certificate or diploma (non-university)",
          "University certificate or diploma"
        ) ~ "Education"
      ) # adding a column to group relevant characteristics together
    )
}

# Themes ----
# dim_list <- list(
#   "Participation in the Labour Market", # rateDT, OverQualDT, youthDT
#   "Civic engagement and political participation", # civicDT & civicDT2
#   "Representation in decision-making positions", # representationDT
#   "Basic needs and housing", # basicDT
#   "Health and wellbeing", # healthDT
#   "Education, training and skills", # educationDT
#   "Income and wealth", # incomeDT
#   "Social connections and personnal networks", # belongingDT
#   "Local community", # incomeDT
#   "Public services and institutions", # confidenceDT
#   "Discrimination and victimization" # discriminationDT, polData
# )

# Data manipulations ----
#' [if this applies to all of the datasets, we can nest the df_list here, rather than explicitly list the dataset]
basicDT <- 
  add_date_charvar(basicDT)

belongingDT <-
  add_date_charvar(belongingDT)

civicDT <- 
  add_date_charvar(civicDT)

civicDT2 <- 
  add_date_charvar(civicDT2)

confidenceDT <- 
  add_date_charvar(confidenceDT)

# Repetitive code ----
#'NOTE [these are the reoccuring sources I seen, I might be missing something]
souce_cchs <-
  "Source: Canadian Community Health Survey (CCHS), September to December 2020"
source_gss <-
  "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
source_census_nhs_census <-
  "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
source_census_nhs <-
  "Source: Censuses of population, 2006 and 2016; National Household Survey, 2011"

# User interface // create layout ----
ui <-
  fluidPage(
    
    titlePanel("Social Inclusion Data Visualization Tool"), # title of dashboard
    tabsetPanel(
      type = "pills", # type of navigation button
    
    ## 1. Basic income  ----
    tabPanel(
      "Basic income",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          pickerInput(
            inputId = "basic_geo", # name this for the server
            label = "Choose a geography", # label of filter
            choices = unique(basicDT$Geography),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "basic_vismin", # name this for the server
            label = "Choose a visible minority group", # label of filter
            choices = as.character(unique(basicDT$VisMin)),
            # create drop-down list option
            multiple = TRUE, 
            selected = as.character(unique(basicDT$VisMin))[1],
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Deselect all",
              `select-all-text` = "Select all"
            )), # single-select
          pickerInput(
            inputId = "basic_characteristic", # name this for the server
            label = "Choose a characteristic", # label of filter
            choices = as.character(unique(basicDT$Characteristic)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "basic_indicator", # name this for the server
            label = "Choose an indicator", # label of filter
            choices = as.character(unique(basicDT$Indicator)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "basic_confidence", # name this for the server
            label = "Choose a confidence", # label of filter
            choices = as.character(unique(basicDT$Confidence)),
            # create drop-down list option
            multiple = FALSE) # single-select
        ),
        mainPanel(plotlyOutput("basic_income", width = "100%"))
      )
    ), 
    
    ## 2. Belonging ----
    tabPanel(
      "Sense of belonging",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          pickerInput(
            inputId = "belonging_vismin", # name this for the server
            label = "Choose a visible minority group", # label of filter
            choices = as.character(unique(belongingDT$VisMin)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "belonging_indicator", # name this for the server
            label = "Choose an indicator", # label of filter
            choices = as.character(unique(belongingDT$Indicator)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "belonging_confidence", # name this for the server
            label = "Choose a confidence", # label of filter
            choices = as.character(unique(belongingDT$Confidence)),
            # create drop-down list option
            multiple = FALSE) # single-select
        ),
        mainPanel(
          h4(paste0("Geography: ", as.character(unique(belongingDT$Geography)))),
          h4(paste0("Reference period: ", as.character(unique(belongingDT$Year)))),
          plotlyOutput("belonging", width = "100%", height = "1500px")
          )
      )
    ),
    
    ## 3. Civic ----
    tabPanel(
      "Civic",
      fluid = TRUE,
      fluidRow(
        h3("Civic data table 1"),
        column(
          width = 3,
          pickerInput(
            inputId = "civic_geo", # name this for the server
            label = "Choose a geography", # label of filter
            choices = unique(civicDT$Geography), # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "civic_vismin", # name this for the server
            label = "Choose a visible minority group", # label of filter
            choices = as.character(unique(civicDT$VisMin)), # create drop-down list option
            multiple = TRUE, # multi-select
            selected = as.character(unique(civicDT$VisMin))[1],
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Deselect all",
              `select-all-text` = "Select all"
            )
          ),
          pickerInput(
            inputId = "civic_characteristic", # name this for the server
            label = "Choose a characteristic", # label of filter
            choices = as.character(unique(civicDT$Characteristic)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "civic_indicator", # name this for the server
            label = "Choose an indicator", # label of filter
            choices = as.character(unique(civicDT$Indicator)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "civic_confidence", # name this for the server
            label = "Choose a confidence", # label of filter
            choices = as.character(unique(civicDT$Confidence)),
            # create drop-down list option
            multiple = FALSE), # single-select
        ),
        column(width = 9,
               plotlyOutput("civic", width = "100%"))
      ), 
      hr(style = "border-color: black"), 
      fluidRow(
        h3("Civic data table 2"),
        column(
          width = 3,
          pickerInput(
            inputId = "civic2_geo", # name this for the server
            label = "Choose a geography", # label of filter
            choices = unique(civicDT2$Geography), # create drop-down list option
            multiple = FALSE
          ), # single-select
          pickerInput(
            inputId = "civic2_vismin", # name this for the server
            label = "Choose a visible minority group", # label of filter
            choices = as.character(unique(civicDT2$VisMin)),
            # create drop-down list option
            multiple = TRUE, # multi-select
            selected = as.character(unique(civicDT2$VisMin))[1],
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Deselect all",
              `select-all-text` = "Select all"
            )
          ),
          pickerInput(
            inputId = "civic2_characteristic", # name this for the server
            label = "Choose a characteristic", # label of filter
            choices = as.character(unique(civicDT2$Characteristic)),
            # create drop-down list option
            multiple = FALSE
          ), # single-select
          pickerInput(
            inputId = "civic2_indicator", # name this for the server
            label = "Choose an indicator", # label of filter
            choices = as.character(unique(civicDT2$Indicator)),
            # create drop-down list option
            multiple = FALSE
          ), # single-select
          pickerInput(
            inputId = "civic2_confidence", # name this for the server
            label = "Choose a confidence", # label of filter
            choices = as.character(unique(civicDT2$Confidence)),
            # create drop-down list option
            multiple = FALSE # single-select
          )
        ),
        column(width = 9,
               plotlyOutput("civic2", width = "100%"))
      )
    ),
    
    ## 4. Confidence  ----
    tabPanel(
      "Confidence",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          pickerInput(
            inputId = "conf_vismin", # name this for the server
            label = "Choose a visible minority group", # label of filter
            choices = as.character(unique(confidenceDT$VisMin)),
            # create drop-down list option
            multiple = TRUE, 
            selected = as.character(unique(confidenceDT$VisMin))[1],
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Deselect all",
              `select-all-text` = "Select all"
            )), # single-select
          #'NOTE [this filter affects the next one]
          pickerInput(
            inputId = "conf_chartype", # name this for the server
            label = "Choose a characteristic group", # label of filter
            choices = as.character(unique(confidenceDT$char_type)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "conf_characteristic", # name this for the server
            label = "Choose a characteristic", # label of filter
            choices = as.character(unique(confidenceDT$Characteristic)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "conf_indicator", # name this for the server
            label = "Choose an indicator", # label of filter
            choices = as.character(unique(confidenceDT$Indicator)),
            # create drop-down list option
            multiple = FALSE), # single-select
          pickerInput(
            inputId = "conf_conf", # name this for the server
            label = "Choose a statistic", # label of filter
            choices = as.character(unique(confidenceDT$Confidence)),
            # create drop-down list option
            multiple = FALSE) # single-select
        ),
        mainPanel(plotlyOutput("confidence", width = "100%"))
      )
    )
  )
)

# Server // determines the data underlying the UI ----
server <- function(input, output, session) {
  
  ## Filters ----
  # FIXME: I feel like we can make this reactive into a function but haven't sorted that out yet
  ### 1. Basic income ----
  basic_income_filter <-
    reactive({
      basicDT %>%
        filter(
          Geography == input$basic_geo,
          VisMin %in% input$basic_vismin,
          Characteristic == input$basic_characteristic,
          Indicator == input$basic_indicator,
          Confidence == input$basic_confidence
        )
    })
  
  ### 2. Belonging ----
  belonging_filter <-
    reactive({
      belongingDT %>%
        filter(
          VisMin == input$belonging_vismin,
          Indicator == input$belonging_indicator,
          Confidence == input$belonging_confidence
        )
    })
  
  ### 3.1 Civic ----
  civic_filter <-
    reactive({
      civicDT %>%
        filter(
          Geography == input$civic_geo,
          VisMin %in% input$civic_vismin,
          Characteristic == input$civic_characteristic,
          Indicator == input$civic_indicator,
          Confidence == input$civic_confidence
        )
    })
  
  ### 3.2 Civic ----
  civic2_filter <-
    reactive({
      civicDT2 %>%
        filter(
          Geography == input$civic2_geo,
          VisMin %in% input$civic2_vismin,
          Characteristic == input$civic2_characteristic,
          Indicator == input$civic2_indicator,
          Confidence == input$civic2_confidence
        )
    })
  
  ### 4. Confidence ----
  #'NOTE [lines 402-407 makes the filter char_type affect the characteristics options]
  observe({
    confidenceDT_2 <-
      as.character(unique(confidenceDT$Characteristic[confidenceDT$char_type == input$conf_chartype]))
    
    updatePickerInput(session, "conf_characteristic", choices = confidenceDT_2)
  })
  
  confidence_filter <-
    reactive({
      confidenceDT %>%
        filter(
          VisMin %in% input$conf_vismin,
          Characteristic == input$conf_characteristic,
          Indicator == input$conf_indicator,
          Confidence == input$conf_conf
        )
    })
  
  ## Plots ----
  ### 1. Basic income ----
  # FIXME: I feel like we can make this plot into a function but haven't sorted that out yet
  output$basic_income <-
    renderPlotly(ggplotly({
      ggplot(basic_income_filter()) +
        geom_bar(stat = "identity",
                 position = "dodge",
                 aes(
                   x = Year,
                   y = Value,
                   colour = VisMin,
                   fill = VisMin,
                   text = paste0(
                     "Visible Minority group: ", VisMin,
                     "<br>",
                     "Value: ", format(Value, big.mark = ","),
                     "<br>",
                     "Year: ", format(Year, "%Y")
                   )
                 ))+
        theme_minimal() +
        scale_x_date(date_labels = "%Y") +
        scale_y_continuous(labels = comma) +
        labs(title = "Basic income",
             x = "Value",
             y = "Year",
             colour = "Visible Minority group",
             fill = "Visible Minority group")
    }, tooltip = "text"))

  ### 2. Belonging ----
  output$belonging <-
    renderPlotly(ggplotly({
      ggplot(belonging_filter()) +
        geom_bar(stat = "identity",
                 position = "dodge",
                 aes(
                   x = Characteristic,
                   y = Value,
                   colour = Characteristic,
                   fill = Characteristic,
                   text = paste0(
                     "Characteristic: ", Characteristic,
                     "<br>",
                     "Value: ", format(Value, big.mark = ","),
                     "<br>"
                   )
                 ))+
        theme_minimal() +
        theme(legend.position = "none") +
        scale_y_continuous(labels = comma) +
        labs(title = "Sense of belonging",
             x = "Characteristic",
             y = "Value") +
        facet_wrap(facets = ~ char_type, nrow = 7, ncol = 1, scales = "free")
    }, tooltip = "text"))
  
  ### 3.1 Civic ----
  output$civic <-
    renderPlotly(ggplotly({
      ggplot(civic_filter()) +
        geom_bar(stat = "identity",
                 position = "dodge",
                 aes(
                   x = Year,
                   y = Value,
                   colour = VisMin,
                   fill = VisMin,
                   text = paste0(
                     "Visible Minority group: ", VisMin,
                     "<br>",
                     "Value: ", format(Value, big.mark = ","),
                     "<br>",
                     "Year: ", format(Year, "%Y")
                   )
                 ))+
        theme_minimal() +
        scale_x_date(date_labels = "%Y") +
        scale_y_continuous(labels = comma) +
        labs(x = "Value",
             y = "Year",
             colour = "Visible Minority group",
             fill = "Visible Minority group")
    }, tooltip = "text"))
  
  ### 3.2 Civic ----
  output$civic2 <-
    renderPlotly(ggplotly({
      ggplot(civic2_filter()) +
        geom_bar(stat = "identity",
                 position = "dodge",
                 aes(
                   x = Year,
                   y = Value,
                   colour = VisMin,
                   fill = VisMin,
                   text = paste0(
                     "Visible Minority group: ", VisMin,
                     "<br>",
                     "Value: ", format(Value, big.mark = ","),
                     "<br>",
                     "Year: ", format(Year, "%Y")
                   )
                 ))+
        theme_minimal() +
        scale_x_date(date_labels = "%Y") +
        scale_y_continuous(labels = comma) +
        labs(x = "Value",
             y = "Year",
             colour = "Visible Minority group",
             fill = "Visible Minority group")
    }, tooltip = "text"))
  
  ### 4. Confidence ----
  output$confidence <-
    renderPlotly(ggplotly({
      ggplot(confidence_filter()) +
        geom_bar(stat = "identity",
                 position = "dodge",
                 aes(
                   x = Year,
                   y = Value,
                   colour = VisMin,
                   fill = VisMin,
                   text = paste0(
                     "Visible Minority group: ", VisMin,
                     "<br>",
                     "Value: ", format(Value, big.mark = ","),
                     "<br>",
                     "Year: ", format(Year, "%Y")
                   )
                 ))+
        theme_minimal() +
        scale_x_date(date_labels = "%Y") +
        scale_y_continuous(labels = comma) +
        labs(x = "Value",
             y = "Year",
             colour = "Visible Minority group",
             fill = "Visible Minority group")
    }, tooltip = "text"))
  
}

# All together now ----
shinyApp(ui, server)
