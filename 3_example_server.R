source(file = "2_example_ui.R", encoding = "UTF-8")

# Server // determines the data underlying the UI ----
server <- function(input, output, session) {
  
  ## Themes and Definitions of Indicators ----
  ### Table 1 ----
  output$def_table <-
    renderDataTable(reactive({
      req(input$theme_0)
      template %>%
        filter(Theme %in% input$theme_0)
    }))
  
  ## Filters ----
  # FIXME: I feel like we can make this reactive into a function but haven't sorted that out yet
  ### 1. Theme: Groups designated as visible Minorities ----
  
  observe({
    filtered_characteristic <-
      unique(
        c(
          as.character(basicDT$Characteristic[basicDT$char_type == input$char_type]),
          as.character(belongingDT$Characteristic[belongingDT$char_type == input$char_type]),
          as.character(civicDT$Characteristic[civicDT$char_type == input$char_type]),
          as.character(confidenceDT$Characteristic[confidenceDT$char_type == input$char_type]),
          as.character(discriminationDT$Characteristic[discriminationDT$char_type == input$char_type]),
          as.character(healthDT$Characteristic[healthDT$char_type == input$char_type])
        )
      )

    updatePickerInput(session, "characteristic", choices = filtered_characteristic)
  })
  
  ## Plots ----
  ### 1. Theme: Groups designated as visible Minorities ----
  # FIXME: I feel like we can make this plot into a function but haven't sorted that out yet
  
  output$plot_vismin <-
    renderPlotly(ggplotly({
      ggplot(
        {
          if (input$theme_1 == "Participation in the Labour Market") {
            reactive({
              rateDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Geography == input$Geography,
                  Age == input$Age,
                  Sex == input$Sex,
                  Indicator == input$Indicator,
                  Immigration == input$Immigration,
                  Degree == input$Degree,
                  Location == input$Location,
                  Language == input$Language
                )
            })
          } else if (input$theme_1 == "Civic engagement and political participation") {
            reactive({
              civicDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Geography == input$Geography,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence
                )
            })
          }  else if (input$theme_1 == "Representation in decision-making positions") {
            reactive({
              representationDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Geography == input$Geography,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence
                )
            })
          } else if (input$theme_1 == "Basic needs and housing") {
            reactive({
              basicDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence
                )
            })
          } else if (input$theme_1 == "Health and wellbeing") {
            reactive({
              healthDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence
                )
            })
          } else if (input$theme_1 == "Education, training and skills") {
            reactive({
              educationDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Geography == input$Geography,
                  Age == input$Age,
                  Sex == input$Sex,
                  Indicator == input$Indicator,
                  Immigration == input$Immigration,
                  Language == input$Language
                )
            })
          } else if (input$theme_1 == "Income and wealth") {
            reactive({
              incomeDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Geography == input$Geography,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence
                )
            })
          } else if (input$theme_1 == "Social connections and personnal networks") {
            reactive({
              belongingDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Geography == input$Geography,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence
                )
            })
          } else if (input$theme_1 == "Local community") {
            # FIXME: did I write down this theme correctly?
            reactive({
              incomeDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Geography == input$Geography,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence
                )
            })
          } else if (input$theme_1 == "Public services and institutions") {
            reactive({
              confidenceDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Geography == input$Geography,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence
                )
            })
          } else if (input$theme_1 == "Discrimination and victimization") {
            reactive({
              discriminationDT %>%
                filter(
                  VisMin %in% input$VisMin,
                  Year == input$Year,
                  Characteristic == input$Characteristic,
                  Indicator == input$Indicator,
                  Confidence == input$Confidence,
                  Motivation == input$Motivation
                )
            })
          }
        }
      ) +
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
