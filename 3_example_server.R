source(file = "2_example_ui.R", encoding = "UTF-8")

# Server // determines the data underlying the UI ----
server <- function(input, output, session) {
  
  ## Themes and Definitions of Indicators ----
  ### Table 1 ----
  #'NOTE [Makes is so the table reacts to the user's selection]
  choose_def <- 
    reactive({
      req(input$theme_0)
      template %>% 
        filter(Theme %in% input$theme_0)
    })
  
  #'NOTE [Table is adjusted depending on the theme the user selects]
  output$def_table <- 
    renderDataTable({
      choose_def() %>% 
        select(-Theme) # Removed theme column because it's not needed in the table
    })
  
  ## Tab 1: Groups Designated as Visible Minorities ----
  ### Filters ----
  #'NOTE [This makes it so the theme filter affects the indicator filter]
  observe(
    updateSelectizeInput(
      session = session,
      inputId = "indicator_1",
      choices = as.character(unique(template$Indicator[template$Theme == input$theme_1]))
    )
  )
  
  #### 1. Participation in the Labour Market ----
  ##### 1.1. Working-age population in the labour force (participation rate) ----
  filtered_vm_lm_1 <- reactive({
    rateDT %>%
      filter(
        Indicator == "Participation rate",
        VisMin %in% input$lm_vismin,
        Degree == input$lm_degree,
        Geography == input$lm_geography,
        Immigration == input$lm_immigration,
        Year == input$lm_year,
        Age == input$lm_age,
        Sex == input$lm_sex
      )
  })
  
  ##### 1.2. Working-age population in employment (employment rate) ----
  filtered_vm_lm_2 <- reactive({
    rateDT %>%
      filter(
        Indicator == "Employment rate",
        VisMin %in% input$lm_vismin,
        Degree == input$lm_degree,
        Geography == input$lm_geography,
        Immigration == input$lm_immigration,
        Year == input$lm_year,
        Age == input$lm_age,
        Sex == input$lm_sex
      )
  })
  
  ##### 1.3. Working-age population in unemployment (unemployment rate) ----
  filtered_vm_lm_3 <- reactive({
    rateDT %>%
      filter(
        Indicator == "Unemployment rate",
        VisMin %in% input$lm_vismin,
        Degree == input$lm_degree,
        Geography == input$lm_geography,
        Immigration == input$lm_immigration,
        Year == input$lm_year,
        Age == input$lm_age,
        Sex == input$lm_sex
      )
  })
  
  ### Plots ----
  #### 1. Participation in the Labour Market ----
  ##### 1.1. Working-age population in the labour force (participation rate) ----
  output$plot_vm_lm_1 <-
    renderPlotly(ggplotly({
      ggplot(filtered_vm_lm_1()) +
        geom_bar(
          stat = "identity",
          position = "dodge",
          aes(
            x = Year,
            y = Value,
            colour = VisMin,
            fill = VisMin,
            text = paste0(
              "Visible Minority group: ",
              VisMin,
              "<br>",
              "Value: ",
              format(Value, big.mark = ","),
              "<br>",
              "Year: ",
              format(Year, "%Y")
            )
          )
        ) +
        theme_minimal() +
        scale_x_date(date_labels = "%Y") +
        scale_y_continuous(labels = comma) +
        labs(
          x = "Year",
          y = "Value",
          colour = "Visible Minority group",
          fill = "Visible Minority group"
        )
    }, tooltip = "text"))
  
  ##### 1.2. Working-age population in employment (employment rate) ----
  output$plot_vm_lm_2 <-
    renderPlotly(ggplotly({
      ggplot(filtered_vm_lm_2()) +
        geom_bar(
          stat = "identity",
          position = "dodge",
          aes(
            x = Year,
            y = Value,
            colour = VisMin,
            fill = VisMin,
            text = paste0(
              "Visible Minority group: ",
              VisMin,
              "<br>",
              "Value: ",
              format(Value, big.mark = ","),
              "<br>",
              "Year: ",
              format(Year, "%Y")
            )
          )
        ) +
        theme_minimal() +
        scale_x_date(date_labels = "%Y") +
        scale_y_continuous(labels = comma) +
        labs(
          x = "Year",
          y = "Value",
          colour = "Visible Minority group",
          fill = "Visible Minority group"
        )
    }, tooltip = "text"))
  
  ##### 1.3. Working-age population in employment (unemployment rate) ----
  output$plot_vm_lm_3 <-
    renderPlotly(ggplotly({
      ggplot(filtered_vm_lm_3()) +
        geom_bar(
          stat = "identity",
          position = "dodge",
          aes(
            x = Year,
            y = Value,
            colour = VisMin,
            fill = VisMin,
            text = paste0(
              "Visible Minority group: ",
              VisMin,
              "<br>",
              "Value: ",
              format(Value, big.mark = ","),
              "<br>",
              "Year: ",
              format(Year, "%Y")
            )
          )
        ) +
        theme_minimal() +
        scale_x_date(date_labels = "%Y") +
        scale_y_continuous(labels = comma) +
        labs(
          x = "Year",
          y = "Value",
          colour = "Visible Minority group",
          fill = "Visible Minority group"
        )
    }, tooltip = "text"))

  #### 2. Civic engagement and political participation ----
  
  #### 3. Representation in decision-making positions ----
  
  #### 4. Basic needs and housing ----
  
  #### 5. Local community ----
  
  #### 6. Health and wellbeing ----
  
  #### 7. Public services and institutions ----
  
  #### 8. Income and wealth ----
  
  #### 9. Social connections and personnal networks ----
  
  #### 10. Discrimination and victimization ----
}

# All together now ----
shinyApp(ui, server)
