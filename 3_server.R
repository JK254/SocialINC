# Define server logic ---------------------------------------------------------------
server <- function(input, output) {
  # Method to call the reset function
  observeEvent(input$reset_button, {
    js$reset()
  })
  
  # Reactive values ----------------------------------------------------------
  # This reactive filters for sex to build the first column graph on the first tab
  
  #Leo
  
  #Filters for Immigration Basic Needs and Health
  
  filtered_2basic5 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecImm2,
        `Indicator` == 'Household severely food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicAge5 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecAge2,
        `Indicator` == 'Household severely food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicSex5 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecSex2,
        `Indicator` == 'Household severely food insecure'
      )
    
    
    return(newDT)
  })
  
  
  
  ###
  
  filtered_2basic4 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecImm2,
        `Indicator` == 'Household moderately food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicAge4 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecAge2,
        `Indicator` == 'Household moderately food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicSex4 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecSex2,
        `Indicator` == 'Household moderately food insecure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  
  filtered_2basic3 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecImm2,
        `Indicator` == 'Household moderately or severely food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicAge3 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecAge2,
        `Indicator` == 'Household moderately or severely food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicSex3 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecSex2,
        `Indicator` == 'Household moderately or severely food insecure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  
  filtered_2basic2 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecImm2,
        `Indicator` == 'Household marginally food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicAge2 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecAge2,
        `Indicator` == 'Household marginally food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicSex2 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecSex2,
        `Indicator` == 'Household marginally food insecure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  
  filtered_2basic1 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecImm2,
        `Indicator` == 'Household food secure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicAge1 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecAge2,
        `Indicator` == 'Household food secure'
      )
    
    
    return(newDT)
  })
  
  filtered_2basicSex1 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM215,
        `Year` %in% input$basicYear2,
        `Geography` %in% input$basicGeo2,
        `Confidence` %in% input$basicConfidence2,
        `Characteristic` %in% input$basicCharSpecSex2,
        `Indicator` == 'Household food secure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  ####
  
  
  filtered_2health14 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived need for mental health care, needs partially met'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge14 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived need for mental health care, needs partially met'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex14 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived need for mental health care, needs partially met'
      )
    
    
    return(newDT)
  })
  
  
  
  
  filtered_2health13 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Unmet health care needs'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge13 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Unmet health care needs'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex13 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Unmet health care needs'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  filtered_2health12 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived need for mental health care, needs not met'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge12 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived need for mental health care, needs not met'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex12 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived need for mental health care, needs not met'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  filtered_2health11 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived need for mental health care, needs partially met or needs not met'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge11 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived need for mental health care, needs partially met or needs not met'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex11 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived need for mental health care, needs partially met or needs not met'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  filtered_2health10 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived need for mental health care, all needs met'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge10 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived need for mental health care, all needs met'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex10 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived need for mental health care, all needs met'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  
  filtered_2health9 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived need for mental health care, no need'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge9 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived need for mental health care, no need'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex9 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived need for mental health care, no need'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  ###
  filtered_2health8 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Has a regular healthcare provider'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge8 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Has a regular healthcare provider'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex8 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Has a regular healthcare provider'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  
  filtered_2health7 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Life satisfaction, satisfied or very satisfied'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge7 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Life satisfaction, satisfied or very satisfied'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex7 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Life satisfaction, satisfied or very satisfied'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  
  ###
  
  filtered_2health6 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived life stress, most days quite a bit or extremely stressful'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge6 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived life stress, most days quite a bit or extremely stressful'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex6 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived life stress, most days quite a bit or extremely stressful'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  ###
  filtered_2health4 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived mental health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge4 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived mental health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex4 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived mental health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  #####
  filtered_2health3 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived mental health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge3 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived mental health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex3 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived mental health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  
  filtered_2health2 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge2 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex2 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  
  
  
  ###
  
  filtered_2health1 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecImm2,
        `Indicator` == 'Perceived health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthAge1 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecAge2,
        `Indicator` == 'Perceived health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  filtered_2healthSex1 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM220,
        `Year` %in% input$healthYear2,
        `Geography` %in% input$healthGeo2,
        `Confidence` %in% input$healthConfidence2,
        `Characteristic` %in% input$healthCharSpecSex2,
        `Indicator` == 'Perceived health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  #Filters for VisMin Basic Needs and Health
  
  filtered_basic5 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecImm,
        `Indicator` == 'Household severely food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicAge5 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecAge,
        `Indicator` == 'Household severely food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicSex5 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecSex,
        `Indicator` == 'Household severely food insecure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  
  ###
  
  filtered_basic4 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecImm,
        `Indicator` == 'Household moderately food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicAge4 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecAge,
        `Indicator` == 'Household moderately food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicSex4 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecSex,
        `Indicator` == 'Household moderately food insecure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  
  filtered_basic3 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecImm,
        `Indicator` == 'Household moderately or severely food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicAge3 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecAge,
        `Indicator` == 'Household moderately or severely food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicSex3 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecSex,
        `Indicator` == 'Household moderately or severely food insecure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  
  filtered_basic2 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecImm,
        `Indicator` == 'Household marginally food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicAge2 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecAge,
        `Indicator` == 'Household marginally food insecure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicSex2 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecSex,
        `Indicator` == 'Household marginally food insecure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  
  filtered_basic1 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecImm,
        `Indicator` == 'Household food secure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicAge1 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecAge,
        `Indicator` == 'Household food secure'
      )
    
    
    return(newDT)
  })
  
  filtered_basicSex1 <- reactive({
    newDT <- basicDT %>%
      
      filter(
        `VisMin` %in% input$VM185,
        `Year` %in% input$basicYear,
        `Geography` %in% input$basicGeo,
        `Confidence` %in% input$basicConfidence,
        `Characteristic` %in% input$basicCharSpecSex,
        `Indicator` == 'Household food secure'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  ####
  
  filtered_health13 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Unmet health care needs'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge13 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Unmet health care needs'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex13 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Unmet health care needs'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  filtered_health12 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived need for mental health care, needs not met'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge12 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived need for mental health care, needs not met'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex12 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived need for mental health care, needs not met'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  filtered_health11 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived need for mental health care, needs partially met or needs not met'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge11 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived need for mental health care, needs partially met or needs not met'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex11 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived need for mental health care, needs partially met or needs not met'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  filtered_health10 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived need for mental health care, all needs met'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge10 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived need for mental health care, all needs met'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex10 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived need for mental health care, all needs met'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  
  filtered_health9 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived need for mental health care, no need'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge9 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived need for mental health care, no need'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex9 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived need for mental health care, no need'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  ###
  filtered_health8 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Has a regular healthcare provider'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge8 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Has a regular healthcare provider'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex8 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Has a regular healthcare provider'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  ###
  
  filtered_health7 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Life satisfaction, satisfied or very satisfied'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge7 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Life satisfaction, satisfied or very satisfied'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex7 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Life satisfaction, satisfied or very satisfied'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  
  
  ###
  
  filtered_health6 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived life stress, most days quite a bit or extremely stressful'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge6 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived life stress, most days quite a bit or extremely stressful'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex6 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived life stress, most days quite a bit or extremely stressful'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  ###
  filtered_health4 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived mental health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge4 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived mental health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex4 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived mental health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  #####
  filtered_health3 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived mental health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge3 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived mental health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex3 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived mental health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  
  filtered_health2 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge2 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex2 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived health, fair or poor'
      )
    
    
    return(newDT)
  })
  
  
  
  
  ###
  
  filtered_health1 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecImm,
        `Indicator` == 'Perceived health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  filtered_healthAge1 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecAge,
        `Indicator` == 'Perceived health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  filtered_healthSex1 <- reactive({
    newDT <- healthDT %>%
      
      filter(
        `VisMin` %in% input$VM180,
        `Year` %in% input$healthYear,
        `Geography` %in% input$healthGeo,
        `Confidence` %in% input$healthConfidence,
        `Characteristic` %in% input$healthCharSpecSex,
        `Indicator` == 'Perceived health, very good or excellent'
      )
    
    
    return(newDT)
  })
  
  
  
  ####
  
  filtered_civic17 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Engaged in political activities'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge17 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Engaged in political activities'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex17 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Engaged in political activities'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen17 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Engaged in political activities'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang17 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Engaged in political activities'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu17 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Engaged in political activities'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  ######
  
  filtered_civic16 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in environmental group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge16 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in environmental group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex16 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in environmental group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen16 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in environmental group'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang16 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in environmental group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu16 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in environmental group'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ######
  filtered_civic15 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in immigrant or ethnic association or club'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge15 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in immigrant or ethnic association or club'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex15 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in immigrant or ethnic association or club'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen15 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in immigrant or ethnic association or club'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang15 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in immigrant or ethnic association or club'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu15 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in immigrant or ethnic association or club'
      )
    
    
    return(newDT)
  })
  
  
  
  #######
  
  filtered_civic14 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in youth organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge14 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in youth organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex14 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in youth organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen14 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in youth organization'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang14 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in youth organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu14 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in youth organization'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #######
  filtered_civic12 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in humanitarian or charitable organization or service club'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge12 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in humanitarian or charitable organization or service club'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex12 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in humanitarian or charitable organization or service club'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen12 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in humanitarian or charitable organization or service club'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang12 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in humanitarian or charitable organization or service club'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu12 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in humanitarian or charitable organization or service club'
      )
    
    
    return(newDT)
  })
  
  
  
  
  ##########
  
  filtered_civic11 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in school group, neighbourhood, civic or community association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge11 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in school group, neighbourhood, civic or community association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex11 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in school group, neighbourhood, civic or community association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen11 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in school group, neighbourhood, civic or community association'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang11 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in school group, neighbourhood, civic or community association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu11 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in school group, neighbourhood, civic or community association'
      )
    
    
    return(newDT)
  })
  
  
  
  
  ##########
  filtered_civic10 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in religious-affiliated group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge10 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in religious-affiliated group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex10 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in religious-affiliated group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen10 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in religious-affiliated group'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang10 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in religious-affiliated group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu10 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in religious-affiliated group'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  #########
  filtered_civic9 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in political party or group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge9 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in political party or group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex9 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in political party or group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen9 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in political party or group'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang9 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in political party or group'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu9 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in political party or group'
      )
    
    
    return(newDT)
  })
  
  
  
  ###
  
  filtered_civic8 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in union or professional association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge8 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in union or professional association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex8 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in union or professional association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen8 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in union or professional association'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang8 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in union or professional association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu8 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in union or professional association'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ##
  
  filtered_civic7 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in cultural, educational or hobby organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge7 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in cultural, educational or hobby organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex7 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in cultural, educational or hobby organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen7 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in cultural, educational or hobby organization'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang7 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in cultural, educational or hobby organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu7 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in cultural, educational or hobby organization'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  filtered_civic6 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant of at least one group, organization or association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge6 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant of at least one group, organization or association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex6 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant of at least one group, organization or association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen6 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant of at least one group, organization or association'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang6 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant of at least one group, organization or association'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu6 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant of at least one group, organization or association'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  
  filtered_civic5 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecImm3,
        `Indicator` == 'Member or participant in sports or recreational organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge5 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecAge3,
        `Indicator` == 'Member or participant in sports or recreational organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex5 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGend3,
        `Indicator` == 'Member or participant in sports or recreational organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen5 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecGen3,
        `Indicator` == 'Member or participant in sports or recreational organization'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang5 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecLang3,
        `Indicator` == 'Member or participant in sports or recreational organization'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu5 <- reactive({
    newDT <- civicDT %>%
      
      filter(
        `VisMin` %in% input$VM175,
        `Year` %in% input$CivicYear3,
        `Geography` %in% input$CivicGeo3,
        `Confidence` %in% input$CivicConf3,
        `Characteristic` %in% input$CivicCharSpecEdu3,
        `Indicator` == 'Member or participant in sports or recreational organization'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  ###
  
  filtered_civic4 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecImm2,
        `Indicator` == 'Voted in last provincial election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge4 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecAge2,
        `Indicator` == 'Voted in last provincial election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex4 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecSex2,
        `Indicator` == 'Voted in last provincial election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen4 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecGen2,
        `Indicator` == 'Voted in last provincial election'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang4 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecLang2,
        `Indicator` == 'Voted in last provincial election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu4 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecEdu2,
        `Indicator` == 'Voted in last provincial election'
      )
    
    
    return(newDT)
  })
  
  
  
  
  ####
  filtered_civic3 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecImm2,
        `Indicator` == 'Voted in last federal election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge3 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecAge2,
        `Indicator` == 'Voted in last federal election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex3 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecSex2,
        `Indicator` == 'Voted in last federal election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen3 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecGen2,
        `Indicator` == 'Voted in last federal election'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang3 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecLang2,
        `Indicator` == 'Voted in last federal election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu3 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecEdu2,
        `Indicator` == 'Voted in last federal election'
      )
    
    
    return(newDT)
  })
  
  
  
  ####
  filtered_civic2 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecImm2,
        `Indicator` == 'Voted in last municipal election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicAge2 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecAge2,
        `Indicator` == 'Voted in last municipal election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicSex2 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecSex2,
        `Indicator` == 'Voted in last municipal election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicGen2 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecGen2,
        `Indicator` == 'Voted in last municipal election'
      )
    
    
    return(newDT)
  })
  
  
  filtered_civicLang2 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecLang2,
        `Indicator` == 'Voted in last municipal election'
      )
    
    
    return(newDT)
  })
  
  filtered_civicEdu2 <- reactive({
    newDT <- civicDT2 %>%
      
      filter(
        `VisMin` %in% input$VM170,
        `Year` %in% input$CivicYear2,
        `Geography` %in% input$CivicGeo2,
        `Confidence` %in% input$CivicConf2,
        `Characteristic` %in% input$CivicCharSpecEdu2,
        `Indicator` == 'Voted in last municipal election'
      )
    
    
    return(newDT)
  })
  
  
  
  #End #####
  filtered_Conf12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecImm,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecAge,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecSex,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecGen,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecLang,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecEdu,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  
  
  #End
  filtered_Conf11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecImm,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecAge,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecSex,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecGen,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecLang,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecEdu,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  
  #End
  
  filtered_Conf10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecImm,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecAge,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecSex,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecGen,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecLang,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecEdu,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  
  #End
  
  
  filtered_Conf9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecImm,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecAge,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecSex,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecGen,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecLang,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecEdu,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  
  
  #End
  
  filtered_Conf8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecImm,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecAge,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecSex,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecGen,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecLang,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM401,
        `Year` %in% input$confYearS,
        `Geography` %in% input$confGeoS,
        `Confidence` %in% input$confConfidence,
        `Characteristic` %in% input$confCharSpecEdu,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  
  #End
  
  
  
  
  filtered_Employ7 <- reactive({
    newDT <- employmentDT %>%
      
      filter(
        `VisMin` %in% input$VM135,
        `Year` %in% input$EmploymentYear7,
        `Geography` %in% input$EmploymentGeo7,
        `Confidence` %in% input$EmploymentConf7,
        `Characteristic` %in% input$EmploymentChar7,
        `Indicator` == 'Have access to disability insurance under employment contract'
      )
    
    
    return(newDT)
  })
  
  filtered_Employ6 <- reactive({
    newDT <- employmentDT %>%
      
      filter(
        `VisMin` %in% input$VM130,
        `Year` %in% input$EmploymentYear6,
        `Geography` %in% input$EmploymentGeo6,
        `Confidence` %in% input$EmploymentConf6,
        `Characteristic` %in% input$EmploymentChar6,
        `Indicator` == 'Match between education and employment'
      )
    
    
    return(newDT)
  })
  
  filtered_Employ5 <- reactive({
    newDT <- employmentDT %>%
      
      filter(
        `VisMin` %in% input$VM125,
        `Year` %in% input$EmploymentYear5,
        `Geography` %in% input$EmploymentGeo5,
        `Confidence` %in% input$EmploymentConf5,
        `Characteristic` %in% input$EmploymentChar5,
        `Indicator` == 'Have access to paid vacation leave under employment contract'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Employ4 <- reactive({
    newDT <- employmentDT %>%
      
      filter(
        `VisMin` %in% input$VM120,
        `Year` %in% input$EmploymentYear4,
        `Geography` %in% input$EmploymentGeo4,
        `Confidence` %in% input$EmploymentConf4,
        `Characteristic` %in% input$EmploymentChar4,
        `Indicator` == 'Employment contract includes at least one type of employment benefits'
      )
    
    
    return(newDT)
  })
  
  filtered_Employ3 <- reactive({
    newDT <- employmentDT %>%
      
      filter(
        `VisMin` %in% input$VM115,
        `Year` %in% input$EmploymentYear3,
        `Geography` %in% input$EmploymentGeo3,
        `Confidence` %in% input$EmploymentConf3,
        `Characteristic` %in% input$EmploymentChar3,
        `Indicator` == 'Have a workplace pension plan'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Employ2 <- reactive({
    newDT <- employmentDT %>%
      
      filter(
        `VisMin` %in% input$VM110,
        `Year` %in% input$EmploymentYear2,
        `Geography` %in% input$EmploymentGeo2,
        `Confidence` %in% input$EmploymentConf2,
        `Characteristic` %in% input$EmploymentChar2,
        `Indicator` == 'Job offers good prospects for career advancement'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Employ <- reactive({
    newDT <- employmentDT %>%
      
      filter(
        `VisMin` %in% input$VM100,
        `Year` %in% input$EmploymentYear,
        `Geography` %in% input$EmploymentGeo,
        `Confidence` %in% input$EmploymentConf,
        `Characteristic` %in% input$EmploymentChar,
        `Indicator` == 'Have access to paid sick leave under employment contract'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_Conf <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecImmP,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecAgeP,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenderP,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenP,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecLangP,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecEduP,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_Conf1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecImmP,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecAgeP,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenderP,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenP,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecLangP,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecEduP,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_Conf2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecImmP,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecAgeP,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenderP,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenP,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecLangP,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecEduP,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_Conf3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecImmP,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecAgeP,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenderP,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenP,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecLangP,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecEduP,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_Conf4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecImmP,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecAgeP,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenderP,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenP,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecLangP,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecEduP,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  filtered_Conf5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecImmP,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecAgeP,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenderP,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenP,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecLangP,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecEduP,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  
  filtered_Conf6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecImmP,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecAgeP,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenderP,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenP,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecLangP,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecEduP,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  ####
  filtered_Conf7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecImmP,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfAge7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecAgeP,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfSex7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenderP,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfGen7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecGenP,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecLangP,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM40,
        `Year` %in% input$confYear,
        `Geography` %in% input$confGeo,
        `Confidence` %in% input$confConfidenceP,
        `Characteristic` %in% input$confCharSpecEduP,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  
  
  ##
  
  #Thursday
  
  filtered_Work <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Work2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_WorkAge <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkAge2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkGender <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkGender2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkGen <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkGen2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkLang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkLang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkEdu <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_WorkEdu2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #end
  
  filtered_Bank <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Bank2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_BankAge <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankAge2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankSex <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankSex2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankGen <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankGen2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankLang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankLang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankEdu <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_BankEdu2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  
  #end
  filtered_Class <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Class2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_ClassAge <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ClassAge2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  #######
  
  filtered_ClassGender <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ClassGender2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  ####
  
  filtered_ClassGen <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ClassGen2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ClassLang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ClassLang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ClassEdu <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Discrimination when attending school or classes, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ClassEdu2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Discrimination when attending school or classes, since the  beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  #end
  
  filtered_Pol <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Pol2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_PolAge <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolAge2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolSex <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolSex2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolGen <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolGen2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolLang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolLang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolEdu <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Discrimination when dealing with the police, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_PolEdu2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Discrimination when dealing with the police, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #end
  
  filtered_Lang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Lang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_LangAge <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangAge2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangSex <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangSex2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangGen <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangGen2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangLang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangLang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangEdu <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination based on language, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_LangEdu2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination based on language, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  #end
  
  filtered_Rel <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Rel2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_RelAge <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelAge2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelSex <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelSex2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelGen <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelGen2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelLang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelLang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelEdu <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination based on religion, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_RelEdu2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination based on religion, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  #end
  
  filtered_Col <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Col2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_ColAge <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColAge2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColSex <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColSex2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColGen <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColGen2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColLang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColLang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColEdu <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_ColEdu2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination based on race or colour, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  #end
  filtered_Cov <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Cov2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_CovAge <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovAge2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovSex <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovSex2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovGen <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovGen2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovLang <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovLang2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovEdu <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovEdu21 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  
  
  filtered_Cov1 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_Cov21 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecImm,
        `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  
  filtered_CovAge1 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovAge21 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecAge,
        `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovSex1 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovSex21 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSex,
        `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovGen1 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovGen21 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecGen,
        `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovLang1 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovLang21 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecLang,
        `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovEdu1 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination, 5 years before COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  filtered_CovEdu2 <- reactive({
    newDT <- discriminationDT %>%
      
      filter(
        `VisMin` %in% input$VM30,
        `Year` %in% input$covYear,
        `Geography` %in% input$covGeo,
        `Confidence` %in% input$covConfidence,
        `Characteristic` %in% input$covCharSpecEdu,
        `Indicator` == 'Experience(s) of discrimination since the beginning of COVID-19 pandemic'
      )
    
    
    return(newDT)
  })
  
  
  ###
  #Mimi
  
  #Filters for Immigration - Public services
  
  
  filtered_2Conf <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecImmIM,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecAgeIM,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenderIM,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenIM,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecLangIM,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecEduIM,
        `Indicator` == 'Confidence in the police service'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_2Conf1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecImmIM,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecAgeIM,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenderIM,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenIM,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecLangIM,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu1 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecEduIM,
        `Indicator` == 'Confidence in Federal Parliament'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_2Conf2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecImmIM,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecAgeIM,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenderIM,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenIM,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecLangIM,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu2 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecEduIM,
        `Indicator` == 'Confidence in the Canadian media'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_2Conf3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecImmIM,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecAgeIM,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenderIM,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenIM,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecLangIM,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu3 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecEduIM,
        `Indicator` == 'Confidence in the school system'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_2Conf4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecImmIM,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecAgeIM,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenderIM,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenIM,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecLangIM,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu4 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecEduIM,
        `Indicator` == 'Confidence in the justice system and courts'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  filtered_2Conf5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecImmIM,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecAgeIM,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenderIM,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenIM,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecLangIM,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu5 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecEduIM,
        `Indicator` == 'Confidence in major corporations'
      )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  
  filtered_2Conf6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecImmIM,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecAgeIM,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenderIM,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenIM,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecLangIM,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu6 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecEduIM,
        `Indicator` == 'Confidence in merchants and local business people'
      )
    
    
    return(newDT)
  })
  
  ####
  filtered_2Conf7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecImmIM,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecAgeIM,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenderIM,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecGenIM,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecLangIM,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu7 <- reactive({
    newDT <- confidenceDT %>%
      
      filter(
        `VisMin` %in% input$VM205,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidenceIM,
        `Characteristic` %in% input$confCharSpecEduIM,
        `Indicator` == 'Confidence in banks'
      )
    
    
    return(newDT)
  })
  
  
  
  #Filters for Immigration - Social connections
  
  filtered_2Conf12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecImm2,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecAge2,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecSex2,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecGen2,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecLang2,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu12 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecEdu2,
        `Indicator` == 'Strong sense of belonging to Canada'
      )
    
    
    return(newDT)
  })
  
  
  
  #End
  filtered_2Conf11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecImm2,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecAge2,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecSex2,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecGen2,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecLang2,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu11 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecEdu2,
        `Indicator` == 'Strong sense of belonging to the province'
      )
    
    
    return(newDT)
  })
  
  
  #End
  
  filtered_2Conf10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecImm2,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecAge2,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecSex2,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecGen2,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecLang2,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu10 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecEdu2,
        `Indicator` == 'Strong sense of belonging to the town or city'
      )
    
    
    return(newDT)
  })
  
  
  #End
  
  
  filtered_2Conf9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecImm2,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecAge2,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecSex2,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecGen2,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecLang2,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu9 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecEdu2,
        `Indicator` == 'Strong sense of belonging to the local community'
      )
    
    
    return(newDT)
  })
  
  
  
  #End
  
  filtered_2Conf8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecImm2,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfAge8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecAge2,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfSex8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecSex2,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfGen8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecGen2,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2ConfLang8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecLang2,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  filtered_2ConfEdu8 <- reactive({
    newDT <- belongingDT %>%
      
      filter(
        `VisMin` %in% input$VM225,
        `Year` %in% input$confYearIM,
        `Geography` %in% input$confGeoIM,
        `Confidence` %in% input$confConfidence2,
        `Characteristic` %in% input$confCharSpecEdu2,
        `Indicator` == 'Reported that most people can be trusted in general'
      )
    
    
    return(newDT)
  })
  
  
  
  #Filters for Immigration - Representation
  filtered_2rep3VM <- reactive({
    newDT <- representationDT %>%
      filter(
        `Immigration` %in% input$RepImm3,
        `Degree` %in% input$RepDegree3,
        `VisMin` %in% input$VM210,
        `Geography` %in% input$RepGeo3,
        `Year` %in% input$RepYear3,
        `Age` %in% input$RepAgeLang3,
        `Sex` %in% input$RepSex3,
        `Indicator` == 'Workers in all management occupations'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2rep2VM <- reactive({
    newDT <- representationDT %>%
      filter(
        `Immigration` %in% input$RepImm3,
        `Degree` %in% input$RepDegree3,
        `VisMin` %in% input$VM210,
        `Geography` %in% input$RepGeo3,
        `Year` %in% input$RepYear3,
        `Age` %in% input$RepAgeLang3,
        `Sex` %in% input$RepSex3,
        `Indicator` == 'Workers in senior management occupations'
      )
    
    
    return(newDT)
  })
  
  
  filtered_2rep1VM <- reactive({
    newDT <- representationDT %>%
      filter(
        `Immigration` %in% input$RepImm3,
        `Degree` %in% input$RepDegree3,
        `VisMin` %in% input$VM210,
        `Geography` %in% input$RepGeo3,
        `Year` %in% input$RepYear3,
        `Age` %in% input$RepAgeLang3,
        `Sex` %in% input$RepSex3,
        `Indicator` == 'Workers in middle management occupations'
      )
    
    
    return(newDT)
  })
  
  
  
  #Filters for Immigration Labor Participation
  
  filtered_rep4VM2 <- reactive({
    newDT <- representationDT %>%
      filter(
        `Immigration` %in% input$RepImm2,
        `Degree` %in% input$RepDegree2,
        `VisMin` %in% input$VM190,
        `Geography` %in% input$RepGeo2,
        `Year` %in% input$RepYear2,
        `Age` %in% input$RepAgeLang2,
        `Sex` %in% input$RepSex2,
        `Indicator` == 'Population in self-employment (unincorporated)'
      )
    
    
    return(newDT)
  })
  
  #end
  
  filtered_youthVM2 <- reactive({
    newDT <- youthDT %>%
      filter(
        `Immigration` %in% input$YouthImm2,
        `Language` %in% input$YouthLang2,
        `VisMin` %in% input$VM195,
        `Geography` %in% input$YouthGeo2,
        `Year` %in% input$YouthYear2,
        `Age` %in% input$YouthAge2,
        `Sex` %in% input$YouthSex2
      )
    
    
    return(newDT)
  })
  
  
  
  #end
  filtered_rate4VM2 <- reactive({
    newDT <- rateDT %>%
      filter(
        `Immigration` %in% input$RateImm2,
        `Degree` %in% input$RateDegree2,
        `VisMin` %in% input$VM245,
        `Geography` %in% input$RateGeo2,
        `Year` %in% input$RateYear2,
        `Age` %in% input$RateAgeLang2,
        `Sex` %in% input$RateSex2,
        `Indicator` == 'Population in full-time employment'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #end
  filtered_rate3VM2 <- reactive({
    newDT <- rateDT %>%
      filter(
        `Immigration` %in% input$RateImm2,
        `Degree` %in% input$RateDegree2,
        `VisMin` %in% input$VM245,
        `Geography` %in% input$RateGeo2,
        `Year` %in% input$RateYear2,
        `Age` %in% input$RateAgeLang2,
        `Sex` %in% input$RateSex2,
        `Indicator` == 'Unemployment rate'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #end
  filtered_rate2VM2 <- reactive({
    newDT <- rateDT %>%
      filter(
        `Immigration` %in% input$RateImm2,
        `Degree` %in% input$RateDegree2,
        `VisMin` %in% input$VM245,
        `Geography` %in% input$RateGeo2,
        `Year` %in% input$RateYear2,
        `Age` %in% input$RateAgeLang2,
        `Sex` %in% input$RateSex2,
        `Indicator` == 'Employment rate'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #end
  filtered_rate1VM2 <- reactive({
    newDT <- rateDT %>%
      filter(
        `Immigration` %in% input$RateImm2,
        `Degree` %in% input$RateDegree2,
        `VisMin` %in% input$VM245,
        `Geography` %in% input$RateGeo2,
        `Year` %in% input$RateYear2,
        `Age` %in% input$RateAgeLang2,
        `Sex` %in% input$RateSex2,
        `Indicator` == 'Participation rate'
      )
    
    
    return(newDT)
  })
  
  
  
  #end
  filtered_inc2VM2 <- reactive({
    newDT <- incomeDT %>%
      filter(
        `Immigration` %in% input$IncImm2,
        `Degree` %in% input$IncDegree2,
        `VisMin` %in% input$VM240,
        `Geography` %in% input$IncGeo2,
        `Year` %in% input$IncYear2,
        `Age` %in% input$IncAgeLang2,
        `Sex` %in% input$IncSex2,
        `Indicator` == 'Average weekly earnings (full-time)'
      )
    
    
    return(newDT)
  })
  
  #end
  filtered_inc1VM2 <- reactive({
    newDT <- incomeDT %>%
      filter(
        `Immigration` %in% input$IncImm2,
        `Degree` %in% input$IncDegree2,
        `VisMin` %in% input$VM240,
        `Geography` %in% input$IncGeo2,
        `Year` %in% input$IncYear2,
        `Age` %in% input$IncAgeLang2,
        `Sex` %in% input$IncSex2,
        `Indicator` == 'Average employment income'
      )
    
    
    return(newDT)
  })
  
  
  #end
  
  
  
  #Filters for Visible Minority Labor Participation
  filtered_rate4VM <- reactive({
    newDT <- rateDT %>%
      filter(
        `Immigration` %in% input$RateImm,
        `Degree` %in% input$RateDegree,
        `VisMin` %in% input$VM235,
        `Geography` %in% input$RateGeo,
        `Year` %in% input$RateYear,
        `Age` %in% input$RateAgeLang,
        `Sex` %in% input$RateSex,
        `Indicator` == 'Population in full-time employment'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #end
  filtered_rate3VM <- reactive({
    newDT <- rateDT %>%
      filter(
        `Immigration` %in% input$RateImm,
        `Degree` %in% input$RateDegree,
        `VisMin` %in% input$VM235,
        `Geography` %in% input$RateGeo,
        `Year` %in% input$RateYear,
        `Age` %in% input$RateAgeLang,
        `Sex` %in% input$RateSex,
        `Indicator` == 'Unemployment rate'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #end
  filtered_rate2VM <- reactive({
    newDT <- rateDT %>%
      filter(
        `Immigration` %in% input$RateImm,
        `Degree` %in% input$RateDegree,
        `VisMin` %in% input$VM235,
        `Geography` %in% input$RateGeo,
        `Year` %in% input$RateYear,
        `Age` %in% input$RateAgeLang,
        `Sex` %in% input$RateSex,
        `Indicator` == 'Employment rate'
      )
    
    
    return(newDT)
  })
  
  
  
  
  #end
  filtered_rate1VM <- reactive({
    newDT <- rateDT %>%
      filter(
        `Immigration` %in% input$RateImm,
        `Degree` %in% input$RateDegree,
        `VisMin` %in% input$VM235,
        `Geography` %in% input$RateGeo,
        `Year` %in% input$RateYear,
        `Age` %in% input$RateAgeLang,
        `Sex` %in% input$RateSex,
        `Indicator` == 'Participation rate'
      )
    
    
    return(newDT)
  })
  
  
  
  #end
  filtered_inc2VM <- reactive({
    newDT <- incomeDT %>%
      filter(
        `Immigration` %in% input$IncImm1,
        `Degree` %in% input$IncDegree1,
        `VisMin` %in% input$VM230,
        `Geography` %in% input$IncGeo1,
        `Year` %in% input$IncYear1,
        `Age` %in% input$IncAgeLang1,
        `Sex` %in% input$IncSex1,
        `Indicator` == 'Average weekly earnings (full-time)'
      )
    
    
    return(newDT)
  })
  
  #end
  filtered_inc1VM <- reactive({
    newDT <- incomeDT %>%
      filter(
        `Immigration` %in% input$IncImm1,
        `Degree` %in% input$IncDegree1,
        `VisMin` %in% input$VM230,
        `Geography` %in% input$IncGeo1,
        `Year` %in% input$IncYear1,
        `Age` %in% input$IncAgeLang1,
        `Sex` %in% input$IncSex1,
        `Indicator` == 'Average employment income'
      )
    
    
    return(newDT)
  })
  
  
  
  
  
  #end
  
  filtered_rep4VM <- reactive({
    newDT <- representationDT %>%
      filter(
        `Immigration` %in% input$RepImm1,
        `Degree` %in% input$RepDegree1,
        `VisMin` %in% input$VM155,
        `Geography` %in% input$RepGeo1,
        `Year` %in% input$RepYear1,
        `Age` %in% input$RepAgeLang1,
        `Sex` %in% input$RepSex1,
        `Indicator` == 'Population in self-employment (unincorporated)'
      )
    
    
    return(newDT)
  })
  
  #end
  
  filtered_rep3VM <- reactive({
    newDT <- representationDT %>%
      filter(
        `Immigration` %in% input$RepImm2,
        `Degree` %in% input$RepDegree2,
        `VisMin` %in% input$VM150,
        `Geography` %in% input$RepGeo2,
        `Year` %in% input$RepYear2,
        `Age` %in% input$RepAgeLang2,
        `Sex` %in% input$RepSex2,
        `Indicator` == 'Workers in all management occupations'
      )
    
    
    return(newDT)
  })
  
  
  filtered_rep2VM <- reactive({
    newDT <- representationDT %>%
      filter(
        `Immigration` %in% input$RepImm2,
        `Degree` %in% input$RepDegree2,
        `VisMin` %in% input$VM150,
        `Geography` %in% input$RepGeo2,
        `Year` %in% input$RepYear2,
        `Age` %in% input$RepAgeLang2,
        `Sex` %in% input$RepSex2,
        `Indicator` == 'Workers in senior management occupations'
      )
    
    
    return(newDT)
  })
  
  
  filtered_rep1VM <- reactive({
    newDT <- representationDT %>%
      filter(
        `Immigration` %in% input$RepImm2,
        `Degree` %in% input$RepDegree2,
        `VisMin` %in% input$VM150,
        `Geography` %in% input$RepGeo2,
        `Year` %in% input$RepYear2,
        `Age` %in% input$RepAgeLang2,
        `Sex` %in% input$RepSex2,
        `Indicator` == 'Workers in middle management occupations'
      )
    
    
    return(newDT)
  })
  
  
  #End
  filtered_youthVM <- reactive({
    newDT <- youthDT %>%
      filter(
        `Immigration` %in% input$YouthImm,
        `Language` %in% input$YouthLang,
        `VisMin` %in% input$VM140,
        `Geography` %in% input$YouthGeo,
        `Year` %in% input$YouthYear,
        `Age` %in% input$YouthAge,
        `Sex` %in% input$YouthSex
      )
    
    
    return(newDT)
  })
  
  
  #End
  filtered_OverVM <- reactive({
    newDT <- OverQualDT %>%
      filter(
        `Immigration` %in% input$OverImm,
        `Degree` %in% input$OverDegree,
        `Language` %in% input$OverLang,
        `Location` %in% input$OverLocation,
        `VisMin` %in% input$VM20,
        `Geography` %in% input$OverGeo,
        `Year` %in% input$OverYear,
        `Age` %in% input$OverAge,
        `Sex` %in% input$OverSex
      )
    
    
    return(newDT)
  })
  
  
  filtered_OverGEO <- reactive({
    newDT <- OverQualDT %>%
      filter(
        `Immigration` %in% input$OverImmGEO,
        `Degree` %in% input$OverDegreeGEO,
        `Language` %in% input$OverLangGEO,
        `Location` %in% input$OverLocationGEO,
        `VisMin` %in% input$OverVMGEO,
        `Geography` %in% input$VM23,
        `Year` %in% input$OverYearGEO,
        `Age` %in% input$OverAgeGEO,
        `Sex` %in% input$OverSexGEO
      )
    
    
    return(newDT)
  })
  
  filtered_OverIS <- reactive({
    newDT <- OverQualDT %>%
      filter(
        `Immigration` %in% input$VM21,
        `Degree` %in% input$OverDegreeIS,
        `Language` %in% input$OverLangIS,
        `Location` %in% input$OverLocationIS,
        `VisMin` %in% input$OverVMIS,
        `Geography` %in% input$OverGeoIS,
        `Year` %in% input$OverYearIS,
        `Age` %in% input$OverAgeIS,
        `Sex` %in% input$OverSexIS
      )
    
    
    return(newDT)
  })
  
  
  filtered_OverSX <- reactive({
    newDT <- OverQualDT %>%
      filter(
        `Immigration` %in% input$OverImmSX,
        `Degree` %in% input$OverDegreeSX,
        `Language` %in% input$OverLangSX,
        `Location` %in% input$OverLocationSX,
        `VisMin` %in% input$OverVMSX,
        `Geography` %in% input$OverGeoSX,
        `Year` %in% input$OverYearSX,
        `Age` %in% input$OverAgeSX,
        `Sex` %in% input$VM22
      )
    
    
    return(newDT)
  })
  
  
  filtered_educationSX1 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduImm3,
        `Language` %in% input$eduLang3,
        `VisMin` %in% input$eduVisMin3,
        `Geography` %in% input$eduGeo3,
        `Year` %in% input$eduYear3,
        `Age` %in% input$eduAge3,
        `Sex` %in% input$VM11,
        `Indicators` == "Population with bachelor’s degree"
      )
    
    
    return(newDT)
  })
  
  filtered_educationSX2 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduImm3,
        `Language` %in% input$eduLang3,
        `VisMin` %in% input$eduVisMin3,
        `Geography` %in% input$eduGeo3,
        `Year` %in% input$eduYear3,
        `Age` %in% input$eduAge3,
        `Sex` %in% input$VM11,
        `Indicators` == "Population with no certificate, diploma or degree"
      )
    
    
    return(newDT)
  })
  
  filtered_educationSX3 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduImm3,
        `Language` %in% input$eduLang3,
        `VisMin` %in% input$eduVisMin3,
        `Geography` %in% input$eduGeo3,
        `Year` %in% input$eduYear3,
        `Age` %in% input$eduAge3,
        `Sex` %in% input$VM11,
        `Indicators` == "Population with high school diploma or equivalency certificate"
      )
    
    
    return(newDT)
  })
  
  
  filtered_educationSX4 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduImm3,
        `Language` %in% input$eduLang3,
        `VisMin` %in% input$eduVisMin3,
        `Geography` %in% input$eduGeo3,
        `Year` %in% input$eduYear3,
        `Age` %in% input$eduAge3,
        `Sex` %in% input$VM11,
        `Indicators` == "Population with postsecondary certificate or diploma below bachelor level"
      )
    
    
    return(newDT)
  })
  
  
  filtered_educationSX5 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduImm3,
        `Language` %in% input$eduLang3,
        `VisMin` %in% input$eduVisMin3,
        `Geography` %in% input$eduGeo3,
        `Year` %in% input$eduYear3,
        `Age` %in% input$eduAge3,
        `Sex` %in% input$VM11,
        `Indicators` == "Population with bachelor’s degree or above"
      )
    
    
    return(newDT)
  })
  
  
  filtered_educationSX6 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduImm3,
        `Language` %in% input$eduLang3,
        `VisMin` %in% input$eduVisMin3,
        `Geography` %in% input$eduGeo3,
        `Year` %in% input$eduYear3,
        `Age` %in% input$eduAge3,
        `Sex` %in% input$VM11,
        `Indicators` == "Population with master’s degree or earned doctorate"
      )
    
    
    return(newDT)
  })
  filtered_educationVM1 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduVisMin2,
        `Language` %in% input$eduLang2,
        `VisMin` %in% input$VM10,
        `Geography` %in% input$eduGeo2,
        `Year` %in% input$eduYear2,
        `Age` %in% input$eduAge2,
        `Sex` %in% input$eduSex2,
        `Indicators` == "Population with bachelor’s degree"
      )
    
    
    return(newDT)
  })
  
  filtered_educationVM2 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduVisMin2,
        `Language` %in% input$eduLang2,
        `VisMin` %in% input$VM10,
        `Geography` %in% input$eduGeo2,
        `Year` %in% input$eduYear2,
        `Age` %in% input$eduAge2,
        `Sex` %in% input$eduSex2,
        `Indicators` == "Population with no certificate, diploma or degree"
      )
    
    
    return(newDT)
  })
  
  filtered_educationVM3 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduVisMin2,
        `Language` %in% input$eduLang2,
        `VisMin` %in% input$VM10,
        `Geography` %in% input$eduGeo2,
        `Year` %in% input$eduYear2,
        `Age` %in% input$eduAge2,
        `Sex` %in% input$eduSex2,
        `Indicators` == "Population with high school diploma or equivalency certificate"
      )
    
    
    return(newDT)
  })
  
  
  filtered_educationVM4 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduVisMin2,
        `Language` %in% input$eduLang2,
        `VisMin` %in% input$VM10,
        `Geography` %in% input$eduGeo2,
        `Year` %in% input$eduYear2,
        `Age` %in% input$eduAge2,
        `Sex` %in% input$eduSex2,
        `Indicators` == "Population with postsecondary certificate or diploma below bachelor level"
      )
    
    
    return(newDT)
  })
  
  
  filtered_educationVM5 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduVisMin2,
        `Language` %in% input$eduLang2,
        `VisMin` %in% input$VM10,
        `Geography` %in% input$eduGeo2,
        `Year` %in% input$eduYear2,
        `Age` %in% input$eduAge2,
        `Sex` %in% input$eduSex2,
        `Indicators` == "Population with bachelor’s degree or above"
      )
    
    
    return(newDT)
  })
  
  
  filtered_educationVM6 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$eduVisMin2,
        `Language` %in% input$eduLang2,
        `VisMin` %in% input$VM10,
        `Geography` %in% input$eduGeo2,
        `Year` %in% input$eduYear2,
        `Age` %in% input$eduAge2,
        `Sex` %in% input$eduSex2,
        `Indicators` == "Population with master’s degree or earned doctorate"
      )
    
    
    return(newDT)
  })
  
  filtered_education1 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$VM9,
        `Language` %in% input$eduLang,
        `VisMin` %in% input$eduVisMin,
        `Geography` %in% input$eduGeo,
        `Year` %in% input$eduYear,
        `Age` %in% input$eduAge,
        `Sex` %in% input$eduSex,
        `Indicators` == "Population with bachelor’s degree"
      )
    
    
    return(newDT)
  })
  
  filtered_education2 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$VM9,
        `Language` %in% input$eduLang,
        `VisMin` %in% input$eduVisMin,
        `Geography` %in% input$eduGeo,
        `Year` %in% input$eduYear,
        `Age` %in% input$eduAge,
        `Sex` %in% input$eduSex,
        `Indicators` == "Population with no certificate, diploma or degree"
      )
    
    
    return(newDT)
  })
  
  
  filtered_education3 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$VM9,
        `Language` %in% input$eduLang,
        `VisMin` %in% input$eduVisMin,
        `Geography` %in% input$eduGeo,
        `Year` %in% input$eduYear,
        `Age` %in% input$eduAge,
        `Sex` %in% input$eduSex,
        `Indicators` == "Population with high school diploma or equivalency certificate"
      )
    
    
    return(newDT)
  })
  
  filtered_education4 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$VM9,
        `Language` %in% input$eduLang,
        `VisMin` %in% input$eduVisMin,
        `Geography` %in% input$eduGeo,
        `Year` %in% input$eduYear,
        `Age` %in% input$eduAge,
        `Sex` %in% input$eduSex,
        `Indicators` == "Population with postsecondary certificate or diploma below bachelor level"
      )
    
    
    return(newDT)
  })
  filtered_education5 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$VM9,
        `Language` %in% input$eduLang,
        `VisMin` %in% input$eduVisMin,
        `Geography` %in% input$eduGeo,
        `Year` %in% input$eduYear,
        `Age` %in% input$eduAge,
        `Sex` %in% input$eduSex,
        `Indicators` == "Population with bachelor’s degree or above"
      )
    
    
    return(newDT)
  })
  filtered_education6 <- reactive({
    newDT <- educationDT %>%
      filter(
        `Immigration` %in% input$VM9,
        `Language` %in% input$eduLang,
        `VisMin` %in% input$eduVisMin,
        `Geography` %in% input$eduGeo,
        `Year` %in% input$eduYear,
        `Age` %in% input$eduAge,
        `Sex` %in% input$eduSex,
        `Indicators` == "Population with master’s degree or earned doctorate"
      )
    
    
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
      filter(`Motivation` %in% input$VisMi2)
    
    
    # Pivot by VisMin
    newDT <-
      pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
    
    return(newDT)
  })
  
  
  filtered_linethreeData <- reactive({
    # Require Sex, Age, Generation Status, VisMin
    req(input$VisMi2)
    
    # Filter values
    newDT <- polData %>%
      filter(
        `Immigration` %in% input$OverImmLINE,
        `Degree` %in% input$OverDegreeLINE,
        `Language` %in% input$OverLangLINE,
        `Location` %in% input$OverLocationLINE,
        `VisMin` %in% input$OverVMLINE,
        `Geography` %in% input$OverGeoLINE,
        `Age` %in% input$OverAgeLINE,
        `Sex` %in% input$OverSexLINE
      )
    
    
    
    # Pivot by VisMin
    newDT <-
      pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
    
    return(newDT)
  })
  
  
  
  
  # This reactive filters the line plot data
  filtered_linetwoData <- reactive({
    # Require Sex, Age, Generation Status, VisMin
    req(input$VisMi)
    
    # Filter values
    newDT <- polData %>%
      filter(`Motivation` %in% input$VisMi)
    
    
    # Pivot by VisMin
    newDT <-
      pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
    
    return(newDT)
  })
  
  
  
  # Output ---------------------------------------------------
  
  #Leo
  #Plotly for Immigration
  
  output$sBar2Rep3 <- renderPlotly({
    req(filtered_2rep3VM())
    
    fig <-
      plot_ly(
        filtered_2rep3VM(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Workers in middle management occupations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBar2Rep2 <- renderPlotly({
    req(filtered_2rep2VM())
    
    fig <-
      plot_ly(
        filtered_2rep2VM(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Workers in senior management occupations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2Rep1 <- renderPlotly({
    req(filtered_2rep1VM())
    
    fig <-
      plot_ly(
        filtered_2rep1VM(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Workers in all management occupations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #end
  output$sBar2Basic5 <- renderPlotly({
    req(filtered_2basic5())
    
    
    fig <-
      plot_ly(
        filtered_2basic5(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicAge5 <- renderPlotly({
    req(filtered_2basicAge5)
    
    
    fig <-
      plot_ly(
        filtered_2basicAge5(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicSex5 <- renderPlotly({
    req(filtered_2basicSex5())
    
    
    fig <-
      plot_ly(
        filtered_2basicSex5(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  ###
  
  output$sBar2Basic3 <- renderPlotly({
    req(filtered_2basic3())
    
    
    fig <-
      plot_ly(
        filtered_2basic3(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately or severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicAge3 <- renderPlotly({
    req(filtered_2basicAge3)
    
    
    fig <-
      plot_ly(
        filtered_2basicAge3(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately or severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicSex3 <- renderPlotly({
    req(filtered_2basicSex3())
    
    
    fig <-
      plot_ly(
        filtered_2basicSex3(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately or severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  ###
  
  output$sBar2Basic4 <- renderPlotly({
    req(filtered_2basic4())
    
    
    fig <-
      plot_ly(
        filtered_2basic4(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicAge4 <- renderPlotly({
    req(filtered_2basicAge4)
    
    
    fig <-
      plot_ly(
        filtered_2basicAge4(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicSex4 <- renderPlotly({
    req(filtered_2basicSex4())
    
    
    fig <-
      plot_ly(
        filtered_2basicSex4(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ####
  output$sBar2Basic2 <- renderPlotly({
    req(filtered_2basic2())
    
    
    fig <-
      plot_ly(
        filtered_2basic2(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household marginally food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicAge2 <- renderPlotly({
    req(filtered_2basicAge2)
    
    
    fig <-
      plot_ly(
        filtered_2basicAge2(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household marginally food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicSex2 <- renderPlotly({
    req(filtered_2basicSex2())
    
    
    fig <-
      plot_ly(
        filtered_2basicSex2(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household marginally food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  ###
  output$sBar2Basic1 <- renderPlotly({
    req(filtered_2basic1())
    
    
    fig <-
      plot_ly(
        filtered_2basic1(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household food secure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicAge1 <- renderPlotly({
    req(filtered_2basicAge1)
    
    
    fig <-
      plot_ly(
        filtered_2basicAge1(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household food secure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2BasicSex1 <- renderPlotly({
    req(filtered_2basicSex1())
    
    
    fig <-
      plot_ly(
        filtered_2basicSex1(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household food secure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  ####
  
  output$sBar2Health14 <- renderPlotly({
    req(filtered_2health14())
    
    
    fig <-
      plot_ly(
        filtered_2health14(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs partially met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge14 <- renderPlotly({
    req(filtered_2healthAge14)
    
    
    fig <-
      plot_ly(
        filtered_2healthAge14(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs partially met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex14 <- renderPlotly({
    req(filtered_2healthSex14())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex14(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs partially met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  
  ##
  output$sBar2Health13 <- renderPlotly({
    req(filtered_2health13())
    
    
    fig <-
      plot_ly(
        filtered_2health13(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Unmet health care needs", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge13 <- renderPlotly({
    req(filtered_2healthAge13)
    
    
    fig <-
      plot_ly(
        filtered_2healthAge13(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Unmet health care needs", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex13 <- renderPlotly({
    req(filtered_2healthSex13())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex13(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Unmet health care needs", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  #####
  
  output$sBar2Health12 <- renderPlotly({
    req(filtered_2health12())
    
    
    fig <-
      plot_ly(
        filtered_2health12(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge12 <- renderPlotly({
    req(filtered_2healthAge12)
    
    
    fig <-
      plot_ly(
        filtered_2healthAge12(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex12 <- renderPlotly({
    req(filtered_2healthSex12())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex12(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ####
  
  output$sBar2Health11 <- renderPlotly({
    req(filtered_2health11())
    
    
    fig <-
      plot_ly(
        filtered_2health11(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs partially met or needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge11 <- renderPlotly({
    req(filtered_2healthAge11)
    
    
    fig <-
      plot_ly(
        filtered_2healthAge11(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs partially met or needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex11 <- renderPlotly({
    req(filtered_2healthSex11())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex11(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs partially met or needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  ###
  
  output$sBar2Health10 <- renderPlotly({
    req(filtered_2health10())
    
    
    fig <-
      plot_ly(
        filtered_2health10(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge10 <- renderPlotly({
    req(filtered_2healthAge10)
    
    
    fig <-
      plot_ly(
        filtered_2healthAge10(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex10 <- renderPlotly({
    req(filtered_2healthSex10())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex10(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  ###
  output$sBar2Health9 <- renderPlotly({
    req(filtered_2health9())
    
    
    fig <-
      plot_ly(
        filtered_2health9(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, no need", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge9 <- renderPlotly({
    req(filtered_2healthAge9)
    
    
    fig <-
      plot_ly(
        filtered_2healthAge9(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, no need", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex9 <- renderPlotly({
    req(filtered_2healthSex9())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex9(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, no need", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ####
  
  output$sBar2Health8 <- renderPlotly({
    req(filtered_2health8())
    
    
    fig <-
      plot_ly(
        filtered_2health8(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Has a regular healthcare provider", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge8 <- renderPlotly({
    req(filtered_2healthAge8)
    
    
    fig <-
      plot_ly(
        filtered_2healthAge8(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Has a regular healthcare provider", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex8 <- renderPlotly({
    req(filtered_2healthSex8())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex8(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Has a regular healthcare provider", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  ###
  
  output$sBar2Health7 <- renderPlotly({
    req(filtered_2health7())
    
    
    fig <-
      plot_ly(
        filtered_2health7(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Life satisfaction, satisfied or very satisfied", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge7 <- renderPlotly({
    req(filtered_2healthAge7())
    
    
    fig <-
      plot_ly(
        filtered_2healthAge7(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Life satisfaction, satisfied or very satisfied", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex7 <- renderPlotly({
    req(filtered_2healthSex7())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex7(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Life satisfaction, satisfied or very satisfied", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  ###
  
  output$sBar2Health6 <- renderPlotly({
    req(filtered_2health6())
    
    
    fig <-
      plot_ly(
        filtered_2health6(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived life stress, most days quite a bit or extremely stressful", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge6 <- renderPlotly({
    req(filtered_2healthAge6())
    
    
    fig <-
      plot_ly(
        filtered_2healthAge6(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived life stress, most days quite a bit or extremely stressful", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex6 <- renderPlotly({
    req(filtered_2healthSex6())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex6(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived life stress, most days quite a bit or extremely stressful", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  #####
  output$sBar2Health4 <- renderPlotly({
    req(filtered_2health4())
    
    
    fig <-
      plot_ly(
        filtered_2health4(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge4 <- renderPlotly({
    req(filtered_2healthAge4())
    
    
    fig <-
      plot_ly(
        filtered_2healthAge4(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex4 <- renderPlotly({
    req(filtered_2healthSex4())
    
    
    fig <-
      plot_ly(
        filtered_healthSex4(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ###
  
  output$sBar2Health3 <- renderPlotly({
    req(filtered_2health3())
    
    
    fig <-
      plot_ly(
        filtered_2health3(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge3 <- renderPlotly({
    req(filtered_2healthAge3())
    
    
    fig <-
      plot_ly(
        filtered_2healthAge3(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex3 <- renderPlotly({
    req(filtered_2healthSex3())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex3(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  ###
  
  output$sBar2Health2 <- renderPlotly({
    req(filtered_2health2())
    
    
    fig <-
      plot_ly(
        filtered_2health2(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge2 <- renderPlotly({
    req(filtered_2healthAge2())
    
    
    fig <-
      plot_ly(
        filtered_2healthAge2(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex2 <- renderPlotly({
    req(filtered_2healthSex2())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex2(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ###
  output$sBar2Health1 <- renderPlotly({
    req(filtered_2health1())
    
    
    fig <-
      plot_ly(
        filtered_2health1(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthAge1 <- renderPlotly({
    req(filtered_2healthAge1())
    
    
    fig <-
      plot_ly(
        filtered_2healthAge1(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  output$sBar2HealthSex1 <- renderPlotly({
    req(filtered_2healthSex1())
    
    
    fig <-
      plot_ly(
        filtered_2healthSex1(),
        x = ~ Characteristic,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  #Plotly for Visible Minority`
  
  output$sBarBasic5 <- renderPlotly({
    req(filtered_basic5())
    
    
    fig <-
      plot_ly(
        filtered_basic5(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicAge5 <- renderPlotly({
    req(filtered_basicAge5)
    
    
    fig <-
      plot_ly(
        filtered_basicAge5(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicSex5 <- renderPlotly({
    req(filtered_basicSex5())
    
    
    fig <-
      plot_ly(
        filtered_basicSex5(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  ###
  
  output$sBarBasic3 <- renderPlotly({
    req(filtered_basic3())
    
    
    fig <-
      plot_ly(
        filtered_basic3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately or severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicAge3 <- renderPlotly({
    req(filtered_basicAge3)
    
    
    fig <-
      plot_ly(
        filtered_basicAge3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately or severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicSex3 <- renderPlotly({
    req(filtered_basicSex3())
    
    
    fig <-
      plot_ly(
        filtered_basicSex3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately or severely food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  ###
  
  output$sBarBasic4 <- renderPlotly({
    req(filtered_basic4())
    
    
    fig <-
      plot_ly(
        filtered_basic4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicAge4 <- renderPlotly({
    req(filtered_basicAge4)
    
    
    fig <-
      plot_ly(
        filtered_basicAge4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicSex4 <- renderPlotly({
    req(filtered_basicSex4())
    
    
    fig <-
      plot_ly(
        filtered_basicSex4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household moderately food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ####
  output$sBarBasic2 <- renderPlotly({
    req(filtered_basic2())
    
    
    fig <-
      plot_ly(
        filtered_basic2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household marginally food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicAge2 <- renderPlotly({
    req(filtered_basicAge2)
    
    
    fig <-
      plot_ly(
        filtered_basicAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household marginally food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicSex2 <- renderPlotly({
    req(filtered_basicSex2())
    
    
    fig <-
      plot_ly(
        filtered_basicSex2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household marginally food insecure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  ###
  output$sBarBasic1 <- renderPlotly({
    req(filtered_basic1())
    
    
    fig <-
      plot_ly(
        filtered_basic1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household food secure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicAge1 <- renderPlotly({
    req(filtered_basicAge1)
    
    
    fig <-
      plot_ly(
        filtered_basicAge1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household food secure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarBasicSex1 <- renderPlotly({
    req(filtered_basicSex1())
    
    
    fig <-
      plot_ly(
        filtered_basicSex1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Household food secure", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  ####
  output$sBarHealth13 <- renderPlotly({
    req(filtered_health13())
    
    
    fig <-
      plot_ly(
        filtered_health13(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Unmet health care needs", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge13 <- renderPlotly({
    req(filtered_healthAge13)
    
    
    fig <-
      plot_ly(
        filtered_healthAge13(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Unmet health care needs", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex13 <- renderPlotly({
    req(filtered_healthSex13())
    
    
    fig <-
      plot_ly(
        filtered_healthSex13(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Unmet health care needs", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  #####
  
  output$sBarHealth12 <- renderPlotly({
    req(filtered_health12())
    
    
    fig <-
      plot_ly(
        filtered_health12(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge12 <- renderPlotly({
    req(filtered_healthAge12)
    
    
    fig <-
      plot_ly(
        filtered_healthAge12(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex12 <- renderPlotly({
    req(filtered_healthSex12())
    
    
    fig <-
      plot_ly(
        filtered_healthSex12(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, needs not met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ####
  
  output$sBarHealth11 <- renderPlotly({
    req(filtered_health11())
    
    
    fig <-
      plot_ly(
        filtered_health11(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge11 <- renderPlotly({
    req(filtered_healthAge11)
    
    
    fig <-
      plot_ly(
        filtered_healthAge11(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex11 <- renderPlotly({
    req(filtered_healthSex11())
    
    
    fig <-
      plot_ly(
        filtered_healthSex11(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  ###
  
  output$sBarHealth10 <- renderPlotly({
    req(filtered_health10())
    
    
    fig <-
      plot_ly(
        filtered_health10(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge10 <- renderPlotly({
    req(filtered_healthAge10)
    
    
    fig <-
      plot_ly(
        filtered_healthAge10(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex10 <- renderPlotly({
    req(filtered_healthSex10())
    
    
    fig <-
      plot_ly(
        filtered_healthSex10(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, all needs met", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  ###
  output$sBarHealth9 <- renderPlotly({
    req(filtered_health9())
    
    
    fig <-
      plot_ly(
        filtered_health9(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, no need", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge9 <- renderPlotly({
    req(filtered_healthAge9)
    
    
    fig <-
      plot_ly(
        filtered_healthAge9(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, no need", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex9 <- renderPlotly({
    req(filtered_healthSex9())
    
    
    fig <-
      plot_ly(
        filtered_healthSex9(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived need for mental health care, no need", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ####
  
  output$sBarHealth8 <- renderPlotly({
    req(filtered_health8())
    
    
    fig <-
      plot_ly(
        filtered_health8(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Has a regular healthcare provider", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge8 <- renderPlotly({
    req(filtered_healthAge8)
    
    
    fig <-
      plot_ly(
        filtered_healthAge8(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Has a regular healthcare provider", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex8 <- renderPlotly({
    req(filtered_healthSex8())
    
    
    fig <-
      plot_ly(
        filtered_healthSex8(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Has a regular healthcare provider", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  ###
  
  output$sBarHealth7 <- renderPlotly({
    req(filtered_health7())
    
    
    fig <-
      plot_ly(
        filtered_health7(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Life satisfaction, satisfied or very satisfied", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge7 <- renderPlotly({
    req(filtered_healthAge7())
    
    
    fig <-
      plot_ly(
        filtered_healthAge7(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Life satisfaction, satisfied or very satisfied", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex7 <- renderPlotly({
    req(filtered_healthSex7())
    
    
    fig <-
      plot_ly(
        filtered_healthSex7(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Life satisfaction, satisfied or very satisfied", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  ###
  
  output$sBarHealth6 <- renderPlotly({
    req(filtered_health6())
    
    
    fig <-
      plot_ly(
        filtered_health6(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived life stress, most days quite a bit or extremely stressful", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge6 <- renderPlotly({
    req(filtered_healthAge6())
    
    
    fig <-
      plot_ly(
        filtered_healthAge6(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived life stress, most days quite a bit or extremely stressful", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex6 <- renderPlotly({
    req(filtered_healthSex6())
    
    
    fig <-
      plot_ly(
        filtered_healthSex6(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived life stress, most days quite a bit or extremely stressful", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  #####
  output$sBarHealth4 <- renderPlotly({
    req(filtered_health4())
    
    
    fig <-
      plot_ly(
        filtered_health4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge4 <- renderPlotly({
    req(filtered_healthAge4())
    
    
    fig <-
      plot_ly(
        filtered_healthAge4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex4 <- renderPlotly({
    req(filtered_healthSex4())
    
    
    fig <-
      plot_ly(
        filtered_healthSex4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ###
  
  output$sBarHealth3 <- renderPlotly({
    req(filtered_health3())
    
    
    fig <-
      plot_ly(
        filtered_health3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge3 <- renderPlotly({
    req(filtered_healthAge3())
    
    
    fig <-
      plot_ly(
        filtered_healthAge3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex3 <- renderPlotly({
    req(filtered_healthSex3())
    
    
    fig <-
      plot_ly(
        filtered_healthSex3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived mental health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  ###
  
  output$sBarHealth2 <- renderPlotly({
    req(filtered_health2())
    
    
    fig <-
      plot_ly(
        filtered_health2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge2 <- renderPlotly({
    req(filtered_healthAge2())
    
    
    fig <-
      plot_ly(
        filtered_healthAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex2 <- renderPlotly({
    req(filtered_healthSex2())
    
    
    fig <-
      plot_ly(
        filtered_healthSex2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, fair or poor", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ###
  output$sBarHealth1 <- renderPlotly({
    req(filtered_health1())
    
    
    fig <-
      plot_ly(
        filtered_health1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthAge1 <- renderPlotly({
    req(filtered_healthAge1())
    
    
    fig <-
      plot_ly(
        filtered_healthAge1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarHealthSex1 <- renderPlotly({
    req(filtered_healthSex1())
    
    
    fig <-
      plot_ly(
        filtered_healthSex1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Perceived health, very good or excellent", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  ###
  
  
  #Plotly for Civic Engagement - VisMin
  
  output$sBarCivic17 <- renderPlotly({
    req(filtered_civic17())
    
    
    fig <-
      plot_ly(
        filtered_civic17(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Engaged in political activities", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge17 <- renderPlotly({
    req(filtered_civicAge17())
    
    
    fig <-
      plot_ly(
        filtered_civicAge17(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Engaged in political activities", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex17 <- renderPlotly({
    req(filtered_civicSex17())
    
    
    fig <-
      plot_ly(
        filtered_civicSex17(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Engaged in political activities", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen17 <- renderPlotly({
    req(filtered_civicGen17())
    
    
    fig <-
      plot_ly(
        filtered_civicGen17(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Engaged in political activities", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang17 <- renderPlotly({
    req(filtered_civicLang17())
    
    
    fig <-
      plot_ly(
        filtered_civicLang17(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Engaged in political activities", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu17 <- renderPlotly({
    req(filtered_civicEdu17())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu17(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Engaged in political activities", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  ####
  
  output$sBarCivic16 <- renderPlotly({
    req(filtered_civic16())
    
    
    fig <-
      plot_ly(
        filtered_civic16(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in environmental group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge16 <- renderPlotly({
    req(filtered_civicAge16())
    
    
    fig <-
      plot_ly(
        filtered_civicAge16(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in environmental group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex16 <- renderPlotly({
    req(filtered_civicSex16())
    
    
    fig <-
      plot_ly(
        filtered_civicSex16(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in environmental group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen16 <- renderPlotly({
    req(filtered_civicGen16())
    
    
    fig <-
      plot_ly(
        filtered_civicGen16(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in environmental group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang16 <- renderPlotly({
    req(filtered_civicLang16())
    
    
    fig <-
      plot_ly(
        filtered_civicLang16(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in environmental group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu16 <- renderPlotly({
    req(filtered_civicEdu16())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu16(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in environmental group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ####
  output$sBarCivic15 <- renderPlotly({
    req(filtered_civic15())
    
    
    fig <-
      plot_ly(
        filtered_civic15(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in immigrant or ethnic association or club", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge15 <- renderPlotly({
    req(filtered_civicAge15())
    
    
    fig <-
      plot_ly(
        filtered_civicAge15(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in immigrant or ethnic association or club", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex15 <- renderPlotly({
    req(filtered_civicSex15())
    
    
    fig <-
      plot_ly(
        filtered_civicSex15(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in immigrant or ethnic association or club", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen15 <- renderPlotly({
    req(filtered_civicGen15())
    
    
    fig <-
      plot_ly(
        filtered_civicGen15(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in immigrant or ethnic association or club", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang15 <- renderPlotly({
    req(filtered_civicLang15())
    
    
    fig <-
      plot_ly(
        filtered_civicLang15(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in immigrant or ethnic association or club", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu15 <- renderPlotly({
    req(filtered_civicEdu15())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu15(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in immigrant or ethnic association or club", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  ###
  
  output$sBarCivic14 <- renderPlotly({
    req(filtered_civic14())
    
    
    fig <-
      plot_ly(
        filtered_civic14(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in youth organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge14 <- renderPlotly({
    req(filtered_civicAge14())
    
    
    fig <-
      plot_ly(
        filtered_civicAge14(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in youth organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex14 <- renderPlotly({
    req(filtered_civicSex14())
    
    
    fig <-
      plot_ly(
        filtered_civicSex14(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in youth organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen14 <- renderPlotly({
    req(filtered_civicGen14())
    
    
    fig <-
      plot_ly(
        filtered_civicGen14(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in youth organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang14 <- renderPlotly({
    req(filtered_civicLang14())
    
    
    fig <-
      plot_ly(
        filtered_civicLang14(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in youth organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu14 <- renderPlotly({
    req(filtered_civicEdu14())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu14(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in youth organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  
  #######
  
  output$sBarCivic12 <- renderPlotly({
    req(filtered_civic12())
    
    
    fig <-
      plot_ly(
        filtered_civic12(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in humanitarian or charitable organization or service club", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge12 <- renderPlotly({
    req(filtered_civicAge12())
    
    
    fig <-
      plot_ly(
        filtered_civicAge12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in humanitarian or charitable organization or service club", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex12 <- renderPlotly({
    req(filtered_civicSex12())
    
    
    fig <-
      plot_ly(
        filtered_civicSex12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in humanitarian or charitable organization or service club", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen12 <- renderPlotly({
    req(filtered_civicGen12())
    
    
    fig <-
      plot_ly(
        filtered_civicGen12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in humanitarian or charitable organization or service club", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang12 <- renderPlotly({
    req(filtered_civicLang12())
    
    
    fig <-
      plot_ly(
        filtered_civicLang12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in humanitarian or charitable organization or service club", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu12 <- renderPlotly({
    req(filtered_civicEdu12())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in humanitarian or charitable organization or service club", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  ####
  
  output$sBarCivic11 <- renderPlotly({
    req(filtered_civic11())
    
    
    fig <-
      plot_ly(
        filtered_civic11(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in school group, neighbourhood, civic or community association", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge11 <- renderPlotly({
    req(filtered_civicAge11())
    
    
    fig <-
      plot_ly(
        filtered_civicAge11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in school group, neighbourhood, civic or community association", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex11 <- renderPlotly({
    req(filtered_civicSex11())
    
    
    fig <-
      plot_ly(
        filtered_civicSex11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in school group, neighbourhood, civic or community association", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen11 <- renderPlotly({
    req(filtered_civicGen11())
    
    
    fig <-
      plot_ly(
        filtered_civicGen11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in school group, neighbourhood, civic or community association", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang11 <- renderPlotly({
    req(filtered_civicLang11())
    
    
    fig <-
      plot_ly(
        filtered_civicLang11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in school group, neighbourhood, civic or community association", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu11 <- renderPlotly({
    req(filtered_civicEdu11())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in school group, neighbourhood, civic or community association", font = list(size = 16)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  #####
  
  output$sBarCivic10 <- renderPlotly({
    req(filtered_civic10())
    
    
    fig <-
      plot_ly(
        filtered_civic10(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in religious-affiliated group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge10 <- renderPlotly({
    req(filtered_civicAge10())
    
    
    fig <-
      plot_ly(
        filtered_civicAge10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in religious-affiliated group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex10 <- renderPlotly({
    req(filtered_civicSex10())
    
    
    fig <-
      plot_ly(
        filtered_civicSex10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in religious-affiliated group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen10 <- renderPlotly({
    req(filtered_civicGen10())
    
    
    fig <-
      plot_ly(
        filtered_civicGen10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in religious-affiliated group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang10 <- renderPlotly({
    req(filtered_civicLang10())
    
    
    fig <-
      plot_ly(
        filtered_civicLang10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in religious-affiliated group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu10 <- renderPlotly({
    req(filtered_civicEdu10())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in religious-affiliated group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  ####
  output$sBarCivic9 <- renderPlotly({
    req(filtered_civic9())
    
    
    fig <-
      plot_ly(
        filtered_civic9(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in political party or group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge9 <- renderPlotly({
    req(filtered_civicAge9())
    
    
    fig <-
      plot_ly(
        filtered_civicAge9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in political party or group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex9 <- renderPlotly({
    req(filtered_civicSex9())
    
    
    fig <-
      plot_ly(
        filtered_civicSex9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in political party or group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen9 <- renderPlotly({
    req(filtered_civicGen9())
    
    
    fig <-
      plot_ly(
        filtered_civicGen9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in political party or group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang9 <- renderPlotly({
    req(filtered_civicLang9())
    
    
    fig <-
      plot_ly(
        filtered_civicLang9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in political party or group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu9 <- renderPlotly({
    req(filtered_civicEdu9())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in political party or group", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  ###
  
  output$sBarCivic8 <- renderPlotly({
    req(filtered_civic8())
    
    
    fig <-
      plot_ly(
        filtered_civic8(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in union or professional association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge8 <- renderPlotly({
    req(filtered_civicAge8())
    
    
    fig <-
      plot_ly(
        filtered_civicAge8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in union or professional association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex8 <- renderPlotly({
    req(filtered_civicSex8())
    
    
    fig <-
      plot_ly(
        filtered_civicSex8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in union or professional association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen8 <- renderPlotly({
    req(filtered_civicGen8())
    
    
    fig <-
      plot_ly(
        filtered_civicGen8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in union or professional association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang8 <- renderPlotly({
    req(filtered_civicLang8())
    
    
    fig <-
      plot_ly(
        filtered_civicLang8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in union or professional association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu8 <- renderPlotly({
    req(filtered_civicEdu8())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in union or professional association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  ###
  
  output$sBarCivic7 <- renderPlotly({
    req(filtered_civic7())
    
    
    fig <-
      plot_ly(
        filtered_civic7(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in cultural, educational or hobby organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge7 <- renderPlotly({
    req(filtered_civicAge7())
    
    
    fig <-
      plot_ly(
        filtered_civicAge7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in cultural, educational or hobby organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex7 <- renderPlotly({
    req(filtered_civicSex7())
    
    
    fig <-
      plot_ly(
        filtered_civicSex7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in cultural, educational or hobby organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen7 <- renderPlotly({
    req(filtered_civicGen7())
    
    
    fig <-
      plot_ly(
        filtered_civicGen7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in cultural, educational or hobby organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang7 <- renderPlotly({
    req(filtered_civicLang7())
    
    
    fig <-
      plot_ly(
        filtered_civicLang7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in cultural, educational or hobby organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu7 <- renderPlotly({
    req(filtered_civicEdu7())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in cultural, educational or hobby organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  ###
  
  output$sBarCivic6 <- renderPlotly({
    req(filtered_civic6())
    
    
    fig <-
      plot_ly(
        filtered_civic6(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant of at least one group, organization or association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge6 <- renderPlotly({
    req(filtered_civicAge6())
    
    
    fig <-
      plot_ly(
        filtered_civicAge6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant of at least one group, organization or association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex6 <- renderPlotly({
    req(filtered_civicSex6())
    
    
    fig <-
      plot_ly(
        filtered_civicSex6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant of at least one group, organization or association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen6 <- renderPlotly({
    req(filtered_civicGen6())
    
    
    fig <-
      plot_ly(
        filtered_civicGen6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant of at least one group, organization or association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang6 <- renderPlotly({
    req(filtered_civicLang6())
    
    
    fig <-
      plot_ly(
        filtered_civicLang6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant of at least one group, organization or association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu6 <- renderPlotly({
    req(filtered_civicEdu6())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant of at least one group, organization or association", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  ####
  
  output$sBarCivic5 <- renderPlotly({
    req(filtered_civic5())
    
    
    fig <-
      plot_ly(
        filtered_civic5(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in sports or recreational organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge5 <- renderPlotly({
    req(filtered_civicAge5())
    
    
    fig <-
      plot_ly(
        filtered_civicAge5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in sports or recreational organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex5 <- renderPlotly({
    req(filtered_civicSex5())
    
    
    fig <-
      plot_ly(
        filtered_civicSex5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in sports or recreational organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen5 <- renderPlotly({
    req(filtered_civicGen5())
    
    
    fig <-
      plot_ly(
        filtered_civicGen5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in sports or recreational organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang5 <- renderPlotly({
    req(filtered_civicLang5())
    
    
    fig <-
      plot_ly(
        filtered_civicLang5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in sports or recreational organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu5 <- renderPlotly({
    req(filtered_civicEdu5())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Member or participant in sports or recreational organization", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ###
  output$sBarCivic4 <- renderPlotly({
    req(filtered_civic4())
    
    
    fig <-
      plot_ly(
        filtered_civic4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last provincial election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge4 <- renderPlotly({
    req(filtered_civicAge4())
    
    
    fig <-
      plot_ly(
        filtered_civicAge4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last provincial election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex4 <- renderPlotly({
    req(filtered_civicSex4())
    
    
    fig <-
      plot_ly(
        filtered_civicSex4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last pronvincial election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen4 <- renderPlotly({
    req(filtered_civicGen4())
    
    
    fig <-
      plot_ly(
        filtered_civicGen4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last provincial election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang4 <- renderPlotly({
    req(filtered_civicLang4())
    
    
    fig <-
      plot_ly(
        filtered_civicLang4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last provincial election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu4 <- renderPlotly({
    req(filtered_civicEdu4())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last provincial election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  ###
  output$sBarCivic2 <- renderPlotly({
    req(filtered_civic2())
    
    
    fig <-
      plot_ly(
        filtered_civic2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last municipal election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge2 <- renderPlotly({
    req(filtered_civicAge2())
    
    
    fig <-
      plot_ly(
        filtered_civicAge2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last municipal election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex2 <- renderPlotly({
    req(filtered_civicSex2())
    
    
    fig <-
      plot_ly(
        filtered_civicSex2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last municipal election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen2 <- renderPlotly({
    req(filtered_civicGen2())
    
    
    fig <-
      plot_ly(
        filtered_civicGen2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last municipal election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang2 <- renderPlotly({
    req(filtered_civicLang2())
    
    
    fig <-
      plot_ly(
        filtered_civicLang2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last municipal election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu2 <- renderPlotly({
    req(filtered_civicEdu2())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last municipal election", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  #####
  output$sBarCivic3 <- renderPlotly({
    req(filtered_civic3())
    
    
    fig <-
      plot_ly(
        filtered_civic3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last federal election ", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicAge3 <- renderPlotly({
    req(filtered_civicAge3())
    
    
    fig <-
      plot_ly(
        filtered_civicAge3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last federal election ", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicSex3 <- renderPlotly({
    req(filtered_civicSex3())
    
    
    fig <-
      plot_ly(
        filtered_civicSex3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last federal election ", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicGen3 <- renderPlotly({
    req(filtered_civicGen3())
    
    
    fig <-
      plot_ly(
        filtered_civicGen3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last federal election ", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarCivicLang3 <- renderPlotly({
    req(filtered_civicLang3())
    
    
    fig <-
      plot_ly(
        filtered_civicLang3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last federal election ", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCivicEdu3 <- renderPlotly({
    req(filtered_civicEdu3())
    
    
    fig <-
      plot_ly(
        filtered_civicEdu3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Voted in last federal election ", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  #####
  
  #end
  output$sBarConf12 <- renderPlotly({
    req(filtered_Conf12())
    
    
    fig <-
      plot_ly(
        filtered_Conf12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge12 <- renderPlotly({
    req(filtered_ConfAge12())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex12 <- renderPlotly({
    req(filtered_ConfSex12())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen12 <- renderPlotly({
    req(filtered_ConfGen12())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang12 <- renderPlotly({
    req(filtered_ConfLang12())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu12 <- renderPlotly({
    req(filtered_ConfEdu12())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu12(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #End
  output$sBarConf11 <- renderPlotly({
    req(filtered_Conf11())
    
    
    fig <-
      plot_ly(
        filtered_Conf11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge11 <- renderPlotly({
    req(filtered_ConfAge11())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex11 <- renderPlotly({
    req(filtered_ConfSex11())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen11 <- renderPlotly({
    req(filtered_ConfGen11())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang11 <- renderPlotly({
    req(filtered_ConfLang11())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu11 <- renderPlotly({
    req(filtered_ConfEdu11())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu11(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #End
  output$sBarConf10 <- renderPlotly({
    req(filtered_Conf10())
    
    
    fig <-
      plot_ly(
        filtered_Conf10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge10 <- renderPlotly({
    req(filtered_ConfAge10())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex10 <- renderPlotly({
    req(filtered_ConfSex10())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen10 <- renderPlotly({
    req(filtered_ConfGen10())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang10 <- renderPlotly({
    req(filtered_ConfLang10())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu10 <- renderPlotly({
    req(filtered_ConfEdu10())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu10(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #End
  
  
  output$sBarConf9 <- renderPlotly({
    req(filtered_Conf9())
    
    
    fig <-
      plot_ly(
        filtered_Conf9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge9 <- renderPlotly({
    req(filtered_ConfAge9())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex9 <- renderPlotly({
    req(filtered_ConfSex9())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen9 <- renderPlotly({
    req(filtered_ConfGen9())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang9 <- renderPlotly({
    req(filtered_ConfLang9())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu9 <- renderPlotly({
    req(filtered_ConfEdu9())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu9(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #End
  
  output$sBarConf8 <- renderPlotly({
    req(filtered_Conf8())
    
    
    fig <-
      plot_ly(
        filtered_Conf8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge8 <- renderPlotly({
    req(filtered_ConfAge8())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex8 <- renderPlotly({
    req(filtered_ConfSex8())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen8 <- renderPlotly({
    req(filtered_ConfGen8())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang8 <- renderPlotly({
    req(filtered_ConfLang8())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu8 <- renderPlotly({
    req(filtered_ConfEdu8())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu8(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  #
  
  
  
  
  
  output$sBarConf1 <- renderPlotly({
    req(filtered_Conf1())
    
    
    fig <-
      plot_ly(
        filtered_Conf1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge1 <- renderPlotly({
    req(filtered_ConfAge1())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex1 <- renderPlotly({
    req(filtered_ConfSex1())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen1 <- renderPlotly({
    req(filtered_ConfGen1())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang1 <- renderPlotly({
    req(filtered_ConfLang1())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu1 <- renderPlotly({
    req(filtered_ConfEdu1())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  #
  output$employmentPlot7 <- renderPlotly({
    req(filtered_Employ7())
    
    
    fig <-
      plot_ly(
        filtered_Employ7(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Have access to disability insurance under employment contract", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$employmentPlot6 <- renderPlotly({
    req(filtered_Employ6())
    
    
    fig <-
      plot_ly(
        filtered_Employ6(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Match between education and employment", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$employmentPlot5 <- renderPlotly({
    req(filtered_Employ5())
    
    
    fig <-
      plot_ly(
        filtered_Employ5(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Have access to paid vacation leave under employment contract", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$employmentPlot4 <- renderPlotly({
    req(filtered_Employ4())
    
    
    fig <-
      plot_ly(
        filtered_Employ4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Employment contract includes at least one type of employment benefits", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$employmentPlot3 <- renderPlotly({
    req(filtered_Employ3())
    
    
    fig <-
      plot_ly(
        filtered_Employ3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Have a workplace pension plan", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$employmentPlot2 <- renderPlotly({
    req(filtered_Employ2())
    
    
    fig <-
      plot_ly(
        filtered_Employ2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Job offers good prospects for career advancement", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$employmentPlot <- renderPlotly({
    req(filtered_Employ())
    
    
    fig <-
      plot_ly(
        filtered_Employ(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Have access to paid sick leave under employment contract", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  output$sBarConf <- renderPlotly({
    req(filtered_Conf())
    
    
    fig <-
      plot_ly(
        filtered_Conf(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge <- renderPlotly({
    req(filtered_ConfAge())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex <- renderPlotly({
    req(filtered_ConfSex())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen <- renderPlotly({
    req(filtered_ConfGen())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang <- renderPlotly({
    req(filtered_ConfLang())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu <- renderPlotly({
    req(filtered_ConfEdu())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConf1 <- renderPlotly({
    req(filtered_Conf1())
    
    
    fig <-
      plot_ly(
        filtered_Conf1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge1 <- renderPlotly({
    req(filtered_ConfAge1())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex1 <- renderPlotly({
    req(filtered_ConfSex1())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen1 <- renderPlotly({
    req(filtered_ConfGen1())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang1 <- renderPlotly({
    req(filtered_ConfLang1())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu1 <- renderPlotly({
    req(filtered_ConfEdu1())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  ##
  
  output$sBarConf2 <- renderPlotly({
    req(filtered_Conf2())
    
    
    fig <-
      plot_ly(
        filtered_Conf2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge2 <- renderPlotly({
    req(filtered_ConfAge2())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex2 <- renderPlotly({
    req(filtered_ConfSex2())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen2 <- renderPlotly({
    req(filtered_ConfGen2())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang2 <- renderPlotly({
    req(filtered_ConfLang2())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu2 <- renderPlotly({
    req(filtered_ConfEdu2())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  output$sBarConf3 <- renderPlotly({
    req(filtered_Conf3())
    
    
    fig <-
      plot_ly(
        filtered_Conf3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge3 <- renderPlotly({
    req(filtered_ConfAge3())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex3 <- renderPlotly({
    req(filtered_ConfSex3())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen3 <- renderPlotly({
    req(filtered_ConfGen3())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang3 <- renderPlotly({
    req(filtered_ConfLang3())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu3 <- renderPlotly({
    req(filtered_ConfEdu3())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  
  output$sBarConf4 <- renderPlotly({
    req(filtered_Conf4())
    
    
    fig <-
      plot_ly(
        filtered_Conf4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge4 <- renderPlotly({
    req(filtered_ConfAge4())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex4 <- renderPlotly({
    req(filtered_ConfSex4())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen4 <- renderPlotly({
    req(filtered_ConfGen4())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang4 <- renderPlotly({
    req(filtered_ConfLang4())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu4 <- renderPlotly({
    req(filtered_ConfEdu4())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  
  output$sBarConf5 <- renderPlotly({
    req(filtered_Conf5())
    
    
    fig <-
      plot_ly(
        filtered_Conf5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge5 <- renderPlotly({
    req(filtered_ConfAge5())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex5 <- renderPlotly({
    req(filtered_ConfSex5())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen5 <- renderPlotly({
    req(filtered_ConfGen5())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang5 <- renderPlotly({
    req(filtered_ConfLang5())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu5 <- renderPlotly({
    req(filtered_ConfEdu5())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  output$sBarConf5 <- renderPlotly({
    req(filtered_Conf5())
    
    
    fig <-
      plot_ly(
        filtered_Conf5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge5 <- renderPlotly({
    req(filtered_ConfAge5())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex5 <- renderPlotly({
    req(filtered_ConfSex5())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen5 <- renderPlotly({
    req(filtered_ConfGen5())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang5 <- renderPlotly({
    req(filtered_ConfLang5())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu5 <- renderPlotly({
    req(filtered_ConfEdu5())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  output$sBarConf6 <- renderPlotly({
    req(filtered_Conf6())
    
    
    fig <-
      plot_ly(
        filtered_Conf6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge6 <- renderPlotly({
    req(filtered_ConfAge6())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex6 <- renderPlotly({
    req(filtered_ConfSex6())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen6 <- renderPlotly({
    req(filtered_ConfGen6())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang6 <- renderPlotly({
    req(filtered_ConfLang6())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu6 <- renderPlotly({
    req(filtered_ConfEdu6())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  output$sBarConf7 <- renderPlotly({
    req(filtered_Conf7())
    
    
    fig <-
      plot_ly(
        filtered_Conf7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge7 <- renderPlotly({
    req(filtered_ConfAge7())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex7 <- renderPlotly({
    req(filtered_ConfSex7())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen7 <- renderPlotly({
    req(filtered_ConfGen7())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang7 <- renderPlotly({
    req(filtered_ConfLang7())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu7 <- renderPlotly({
    req(filtered_ConfEdu7())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu7(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  ##
  #Discrimination
  
  
  #Today
  
  output$sBarBan <- renderPlotly({
    req(filtered_Bank())
    
    
    fig <-
      plot_ly(
        filtered_Bank(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBan2 <- renderPlotly({
    req(filtered_Bank2())
    
    
    fig <-
      plot_ly(
        filtered_Bank2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBanAge <- renderPlotly({
    req(filtered_BankAge())
    
    
    fig <-
      plot_ly(
        filtered_BankAge(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBanAge2 <- renderPlotly({
    req(filtered_BankAge2())
    
    
    fig <-
      plot_ly(
        filtered_BankAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBanSex <- renderPlotly({
    req(filtered_BankSex())
    
    
    fig <-
      plot_ly(
        filtered_BankSex(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBanSex2 <- renderPlotly({
    req(filtered_BankSex2())
    
    
    fig <-
      plot_ly(
        filtered_BankSex2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBanGen <- renderPlotly({
    req(filtered_BankGen())
    
    
    fig <-
      plot_ly(
        filtered_BankGen(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60),
                     title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBanGen2 <- renderPlotly({
    req(filtered_BankGen2())
    
    
    fig <-
      plot_ly(
        filtered_BankGen2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarBanLang <- renderPlotly({
    req(filtered_BankLang())
    
    
    fig <-
      plot_ly(
        filtered_BankLang(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBanLang2 <- renderPlotly({
    req(filtered_BankLang2())
    
    
    fig <-
      plot_ly(
        filtered_BankLang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarBanEdu <- renderPlotly({
    req(filtered_BankEdu())
    
    
    fig <-
      plot_ly(
        filtered_BankEdu(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarBanEdu2 <- renderPlotly({
    req(filtered_BankEdu2())
    
    
    fig <-
      plot_ly(
        filtered_BankEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  #end
  
  output$sBarClass <- renderPlotly({
    req(filtered_Class())
    
    
    fig <-
      plot_ly(
        filtered_Class(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarClass2 <- renderPlotly({
    req(filtered_Class2())
    
    
    fig <-
      plot_ly(
        filtered_Class2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarClassAge <- renderPlotly({
    req(filtered_ClassAge())
    
    
    fig <-
      plot_ly(
        filtered_ClassAge(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarClassAge2 <- renderPlotly({
    req(filtered_ClassAge2())
    
    
    fig <-
      plot_ly(
        filtered_ClassAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  output$sBarClassGen <- renderPlotly({
    req(filtered_ClassGen())
    
    
    fig <-
      plot_ly(
        filtered_ClassGen(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60),
                     title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarClassGen2 <- renderPlotly({
    req(filtered_ClassGen2())
    
    
    fig <-
      plot_ly(
        filtered_ClassGen2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarClassLang <- renderPlotly({
    req(filtered_ClassLang())
    
    
    fig <-
      plot_ly(
        filtered_ClassLang(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarClassLang2 <- renderPlotly({
    req(filtered_ClassLang2())
    
    
    fig <-
      plot_ly(
        filtered_ClassLang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarClassEdu <- renderPlotly({
    req(filtered_ClassEdu())
    
    
    fig <-
      plot_ly(
        filtered_ClassEdu(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarClassEdu2 <- renderPlotly({
    req(filtered_ClassEdu2())
    
    
    fig <-
      plot_ly(
        filtered_ClassEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  #end
  output$sBarJob <- renderPlotly({
    req(filtered_Work())
    
    
    fig <-
      plot_ly(
        filtered_Work(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJob2 <- renderPlotly({
    req(filtered_Work2())
    
    
    fig <-
      plot_ly(
        filtered_Work2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJobAge <- renderPlotly({
    req(filtered_WorkAge())
    
    
    fig <-
      plot_ly(
        filtered_WorkAge(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJobAge2 <- renderPlotly({
    req(filtered_WorkAge2())
    
    
    fig <-
      plot_ly(
        filtered_WorkAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJobSex <- renderPlotly({
    req(filtered_WorkGender())
    
    
    fig <-
      plot_ly(
        filtered_WorkGender(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJobSex2 <- renderPlotly({
    req(filtered_WorkGender2())
    
    
    fig <-
      plot_ly(
        filtered_WorkGender2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJobGen <- renderPlotly({
    req(filtered_WorkGen())
    
    
    fig <-
      plot_ly(
        filtered_WorkGen(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60),
                     title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJobGen2 <- renderPlotly({
    req(filtered_WorkGen2())
    
    
    fig <-
      plot_ly(
        filtered_WorkGen2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarJobLang <- renderPlotly({
    req(filtered_WorkLang())
    
    
    fig <-
      plot_ly(
        filtered_WorkLang(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJobLang2 <- renderPlotly({
    req(filtered_WorkLang2())
    
    
    fig <-
      plot_ly(
        filtered_WorkLang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarJobEdu <- renderPlotly({
    req(filtered_WorkEdu())
    
    
    fig <-
      plot_ly(
        filtered_WorkEdu(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarJobEdu2 <- renderPlotly({
    req(filtered_WorkEdu2())
    
    
    fig <-
      plot_ly(
        filtered_WorkEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #end
  
  output$sBarPol <- renderPlotly({
    req(filtered_Pol())
    
    
    fig <-
      plot_ly(
        filtered_Pol(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPol2 <- renderPlotly({
    req(filtered_Pol2())
    
    
    fig <-
      plot_ly(
        filtered_Pol2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPolAge <- renderPlotly({
    req(filtered_PolAge())
    
    
    fig <-
      plot_ly(
        filtered_PolAge(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPolAge2 <- renderPlotly({
    req(filtered_PolAge2())
    
    
    fig <-
      plot_ly(
        filtered_PolAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPolSex <- renderPlotly({
    req(filtered_PolSex())
    
    
    fig <-
      plot_ly(
        filtered_PolSex(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPolSex2 <- renderPlotly({
    req(filtered_PolSex2())
    
    
    fig <-
      plot_ly(
        filtered_PolSex2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPolGen <- renderPlotly({
    req(filtered_PolGen())
    
    
    fig <-
      plot_ly(
        filtered_PolGen(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60),
                     title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPolGen2 <- renderPlotly({
    req(filtered_PolGen2())
    
    
    fig <-
      plot_ly(
        filtered_PolGen2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarPolLang <- renderPlotly({
    req(filtered_PolLang())
    
    
    fig <-
      plot_ly(
        filtered_PolLang(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPolLang2 <- renderPlotly({
    req(filtered_PolLang2())
    
    
    fig <-
      plot_ly(
        filtered_PolLang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarPolEdu <- renderPlotly({
    req(filtered_PolEdu())
    
    
    fig <-
      plot_ly(
        filtered_PolEdu(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarPolEdu2 <- renderPlotly({
    req(filtered_PolEdu2())
    
    
    fig <-
      plot_ly(
        filtered_PolEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #end
  
  output$sBarLang <- renderPlotly({
    req(filtered_Lang())
    
    
    fig <-
      plot_ly(
        filtered_Lang(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLang2 <- renderPlotly({
    req(filtered_Lang2())
    
    
    fig <-
      plot_ly(
        filtered_Lang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLangAge <- renderPlotly({
    req(filtered_LangAge())
    
    
    fig <-
      plot_ly(
        filtered_LangAge(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLangAge2 <- renderPlotly({
    req(filtered_LangAge2())
    
    
    fig <-
      plot_ly(
        filtered_LangAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLangSex <- renderPlotly({
    req(filtered_LangSex())
    
    
    fig <-
      plot_ly(
        filtered_LangSex(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLangSex2 <- renderPlotly({
    req(filtered_LangSex2())
    
    
    fig <-
      plot_ly(
        filtered_LangSex2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLangGen <- renderPlotly({
    req(filtered_LangGen())
    
    
    fig <-
      plot_ly(
        filtered_LangGen(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60),
                     title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLangGen2 <- renderPlotly({
    req(filtered_LangGen2())
    
    
    fig <-
      plot_ly(
        filtered_LangGen2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarLangLang <- renderPlotly({
    req(filtered_LangLang())
    
    
    fig <-
      plot_ly(
        filtered_LangLang(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLangLang2 <- renderPlotly({
    req(filtered_LangLang2())
    
    
    fig <-
      plot_ly(
        filtered_LangLang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarLangEdu <- renderPlotly({
    req(filtered_LangEdu())
    
    
    fig <-
      plot_ly(
        filtered_LangEdu(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarLangEdu2 <- renderPlotly({
    req(filtered_LangEdu2())
    
    
    fig <-
      plot_ly(
        filtered_LangEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #end
  
  output$sBarRel <- renderPlotly({
    req(filtered_Rel())
    
    
    fig <-
      plot_ly(
        filtered_Rel(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRel2 <- renderPlotly({
    req(filtered_Rel2())
    
    
    fig <-
      plot_ly(
        filtered_Rel2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRelAge <- renderPlotly({
    req(filtered_RelAge())
    
    
    fig <-
      plot_ly(
        filtered_RelAge(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRelAge2 <- renderPlotly({
    req(filtered_RelAge2())
    
    
    fig <-
      plot_ly(
        filtered_RelAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRelSex <- renderPlotly({
    req(filtered_RelSex())
    
    
    fig <-
      plot_ly(
        filtered_RelSex(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRelSex2 <- renderPlotly({
    req(filtered_RelSex2())
    
    
    fig <-
      plot_ly(
        filtered_RelSex2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRelGen <- renderPlotly({
    req(filtered_RelGen())
    
    
    fig <-
      plot_ly(
        filtered_RelGen(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60),
                     title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRelGen2 <- renderPlotly({
    req(filtered_RelGen2())
    
    
    fig <-
      plot_ly(
        filtered_RelGen2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarRelLang <- renderPlotly({
    req(filtered_RelLang())
    
    
    fig <-
      plot_ly(
        filtered_RelLang(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRelLang2 <- renderPlotly({
    req(filtered_RelLang2())
    
    
    fig <-
      plot_ly(
        filtered_RelLang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarRelEdu <- renderPlotly({
    req(filtered_RelEdu())
    
    
    fig <-
      plot_ly(
        filtered_RelEdu(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRelEdu2 <- renderPlotly({
    req(filtered_RelEdu2())
    
    
    fig <-
      plot_ly(
        filtered_RelEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #end
  output$sBarCol <- renderPlotly({
    req(filtered_Col())
    
    
    fig <-
      plot_ly(
        filtered_Col(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCol2 <- renderPlotly({
    req(filtered_Cov2())
    
    
    fig <-
      plot_ly(
        filtered_Col2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarColAge <- renderPlotly({
    req(filtered_ColAge())
    
    
    fig <-
      plot_ly(
        filtered_ColAge(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarColAge2 <- renderPlotly({
    req(filtered_ColAge2())
    
    
    fig <-
      plot_ly(
        filtered_ColAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarColSex <- renderPlotly({
    req(filtered_ColSex())
    
    
    fig <-
      plot_ly(
        filtered_ColSex(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarColSex2 <- renderPlotly({
    req(filtered_ColSex2())
    
    
    fig <-
      plot_ly(
        filtered_ColSex2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarColGen <- renderPlotly({
    req(filtered_ColGen())
    
    
    fig <-
      plot_ly(
        filtered_ColGen(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60),
                     title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarColGen2 <- renderPlotly({
    req(filtered_ColGen2())
    
    
    fig <-
      plot_ly(
        filtered_ColGen2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarColLang <- renderPlotly({
    req(filtered_ColLang())
    
    
    fig <-
      plot_ly(
        filtered_ColLang(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarColLang2 <- renderPlotly({
    req(filtered_ColLang2())
    
    
    fig <-
      plot_ly(
        filtered_ColLang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarColEdu <- renderPlotly({
    req(filtered_ColEdu())
    
    
    fig <-
      plot_ly(
        filtered_ColEdu(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarColEdu2 <- renderPlotly({
    req(filtered_ColEdu2())
    
    
    fig <-
      plot_ly(
        filtered_ColEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #end
  output$sBarCov <- renderPlotly({
    req(filtered_Cov())
    
    
    fig <-
      plot_ly(
        filtered_Cov(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCov2 <- renderPlotly({
    req(filtered_Cov2())
    
    
    fig <-
      plot_ly(
        filtered_Cov2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovAge <- renderPlotly({
    req(filtered_CovAge())
    
    
    fig <-
      plot_ly(
        filtered_CovAge(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovAge2 <- renderPlotly({
    req(filtered_CovAge2())
    
    
    fig <-
      plot_ly(
        filtered_CovAge2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovSex <- renderPlotly({
    req(filtered_CovSex())
    
    
    fig <-
      plot_ly(
        filtered_CovSex(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovSex2 <- renderPlotly({
    req(filtered_CovSex2())
    
    
    fig <-
      plot_ly(
        filtered_CovSex2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovGen <- renderPlotly({
    req(filtered_CovGen())
    
    
    fig <-
      plot_ly(
        filtered_CovGen(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60),
                     title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovGen2 <- renderPlotly({
    req(filtered_CovGen2())
    
    
    fig <-
      plot_ly(
        filtered_CovGen2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarCovLang <- renderPlotly({
    req(filtered_CovLang())
    
    
    fig <-
      plot_ly(
        filtered_CovLang(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovLang2 <- renderPlotly({
    req(filtered_CovLang2())
    
    
    fig <-
      plot_ly(
        filtered_CovLang2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarCovEdu <- renderPlotly({
    req(filtered_CovEdu())
    
    
    fig <-
      plot_ly(
        filtered_CovEdu(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovEdu2 <- renderPlotly({
    req(filtered_CovEdu2())
    
    
    fig <-
      plot_ly(
        filtered_CovEdu2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  ####
  
  output$sBarCov1 <- renderPlotly({
    req(filtered_Cov1())
    
    
    fig <-
      plot_ly(
        filtered_Cov1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCov21 <- renderPlotly({
    req(filtered_Cov21())
    
    
    fig <-
      plot_ly(
        filtered_Cov21(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovAge1 <- renderPlotly({
    req(filtered_CovAge1())
    
    
    fig <-
      plot_ly(
        filtered_CovAge1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovAge21 <- renderPlotly({
    req(filtered_CovAge21())
    
    
    fig <-
      plot_ly(
        filtered_CovAge21(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovSex1 <- renderPlotly({
    req(filtered_CovSex1())
    
    
    fig <-
      plot_ly(
        filtered_CovSex1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovSex21 <- renderPlotly({
    req(filtered_CovSex21())
    
    
    fig <-
      plot_ly(
        filtered_CovSex21(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovGen1 <- renderPlotly({
    req(filtered_CovGen1())
    
    
    fig <-
      plot_ly(
        filtered_CovGen1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovGen21 <- renderPlotly({
    req(filtered_CovGen21())
    
    
    fig <-
      plot_ly(
        filtered_CovGen21(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarCovLang1 <- renderPlotly({
    req(filtered_CovLang1())
    
    
    fig <-
      plot_ly(
        filtered_CovLang1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovLang21 <- renderPlotly({
    req(filtered_CovLang21())
    
    
    fig <-
      plot_ly(
        filtered_CovLang21(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarCovEdu1 <- renderPlotly({
    req(filtered_CovEdu1())
    
    
    fig <-
      plot_ly(
        filtered_CovEdu1(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "5 years before Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarCovEdu21 <- renderPlotly({
    req(filtered_CovEdu21())
    
    
    fig <-
      plot_ly(
        filtered_CovEdu21(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar',
        marker = list(color = "#FF6105")
      ) %>%
      
      
      
      
      layout(
        title = list(text = "Since the beginning of Covid-19 pandemic", font = list(size = 18)),
        yaxis = list(range = c(0, 60), title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  ###
  
  #Graph for Hate Crime by VisMin
  output$immdisPlot <- renderPlotly({
    req(filtered_immdisc())
    
    fig <-
      plot_ly(
        filtered_immdisc(),
        x = ~ Motivation,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Police reported hate crimes by groups designated as visible minority", font = list(size = 18)),
        yaxis = list(title = 'Number'),
        xaxis = list(title = 'Race or ethnicity')
      )
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$immdis2Plot <- renderPlotly({
    req(filtered_imm2disc())
    
    fig <-
      plot_ly(
        filtered_imm2disc(),
        x = ~ Motivation,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Police reported hate crimes by groups designated as visible minority", font = list(size = 18)),
        yaxis = list(title = 'Number'),
        xaxis = list(title = 'Motive')
      )
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarOver <- renderPlotly({
    req(filtered_OverVM())
    
    fig <-
      plot_ly(
        filtered_OverVM(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Overqualified workers with a university degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #Mimi
  
  #Plotly Immigration
  
  output$sBarConf1 <- renderPlotly({
    req(filtered_Conf1())
    
    
    fig <-
      plot_ly(
        filtered_Conf1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfAge1 <- renderPlotly({
    req(filtered_ConfAge1())
    
    
    fig <-
      plot_ly(
        filtered_ConfAge1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfSex1 <- renderPlotly({
    req(filtered_ConfSex1())
    
    
    fig <-
      plot_ly(
        filtered_ConfSex1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfGen1 <- renderPlotly({
    req(filtered_ConfGen1())
    
    
    fig <-
      plot_ly(
        filtered_ConfGen1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarConfLang1 <- renderPlotly({
    req(filtered_ConfLang1())
    
    
    fig <-
      plot_ly(
        filtered_ConfLang1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarConfEdu1 <- renderPlotly({
    req(filtered_ConfEdu1())
    
    
    fig <-
      plot_ly(
        filtered_ConfEdu1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  #
  output$employmentPlot7 <- renderPlotly({
    req(filtered_Employ7())
    
    
    fig <-
      plot_ly(
        filtered_Employ7(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Have access to disability insurance under employment contract", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$employmentPlot6 <- renderPlotly({
    req(filtered_Employ6())
    
    
    fig <-
      plot_ly(
        filtered_Employ6(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Match between education and employment", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$employmentPlot5 <- renderPlotly({
    req(filtered_Employ5())
    
    
    fig <-
      plot_ly(
        filtered_Employ5(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Have access to paid vacation leave under employment contract", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$employmentPlot4 <- renderPlotly({
    req(filtered_Employ4())
    
    
    fig <-
      plot_ly(
        filtered_Employ4(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Employment contract includes at least one type of employment benefits", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$employmentPlot3 <- renderPlotly({
    req(filtered_Employ3())
    
    
    fig <-
      plot_ly(
        filtered_Employ3(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Have a workplace pension plan", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$employmentPlot2 <- renderPlotly({
    req(filtered_Employ2())
    
    
    fig <-
      plot_ly(
        filtered_Employ2(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Job offers good prospects for career advancement", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$employmentPlot <- renderPlotly({
    req(filtered_Employ())
    
    
    fig <-
      plot_ly(
        filtered_Employ(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Have access to paid sick leave under employment contract", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #Plotly for Immigration Public services
  
  output$sBar2Conf <- renderPlotly({
    req(filtered_2Conf())
    
    
    fig <-
      plot_ly(
        filtered_2Conf(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge <- renderPlotly({
    req(filtered_2ConfAge())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex <- renderPlotly({
    req(filtered_2ConfSex())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen <- renderPlotly({
    req(filtered_2ConfGen())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang <- renderPlotly({
    req(filtered_2ConfLang())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu <- renderPlotly({
    req(filtered_2ConfEdu())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2Conf1 <- renderPlotly({
    req(filtered_2Conf1())
    
    
    fig <-
      plot_ly(
        filtered_2Conf1(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge1 <- renderPlotly({
    req(filtered_2ConfAge1())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge1(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex1 <- renderPlotly({
    req(filtered_2ConfSex1())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex1(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen1 <- renderPlotly({
    req(filtered_2ConfGen1())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen1(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang1 <- renderPlotly({
    req(filtered_2ConfLang1())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang1(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu1 <- renderPlotly({
    req(filtered_2ConfEdu1())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu1(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the police", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  ##
  
  output$sBar2Conf2 <- renderPlotly({
    req(filtered_2Conf2())
    
    
    fig <-
      plot_ly(
        filtered_2Conf2(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge2 <- renderPlotly({
    req(filtered_2ConfAge2())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge2(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex2 <- renderPlotly({
    req(filtered_2ConfSex2())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex2(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen2 <- renderPlotly({
    req(filtered_2ConfGen2())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen2(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang2 <- renderPlotly({
    req(filtered_2ConfLang2())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang2(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu2 <- renderPlotly({
    req(filtered_2ConfEdu2())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu2(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the Canadian media", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  output$sBar2Conf3 <- renderPlotly({
    req(filtered_2Conf3())
    
    
    fig <-
      plot_ly(
        filtered_2Conf3(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge3 <- renderPlotly({
    req(filtered_2ConfAge3())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge3(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex3 <- renderPlotly({
    req(filtered_2ConfSex3())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex3(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen3 <- renderPlotly({
    req(filtered_2ConfGen3())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen3(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang3 <- renderPlotly({
    req(filtered_2ConfLang3())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang3(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu3 <- renderPlotly({
    req(filtered_2ConfEdu3())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu3(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the school system", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  
  output$sBar2Conf4 <- renderPlotly({
    req(filtered_2Conf4())
    
    
    fig <-
      plot_ly(
        filtered_2Conf4(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge4 <- renderPlotly({
    req(filtered_2ConfAge4())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge4(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex4 <- renderPlotly({
    req(filtered_2ConfSex4())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex4(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen4 <- renderPlotly({
    req(filtered_2ConfGen4())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen4(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang4 <- renderPlotly({
    req(filtered_2ConfLang4())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang4(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu4 <- renderPlotly({
    req(filtered_2ConfEdu4())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu4(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in the justice system and courts", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  
  output$sBar2Conf5 <- renderPlotly({
    req(filtered_2Conf5())
    
    
    fig <-
      plot_ly(
        filtered_2Conf5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge5 <- renderPlotly({
    req(filtered_2ConfAge5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex5 <- renderPlotly({
    req(filtered_2ConfSex5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen5 <- renderPlotly({
    req(filtered_2ConfGen5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang5 <- renderPlotly({
    req(filtered_2ConfLang5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu5 <- renderPlotly({
    req(filtered_2ConfEdu5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu4(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  output$sBar2Conf5 <- renderPlotly({
    req(filtered_2Conf5())
    
    
    fig <-
      plot_ly(
        filtered_2Conf5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge5 <- renderPlotly({
    req(filtered_2ConfAge5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex5 <- renderPlotly({
    req(filtered_2ConfSex5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen5 <- renderPlotly({
    req(filtered_2ConfGen5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang5 <- renderPlotly({
    req(filtered_2ConfLang5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang5(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu5 <- renderPlotly({
    req(filtered_2ConfEdu5())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu4(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in major corporations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  output$sBar2Conf6 <- renderPlotly({
    req(filtered_2Conf6())
    
    
    fig <-
      plot_ly(
        filtered_2Conf6(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge6 <- renderPlotly({
    req(filtered_2ConfAge6())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge6(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex6 <- renderPlotly({
    req(filtered_2ConfSex6())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex6(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen6 <- renderPlotly({
    req(filtered_2ConfGen6())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen6(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang6 <- renderPlotly({
    req(filtered_2ConfLang6())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang6(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu6 <- renderPlotly({
    req(filtered_2ConfEdu6())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu6(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in merchants and local business people", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  ##
  
  output$sBar2Conf7 <- renderPlotly({
    req(filtered_2Conf7())
    
    
    fig <-
      plot_ly(
        filtered_2Conf7(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge7 <- renderPlotly({
    req(filtered_2ConfAge7())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge7(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex7 <- renderPlotly({
    req(filtered_2ConfSex7())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex7(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen7 <- renderPlotly({
    req(filtered_2ConfGen7())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen7(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang7 <- renderPlotly({
    req(filtered_2ConfLang7())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang7(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu7 <- renderPlotly({
    req(filtered_2ConfEdu7())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu7(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Confidence in banks", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #END
  
  
  output$sBar2Conf12 <- renderPlotly({
    req(filtered_2Conf12())
    
    
    fig <-
      plot_ly(
        filtered_2Conf12(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge12 <- renderPlotly({
    req(filtered_2ConfAge12())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge12(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex12 <- renderPlotly({
    req(filtered_2ConfSex12())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex12(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen12 <- renderPlotly({
    req(filtered_2ConfGen12())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen12(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang12 <- renderPlotly({
    req(filtered_2ConfLang12())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang12(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu12 <- renderPlotly({
    req(filtered_2ConfEdu12())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu12(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to Canada", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #End
  output$sBar2Conf11 <- renderPlotly({
    req(filtered_2Conf11())
    
    
    fig <-
      plot_ly(
        filtered_2Conf11(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge11 <- renderPlotly({
    req(filtered_2ConfAge11())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge11(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex11 <- renderPlotly({
    req(filtered_2ConfSex11())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex11(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen11 <- renderPlotly({
    req(filtered_2ConfGen11())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen11(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang11 <- renderPlotly({
    req(filtered_2ConfLang11())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang11(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu11 <- renderPlotly({
    req(filtered_2ConfEdu11())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu11(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the province", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #End
  output$sBar2Conf10 <- renderPlotly({
    req(filtered_2Conf10())
    
    
    fig <-
      plot_ly(
        filtered_2Conf10(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge10 <- renderPlotly({
    req(filtered_2ConfAge10())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge10(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex10 <- renderPlotly({
    req(filtered_2ConfSex10())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex10(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen10 <- renderPlotly({
    req(filtered_2ConfGen10())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen10(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang10 <- renderPlotly({
    req(filtered_2ConfLang10())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang10(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu10 <- renderPlotly({
    req(filtered_2ConfEdu10())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu10(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the town or city", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #End
  
  
  output$sBar2Conf9 <- renderPlotly({
    req(filtered_2Conf9())
    
    
    fig <-
      plot_ly(
        filtered_2Conf9(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge9 <- renderPlotly({
    req(filtered_2ConfAge9())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge9(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex9 <- renderPlotly({
    req(filtered_2ConfSex9())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex9(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen9 <- renderPlotly({
    req(filtered_2ConfGen9())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen9(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang9 <- renderPlotly({
    req(filtered_2ConfLang9())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang9(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu9 <- renderPlotly({
    req(filtered_2ConfEdu9())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu9(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Strong sense of belonging to the local community", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #End
  
  output$sBar2Conf8 <- renderPlotly({
    req(filtered_2Conf8())
    
    
    fig <-
      plot_ly(
        filtered_2Conf8(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfAge8 <- renderPlotly({
    req(filtered_2ConfAge8())
    
    
    fig <-
      plot_ly(
        filtered_2ConfAge8(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfSex8 <- renderPlotly({
    req(filtered_2ConfSex8())
    
    
    fig <-
      plot_ly(
        filtered_2ConfSex8(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfGen8 <- renderPlotly({
    req(filtered_2ConfGen8())
    
    
    fig <-
      plot_ly(
        filtered_2ConfGen8(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBar2ConfLang8 <- renderPlotly({
    req(filtered_2ConfLang8())
    
    
    fig <-
      plot_ly(
        filtered_2ConfLang8(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBar2ConfEdu8 <- renderPlotly({
    req(filtered_2ConfEdu8())
    
    
    fig <-
      plot_ly(
        filtered_2ConfEdu8(),
        x = ~ Characteristic,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      
      layout(
        title = list(text = "Reported that most people can be trusted in general", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  #
  
  
  
  #end
  
  output$sBarYouth2 <- renderPlotly({
    req(filtered_youthVM2())
    
    fig <-
      plot_ly(
        filtered_youthVM2(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Youth not in employment, education or training", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  #end
  
  output$sBar2Rep4 <- renderPlotly({
    req(filtered_rep4VM2())
    
    fig <-
      plot_ly(
        filtered_rep4VM2(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population in self-employment (unincorporated)", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  #end
  
  output$sBar2Rate4 <- renderPlotly({
    req(filtered_rate4VM2())
    
    fig <-
      plot_ly(
        filtered_rate4VM2(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population in full-time employment", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  #end
  
  output$sBar2Rate3 <- renderPlotly({
    req(filtered_rate3VM2())
    
    fig <-
      plot_ly(
        filtered_rate3VM2(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Unemployment rate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  #end
  
  output$sBar2Rate2 <- renderPlotly({
    req(filtered_rate2VM2())
    
    fig <-
      plot_ly(
        filtered_rate2VM2(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Employment rate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  #end
  output$sBar2Rate1 <- renderPlotly({
    req(filtered_rate1VM2())
    
    fig <-
      plot_ly(
        filtered_rate1VM2(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Participation rate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  #end
  
  output$sBar2Inc2 <- renderPlotly({
    req(filtered_inc2VM2())
    
    fig <-
      plot_ly(
        filtered_inc2VM2(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Average weekly earnings (full-time)", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  #end
  
  output$sBar2Inc1 <- renderPlotly({
    req(filtered_inc1VM2())
    
    fig <-
      plot_ly(
        filtered_inc1VM2(),
        x = ~ Immigration,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Average employment income", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  #Plotly VisMin Labour Participation
  output$sBarRate4 <- renderPlotly({
    req(filtered_rate4VM())
    
    fig <-
      plot_ly(
        filtered_rate4VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population in full-time employment", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #end
  output$sBarRate3 <- renderPlotly({
    req(filtered_rate3VM())
    
    fig <-
      plot_ly(
        filtered_rate3VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Unemployment rate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  #end
  output$sBarRate2 <- renderPlotly({
    req(filtered_rate2VM())
    
    fig <-
      plot_ly(
        filtered_rate2VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Employment rate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  #end
  output$sBarRate1 <- renderPlotly({
    req(filtered_rate1VM())
    
    fig <-
      plot_ly(
        filtered_rate1VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Participation rate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #end
  
  output$sBarInc2 <- renderPlotly({
    req(filtered_inc2VM())
    
    fig <-
      plot_ly(
        filtered_inc2VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Average weekly earnings (full-time)", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  #end
  
  output$sBarInc1 <- renderPlotly({
    req(filtered_inc1VM())
    
    fig <-
      plot_ly(
        filtered_inc1VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Average employment income", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  #end
  output$sBarRep4 <- renderPlotly({
    req(filtered_rep4VM())
    
    fig <-
      plot_ly(
        filtered_rep4VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population in self-employment (unincorporated)", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  ######
  
  output$sBarRep3 <- renderPlotly({
    req(filtered_rep3VM())
    
    fig <-
      plot_ly(
        filtered_rep3VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Workers in middle management occupations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  output$sBarRep2 <- renderPlotly({
    req(filtered_rep2VM())
    
    fig <-
      plot_ly(
        filtered_rep2VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Workers in senior management occupations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarRep1 <- renderPlotly({
    req(filtered_rep1VM())
    
    fig <-
      plot_ly(
        filtered_rep1VM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Workers in all management occupations", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  ####end
  output$sBarYouth <- renderPlotly({
    req(filtered_youthVM())
    
    fig <-
      plot_ly(
        filtered_youthVM(),
        x = ~ VisMin,
        y = ~ Value,
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Youth not in employment, education or training", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  
  #End
  
  
  #Graph for Hate Crime by VisMin
  output$sBarOver <- renderPlotly({
    req(filtered_OverVM())
    
    fig <-
      plot_ly(
        filtered_OverVM(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Overqualified workers with a university degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  
  #Graph for Hate Crime by VisMin
  output$sBarOverIS <- renderPlotly({
    req(filtered_OverIS())
    
    fig <-
      plot_ly(
        filtered_OverIS(),
        x = ~ Immigration,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Overqualified workers with a university degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarOverGEO <- renderPlotly({
    req(filtered_OverGEO())
    
    fig <-
      plot_ly(
        filtered_OverGEO(),
        x = ~ Geography,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Overqualified workers with a university degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Geography')
      )
    
    fig
    
  })
  
  output$sBarOverSX <- renderPlotly({
    req(filtered_OverIS())
    
    fig <-
      plot_ly(
        filtered_OverSX(),
        x = ~ Sex,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Overqualified workers with a university degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Sex')
      )
    
    fig
    
  })
  
  
  
  #Graph for Hate Crime by VisMin
  output$sBarEduSX <- renderPlotly({
    req(filtered_educationSX1())
    
    fig <-
      plot_ly(
        filtered_educationSX1(),
        x = ~ Sex,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with bachelor’s degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Sex')
      )
    
    fig
    
  })
  
  output$sBarEduSX1 <- renderPlotly({
    req(filtered_educationSX2())
    
    fig <-
      plot_ly(
        filtered_educationSX2(),
        x = ~ Sex,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with no certificate, diploma or degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Sex')
      )
    
    fig
    
  })
  
  output$sBarEduSX2 <- renderPlotly({
    req(filtered_educationSX3())
    
    fig <-
      plot_ly(
        filtered_educationSX3(),
        x = ~ Sex,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with high school diploma or equivalency certificate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Sex')
      )
    
    fig
    
  })
  
  
  output$sBarEduSX3 <- renderPlotly({
    req(filtered_educationSX4())
    
    fig <-
      plot_ly(
        filtered_educationSX4(),
        x = ~ Sex,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with postsecondary certificate or diploma below bachelor level", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Sex')
      )
    
    fig
    
  })
  
  
  output$sBarEduSX4 <- renderPlotly({
    req(filtered_educationSX5())
    
    fig <-
      plot_ly(
        filtered_educationSX5(),
        x = ~ Sex,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with bachelor’s degree or above", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Sex')
      )
    
    fig
    
  })
  
  
  output$sBarEduSX5 <- renderPlotly({
    req(filtered_educationSX6())
    
    fig <-
      plot_ly(
        filtered_educationSX6(),
        x = ~ Sex,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with master’s degree or earned doctorate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Sex')
      )
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarEduVM <- renderPlotly({
    req(filtered_educationVM1())
    
    fig <-
      plot_ly(
        filtered_educationVM1(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with bachelor’s degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarEduVM1 <- renderPlotly({
    req(filtered_educationVM2())
    
    fig <-
      plot_ly(
        filtered_educationVM2(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with no certificate, diploma or degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  output$sBarEduVM2 <- renderPlotly({
    req(filtered_educationVM3())
    
    fig <-
      plot_ly(
        filtered_educationVM3(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with high school diploma or equivalency certificate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarEduVM3 <- renderPlotly({
    req(filtered_educationVM4())
    
    fig <-
      plot_ly(
        filtered_educationVM4(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with postsecondary certificate or diploma below bachelor level", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarEduVM4 <- renderPlotly({
    req(filtered_educationVM5())
    
    fig <-
      plot_ly(
        filtered_educationVM5(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with bachelor’s degree or above", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  output$sBarEduVM5 <- renderPlotly({
    req(filtered_educationVM6())
    
    fig <-
      plot_ly(
        filtered_educationVM6(),
        x = ~ VisMin,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with master’s degree or earned doctorate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Visible minority status')
      )
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarEdu <- renderPlotly({
    req(filtered_education1())
    
    fig <-
      plot_ly(
        filtered_education1(),
        x = ~ Immigration,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with bachelor’s degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  #Graph for Hate Crime by VisMin
  output$sBarEdu1 <- renderPlotly({
    req(filtered_education2())
    
    fig <-
      plot_ly(
        filtered_education2(),
        x = ~ Immigration,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with no certificate, diploma or degree", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  output$sBarEdu2 <- renderPlotly({
    req(filtered_education3())
    
    fig <-
      plot_ly(
        filtered_education3(),
        x = ~ Immigration,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with high school diploma or equivalency certificate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  output$sBarEdu3 <- renderPlotly({
    req(filtered_education4())
    
    fig <-
      plot_ly(
        filtered_education4(),
        x = ~ Immigration,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with postsecondary certificate or diploma below bachelor level", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  output$sBarEdu4 <- renderPlotly({
    req(filtered_education5())
    
    fig <-
      plot_ly(
        filtered_education5(),
        x = ~ Immigration,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with bachelor’s degree or above", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  output$sBarEdu5 <- renderPlotly({
    req(filtered_education6())
    
    fig <-
      plot_ly(
        filtered_education6(),
        x = ~ Immigration,
        y = ~ Value,
        name = "2014",
        type = 'bar'
      ) %>%
      
      layout(
        title = list(text = "Population with master’s degree or earned doctorate", font = list(size = 18)),
        yaxis = list(title = 'Percent (%)'),
        xaxis = list(title = 'Immigrant and generation status')
      )
    
    fig
    
  })
  
  
  
  
  
  
  
  
  
  
  # Line graph
  output$lgraph <- renderPlotly({
    # Require filtered_lineData
    req(filtered_lineData())
    
    # Create the base graph
    lp <- plot_ly(data = filtered_lineData(), x = ~ Year)
    
    # Add each series one-by-one as new traces
    for (i in 3:length(colnames(filtered_lineData()))) {
      lp <- lp %>%
        add_trace(
          x = filtered_lineData()$Year,
          y = filtered_lineData()[[i]],
          type = "scatter",
          mode = "lines+markers",
          name = colnames(filtered_lineData())[i]
        )
    }
    
    
    
    
    # Note hovermode = "x unified" is not working as it is supposed to
    # Best work-around was used in xaxis with spike layout
    lp <- lp %>%
      layout(
        title = "Police Reported Hate Crime Time Series Analysis",
        hovermode = "Police Reported Hate Crime Time Series Analysis",
        xaxis = list(
          title = "Year",
          showspikes = TRUE,
          spikecolor = "black",
          spikethickness = 2,
          spikemode  = 'toaxis+across',
          spikesnap = 'data',
          showline = TRUE
        ),
        yaxis = list(title = "Number")
      )
    
    lp
  })
  
  
  
  # Line graph
  output$ltwograph <- renderPlotly({
    # Require filtered_lineData
    req(filtered_linetwoData())
    
    # Create the base graph
    lp <- plot_ly(data = filtered_linetwoData(), x = ~ Year)
    
    # Add each series one-by-one as new traces
    for (i in 3:length(colnames(filtered_linetwoData()))) {
      lp <- lp %>%
        add_trace(
          x = filtered_linetwoData()$Year,
          y = filtered_linetwoData()[[i]],
          type = "scatter",
          mode = "lines+markers",
          name = colnames(filtered_linetwoData())[i]
        )
    }
    
    
    
    
    # Note hovermode = "x unified" is not working as it is supposed to
    # Best work-around was used in xaxis with spike layout
    lp <- lp %>%
      layout(
        title = "Police Reported Hate Crime Time Series Analysis",
        hovermode = "Police Reported Hate Crime Time Series Analysis",
        xaxis = list(
          title = "Year",
          showspikes = TRUE,
          spikecolor = "black",
          spikethickness = 2,
          spikemode  = 'toaxis+across',
          spikesnap = 'data',
          showline = TRUE
        ),
        yaxis = list(title = "Number")
      )
    
    lp
  })
}
# Run the app -------------------------------------------------------
shinyApp(ui = ui, server = server)