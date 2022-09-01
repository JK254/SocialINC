library("reactlog")

#'NOTE [https://rstudio.github.io/reactlog/]

# Tell shiny to log all reactivity
reactlog_enable()

# Run the app
source("./3_example_server.R")
shinyApp(ui, server)

# Once app has closed, display reactlog from shiny in an HTML page
shiny::reactlogShow()