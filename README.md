Task: Create a data visualization for SII-IIS
Authors: Suhayl Sayed, Jecinta Kemboi
Date: 2022-06-10

# About

This project intends to create a dashboard for social indicators.

## List of themes

| Themes | Table(s) included within the theme |
|---|---|
| Participation in the Labour Market | rateDT, OverQualDT, youthDT |
| Civic engagement and political participation | civicDT & civicDT2 |
| Representation in decision-making positions | representationDT |
| Basic needs and housing | basicDT |
| Health and wellbeing | healthDT |
| Education, training and skills | educationDT |
| Income and wealth | incomeDT |
| Social connections and personnal networks | belongingDT |
| Local community | incomeDT |
| Public services and institutions | confidenceDT |
| Discrimination and victimization | discriminationDT, polData |

## Notes about the data 

These data sets have 10 levels for VisMin:
- basicDT
- belongingDT
- civicDT
- civicDT2
- confidenceDT
- discriminationDT
- healthDT

These data sets have 15 levels for VisMin:
- incomeDT
- OverQualDT
- rateDT
- representationDT
- youthDT

polData has no breakdown by VisMin

# Required packages

The following code will install the required packages if they are not currently installed.

```
list_packages <-
  c(
    "shiny",
    "tidyverse",
    "data.table",
    "readxl",
    "plotly",
    "leaflet",
    "htmltools",
    "RColorBrewer",
    "shinyjs",
    "cansim"
  )

for (i in list_packages) {
  if (!i %in% installed.packages()) {
    install.packages(i, dependencies = TRUE)
  }
  require(i)
}
```

# Ressources

## General

- Here is a tutorial for [RShiny](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/
- For examples of [selectizeInput](https://shiny.rstudio.com/gallery/selectize-examples.html) (for the ui.R tab)
- To create a hierarchy of buttons, use [conditional panels](https://shiny.rstudio.com/reference/shiny/1.4.0/conditionalPanel.html)
- For [graphing help](https://plotly.com/r/. Note that traces, values to be plotted x, have to be columns)
- For [choropleth/leaflet help](https://rstudio.github.io/leaflet/json.html)

## How-to's for R shiny layouts

- [Application layout guide](https://shiny.rstudio.com/articles/layout-guide.html)
- [Layouts](https://www.bioinformatics.babraham.ac.uk/shiny/Intro_to_Shiny_course/examples/04.1_layouts/)
- [Improve your shiny application appearance](https://www.christophenicault.com/post/improve_shiny_ui/)

## Documentation
- [Best Practices For Coding In R](https://surge-team.pages.cloud.statcan.ca/blog/2022-07/best_practices_for_coding_in_r/)
- [Data Analytics Standard](https://digital.pages.cloud.statcan.ca/playbook/solutions/data-analytics/standard/)
- [Rainbow Parentheses In RStudio](https://surge-team.pages.cloud.statcan.ca/blog/2022-03/rainbow_parentheses/)
