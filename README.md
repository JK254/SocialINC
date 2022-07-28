# About

Task: Create a data visualization for SII-IIS

Authors: 

- Suhayl Sayed
- Jecinta Kemboi

Date: 2022-06-10

This project intends to create a dashboard for social indicators.

# R scripts

## Required packages

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

## [1_collect_data.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/1_collect_data.R)

This file retrieves all the data from the following tables and outputs them as .parquet files in the _tempfile folder:

| CODR # | Table name | Dataset name | Notes |
|---|---|---|----|
| 43100069 | Selected labour force status | rateDT | |
| 43100068 | Average employment income indicators | incomeDT | |
| 13100841 | Basic needs and housing data | basicDT | |
| 13100841 | Health indicators | healthDT | Same as basicDT |
| 43100065 | Civic engagement| civicDT | |
| 43100066 | Civic engagement and engagement in political activities | civicDT2 | |
| 43100070 | Representation (Selected management occupations and self-employed class of worker) | representationDT | |
| 43100072 | Youth not in employment | youthDT | |
| 43100064 | Sense of belonging | belongingDT | |
| 43100059 | Employment | employmentDT | Has not been released |
| 43100062 | Confidence in Canadian institutions | confidenceDT | |
| 43100061 | Discrimination | discriminationDT | |
| 43100067 | Education | educationDT | |
| 43100071 | Overqualification rate | OverQualDT & OverQualDT_cma | |
| 35100066 | Police-reported hate crime | polData | |

This script does not need to be run everytime as these surveys are not frequent. The script takes roughly **2.5 hours** to run if you want to output the most recent data.

This script does the following:

1. it uses the `cansim` package to call a table
2. select and rename relevant variables
3. export the table to a .parquet file
4. remove the table to clear up memory
5. rinse and repeat for all CODR tables

## [2_ui.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/2_ui.R)

This script separates the user interface from the original script with some sectioning to better navigate the code.

## [3_server.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/3_server.R)

This script separates the server from the original script with some sectioning to better navigate the code.

## [test.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/test.R)

This script is a sample that provides different ways to show the data more efficiently. This consists of both the user interface and the server components. The following sections will give a brief description of the tabs.

### Tab 1 (Basic income)

This is the most bare bones of the tabs to show the filters using the table as it comes loaded.

### Tab 2 (Sense of belonging)

The tab uses the `facet_wrap()` function to split the graph into the characteristic groupings (AKA `char_type`).

### Tab 3 (Civic)

This tab was created to show how multiple filters and plots can be integrated into one tab.

### Tab 4 (Confidence)

This tab uses the `char_type` variable that was created to add a conditional filter to group relevant characteristics together. 

# About the data

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

## General notes

### Visibile minority

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

# Resources

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