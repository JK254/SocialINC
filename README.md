# About

Task: Create a data visualization for SII-IIS

Authors: 

- Suhayl Sayed
- Jecinta Kemboi

Date: 2022-06-10

This project intends to create a dashboard for social indicators.

# R scripts

## Required packages

The scripts will install the required packages if they are not currently installed.

You'll need the following to run [0_collect_data.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/1_collect_data.R):

- tidyverse
- cansim
- arrow

You'll need the following to run example dashboard:

- arrow
- dplyr
- plotly
- shiny
- shinyWidgets

> Note: these are separated because you only need to run [0_collect_data.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/1_collect_data.R) to update the data.

## [0_collect_data.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/1_collect_data.R)

> Note: it is highly recommended that you use the `Document Outline` function in RStudio to navigate going through the code.

This file retrieves all the data from the following tables and outputs them as .parquet files in the _tempfile folder:

| CODR # | Table name | Dataset name | Notes |
|---|---|---|----|
| 43100069 | Selected labour force status | rateDT | |
| 43100068 | Average employment income indicators | incomeDT | |
| 13100841 | Basic needs and housing data | basicDT | |
| 13100841 | Health indicators | basicDT | |
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

## [2_example_ui.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/2_ui.R)

This script separates the user interface from the original script with some sectioning to better navigate the code.

## [3_example_server.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/3_server.R)

This script separates the server from the original script with some sectioning to better navigate the code.

## [test.R](https://github.com/klaxonklaxoff/SocialINC/blob/main/test.R)

> Note: this is a miscellaneous dashboard to see what other options there are to present the data.

This script is a sample that provides different ways to show the data more efficiently. This consists of both the user interface and the server components. The following sections will give a brief description of the tabs.

### Tab 1 (Basic income)

This is the most bare bones of the tabs to show the filters using the table as it comes loaded.

### Tab 2 (Sense of belonging)

The tab uses the `facet_wrap()` function to split the graph into the characteristic groupings (AKA `char_type`).

### Tab 3 (Civic)

This tab was created to show how multiple filters and plots can be integrated into one tab.

### Tab 4 (Confidence)

This tab uses the `char_type` variable that was created to add a conditional filter to group relevant characteristics together. 

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