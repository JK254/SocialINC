# About

Task: Create a data visualization for SII-IIS

Authors: 

- Jecinta Kemboi
- Suhayl Sayed


Date: 2022-06-10

This project intends to create a dashboards for the follwing social indicators.

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
| 43-10-0058-01 | Community and neighbourhood satisfaction | communityDT | |
| 43-10-0059-01 | Perceptions of police performance and personal safety satisfaction | perceptionDT | |
| 43-10-0060-01 | Selected housing characteristics low income indicators and knowledge of official languages | languagesDT | |
| 43-10-0060-01 | Size and composition of personal networks | networkDT | |
| 43-10-0073-01 | Selected economic housing characteristics | housingDT | |
| 43-10-0061-01 | Experience(s) of discrimination 5 years before and since the beginning of COVID-19 pandemic | discriminationDT | |
| 43-10-0062-01 | Confidence in Canadian institutions | confidenceDT | |
| 43-10-0064-01 | Sense of belonging to the local community town province and Canada and trust in people | belongingDT | |
| 43-10-0065-01 | Civic engagement and engagement in political activities | civicDT | |
| 43-10-0066-01 | Vote in the last federal provincial and municipal elections | civicDT2 | |
| 43-10-0067-01 | Highest certificate degree or diploma | educationDT | |
| 43-10-0068-01 | Average employment income indicators | incomeDT | |
| 43-10-0069-01 | Selected labour force status | rateDT | |
| 43-10-0070-01 | Selected management occupations and self-employed class of worker | representationDT | |
| 43-10-0071-01 | Overqualification rate | OverQualDT | |
| 43-10-0072-01 | Youth not in employment education or training in percent |  youthDT | |
| 13-10-0841-01 | Health indicators (demographic characteristics) | basicDT | |
| 13-10-0842-01 | Health indicators (sociodemographic characteristics) | basicDT | |
| 35-10-0066-01 | Police-reported hate crime |  polData | |

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
