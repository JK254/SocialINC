# About

Task: Create a data visualization tools for SII-IIS

Authors: 

- Jecinta Kemboi
- Suhayl Sayed


Date: 2022-06-10

This project intends to create a dashboards for the following social inclusion themes with their indicators
## Participation in the labor market
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023004-eng.htm)

## Representation in decision-making positions 
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023005-eng.htm)

## Civic engagement and political representation
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023006-eng.htm)

## Basic needs and housing 
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023007-eng.htm)

## Health and well-being 
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023008-eng.htm)

## Income and wealth 
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023009-eng.htm)

## Social connections and personal networks
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023010-eng.htm)

## Education and skills development
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023011-eng.htm)

## Public services and institutions
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023012-eng.htm)

## Local community 
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023013-eng.htm)

## Discrimination and victimization
(https://stc-ndm-prod-wc.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023014-eng.htm)

# R script

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

### Participation in the labor market
(https://github.com/JK254/SocialINC/blob/main/theme_1_lm_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_1_lm_fr.R)

### Representation in decision-making positions
(https://github.com/JK254/SocialINC/blob/main/theme_2_representation_fr.R)
(https://github.com/JK254/SocialINC/blob/main/theme_2_representattion_en.R)

### Civic engagement and political representation
(https://github.com/JK254/SocialINC/blob/main/theme_3_civic_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_3_civic_fr.R)

### Basic needs and housing
(https://github.com/JK254/SocialINC/blob/main/theme_4_basic_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_4_basic_fr.R)

### Health and well-being
(https://github.com/JK254/SocialINC/blob/main/theme_5_health_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_5_health_fr.R)

### Income and wealth
(https://github.com/JK254/SocialINC/blob/main/theme_6_income_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_6_income_fr.R)

### Social connections and personal networks
(https://github.com/JK254/SocialINC/blob/main/theme_7_social_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_7_social_fr.R)

### Education and skills development
(https://github.com/JK254/SocialINC/blob/main/theme_8_education_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_8_education_fr.R)

### Public services and institutions
(https://github.com/JK254/SocialINC/blob/main/theme_9_publicserv_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_9_publicserv_fr.R)

### Local community
(https://github.com/JK254/SocialINC/blob/main/theme_10_community_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_10_community_fr.R)

### Discrimination and victimization
(https://github.com/JK254/SocialINC/blob/main/theme_11_discrimination_en.R)
(https://github.com/JK254/SocialINC/blob/main/theme_11_discrimination_fr.R)

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

