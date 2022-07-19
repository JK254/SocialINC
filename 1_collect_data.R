# Load packages ----
library(tidyverse) # for dplyr
library(cansim) # for retrieving CODR tables
library(arrow) # to write files to parquet (smaller than .csv files)

#'NOTE [You might need to run each data pull one by one otherwise it might crash your session since it's a lot of data. However, once you've done pulled the data you don't need to do it again since it looks like the data is periodically collected and dissemination.]

# Repeated values ----
prov_filter <- 
  c(
    "11124", # Canada
    "10", # Newfoundland and Labrador
    "11", # Prince Edward Island Nova Scotia
    "12", # Nova Scotia
    "13", # New Brunswick
    "2", # Quebec region
    "3", # Ontario region
    "46", # Manitoba
    "47", # Saskatchewan
    "48", # Alberta
    "5", # British Columbia region
    "60", # Northwest Territories
    "61", # Yukon
    "62" # Nunavut
  ) # can only retrieve at the provincial level because otherwise it's costs too much memory

# Functions ----
#'NOTE [you need to specific what variable grouping you want to pull and what CODR table you want. E.g., if you want just REF_DATE, GEO, `Visible minority`, `Selected sociodemographic characteristics`, Indicators, and VALUE you'd only need to reference the "base" pull]
func_codr <- function(pull_type, codr_no) {
  if (pull_type == "base") {
    #'NOTE [Return reference period, geography, visible minority, characteristic, indicator, and value as a base retrieval]
    df <- 
      get_cansim(codr_no) %>% 
      select(
        Year = REF_DATE,
        Geography = GEO,
        VisMin = `Visible minority`,
        Characteristic = `Selected sociodemographic characteristics`,
        Indicator = Indicators,
        Value = VALUE
      ) # select and rename the relevant variables
  } else if (pull_type == "condfidence") {
    #'NOTE [Retrieve confidence/characteristics on top of base retrieval]
    df <- 
      get_cansim(codr_no) %>%
      select(
        Year = REF_DATE,
        Geography = GEO,
        VisMin = `Visible minority`,
        Characteristic = `Selected sociodemographic characteristics`,
        Indicator = Indicators,
        Confidence = Characteristics, ### Different from the following function ###
        Value = VALUE
      ) # select and rename the relevant variables
  } else if (pull_type == "characteristics") {
    #'NOTE [Retrieve confidence/statistics on top of base retrieval]
    df <- 
      get_cansim(codr_no) %>%
      select(
        Year = REF_DATE,
        Geography = GEO,
        VisMin = `Visible minority`,
        Characteristic = `Selected sociodemographic characteristics`,
        Indicator = Indicators,
        Confidence = Statistics, ### Different from the previous function ###
        Value = VALUE
      ) # select and rename the relevant variables
  }
  gc()
  return(df)
}

#'NOTE [Return first official language spoken (on top of base variables)]
#'NOTE [These tables are huge and require the get_cansim_sqlite function instead of the regular get_cansim function, otherwise we run out of memory]
func_sql <- function(x) {
  df <- 
    get_cansim_sqlite(x,
                    refresh = TRUE,
                    timeout = 1000,
                    language = "en") %>%
    filter(GeoUID %in% prov_filter) %>% 
    collect_and_normalize() %>% # required to properly index the data
    select(
      Year = REF_DATE,
      Geography = GEO,
      Sex,
      Age = `Age group and first officiel language spoken`,
      Immigration = `Immigrant and generation status`,
      VisMin = `Visible minority status`,
      Degree = `Highest certificate, diploma or degree`,
      Indicator = Indicators,
      Value = VALUE
    ) # select and rename the relevant variables
  gc()
  return(df)
}

# Data loading and pre-processing ----
## Selected labour force status ----
rateDT <-
  func_sql(4310006901)

## Average employment income indicators ----
incomeDT <-
  func_sql(4310006801)

## Basic needs and housing data ----
basicDT <- 
  func_codr(pull_type = "condfidence", codr_no = 1310084101)

## Health indicators ----
healthDT <- 
  basicDT 
#'NOTE [Q: is this supposed to be a copy of the other table? A: yes basic and health are almost similar but one has expounded geography]

## Civic engagement data ----
civicDT <- 
  func_codr(pull_type = "characteristics", codr_no = 4310006501)

### Civic engagement and engagement in political activities ----
civicDT2 <- 
  func_codr(pull_type = "characteristics", codr_no = 4310006601)

## Representation (Selected management occupations and self-employed class of worker) ----
representationDT <-
  func_sql(4310007001)

## Youth not in employment ----
youthDT <- 
  get_cansim(4310007201) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    Age = `Age group`,
    Language = `First official language spoken`,
    Immigration = `Generation status`, 
    # This variable ^ isn't found in the other tables hence no function for this CODR
    VisMin = `Visible minority status`,
    Indicator = Indicators,
    Value = VALUE
  ) # select and rename the relevant variables
gc()

## Sense of belonging ----
belongingDT <-
  func_codr(pull_type = "characteristics", codr_no = 4310006401)

#'NOTE [this table has not been released yet / 2022-07-04]
# ## Employment data ----
# employmentDT <- 
#   func_codr(pull_type = "base", codr_no = 4310005901)

## Confidence in Canadian institutions ----
confidenceDT <-
  func_codr(pull_type = "base", codr_no = 4310006201)

## Discrimination ----
discriminationDT <-
  func_codr(pull_type = "characteristics", y = 4310006101)

## Education ----
educationDT <- 
  get_cansim_sqlite(4310006701,
                    refresh = TRUE,
                    timeout = 1000,
                    language = "en") %>%
  filter(GeoUID %in% prov_filter) %>% 
  collect_and_normalize() %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    Age = `Age group`,
    Language =  `First official language spoken`,
    Immigration = `Immigrant and generation status`, 
    # This variable ^ isn't found in the other tables hence no function for this CODR
    VisMin = `Visible minority status`,
    Indicator = Indicators,
    Value = VALUE
  ) # select and rename the relevant variables
gc()

## Overqualification rate ----
OverQualDT <-
  get_cansim_sqlite(4310007101,
                    refresh = TRUE,
                    timeout = 1000,
                    language = "en") %>%
  filter(GeoUID == "11124") %>% # Canada
  collect_and_normalize() %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Sex,
    Age = `Age group`,
    Language =  `First official language spoken`,
    Immigration = `Immigrant and generation status`,
    # This variable ^ isn't found in the other tables hence no function for this CODR
    VisMin = `Visible minority status`,
    Location = `Location of study`,
    # This variable ^ isn't found in the other tables hence no function for this CODR
    Degree = `Highest certificate, diploma or degree`,
    # This variable ^ isn't found in the other tables hence no function for this CODR
    Indicator = Indicators,
    Value = VALUE
  ) # select and rename the relevant variables
gc()

## Police-reported hate crime ----
polData <-
  get_cansim(3510006601) %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Motivation = `Type of motivation`,
    Value = VALUE
  ) # select and rename the relevant variables
gc()

# Export files to parquet -------------------------------------------------
# Create list of datasets
df_list <- names(which(unlist(eapply(
  .GlobalEnv, is.data.frame
))))

# Export to parquet
for (i in df_list) {
  write_parquet(x = get(i),
                sink = paste0("./_tempdata/", i, ".parquet"))
  
}