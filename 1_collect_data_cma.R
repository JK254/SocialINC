# Load packages ----
library(tidyverse) # for dplyr
library(cansim) # for retrieving CODR tables
library(arrow) # to write files to parquet (smaller than .csv files)

cma_filter <- 
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
    "62", # Nunavut
    
    "1", # Atlantic Region
    "4", # Prairies Region
    "6" # Territories
  ) # retrieve only CMAs

# Functions ----
func_sql_cma <- function(x) {
  df <- get_cansim_sqlite(x,
                    refresh = TRUE,
                    timeout = 1000,
                    language = "en") %>% 
    filter(!GeoUID %in% cma_filter) %>% 
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
rateDT_cma <- 
  func_sql_cma(4310006901)

## Average employment income indicators ----
incomeDT_cma <-
  func_sql_cma(4310006801)

## Representation (Selected management occupations and self-employed class of worker) ----
representationDT_cma <-
  func_sql_cma(4310007001)

## Education ----
educationDT_cma <- 
  get_cansim_sqlite(4310006701,
                    refresh = TRUE,
                    timeout = 1000,
                    language = "en") %>% 
  filter(!GeoUID %in% cma_filter) %>% 
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
OverQualDT_prov <-
  get_cansim_sqlite(4310007101,
                    refresh = TRUE,
                    timeout = 1000,
                    language = "en") %>% 
  filter(GeoUID %in% c(
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
  )) %>% 
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

OverQualDT_cma <-
  get_cansim_sqlite(4310007101,
                    refresh = TRUE,
                    timeout = 1000,
                    language = "en") %>% 
  filter(!GeoUID %in% cma_filter) %>% 
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