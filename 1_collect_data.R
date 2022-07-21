# Load packages ----
library(tidyverse) # for dplyr
library(cansim) # for retrieving CODR tables
library(arrow) # to write files to parquet (smaller than .csv files)

#'NOTE [You might need to run each data pull one by one otherwise it might crash your session since it's a lot of data. However, once you've done pulled the data you don't need to do it again since it looks like the data is periodically collected and disseminated.]

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
  conn <- get_cansim_sqlite(x,
                            refresh = TRUE,
                            timeout = 1000,
                            language = "en")
  
  df <- 
    conn %>%
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
  
  gc() # this clears up the memory so we can continute to retrieve more data
  
  disconnect_cansim_sqlite(conn)
  
  remove_cansim_sqlite_cached_table(cansimTableNumber = x)
  
  return(df)
}

# Data loading and pre-processing ----
## Selected labour force status ----
rateDT <-
  func_sql("43-10-0069")

## Average employment income indicators ----
incomeDT <-
  func_sql("43-10-0068")

## Basic needs and housing data ----
basicDT <- 
  func_codr(pull_type = "condfidence", codr_no = "13-10-0841")

## Health indicators ----
healthDT <- 
  basicDT 
#'NOTE [Q: is this supposed to be a copy of the other table? A: yes basic and health are almost similar but one has expounded geography]

## Civic engagement data ----
civicDT <- 
  func_codr(pull_type = "characteristics", codr_no = "43-10-0065")

### Civic engagement and engagement in political activities ----
civicDT2 <- 
  func_codr(pull_type = "characteristics", codr_no = "43-10-0066")

## Representation (Selected management occupations and self-employed class of worker) ----
representationDT <-
  func_sql("43-10-0070")

remove_cansim_sqlite_cached_table(cansimTableNumber = "43-10-0070")

## Youth not in employment ----
youthDT <- 
  get_cansim("43-10-0072") %>%
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
  func_codr(pull_type = "characteristics", codr_no = "43-10-0064")

#'NOTE [this table has not been released yet / 2022-07-04]
# ## Employment data ----
# employmentDT <- 
#   func_codr(pull_type = "base", codr_no = "43-10-0059")

## Confidence in Canadian institutions ----
confidenceDT <-
  func_codr(pull_type = "base", codr_no = "43-10-0062")

## Discrimination ----
discriminationDT <-
  func_codr(pull_type = "characteristics", y = "43-10-0061")

## Education ----
education_filter <-
  c(
    "11124", # Canada
    "1", # Maritime region
    "10", # Newfoundland and Labrador
    "11", # Prince Edward Island Nova Scotia
    "12", # Nova Scotia
    "13", # New Brunswick
    "2", # Quebec region
    "3", # Ontario region
    "4", # Prairies region
    "46", # Manitoba
    "47", # Saskatchewan
    "48", # Alberta
    "5", # British Columbia region
    "6", # Territories region
    "60", # Northwest Territories
    "61", # Yukon
    "62" # Nunavut
  ) # can only retrieve at the provincial level because otherwise it's costs too much memory

conn <-
  get_cansim_sqlite(
    "43-10-0067",
    refresh = TRUE,
    timeout = 1000,
    language = "en"
  )

educationDT_prov <- 
  conn %>%
  filter(GeoUID %in% education_filter) %>%
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

educationDT_cma <- 
  conn %>%
  filter(!GeoUID %in% education_filter) %>%
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

disconnect_cansim_sqlite(conn)

remove_cansim_sqlite_cached_table(cansimTableNumber = "43-10-0067")

educationDT <-
  rbind(educationDT_prov, educationDT_cma)

rm(educationDT_prov, educationDT_cma)

## Overqualification rate ----
conn <-
  get_cansim_sqlite(
    "43-10-0071",
    refresh = TRUE,
    timeout = 1000,
    language = "en"
  )
  
OverQualDT_canada <-
  conn %>%
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

OverQualDT_region <-
  conn %>%
  filter(GeoUID %in% 
           c("1", # Maritimes
             "2", # Quebec
             "3", # Ontario
             "4", # Prairies
             "5", # British Columbia
             "6") # Territories
         ) %>%
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

OverQualDT_prov <-
  conn %>%
  filter(GeoUID %in% 
           c("10", # Newfoundland and Labrador
             "11", # Prince Edward Island Nova Scotia
             "12", # Nova Scotia
             "13", # New Brunswick
             
             "46", # Manitoba
             "47", # Saskatchewan
             "48", # Alberta
             
             "60", # Northwest Territories
             "61", # Yukon
             "62") # Nunavut
         ) %>% 
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

remove_cansim_sqlite_cached_table(cansimTableNumber = "43-10-0071")

OverQualDT <-
  rbind(OverQualDT_canada,
        OverQualDT_region,
        OverQualDT_prov)

rm(OverQualDT_canada,
   OverQualDT_region,
   OverQualDT_prov)

gc()

disconnect_cansim_sqlite(conn)

#'NOTE [need to separate CMAs because it's too much memory]
OverQualDT_cma <-
  conn %>%
  filter(!GEO %in%
           c(
             "Canada",
             "Atlantic Region",
             "Quebec Region",
             "Ontario Region",
             "Prairies Region",
             "British Columbia Region",
             "Territories",
             "Newfoundland and Labrador",
             "Prince Edward Island",
             "Nova Scotia",
             "New Brunswick",
             "Manitoba",
             "Saskatchewan",
             "Alberta",
             "Yukon",
             "North-West Territories",
             "Nunavut"
           )
  ) %>%
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
  get_cansim("35-10-0066") %>%
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