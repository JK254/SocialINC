# Install packages ----
for (i in c(
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
)) {
  if (!i %in% installed.packages()) {
    install.packages(i, dependencies = TRUE)
  }
  require(i)
}

# Load packages ----
library(tidyverse) # for dplyr
library(cansim) # for retrieving CODR tables
library(arrow) # to write files to parquet (smaller than .csv files)

#'NOTE [You might need to run each data pull one by one otherwise it might crash your session since it's a lot of data. However, once you've done pulled the data you don't need to do it again since it looks like the data is periodically collected and disseminated.]

# Functions ----
#'NOTE [you need to specific: 1. what CODR table you want, and 2. what you want to name this file, and 3. what variable grouping you want to pull (E.g., if you want just REF_DATE, GEO, `Visible minority`, `Selected sociodemographic characteristics`, Indicators, and VALUE you'd only need to reference the "base" pull)]
func_codr <- function(codr_no, df_name, pull_type) {
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
    #'NOTE [Retrieve confidence/**characteristics** on top of base retrieval]
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
    #'NOTE [Retrieve confidence/**statistics** on top of base retrieval]
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
  
  write_parquet(x = df, sink = paste0("./_tempdata/", df_name,".parquet"))
  
  gc()
}

#'NOTE [you need to specific 1. what CODR table you want and 2. what you want to name this file]
#'NOTE [Return first official language spoken (on top of base variables)]
#'NOTE [These tables are huge and require the get_cansim_sqlite function instead of the regular get_cansim function, otherwise we run out of memory]
func_sql <- function(codr_no, df_name) {
  conn <- get_cansim_sqlite(codr_no,
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
  disconnect_cansim_sqlite(conn) # this disconnects the SQL connection
  remove_cansim_sqlite_cached_table(cansimTableNumber = codr_no) # this removes cached data
  write_parquet(x = df, sink = paste0("./_tempdata/", df_name, ".parquet")) # this exports the dataframe into a parquet file
  
}

# Data loading and pre-processing ----
## Selected labour force status ----
func_sql(codr_no = "43-10-0069", df_name = "rateDT")

## Average employment income indicators ----
func_sql(codr_no = "43-10-0068", df_name = "incomeDT")

## Basic needs and housing data ----
func_codr(codr_no = "13-10-0841",
          df_name = "basicDT",
          pull_type = "condfidence")

## Health indicators ----
#'NOTE [Q: is this supposed to be a copy of the other table? A: yes basic and health are almost similar but one has expounded geography]
func_codr(codr_no = "13-10-0841",
          df_name = "healthDT",
          pull_type = "condfidence")

## Civic engagement ----
func_codr(codr_no = "43-10-0065",
          df_name = "civicDT",
          pull_type = "characteristics")

### Civic engagement and engagement in political activities ----
func_codr(codr_no = "43-10-0066",
          df_name = "civicDT2",
          pull_type = "characteristics")

## Representation (Selected management occupations and self-employed class of worker) ----
func_sql(codr_no = "43-10-0070", df_name = "representationDT")

## Youth not in employment ----
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
  ) %>% # select and rename the relevant variables
  write_parquet(sink = paste0("./_tempdata/youthDT.parquet"))
gc()

## Sense of belonging ----
func_codr(codr_no = "43-10-0064",
          df_name = "belongingDT",
          pull_type = "characteristics")

#'NOTE [this table has not been released yet / 2022-07-04]
# ## Employment ----
# func_codr(codr_no = "43-10-0059",
#           df_name = "employmentDT",
#           pull_type = "base")

## Confidence in Canadian institutions ----
func_codr(codr_no = "43-10-0062",
          df_name = "confidenceDT", 
          pull_type = "characteristics")

## Discrimination ----
func_codr(codr_no = "43-10-0061",
          df_name = "discriminationDT",
          pull_type = "characteristics")

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
  ) # can only retrieve at the provincial level because otherwise it costs too much memory

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

bind_rows(educationDT_prov, educationDT_cma) %>%
  write_parquet(sink = paste0("./_tempdata/educationDT.parquet"))

rm(educationDT_prov, educationDT_cma)

## Overqualification rate ----
conn <-
  get_cansim_sqlite(
    "43-10-0071",
    refresh = TRUE,
    timeout = 1000,
    language = "en"
  )

### Canada ----  
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

### Regions ----
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

### Provinces & territories ----
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

bind_rows(OverQualDT_canada,
          OverQualDT_region,
          OverQualDT_prov) %>%
  write_parquet(sink = paste0("./_tempdata/OverQualDT.parquet"))

rm(OverQualDT_canada,
   OverQualDT_region,
   OverQualDT_prov)

### CMA ----
#### Part 1 ----
#'NOTE [need to separate CMAs because it's too much memory]
OverQualDT_cma_1 <-
  conn %>%
  filter(GeoUID %in%
           c(
             "001",
             "205",
             "305",
             "310",
             "408",
             "421",
             "433",
             "442",
             "462",
             
             "505",
             "24505",
             "35505"
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

#### Part 2 ----
OverQualDT_cma_2 <-
  conn %>%
  filter(GeoUID %in%
           c(
             "521",
             "522",
             "529",
             "532",
             "535",
             "537",
             "539",
             "541",
             "543",
             "550",
             "555",
             "559"
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

#### Part 3 ----
OverQualDT_cma_3 <-
  conn %>%
  filter(GeoUID %in%
           c(
             "568",
             "580",
             "595",
             "602",
             "705",
             "725",
             "810",
             "825",
             "835",
             "915",
             "932",
             "933",
             "935"
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

remove_cansim_sqlite_cached_table(cansimTableNumber = "43-10-0071")
disconnect_cansim_sqlite(conn)

# FIXME: it would be ideally to run this to have everything related to overqualification together but this whole process takes a really long time/it crashes so to be safe I'm outputting two data files which I'll combine later

bind_rows(OverQualDT_cma_1,
            OverQualDT_cma_2,
            OverQualDT_cma_3) %>%
  write_parquet(sink = paste0("./_tempdata/OverQualDT_cma.parquet"))
gc()

rm(conn,
   OverQualDT_cma_1,
   OverQualDT_cma_2,
   OverQualDT_cma_3)

## Police-reported hate crime ----
get_cansim("35-10-0066") %>%
  select(
    Year = REF_DATE,
    Geography = GEO,
    Motivation = `Type of motivation`,
    Value = VALUE
  ) %>% # select and rename the relevant variables
  write_parquet(sink = paste0("./_tempdata/polData.parquet"))
gc()