# U.S. Bureau of Labor Statistics
# Determining Unemployment Rate 

# set up ----
# packages 

library(here)
library(tidyverse)
library(curl)
library(readxl)

# USA level ----
# download 
curl_download(url = "https://www.bls.gov/cps/cpsaat01.xlsx", # from https://www.bls.gov/cps/tables.htm#empstat
              destfile = "data/raw/us_bureau_of_labor_usa.xlsx",
              quiet = FALSE)

# read data
labor_usa <- read_excel(
  "data/raw/us_bureau_of_labor_usa.xlsx",
  skip = 8,
  n_max = 71,
  col_names = c(
    "year",
    "civilian_noninstitutional_population",
    "civilian_labor_total",
    "civilian_labor_percent",
    "civilian_labor_employed_total",
    "civilian_labor_employed_percent",
    "civilian_labor_employed_agriculture",
    "civilian_labor_employed_non_ag",
    "civilian_labor_unemployed_number",
    "civilian_labor_unemployed_percent",
    "not_in_labor"
  ),
  col_types = c("text",
                "numeric",
                "numeric",
                "numeric",
                "numeric",
                "numeric",
                "numeric",
                "numeric",
                "numeric",
                "numeric",
                "numeric")
)

# adjust percentages to be true proportions
# i.e. divide by 100
labor_usa <- labor_usa %>%
  mutate(`civilian_labor_percent` = `civilian_labor_percent` / 100,
         `civilian_labor_employed_percent` = `civilian_labor_employed_percent` / 100,
         `civilian_labor_unemployed_percent` = `civilian_labor_unemployed_percent` / 100)

write_rds(labor_usa, "data/tidy/labor_usa_1949-2019.rds")

labor_usa %>%
  select(year, civilian_labor_unemployed_percent) %>%
  tail()

# Arizona level ----
# source https://www.bls.gov/lau/rdscnp16.htm
# download 
curl_download(url = "https://www.bls.gov/lau/staadata.txt", 
              destfile = "data/raw/us_bureau_of_labor_states.txt",
              quiet = FALSE)

# read data
# az only rows 118-161
labor_az <- read_delim("data/raw/us_bureau_of_labor_states.txt",
                                   delim = " ",
                                   col_names = c(
                                     "year",
                                     "skip",
                                     "civilian_noninstitutional_population",
                                     "civilian_labor_total",
                                     "civilian_labor_percent",
                                     "civilian_labor_employed_total",
                                     "civilian_labor_employed_percent",
                                     "civilian_labor_unemployed_number",
                                     "civilian_labor_unemployed_percent"
                                   ),
                                   skip = 117,
                           trim_ws = TRUE,
                           n_max = 44
                                   )

labor_az <- labor_az %>%
  select(!(skip)) %>%
  mutate(`civilian_labor_percent` = `civilian_labor_percent` / 100,
         `civilian_labor_employed_percent` = `civilian_labor_employed_percent` / 100,
         `civilian_labor_unemployed_percent` = `civilian_labor_unemployed_percent` / 100)
  
write_rds(labor_az, "data/tidy/labor_az_1976-2019.rds")

labor_az %>% 
  select(year,
         civilian_labor_unemployed_percent) %>%
  tail()


# County level
# download 
curl_download(url = "https://www.bls.gov/web/metro/laucntycur14.txt",
              destfile = "data/raw/us_bureau_of_labor_by_county.txt",
              quiet = FALSE
              )

# read data 
labor <- read_delim("data/raw/us_bureau_of_labor_by_county.txt",
           delim = "|",
           col_names = c("LAUS Area Code",
                         "State FIPS Code",
                         "County FIPS Code",
                         "Area Title",
                         "Period",
                         "Civilian Labor Force",
                         "Employed",
                         "Unemployed Level",
                         "Unemployed Rate"),
           col_types = cols("LAUS Area Code" = col_character(),
                              "State FIPS Code" = col_character(),
                              "County FIPS Code" = col_character(),
                              "Area Title" = col_character(),
                              "Period" = col_character(),
                              "Civilian Labor Force" = col_number(),
                              "Employed" = col_number(),
                              "Unemployed Level" = col_number(),
                              "Unemployed Rate" = col_number()),
           na = c("", "NA"),
           trim_ws = TRUE,
           skip = 6,
           n_max = 45066
           )

unique(labor$Period)

# filter to AZ catchment counties
# calculate rate
labor %>%
  filter(`State FIPS Code` == "04",
         Period == "Jun-20",
         `County FIPS Code` %in% c("003",
                                   "019",
                                   "021",
                                   "023",
                                   "027")) %>%
  summarise(`Unemployed Level` = sum(`Unemployed Level`),
            `Civilian Labor Force` = sum(`Civilian Labor Force`)) %>%
  mutate(rate = `Unemployed Level` / `Civilian Labor Force`)

# show rate for each county
labor %>%
  filter(`State FIPS Code` == "04",
         Period == "Jun-20",
         `County FIPS Code` %in% c("003",
                                   "019",
                                   "021",
                                   "023",
                                   "027")) %>%
  select("Area Title",
         Period,
         "Unemployed Rate")


# most recent monthly data for AZ
# read data
labor_az_monthly <- read_excel(
  "data/raw/us_bureau_of_labor_az_SeriesReport-20200908192755_c65033.xlsx",
  skip = 10,
  n_max = 11,
  col_names = TRUE)

labor_az_monthly

labor_az_monthly[11,]


# most recent monthly data for usa 
# read data 
labor_usa_monthly <- read_excel(
  "data/raw/us_bureau_of_labor_usa_SeriesReport-20200908193628_3819c3.xlsx",
  skip = 11,
  n_max = 12,
  col_names = TRUE)

labor_usa_monthly

labor_usa_monthly[11,]
