# U.S. Bureau of Labor Statistics
# Determining Unemployment Rate 

# set up
# packages 

library(here)
library(tidyverse)
library(curl)

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

# filter to AZ

labor %>%
  filter(`State FIPS Code` == "04",
         Period == "Jun-20(p)",
         `County FIPS Code` %in% c("003",
                                   "019",
                                   "021",
                                   "023",
                                   "027")) %>%
  summarise(`Unemployed Level` = sum(`Unemployed Level`),
            `Civilian Labor Force` = sum(`Civilian Labor Force`)) %>%
  mutate(rate = `Unemployed Level` / `Civilian Labor Force`)
