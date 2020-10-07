# setup ----
library(here)
library(tidycensus)
library(tidyverse)
# library(dataMaid)
# library(tigris)

# for reference
# load variable tables to find variables of interest

var_acs_2018_acs5 <- load_variables(2018, "acs5", cache = TRUE)
var_acs_2018_acs5_profile <- load_variables(2018, "acs5/profile", cache = TRUE)
var_acs_2018_acs5_subject <- load_variables(2018, "acs5/subject", cache = TRUE)
var_acs_2018_acs1 <- load_variables(2018, "acs1", cache = TRUE)

# write_csv(var_acs_2018_acs5, "data/tidy/ACS5_2018_variables.csv")

# function that removes clutter from each county name
# for example change from Apache County, Arizona to Apache
trim_county_name <- function(x ){
  x %>%
    mutate(NAME = str_replace(x$NAME, " County, Arizona", ""))
}

# median age 
# median age usa ----

acs5_median_age_usa <- get_acs(
  geography = "us",
  variables = c("median_age" = "B01002_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_median_age_usa

# median age az ----

acs5_median_age_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c("median_age" = "B01002_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_median_age_az

# median age catchment ----
# all az counties 

acs5_median_age_az_county <- get_acs(
  geography = "county",
  variables = c(
    "median_age" = "B01002_001"
  ),
  cache_table = TRUE,
  year = 2018,
  state = "az",
  survey = "acs5"
)

acs5_median_age_az_county <- acs5_median_age_az_county %>%
  trim_county_name()

acs5_median_age_az_county

# median age data table
table_median_age <- bind_rows(acs5_median_age_usa, 
          acs5_median_age_az,
          acs5_median_age_az_county) %>%
  #select(GEOID, NAME, variable, estimate) %>%
  mutate(variable = "median age",
    race = "all",
         sex = "all",
         year = "2014-2018",
         source = "US Census: ACS5")

table_median_age

# catchment counties only 
# function 
subset_catchment <- function(x){
  counties <- c(
    "Cochise",
    "Pima",
    "Pinal",
    "Santa Cruz",
    "Yuma"
  )
  x %>%
    filter(x$NAME %in% counties)
}

acs5_median_age_az_county %>%
  subset_catchment()

# range of catchment counties only 
acs5_median_age_az_county %>%
  subset_catchment %>%
  summarise(min(estimate),
            max(estimate))

# Median age catchment race ----
# MEDIAN AGE BY SEX (HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: People who are Hispanic or Latino
# Year: 2018
# Estimates: 5-Year
# Table ID: B01002I
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

median_age <- get_acs(
  geography = "county",
  state = "az",
  variables = c("median_age_white" = "B01002H_001",
                "median_age_hisp" = "B01002I_001",
                "median_age_ai" = "B01002C_001",
                "median_age_black" = "B01002B_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

median_age <- median_age %>% trim_county_name()

glimpse(median_age)

# function to generate range of median age values for each race
get_range_of_median_age <- function(x, race){
  x %>%
    filter(variable == race) %>%
    summarise(min(estimate),
              max(estimate))
}

# median age for WHITE ALONE, NOT HISPANIC OR LATINO in catchment
median_age %>%
  subset_catchment() %>%
  get_range_of_median_age("median_age_white")

# median age for BLACK OR AFRICAN AMERICAN ALONE in catchment
median_age %>%
  subset_catchment() %>%
  get_range_of_median_age("median_age_black")

# median age for HISPANIC OR LATINO in catchment
median_age %>%
  subset_catchment() %>%
  get_range_of_median_age("median_age_hisp")

# median age for AMERICAN INDIAN AND ALASKA NATIVE ALONE in catchment
median_age %>%
  subset_catchment() %>%
  get_range_of_median_age("median_age_ai")

# assemble table for median age including races
median_age <- median_age %>%
  mutate(race = case_when(
    variable == "median_age_white" ~ "WHITE ALONE, NOT HISPANIC OR LATINO",
    variable == "median_age_ai" ~ "AMERICAN INDIAN AND ALASKA NATIVE ALONE",
    variable == "median_age_hisp" ~ "HISPANIC OR LATINO",
    variable == "median_age_black" ~ "BLACK OR AFRICAN AMERICAN ALONE",
    TRUE ~ "NA"
  ),
  sex = "all",
  year = "2014-2018",
  source = "US Census: ACS5",
  variable = "median age")

glimpse(median_age)

# assemble table for median age for all known values 
table_median_age <- bind_rows(table_median_age, median_age)
glimpse(table_median_age)
table_median_age

# save 
write_rds(table_median_age, "data/tidy/acs5_median_age.rds")

# age 65 and over ----
# POPULATION 65 YEARS AND OVER IN THE UNITED STATES
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0103
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

# USA
# age

acs5_age_65_usa <- get_acs(
  geography = "us",
  variables = c("65_and_over" = "S0101_C01_030",
                "total" = "S0101_C01_001"),
  year = 2018,
  survey = "acs5")

acs5_age_65_usa

tidy_age_65 <- function(x){
  x %>%
    select(!(moe)) %>%
    spread(key = variable,
           value = estimate) %>%
    mutate(prop = `65_and_over` / total) %>%
    select(GEOID,
           NAME, 
           estimate = prop)
}

age_65_usa <- acs5_age_65_usa %>%
  tidy_age_65

age_65_usa

# AZ
acs5_age_65_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c("65_and_over" = "S0101_C01_030",
                "total" = "S0101_C01_001"),
  year = 2018,
  survey = "acs5")

acs5_age_65_az

age_65_az <- acs5_age_65_az %>%
  tidy_age_65()

age_65_az

# Catchment counties

acs5_age_65_catchment <- get_acs(
  geography = "county",
  state = "az",
  variables = c("65_and_over" = "S0101_C01_030",
                "total" = "S0101_C01_001"),
  year = 2018,
  survey = "acs5")

age_65_catchment <- acs5_age_65_catchment %>%
  trim_county_name() %>%
  subset_catchment() %>%
  tidy_age_65()

age_65_catchment

# catchment total
age_65_catchment_sum <- acs5_age_65_catchment %>%
trim_county_name() %>%
  subset_catchment() %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate) %>%
  summarise(`65_and_over` = sum(`65_and_over`),
            total = sum(total)) %>%
  mutate(prop = `65_and_over` / total,
         GEOID = "NA",
         NAME = "Catchment") %>%
  select(GEOID,
         NAME,
         estimate = prop)

age_65_catchment_sum

table_acs5_age65 <- bind_rows(age_65_usa,
          age_65_az,
          age_65_catchment,
          age_65_catchment_sum) %>%
  mutate(variable = "65 years and over",
         race = "all",
         sex = "all",
         year = "2014-2018",
         source = "US Census: ACS5")

table_acs5_age65

# catchment counties race
# WHITE ALONE, NOT HISPANIC OR LATINO
acs5_age_65_catchment_race_white <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "white_total" = "B01001H_001",
    "white_male_65-74" = "B01001H_014",
    "white_male_75-84" = "B01001H_015",
    "white_male_85" = "B01001H_016",
    "white_female_65-74" = "B01001H_029",
    "white_female_75-84" = "B01001H_030",
    "white_female_85" = "B01001H_031"
  ),
  year = 2018,
  survey = "acs5")

acs5_age_65_catchment_race_white

age_65_catchment_race_white_total <- acs5_age_65_catchment_race_white %>%
  trim_county_name() %>%
  subset_catchment() %>%
  filter(variable == "white_total") %>%
  summarize(white_total = (sum(estimate)))

age_65_catchment_race_white_total

acs5_age_65_catchment_race_white %>%
  trim_county_name() %>%
  subset_catchment() %>%
  filter(variable != "white_total") %>%
  summarize(white_65 = (sum(estimate))) %>%
  mutate(prop = white_65 /  age_65_catchment_race_white_total,
         variable = "65 years and over",
         race = "all",
         sex = "all",
         year = "2014-2018",
         source = "US Census: ACS5",
         GEOID = "NA",
         NAME = "Catchment") %>%
  select(GEOID,
         NAME,
         variable,
         estimate = prop,
         race,
         sex,
         year,
         source)

# HISPANIC OR LATINO
age_65_catchment_race_hisp <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "hisp_total" = "B01001I_001",
    "hisp_male_65-74" = "B01001I_014",
    "hisp_male_75-84" = "B01001I_015",
    "hisp_male_85" = "B01001I_016",
    "hisp_female_65-74" = "B01001I_029",
    "hisp_female_75-84" = "B01001I_030",
    "hisp_female_85" = "B01001I_031"
  ),
  year = 2018,
  survey = "acs5")

age_65_catchment_race_hisp_total <- age_65_catchment_race_hisp %>%
  mutate(NAME = str_replace(age_65_catchment_race_hisp$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "hisp_total") %>%
  summarize(hisp_total = (sum(estimate)))

age_65_catchment_race_hisp %>%
  mutate(NAME = str_replace(age_65_catchment_race_hisp$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable != "hisp_total") %>%
  summarize(hisp_65 = (sum(estimate))) %>%
  mutate(prop = hisp_65 /  age_65_catchment_race_hisp_total)

# AMERICAN INDIAN AND ALASKA NATIVE ALONE
age_65_catchment_race_ai <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "ai_total" = "B01001C_001",
    "ai_male_65-74" = "B01001C_014",
    "ai_male_75-84" = "B01001C_015",
    "ai_male_85" = "B01001C_016",
    "ai_female_65-74" = "B01001C_029",
    "ai_female_75-84" = "B01001C_030",
    "ai_female_85" = "B01001C_031"
  ),
  year = 2018,
  survey = "acs5")

age_65_catchment_race_ai_total <- age_65_catchment_race_ai %>%
  mutate(NAME = str_replace(age_65_catchment_race_ai$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "ai_total") %>%
  summarize(ai_total = (sum(estimate)))

age_65_catchment_race_ai %>%
  mutate(NAME = str_replace(age_65_catchment_race_ai$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable != "ai_total") %>%
  summarize(ai_65 = (sum(estimate))) %>%
  mutate(prop = ai_65 /  age_65_catchment_race_ai_total)


# load tables ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 1-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

acs5_age_sex <- get_acs(
  geography = "county",
  table = "S0101",
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

# use stringr to clean up county name
# and save
acs5_age_sex <- acs5_age_sex %>%
  mutate(NAME = str_replace(acs5_age_sex$NAME, " County, Arizona", ""))

# save to file
write_rds(acs5_age_sex, "data/tidy/acs5_2018_age_sex.rds")

# S0101_C01_030

# population age USA ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_age_USA <- get_acs(
  geography = "us",
  variable = c("under_18" = "S0101_C01_022",
    "age_group_65" = "S0101_C01_030",
               "Total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

write_rds(acs5_age_USA, "data/tidy/acs5_population_age_18_65.rds")

acs5_age_USA <- acs5_age_USA %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate)

acs5_age_USA %>%
  mutate(prop_65 = round(age_group_65 / Total, digits = 3),
         prop_18 = round(under_18 / Total, digits = 3))

acs5_age_USA %>%
  filter(variable != "Total") %>%
  ggplot(mapping = aes(x = variable, y = estimate)) +
  geom_bar(stat = "identity", fill = "variable")


# population age AZ ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_age_AZ <- get_acs(
  geography = "state",
  state = "az",
  variable = c("under_18" = "S0101_C01_022",
               "age_group_65" = "S0101_C01_030",
               "Total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_AZ <- acs5_age_AZ %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate)

acs5_age_AZ %>%
  mutate(prop_65 = round(age_group_65 / Total, digits = 3),
         prop_18 = round(under_18 / Total, digits =3))

# population age UAZCC Catchment ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_age_catch <- get_acs(
  geography = "county",
  state = "az",
  variable = c("under_18" = "S0101_C01_022",
               "age_group_65" = "S0101_C01_030",
               "Total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_catch <- acs5_age_catch %>%
  mutate(NAME = str_replace(acs5_age_catch$NAME, " County, Arizona", "")) %>%
  select(!(moe)) %>%
  filter(NAME %in% counties)

acs5_population_age <- full_join(acs5_age_USA, acs5_age_AZ)

acs5_population_age <- full_join(acs5_population_age, acs5_age_catch)

write_rds(acs5_population_age, "data/tidy/acs5_population_age_18_65.rds")

# plot

acs5_population_age %>%
  filter(variable != "Total") %>%
  ggplot(mapping = aes(x = NAME, y = estimate, fill = variable)) +
  geom_bar(position = "fill", stat = "identity")


acs5_age_catch <- acs5_age_catch %>%
  mutate(NAME = str_replace(acs5_age_catch$NAME, " County, Arizona", "")) %>%
  select(!(moe)) %>%
  filter(NAME %in% counties) %>%
  spread(key = variable,
         value = estimate)

acs5_age_catch %>%
  summarise(under_18 = sum(under_18),
    age_group_65 = sum(age_group_65),
            total = sum(Total)) %>%
  mutate(prop_18 = round(under_18 / total, digits =3),
         prop_65 = round(age_group_65 / total, digits = 3))

# filter to southern az catchment
acs5_age_sex_catch <- acs5_age_sex %>%
  filter(NAME %in% counties)

# from AGE AND SEX
# total population for each county
# group by county
# filter to variable "S0101_C01_001" Estimate!!Total!!Total population
acs5_age_sex_catch %>%
  group_by(NAME) %>%
  filter(variable == "S0101_C01_001")

# from age and sex
# filter to age 60 and older
# S0101_C01_028
# Estimate!!Total!!Total population!!SELECTED AGE CATEGORIES!!60 years and over
acs5_age_sex_catch %>%
  group_by(NAME) %>%
  filter(variable == "S0101_C01_028") # AGE CATEGORIES!!60 years and over

# show age 60 and older as proportion of total population
# first filter to age 60 and older only
# save
acs5_age_sex_catch_60 <- acs5_age_sex_catch %>%
  group_by(NAME) %>%
  filter(variable == "S0101_C01_028") %>%
  mutate("60+" = estimate)

# then filter to total
# save
acs5_age_sex_catch_total <- acs5_age_sex_catch %>%
  group_by(NAME) %>%
  filter(variable == "S0101_C01_001") %>%
  mutate(total = estimate)

# last combine
age_sex_profiles <- inner_join(age_sex_60, age_sex_total, by = c("GEOID", "NAME"))

# select and rename columns
age_sex_profiles <- age_sex_profiles %>%
  select(GEOID, NAME, "60+", total) %>%
  mutate("prop_60+" = round(`60+` / total, digits = 2))

# testing
# in progress
age_sex_subject <- get_acs(
  geography = "county",
  variables = c(
    median_age = "S0101_C01_032",
    total = "S0101_C01_001",
    "age_60+" = "S0101_C01_028",
    male_median_age = "S0101_C04_032",
    male_total = "S0101_C04_001",
    "male_age_60+" = "S0101_C04_028",
    female_median_age = "S0101_C05_032",
    female_total = "S0101_C05_001",
    "female_age_60+" = "S0101_C05_028"
  ),
  cache_table = TRUE,
  year = 2018,
  state = "AZ"
)
age_sex_subject %>%
  filter(variable == "male_total")

# end test

# for race ----
# B02001

# read data from acs
# RACE
race <- get_acs(
  geography = "county",
  table = "B02001",
  state = "AZ",
  year = 2018,
  cache_table = TRUE
)


# total population by race for each county
# group by county then race
race

# use stringr to clean up county name
# and save
race <- race %>%
  mutate(NAME = str_replace(race$NAME, " County, Arizona", ""))

# filter to southern az catchment
race <- race %>%
  filter(NAME %in% counties)

# Census race has five options
# "White alone",
# "Black or African American alone",
# "Asian alone",
# "Native Hawaiian and Other Pacific Islander alone",
# "Some other race alone"

# save race variables to value
var_race <- c(
  "B02001_001", # total including all races
  "B02001_002", # "White alone"
  "B02001_003", # "Black or African American alone",
  "B02001_004", # "Asian alone"
  "B02001_005"
) # "Native Hawaiian and Other Pacific Islander alone",
# "B02001_006") # # "Some other race alone"

# use race variables to filter
race <- race %>%
  filter(variable %in% var_race)

# recode variables from code to label
race$variable <- recode(race$variable,
  "B02001_001" = "Total",
  "B02001_002" = "White",
  "B02001_003" = "Black",
  "B02001_004" = "Asian",
  "B02001_005" = "Native Hawaiian"
)
# "B02001_006" = "Other")

# calculate "other" and add to table
# sum named race as race_sub
race_subtotal <- race %>%
  group_by(NAME) %>%
  filter(variable != "Total") %>%
  summarize(race_sub = sum(estimate))

# generate total population for each county
race_total <- race %>%
  group_by(NAME) %>%
  filter(variable == "Total") %>%
  summarize(Total = sum(estimate))

# combine total and subtotal to calculate "other"
race_table <- full_join(race_subtotal, race_total)

# calculate "other"
race_other <- race_table %>%
  mutate(other = Total - race_sub) %>%
  select(NAME, "Other" = other) %>%
  gather(Other,
    key = "variable",
    value = "estimate"
  )

# add other variable back to original table
race <- full_join(race, race_other)

# convert race variable to factor
race$variable <- as_factor(race$variable)

# rearrange table in order by county then by race variable
race <- race %>%
  arrange(NAME, variable)

# save data
write_rds(race, "data/tidy/acs5_2018_race.rds")

# spread table to put each race on it's own column and county it's own row
race %>%
  select(NAME, variable, estimate) %>%
  spread(variable, estimate) %>%
  kable()

# table of proportions by race
race %>%
  select(NAME, variable, estimate) %>%
  spread(variable, estimate) %>%
  mutate(
    White_prop = round(White / Total, digits = 2),
    Black_prop = round(Black / Total, digits = 2),
    Asian_prop = round(Asian / Total, digits = 2),
    NH_prop = round(`Native Hawaiian` / Total, digits = 2),
    Other_prop = round(Other / Total, digits = 2)
  ) %>%
  select(NAME, White_prop, Black_prop, Asian_prop, NH_prop, Other_prop) %>%
  kable()

# collapse by summarizing
race %>%
  group_by(NAME) %>%
  summarize()


race_total <- race %>%
  group_by(NAME) %>%
  filter(variable == "Total")

race %>%
  group_by(NAME) %>%
  filter(variable != "Total") %>%
  summarize(total = sum(estimate))

# Pima County ----

# Total Population ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 1-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

az_population_total <- get_acs(
  geography = "county",
  table = "B01003",
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs1"
)

pima_population_total_acs5 <- az_population_total %>% filter(NAME == "Pima County, Arizona")
pima_population_total_acs5


# Race ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 1-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

az_population_race <- get_acs(
  geography = "county",
  variables = c(
    "White alone" = "B02001_002",
    "Black or African American alone" = "B02001_003",
    "American Indian and Alaska Native alone" = "B02001_004",
    "Asian alone" = "B02001_005",
    "Native Hawaiian and Other Pacific Islander alone" = "B02001_006",
    "Some other race alone" = "B02001_007",
    "Two or more races" = "B02001_008",
    "Two races including Some other race" = "B02001_009",
    "Two races excluding Some other race, and three or more races" = "B02001_010"
  ),
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

write_rds(az_population_race, "data/tidy/county_by_race.rds")


az_population_race %>%
  filter(NAME == "Pima County, Arizona") %>%
  mutate(prop = round(estimate / sum(estimate), digits = 3))

az_population_race %>%
  filter(NAME == "Pima County, Arizona") %>%
  summarise(total = sum(estimate))


# Population Race Proportions ----
# Sex by Age
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 1-Year
# Table ID: B01001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

az_population_sex_age <- get_acs(
  geography = "county",
  variables = c(
    "WHITE ALONE" = "B01001A_001",
    "BLACK OR AFRICAN AMERICAN ALONE" = "B01001B_001",
    "AMERICAN INDIAN AND ALASKA NATIVE ALONE" = "B01001C_001",
    "ASIAN ALONE" = "B01001D_001",
    "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE" = "B01001E_001",
    "SOME OTHER RACE ALONE" = "B01001F_001",
    "TWO OR MORE RACES" = "B01001G_001",
    "WHITE ALONE, NOT HISPANIC OR LATINO" = "B01001H_001",
    "HISPANIC OR LATINO" = "B01001I_001"
  ),
  state = "AZ",
  year = 2018,
  cache_table = TRUE
)

write_rds(az_population_sex_age, "data/tidy/county_by_sex_age.rds")

az_population_sex_age %>%
  filter(NAME == "Pima County, Arizona") %>%
  mutate(prop = round(estimate / sum(estimate), digits = 3)) %>%
  ggplot(mapping = aes(x = prop, y = reorder(variable, prop))) +
  geom_col() +
  geom_label(aes(label = prop), nudge_x = .075) +
  labs(
    title = "Population Proportion by Race",
    subtitle = "Pima County, AZ",
    x = "Proportion",
    y = "Race / Ethnicity",
    caption = "Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates"
  )

# Population Race by Age ----

az_population_race <- get_acs(
  geography = "county",
  variables = c(
    "WHITE ALONE" = "B01001A_001",
    "BLACK OR AFRICAN AMERICAN ALONE" = "B01001B_001",
    "AMERICAN INDIAN AND ALASKA NATIVE ALONE" = "B01001C_001",
    "ASIAN ALONE" = "B01001D_001",
    "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE" = "B01001E_001",
    "SOME OTHER RACE ALONE" = "B01001F_001",
    "TWO OR MORE RACES" = "B01001G_001",
    "WHITE ALONE, NOT HISPANIC OR LATINO" = "B01001H_001",
    "HISPANIC OR LATINO" = "B01001I_001"
  ),
  state = "AZ",
  year = 2018,
  cache_table = TRUE
)


# attribute table ----
# total population usa ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_total_usa <- get_acs(
  geography = "us",
  table = "B01003",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_total_usa

# total population az ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_total_az <- get_acs(
  geography = "state",
  state = "AZ",
  table = "B01003",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_total_az

# total population catchment ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_total_az_county <- get_acs(
  geography = "county",
  table = "B01003",
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_total_az_county %>%
  mutate(NAME = str_replace(acs5_population_total_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  summarise(total = sum(estimate))

# total population County ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_population_total_az_county %>%
  mutate(NAME = str_replace(acs5_population_total_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties)

# sex female usa ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_age_sex_usa <- get_acs(
  geography = "us",
  variables = c("Female" = "S0101_C05_001",
                "total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_sex_usa %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  mutate(prop = Female/total)

# sex female az ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_age_sex_az <- get_acs(
  geography = "state",
  state = "AZ",
  variables = c("Female" = "S0101_C05_001",
                "total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_sex_az %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  mutate(prop = Female/total)

# sex female catchment ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_age_sex_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Female" = "S0101_C05_001",
                "total" = "S0101_C01_001"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# for catchment
acs5_age_sex_catch %>%
  mutate(NAME = str_replace(acs5_age_sex_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  summarise(Female = (sum(Female)),
            total = sum(total)) %>%
  mutate(prop = Female/total)

# for each county
acs5_age_sex_catch %>%
  mutate(NAME = str_replace(acs5_age_sex_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  mutate(prop = Female/total)

# population by sex female for catchment each race ----

acs5_sex_catch_race <- get_acs(geography = "county",
                               state = "az",
                               variables = c(
                                 "white" = "B01001H_017",
                                 "hispanic" = "B01001I_017",
                                 "amer_indian" = "B01001C_017",
                                 "white_total" = "B01001H_001",
                                 "hisp_total" = "B01001I_001",
                                 "ai_total" = "B01001C_001"
                               ),
                               cache_table = TRUE,
                               year = 2018,
                               survey = "acs5"
                                 )

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_sex_catch_race %>%
  mutate(NAME = str_replace(acs5_sex_catch_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  summarise(white = sum(white),
            hispanic = sum(hispanic),
            amer_indian = sum(amer_indian),
            white_total = sum(white_total),
            hisp_total = sum(hisp_total),
            ai_total = sum(ai_total)) %>%
  mutate(prop_white = white / white_total,
         prop_hisp = hispanic / hisp_total,
         prop_ai = amer_indian / ai_total) %>%
  select(prop_white,
         prop_hisp,
         prop_ai)

# population by sex for each county ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_age_sex <- get_acs(
  geography = "county",
  variables = c(
    "Female" = "S0101_C05_001",
    "Total" = "S0101_C01_001"
  ),
  state = "AZ",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_age_sex %>%
  mutate(NAME = str_replace(acs5_age_sex$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  mutate(prop = Female / Total)

# population USA hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_hispanic_usa <- get_acs(
  geography = "us",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071P",
                "White alone Not Hispanic or Latino" = "DP05_0077P",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078P",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079P"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_hispanic_usa

# population AZ hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_hispanic_az <- get_acs(
  geography = "state",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071P",
                "White alone Not Hispanic or Latino" = "DP05_0077P",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078P",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079P"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_hispanic_az

# population catchment hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_hispanic_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071",
                "White alone Not Hispanic or Latino" = "DP05_0077",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079",
                "Total" = "DP05_0070"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

val_catch_hisp <- acs5_hispanic_catch %>%
  mutate(NAME = str_replace(acs5_hispanic_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "Total") %>%
  summarise(estimate = sum(estimate))

acs5_hispanic_catch %>%
  mutate(NAME = str_replace(acs5_hispanic_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable != "Total") %>%
  group_by(variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = val_catch_hisp,
         prop = estimate / total)

# population race ethnicity for each county ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_catch_county_race <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071",
                "White alone Not Hispanic or Latino" = "DP05_0077",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079",
                "Total" = "DP05_0070"),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

val_catch_race <- acs5_catch_county_race %>%
  mutate(NAME = str_replace(acs5_catch_county_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "Total") %>%
  summarise(estimate = sum(estimate))

race_ethnicity <- acs5_catch_county_race %>%
  mutate(NAME = str_replace(acs5_catch_county_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable != "Total") %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate)

names(race_ethnicity)

acs5_catch_county_race %>%
  mutate(NAME = str_replace(acs5_catch_county_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  select(NAME,
         Total,
         AI = 'American Indian and Alaska Native alone Not Hispanic or Latino',
         Black = "Black or African American alone Not Hispanic or Latino",
         Hisp = "Hispanic or Latino (of any race)",
         White = "White alone Not Hispanic or Latino") %>%
  mutate(AI_prop = AI / Total,
         black_prop = Black / Total,
         hisp_prop = Hisp / Total,
         white_prop = White / Total) %>%
  select(NAME, AI_prop, black_prop, hisp_prop, white_prop)

# population USA race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_usa <- get_acs(
  geography = "us",
  table = "B02001",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_usa %>%
  filter(variable == "B02001_002")

# population USA race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_usa %>%
  filter(variable == "B02001_004")

# population USA race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_usa %>%
  filter(variable == "B02001_003")

# population AZ race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az <- get_acs(
  geography = "state",
  state = "AZ",
  table = "B02001",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_az %>%
  filter(variable == "B02001_002")

# population AZ race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az %>%
  filter(variable == "B02001_004")

# population AZ race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az %>%
  filter(variable == "B02001_003")

# population catchment race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az_county <- get_acs(
  geography = "county",
  state = "AZ",
  table = "B02001",
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_002") %>%
  summarise(total = sum(estimate))

# population catchment race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_004") %>%
  summarise(total = sum(estimate))

# population catchment race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_003") %>%
  summarise(total = sum(estimate))

# population urban and rural usa ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2010 Census.

population_rural_usa <- 27684999 / 131704730
population_rural_usa

# from table P2:
population_rural_usa <- 59492267 / 308745538
population_rural_usa

# population urban and rural az ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

population_rural_az <- 329022 / 2844526
population_rural_az

# from table P2:
population_rural_az <- 651358 / 6392017
population_rural_az

# population urban and rural catchment ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

population_rural_catch <- tibble(
  area = c(
    "AZ",
    "Cochise",
    "Pima",
    "Pinal",
    "Santa Cruz",
    "Yuma"
  ),
  estimate = c(
    329022,
    22851,
    33543,
    30511,
    6446,
    NA
  ),
  total = c(
    "AZ",
    "Cochise",
    "Pima",
    "Pinal",
    "Santa Cruz",
    "Yuma"
  )
)

population_rural_catch %>%
  filter(area != "AZ") %>%
  drop_na() %>%
  summarise(total = sum(estimate))

population_rural_catch %>%
  mutate(total = 329022,
         prop = estimate / 329022)

# from table P2:
population_rural_catch <- tibble(
  area = c(
    "AZ",
    "Cochise",
    "Pima",
    "Pinal",
    "Santa Cruz",
    "Yuma"
  ),
  estimate = c(
    651358,
    47680,
    73750,
    82311,
    12748,
    20416
  ),
  total = c(
    6392017,
    131346,
    980263,
    375770,
    47420,
    195751
  )
)

# for the catchment altogether
population_rural_catch %>%
  filter(area != "AZ") %>%
  drop_na() %>%
  summarise(estimate_total = sum(estimate),
            total = sum(total)) %>%
  mutate(estimate = estimate_total / total)

# for each catchment county
population_rural_catch %>%
  filter(area != "AZ") %>%
  mutate(prop = estimate / total)


# uninsured usa ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: CP03

# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_insurance <- tibble(
  area = c("United States",
           "AZ",
           "Cochise",
           "Pima",
           "Pinal",
           "Santa Cruz",
           "Yuma"),
  prop = c(0.094,
           0.109,
           0.082,
           0.099,
           0.088,
           0.116,
           0.133),
  denominator = c(317941631,
                  6838270,
                  116140,
                  998748,
                  393843,
                  46251,
                  201230))

acs5_population_insurance <- acs5_population_insurance %>%
  mutate(estimate = prop * denominator) %>%
  select(area, estimate, denominator, prop)

acs5_population_insurance %>%
  filter(area == "United States")

# uninsured az ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_insurance %>%
  filter(area == "AZ")

# uninsured catchment ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_insurance %>%
  filter(!(area == "AZ" | area == "United States")) %>%
  summarise(estimate = sum(estimate),
            denominator = sum(denominator)) %>%
  mutate(prop = estimate / denominator)

# uninsured county ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_insurance %>%
  filter(!(area == "AZ" | area == "United States")) %>%
  mutate(prop = estimate / denominator)

# uninsured catchment white ----
# HEALTH INSURANCE COVERAGE STATUS BY AGE (WHITE ALONE, NOT HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino civilian noninstitutionalized population
# Year: 2018
# Estimates: 1-Year
# Table ID: B27001H
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

acs1_population_insurance_white <- get_acs(geography = "county",
                                          state = "az",
                                          year = 2018,
                                          survey = "acs1",
                                          variables = c(
                                            "Total" = "B27001H_001",
                                            "age 6 under" = "B27001H_004",
                                            "age 6-18" = "B27001H_007",
                                            "age 19-25" = "B27001H_010",
                                            "age 26-34" = "B27001H_013",
                                            "age 35-44" = "B27001H_016",
                                            "age 45-54" = "B27001H_019",
                                            "age 55-64" = "B27001H_022",
                                            "age 65-74" = "B27001H_025",
                                            "age 75 over" = "B27001H_028"
                                          )
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# total denominator
acs1_population_insurance_white_den <- acs1_population_insurance_white %>%
  mutate(NAME = str_replace(acs1_population_insurance_white$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "Total") %>%
  summarise(estimate = sum(estimate))

# numerator
acs1_population_insurance_white %>%
  mutate(NAME = str_replace(acs1_population_insurance_white$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable != "Total") %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = acs1_population_insurance_white_den,
         prop = estimate / total)

# population uninsured catchment hispanic ----
# HEALTH INSURANCE COVERAGE STATUS BY AGE (HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino civilian noninstitutionalized population
# Year: 2018
# Estimates: 1-Year
# Table ID: B27001I
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

acs1_population_insurance_hisp <- get_acs(geography = "county",
                                          state = "az",
                                          year = 2018,
                                          survey = "acs1",
                                          variables = c(
                                            "Total" = "B27001I_001",
                                            "age 6 under" = "B27001I_004",
                                            "age 6-18" = "B27001I_007",
                                            "age 19-25" = "B27001I_010",
                                            "age 26-34" = "B27001I_013",
                                            "age 35-44" = "B27001I_016",
                                            "age 45-54" = "B27001I_019",
                                            "age 55-64" = "B27001I_022",
                                            "age 65-74" = "B27001I_025",
                                            "age 75 over" = "B27001I_028"
                                          )
                                          )

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# total denominator
acs1_population_insurance_hisp_den <- acs1_population_insurance_hisp %>%
  mutate(NAME = str_replace(acs1_population_insurance_hisp$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "Total") %>%
  summarise(estimate = sum(estimate))

# numerator
acs1_population_insurance_hisp %>%
  mutate(NAME = str_replace(acs1_population_insurance_hisp$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable != "Total") %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = acs1_population_insurance_hisp_den,
         prop = estimate / total)

# population uninsured catchment american indian ----
# HEALTH INSURANCE COVERAGE STATUS BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino civilian noninstitutionalized population
# Year: 2018
# Estimates: 1-Year
# Table ID: B27001C
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

acs1_population_insurance_ai <- get_acs(geography = "county",
                                          state = "az",
                                          year = 2018,
                                          survey = "acs1",
                                          variables = c(
                                            "Total" = "B27001C_001",
                                            "age 6 under" = "B27001C_004",
                                            "age 6-18" = "B27001C_007",
                                            "age 19-25" = "B27001C_010",
                                            "age 26-34" = "B27001C_013",
                                            "age 35-44" = "B27001C_016",
                                            "age 45-54" = "B27001C_019",
                                            "age 55-64" = "B27001C_022",
                                            "age 65-74" = "B27001C_025",
                                            "age 75 over" = "B27001C_028"
                                          )
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# total denominator
acs1_population_insurance_ai_den <- acs1_population_insurance_ai %>%
  mutate(NAME = str_replace(acs1_population_insurance_ai$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "Total") %>%
  drop_na()%>%
  summarise(estimate = sum(estimate))

# numerator
acs1_population_insurance_ai %>%
  mutate(NAME = str_replace(acs1_population_insurance_ai$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable != "Total") %>%
  drop_na() %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = acs1_population_insurance_ai_den,
         prop = estimate / total)

# population uninsured catchment black ----
# HEALTH INSURANCE COVERAGE STATUS BY AGE (HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino civilian noninstitutionalized population
# Year: 2018
# Estimates: 1-Year
# Table ID: B27001I
#
# Source: U.S. Census Bureau, 2018 American Community Survey 1-Year Estimates

acs1_population_insurance_black <- get_acs(geography = "county",
                                        state = "az",
                                        year = 2018,
                                        survey = "acs1",
                                        variables = c(
                                          "Total" = "B27001B_001",
                                          "age 6 under" = "B27001B_004",
                                          "age 6-18" = "B27001B_007",
                                          "age 19-25" = "B27001B_010",
                                          "age 26-34" = "B27001B_013",
                                          "age 35-44" = "B27001B_016",
                                          "age 45-54" = "B27001B_019",
                                          "age 55-64" = "B27001B_022",
                                          "age 65-74" = "B27001B_025",
                                          "age 75 over" = "B27001B_028"
                                        )
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# total denominator
acs1_population_insurance_black_den <- acs1_population_insurance_black %>%
  mutate(NAME = str_replace(acs1_population_insurance_black$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "Total") %>%
  drop_na()%>%
  summarise(estimate = sum(estimate))

# numerator
acs1_population_insurance_black %>%
  mutate(NAME = str_replace(acs1_population_insurance_black$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable != "Total") %>%
  drop_na() %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = acs1_population_insurance_black_den,
         prop = estimate / total)

# educational attainment usa ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_edu_usa <- get_acs(
  geography = "us",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher" = "S1501_C01_014",
    "Some college, no degree" = "S1501_C01_010",
    "Bachelor's degree or higher" = "S1501_C01_015"
  ),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_edu_usa %>%
  mutate(prop = estimate / 218446071)

# educational attainment az ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_edu_az <- get_acs(
  geography = "state",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher" = "S1501_C01_014",
    "Some college, no degree" = "S1501_C01_010",
    "Bachelor's degree or higher" = "S1501_C01_015"
  ),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_edu_az %>%
  mutate(prop = estimate / 4633932)

# educational attainment catchment ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_edu_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher" = "S1501_C01_014",
    "Some college, no degree" = "S1501_C01_010",
    "Bachelor's degree or higher" = "S1501_C01_015"
  ),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

val_catch_edu_den <- acs5_population_edu_catch %>%
  mutate(NAME = str_replace(acs5_population_edu_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "Population 25 years and over") %>%
  summarise(estimate = sum(estimate))

# for catchment
acs5_population_edu_catch %>%
  mutate(NAME = str_replace(acs5_population_edu_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable != "Population 25 years and over") %>%
  group_by(variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(denominator = val_catch_edu_den,
         prop = estimate / denominator)

# for counties
acs5_population_edu_catch %>%
  mutate(NAME = str_replace(acs5_population_edu_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable, value = estimate) %>%
  select(NAME,
         HS = `High school graduate or higher`,
         some_college = `Some college, no degree`,
         bach = `Bachelor's degree or higher`,
         total = `Population 25 years and over`) %>%
  mutate(HS_prop = HS / total,
         some_college_prop = some_college / total,
         bach_prop = bach / total)

  group_by(variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(denominator = val_catch_edu_den,
         prop = estimate / denominator)

# education attainment catchment race ----
# high school graduate or equivalency

acs5_edu_catch_race <- get_acs(
  geography = "county",
  state = "az",
  cache_table = TRUE,
  year = 2018,
  survey = "acs5",
  variables = c(
    "white_total" = "S1501_C01_031",
    "white_hs" = "S1501_C01_032",
    "hisp_total" = "S1501_C01_052",
    "hisp_hs" = "S1501_C01_053",
    "ai_total" = "S1501_C01_037",
    "ai_hs" = "S1501_C01_038"
  )
)

acs5_edu_catch_race

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# white hs grad
acs5_edu_catch_race_white <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("white_total", "white_hs")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable != "white_total") %>%
  summarise(white_edu = sum(estimate))

# white total
acs5_edu_catch_race_white_total <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("white_total", "white_hs")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable == "white_total") %>%
  summarise(white_edu = sum(estimate))

# calculate proportion
acs5_edu_catch_race_white / acs5_edu_catch_race_white_total

# FLAG
# hisp hs grad
acs5_edu_catch_race_hisp <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("hisp_total", "hisp_hs")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable != "hisp_total") %>%
  summarise(hisp_edu = sum(estimate))

# hisp total
acs5_edu_catch_race_hisp_total <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("hisp_total", "hisp_hs")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable == "hisp_total") %>%
  summarise(hisp_edu = sum(estimate))

# calculate proportion
acs5_edu_catch_race_hisp / acs5_edu_catch_race_hisp_total

# american indian hs grad
acs5_edu_catch_race_ai <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("ai_total", "ai_hs")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable != "ai_total") %>%
  summarise(ai_edu = sum(estimate))

# american indian total
acs5_edu_catch_race_ai_total <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("ai_total", "ai_hs")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable == "ai_total") %>%
  summarise(ai_edu = sum(estimate))

# calculate proportion
acs5_edu_catch_race_ai / acs5_edu_catch_race_ai_total

# college graduate

acs5_edu_catch_race <- get_acs(
  geography = "county",
  state = "az",
  cache_table = TRUE,
  year = 2018,
  survey = "acs5",
  variables = c(
    "white_total" = "S1501_C01_031",
    "white_bach" = "S1501_C01_033",
    "hisp_total" = "S1501_C01_052",
    "hisp_bach" = "S1501_C01_054",
    "ai_total" = "S1501_C01_037",
    "ai_bach" = "S1501_C01_039"
  )
)

acs5_edu_catch_race

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# white hs grad
acs5_edu_catch_race_white <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("white_total", "white_bach")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable != "white_total") %>%
  summarise(white_edu = sum(estimate))

# white total
acs5_edu_catch_race_white_total <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("white_total", "white_bach")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable == "white_total") %>%
  summarise(white_edu = sum(estimate))

# calculate proportion
acs5_edu_catch_race_white / acs5_edu_catch_race_white_total

# hisp hs grad
#FLAG
acs5_edu_catch_race_hisp <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("hisp_total", "hisp_bach")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable != "hisp_total") %>%
  summarise(hisp_edu = sum(estimate))

# hisp total
acs5_edu_catch_race_hisp_total <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("hisp_total", "hisp_bach")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable == "hisp_total") %>%
  summarise(hisp_edu = sum(estimate))

# calculate proportion
acs5_edu_catch_race_hisp / acs5_edu_catch_race_hisp_total

# american indian hs grad
acs5_edu_catch_race_ai <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("ai_total", "ai_bach")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable != "ai_total") %>%
  summarise(ai_edu = sum(estimate))

# american indian total
acs5_edu_catch_race_ai_total <- acs5_edu_catch_race %>%
  mutate(NAME = str_replace(acs5_edu_catch_race$NAME, " County, Arizona", "")) %>%
  filter(variable %in% c("ai_total", "ai_bach")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  filter(variable == "ai_total") %>%
  summarise(ai_edu = sum(estimate))

# calculate proportion
acs5_edu_catch_race_ai / acs5_edu_catch_race_ai_total

# educational attainment county ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_population_edu_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher" = "S1501_C01_014",
    "Some college, no degree" = "S1501_C01_010",
    "Bachelor's degree or higher" = "S1501_C01_015"
  ),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

# population educational attainment catchment hispanic ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_population_edu_catch_hisp <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_052",
    "Bachelor's degree or higher" = "S1501_C01_054"
  ),
  year = 2018,
  cache_table = TRUE,
  survey = "acs5"
)

# for entire catchment
acs5_population_edu_catch_hisp %>%
  mutate(NAME = str_replace(acs5_population_edu_catch_hisp$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate) %>%
  summarise(college = sum(`Bachelor's degree or higher`),
            total = sum(`Population 25 years and over`)) %>%
  mutate(prop = college / total)

# for each county in catchment
acs5_population_edu_catch_hisp %>%
  mutate(NAME = str_replace(acs5_population_edu_catch_hisp$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate) %>%
  mutate(prop = `Bachelor's degree or higher` / `Population 25 years and over`)

# val_catch_edu_den <- acs5_population_edu_catch %>%
#   mutate(NAME = str_replace(acs5_population_edu_catch$NAME, " County, Arizona", "")) %>%
#   filter(NAME %in% counties) %>%
#   filter(variable == "Population 25 years and over") %>%
#   summarise(estimate = sum(estimate))

acs5_population_edu_catch %>%
  mutate(NAME = str_replace(acs5_population_edu_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  select(NAME,
         Total = "Population 25 years and over",
         "some_college" = "Some college, no degree",
         "high_school_grad" = "High school graduate or higher",
         "college_grad" = "Bachelor's degree or higher") %>%
  mutate(some_college_prop = some_college / Total,
         high_school_prop = high_school_grad / Total,
         college_grad_prop = college_grad / Total) %>%
  select(NAME, high_school_prop, some_college_prop, college_grad_prop)

# unemployment usa ----
# EMPLOYMENT STATUS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S2301
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_unemployment_usa <- get_acs(
  geography = "us",
  variables = c(
    "Rate" = "S2301_C04_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_unemployment_usa

# unemployment az ----
# EMPLOYMENT STATUS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S2301
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_unemployment_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c(
    "Rate" = "S2301_C04_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_unemployment_az

# unemployment catchment ----
# EMPLOYMENT STATUS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S2301
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_unemployment_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "Rate" = "S2301_C04_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_unemployment_catch %>%
  mutate(NAME = str_replace(acs5_unemployment_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  summarise(min(estimate),
            max(estimate))

# unemployment catchment race ----
# EMPLOYMENT STATUS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S2301
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_unemployment_catch_race <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "white" = "S2301_C04_020",
    "hisp" = "S2301_C04_019",
    "ai" = "S2301_C04_014"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_unemployment_catch_race %>%
  mutate(NAME = str_replace(acs5_unemployment_catch_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  group_by(variable) %>%
  summarise(min(estimate),
            max(estimate))

# population unemployment county ----
# EMPLOYMENT STATUS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S2301
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_unemployment_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "Rate" = "S2301_C04_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_unemployment_catch %>%
  mutate(NAME = str_replace(acs5_unemployment_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME:estimate) %>%
  mutate(estimate = estimate/100)

# Poverty USA ----
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_poverty_usa <- get_acs(
  geography = "us",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_poverty_usa %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(prop = 100*(number/total))

# Poverty AZ ----
# population unemployment az
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_poverty_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_poverty_az %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(prop = 100*(number/total))

# Poverty Catchment ----
# population unemployment catch
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_poverty_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_poverty_catch %>%
  mutate(NAME = str_replace(acs5_poverty_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  summarise(number = sum(number),
            total = sum(total)) %>%
  mutate(prop = 100*(number/total))

# Poverty County ----
# population unemployment catch
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_poverty_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_poverty_catch %>%
  mutate(NAME = str_replace(acs5_poverty_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  select(NAME, rate) %>%
  mutate(rate = rate/100)

# Poverty Catchment Hispanic ----

# S1701_C03_020

acs5_poverty_catch_race <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "percent_white" = "S1701_C03_021",
    "percent_hisp" = "S1701_C03_020",
    "percent_ai" = "S1701_C03_015",
    "percent_black" = "S1701_C03_014"
  ),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_poverty_catch_race %>%
  mutate(NAME = str_replace(acs5_poverty_catch_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "percent_white") %>%
  summarise(min(estimate),
            max(estimate))

acs5_poverty_catch_race %>%
  mutate(NAME = str_replace(acs5_poverty_catch_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "percent_hisp") %>%
  summarise(min(estimate),
            max(estimate))

acs5_poverty_catch_race %>%
  mutate(NAME = str_replace(acs5_poverty_catch_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "percent_ai") %>%
  summarise(min(estimate),
            max(estimate))

acs5_poverty_catch_race %>%
  mutate(NAME = str_replace(acs5_poverty_catch_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties,
         variable == "percent_black") %>%
  summarise(min(estimate),
            max(estimate))


# Food Insecurity AZ ----
# https://www.ers.usda.gov/data-products/food-environment-atlas/go-to-the-atlas/
# Household food insecurity
# 3 year average 2013-2015

food_insecurity <- tibble(
"area" = c("AZ"),
"rate" = c(.1920)
)

# Food Security in the United States: How Do States Compare?
# https://www.ers.usda.gov/topics/food-nutrition-assistance/food-security-in-the-us/interactive-charts-and-highlights/

# food insecurity 2016-2018
# 12.4%

# very low food insecurity 2016-2018
# 5.1%

# Population Foreign Born USA ----
# PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B05002
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_foreign_born_USA <- get_acs(
  geography = "us",
  variables = c("foreign_born" = "B05002_013",
                "total" = "B05002_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_foreign_born_USA %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  mutate(prop = foreign_born / total)

# Population Foreign Born AZ ----
# PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B05002
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_foreign_born_AZ <- get_acs(
  geography = "state",
  state = "az",
  variables = c("foreign_born" = "B05002_013",
                "total" = "B05002_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

acs5_foreign_born_AZ %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  mutate(prop = foreign_born / total)

# Population Foreign Born Catch ----
# PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2018
# Estimates: 5-Year
# Table ID: B05002
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_foreign_born_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c("foreign_born" = "B05002_013",
                "total" = "B05002_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_foreign_born_catch %>%
  mutate(NAME = str_replace(acs5_foreign_born_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  summarise(foreign_born = sum(foreign_born),
            total = sum(total)) %>%
  mutate(prop = foreign_born / total)

# Population Foreign Born Catch hispanic ----
# PLACE OF BIRTH (HISPANIC OR LATINO) IN THE UNITED STATES
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino population in the United States
# Year: 2018
# Estimates: 5-Year
# Table ID: B06004I
#
# Although the American Community Survey (ACS) produces population, demographic and housing unit estimates, it is the Census Bureau's Population Estimates Program that produces and disseminates the official estimates of the population for the nation, states, counties, cities, and towns and estimates of housing units for states and counties.
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_foreign_born_catch_hisp <- get_acs(
  geography = "county",
  state = "az",
  variables = c("foreign_born" = "B06004I_005",
                "total" = "B06004I_001"),
  cache_table = TRUE,
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_foreign_born_catch_hisp %>%
  mutate(NAME = str_replace(acs5_foreign_born_catch_hisp$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  summarise(foreign_born = sum(foreign_born),
            total = sum(total)) %>%
  mutate(prop = foreign_born / total)



# language other than english usa ----
# from table S1601
# language spoken at home: speak a language other than english

# 21.5%

# language other than english az ----
# from table S1601
# language spoken at home: speak a language other than english

# 27.2%


# language other than english catchment ----
# from table S1601
# language spoken at home: speak a language other than english

acs5_language_catch <- get_acs(  geography = "county",
                                 variables = c(
                                   "non_english" = "S1601_C01_003",
                                   "total" = "S1601_C01_001"
                                 ),
                                 cache_table = TRUE,
                                 year = 2018,
                                 state = "az",
                                 survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_language_catch  %>%
  mutate(NAME = str_replace(acs5_language_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate) %>%
  summarise(non_english = sum(non_english),
            total = sum(total)) %>%
  mutate(prop = non_english / total)

# language other than English Hispanic or Latino ----
# LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino population 5 years and over
# Year: 2018
# Estimates: 5-Year
# Table ID: B16006
#
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

acs5_language_hispanic_usa <- get_acs(
  geography = "us",
  variables = c(
    "total" = "B16006_001",
    "only_Eng" = "B16006_002"
  ),
  year = 2018,
  survey = "acs5"
)

# USA
acs5_language_hispanic_usa %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate) %>%
  mutate(prop_non_eng = 1- only_Eng / total)

# AZ
acs5_language_hispanic_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c(
    "total" = "B16006_001",
    "only_Eng" = "B16006_002"
  ),
  year = 2018,
  survey = "acs5"
)

acs5_language_hispanic_az %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate) %>%
  mutate(prop_non_eng = 1- only_Eng / total)

# catchment altogether
acs5_language_hispanic_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "total" = "B16006_001",
    "only_Eng" = "B16006_002"
  ),
  year = 2018,
  survey = "acs5"
)

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

acs5_language_hispanic_catch %>%
  mutate(NAME = str_replace(acs5_language_hispanic_catch$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate) %>%
  summarize(only_Eng = sum(only_Eng),
            total = sum(total)) %>%
  mutate(prop_non_eng = 1- only_Eng / total)


# educational attainment some college white and hispanic ----
# table C15002A
# SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (WHITE ALONE, NOT HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: White alone population 25 years and over
# Year: 2018
# Estimates: 5-Year
# Table ID: C15002H
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

some_college <- get_acs(geography = "county",
                        state = "az",
                        variables = c("total_white" = "C15002H_001",
                                      "some_college_white" = "C15002H_005",
                                      "total_hisp" = "C15002I_001",
                                      "some_college_hisp" = "C15002I_005",
                                      "total_ai" = "C15002C_001",
                                      "some_college_ai" = "C15002C_005"),
                        year = 2018,
                        survey = "acs5")

counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

some_college %>%
  mutate(NAME = str_replace(some_college$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate) %>%
  summarise(hisp_total = sum(total_hisp),
            white_total = sum(total_white),
            hisp_coll = sum(some_college_hisp),
            white_coll = sum(some_college_white),
            ai_coll = sum(some_college_ai),
            ai_total = sum(total_ai)) %>%
  mutate(hisp_prop_coll = hisp_coll / hisp_total,
         white_prop_coll = white_coll / white_total,
         ai_prop_coll = ai_coll / ai_total) %>%
  select(white_prop_coll, hisp_prop_coll, ai_prop_coll)

# median household income ----
# INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)
# Survey/Program: American Community Survey
# Year: 2018
# Estimates: 5-Year
# Table ID: S1901
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

median_income_usa <- get_acs(geography = "us",
                         variables = c("Median Household Income" = "S1901_C01_012"),
                         year = 2018,
                         survey = "acs5")

median_income_usa

median_income_az <- get_acs(geography = "state",
                            state = "az",
                             variables = c("Median Household Income" = "S1901_C01_012"),
                             year = 2018,
                             survey = "acs5")

median_income_az

median_income_catchment <- get_acs(geography = "county",
                            state = "az",
                            variables = c("Median Household Income" = "S1901_C01_012"),
                            year = 2018,
                            survey = "acs5")

median_income_catchment %>%
  mutate(NAME = str_replace(median_income_catchment$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate)

median_income_catchment %>%
  mutate(NAME = str_replace(median_income_catchment$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  summarise(estimate = mean())

# median household income race in catchment ----
# MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)
# Survey/Program: American Community Survey
# Universe: Households with a householder who is specified race
# Year: 2018
# Estimates: 5-Year
# Table ID: B19013I
# Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

median_income_catch_race <- get_acs(geography = "county",
                                    state = "az",
                                    variables = c("white" = "B19013H_001",
                                                  "hispanic" = "B19013I_001",
                                                  "ai" = "B19013C_001"),
                                    year = 2018,
                                    survey = "acs5")

median_income_catch_race%>%
  mutate(NAME = str_replace(median_income_catch_race$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable,
         value = estimate)


