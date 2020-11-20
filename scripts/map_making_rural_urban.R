# generate choropleth map of rural and urban in AZ

# set up
# packages
library(here)
library(tidyverse)
library(choroplethrZip)
library(readxl)

# read data
population <- read_excel("data/raw/UACC_Pop_by_ZIP_Type.xlsx",
                         col_types = c("text", "text", "text", 
                                       "numeric", "text", "numeric", "numeric", 
                                       "numeric")
)
glimpse(population)

population

population %>%
  distinct(county_name)

pop <- population %>%
  select(region = ZIP,
         value = `Primary RUCA Code`)

mapped_pop <- zip_choropleth(pop, title = "map",
               legend = "",
               num_colors = 5,
               state_zoom = "arizona")

print(mapped_pop)

pop_alt <- population

pop_alt %>%
  distinct(`ZIP Category`)

pop_alt <- pop_alt %>% 
  mutate(`ZIP Category` = recode_factor(`ZIP Category`,
  #old = new
  `Urban area (UW)` = 1,
  `Large rural city (UW)` = 2,
  `Small, Isolated, Rural (UW)` = 3,
  `Frontier and Remote ZIP (USDA-ERS)` = 4,
  .ordered = TRUE
))

pop_alt <- pop_alt %>%
  select(region = ZIP,
         value = `ZIP Category`) %>%
  drop_na()

mapped_pop_alt <- zip_choropleth(pop_alt, title = "2010 Population by Zip Code",
                             legend = "Urban-Rural Category",
                             num_colors = 4,
                             state_zoom = "arizona")

print(mapped_pop_alt)

data("zip.map")

gis_pop <- inner_join(pop_alt, zip.map, by = "region")

write_csv(gis_pop, "data/tidy/gis_population_rural_urban.csv")
