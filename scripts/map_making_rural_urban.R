# generate choropleth map of rural and urban in AZ

# set up
# packages
library(here)
library(tidyverse)
library(choroplethrZip)
library(choroplethr)
library(choroplethrMaps)
library(readxl)
library(tigris)
library(geojsonio)
library(sf)
library(ggthemes)

options(tigris_use_cache = TRUE)

data(county.regions)

# read data
population <- read_excel("data/raw/UACC_Pop_by_ZIP_Type.xlsx",
  col_types = c(
    "text", "text", "text",
    "numeric", "text", "numeric", "numeric",
    "numeric"
  )
)

# checking data 
glimpse(population)
population
population %>%
  distinct(county_name)
#####

az_zip_codes <- zctas()

spatial_population <- geo_join(
  spatial_data = az_zip_codes,
  data_frame = population,
  by_sp = "ZCTA5CE10",
  by_df = "ZIP",
  how = "inner"
)

st_write(obj = spatial_population,
         dsn = "data/spatial/population_rural_urban/ruca.shp"
         )

spatial_az_counties <- readRDS("../UAZCC_COE_Program_Evaluation/data/tidy/spatial_az_counties.rds")

spatial_population %>%
  ggplot() +
  geom_sf(data = spatial_az_counties) +
  geom_sf(mapping = aes(fill = `2010 Population of the 2010 ZCTA`)) +
  scale_fill_gradient(low = "#81D3EB", high = "#001C48") +
  theme_void() +
  xlim(-115, -109) +
  ylim(31.2, 33.5) +
  labs(title = "2010 Population of each Zip Code")

spatial_population %>%
  ggplot() +
  geom_sf(data = spatial_az_counties) +
  geom_sf(mapping = aes(fill = `Primary RUCA Code`)) +
  scale_fill_gradient(high = "#81D3EB", low = "#001C48") +
  theme_void() +
  xlim(-115, -109) +
  ylim(31.2, 33.5) +
  labs(title = "RUCA designation",
       subtitle = "1 = urban, 10 = rural")




spatial_list <- geojson_list(spatial_population$geometry)
centroid(spatial_list)
centroid(st_geometry(spatial_population$geometry))
centroid(st_simplify(spatial_population$geometry))
st_multipolygon(spatial_population)
st_area(spatial_population)
st_make_grid(spatial_population)
st_cast(x = spatial_population, to = "multipoint")
spatial_coordinates <- st_coordinates(spatial_population)


ggplot() +
  stat_sf_coordinates(data = spatial_population, mapping = aes(color = `Primary RUCA Code`)) 



#####
# county basemap
pop_county <- population %>%
  select(county.name = county_name, 
         value = `2010 Population of the 2010 County`) %>%
  distinct() 

pop_county$county.name <- str_to_lower(pop_county$county.name, locale = "en")

pop_county <- inner_join(pop_county, county.regions, by = "county.name")

pop_county <- pop_county %>%
  filter(state.name == "arizona")

# map
basemap <- county_choropleth(pop_county,
                             title = "basemap",
                             state_zoom = "arizona",
                             reference_map = FALSE)

basemap

# assign values according to primary ruca code
pop <- population %>%
  select(
    region = ZIP,
    value = `Primary RUCA Code`
  )

# prepare map
mapped_pop <- zip_choropleth(pop,
  title = "map",
  legend = "",
  num_colors = 5,
  state_zoom = "arizona"
)

print(mapped_pop)

# assign values according to category
# save as alternate table
pop_alt <- population

# examine
pop_alt %>%
  distinct(`ZIP Category`)

pop_alt$`ZIP Category` <- fct_relevel(pop_alt$`ZIP Category`,
              levels = c("Urban area (UW)",
                         "Large rural city (UW)",
                         "Small, Isolated, Rural (UW)",
                         "Frontier and Remote ZIP (USDA-ERS)"))

# recode
pop_alt <- pop_alt %>%
  mutate(`ZIP Category` = recode_factor(`ZIP Category`,
    # old = new
    `Urban area (UW)` = `Urban area (UW)`,
    `Large rural city (UW)` = `Large rural city (UW)`,
    `Small, Isolated, Rural (UW)` = `Small, Isolated, Rural (UW)`,
    `Frontier and Remote ZIP (USDA-ERS)` = `Frontier and Remote ZIP (USDA-ERS)`,
    .ordered = TRUE
  ))

# prepare for mapping
pop_alt <- pop_alt %>%
  select(
    region = ZIP,
    value = `ZIP Category`
  ) %>%
  drop_na()

# generate maps
# AZ
mapped_pop_alt <- zip_choropleth(pop_alt,
                                         title = "2010 Population by Zip Code",
                                         legend = "Urban-Rural Category",
                                         num_colors = 4,
                                         state_zoom = "arizona"
)

print(mapped_pop_alt)

# cochise
mapped_pop_alt_cochise <- zip_choropleth(pop_alt,
  title = "2010 Population by Zip Code Cochise",
  legend = "Urban-Rural Category",
  num_colors = 4,
  county_zoom = "4003"
)

print(mapped_pop_alt_cochise)

# pima
mapped_pop_alt_pima <- zip_choropleth(pop_alt,
                                         title = "2010 Population by Zip Code Pima",
                                         legend = "Urban-Rural Category",
                                         num_colors = 4,
                                         county_zoom = "4019"
)

print(mapped_pop_alt_pima)

#pinal 
mapped_pop_alt_pinal <- zip_choropleth(pop_alt,
                                      title = "2010 Population by Zip Code Pinal",
                                      legend = "Urban-Rural Category",
                                      num_colors = 4,
                                      county_zoom = "4021"
)

print(mapped_pop_alt_pinal)

# santa cruz
mapped_pop_alt_santa_cruz <- zip_choropleth(pop_alt,
                                       title = "2010 Population by Zip Code Santa Cruz",
                                       legend = "Urban-Rural Category",
                                       num_colors = 4,
                                       county_zoom = "4023"
)

print(mapped_pop_alt_santa_cruz)

#yuma
mapped_pop_alt_yuma <- zip_choropleth(pop_alt,
                                            title = "2010 Population by Zip Code Yuma",
                                            legend = "Urban-Rural Category",
                                            num_colors = 4,
                                            county_zoom = "4027"
)

print(mapped_pop_alt_yuma)

data("zip.map")

gis_pop <- inner_join(pop_alt, zip.map, by = "region")

write_csv(gis_pop, "data/tidy/gis_population_rural_urban.csv")
