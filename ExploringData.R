# Packages ----
library(dplyr)
library(stringr)

library(sf)
library(ggplot2)
library(mapview)

df <- read.csv("Data/2021_2015_Char.csv", 
               header = TRUE,
               skip = 6)

#Cleaning Data ----
df.clean <- df %>% 
  mutate(across(everything(), ~str_trim(str_replace(.,"â€", "NA")))) # Replace special characters with NA

# Plotting Charter vs Public ----
## Getting Data Spatial ----
df.spatial <- df.clean %>% 
  select(School.Name, School.ID...NCES.Assigned..Public.School..Latest.available.year,
         Charter.School..Public.School..2019.20, Longitude..Public.School..2019.20, Latitude..Public.School..2019.20) %>%
  mutate(Longitude..Public.School..2019.20=as.numeric(Longitude..Public.School..2019.20)) %>% 
  filter(!is.na(Longitude..Public.School..2019.20)) %>% 
  st_as_sf(coords = c("Longitude..Public.School..2019.20", "Latitude..Public.School..2019.20"))
  
## Getting Texas County ----
library(tigris)
TX.county <- counties(state = "TX")
#st_crs(TX.county) <- 4269

## Trying ggplot ----
ggplot(df.spatial) +
  geom_sf(mapping = aes(col = Charter.School..Public.School..2019.20)) +
  xlab("Longitude") + ylab("Latitude")
  

## Trying mapview ----

mapview(TX.county) +
  mapview(df.spatial,
          zcol = "Charter.School..Public.School..2019.20",
          map.types = "OpenStreetMap")


