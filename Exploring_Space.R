#This script explores space and not time of my outcomes


#Packages ----
library(dplyr)
library(ggplot2)
library(stringr)

## Spatial libraries ----
library(sf)
library(tidycensus)
library(spdep)

# Reading in Data ----
df <- read.csv("Data2/School_Panel.csv")



# Creating output variables ----

df <- df %>% 
  mutate(Hispanic.Percent = Hispanic.Students/Total.Students,
         Black.Percent = Black.Students/Total.Students,
         Asian.Percent = Asian.Students/Total.Students,
         White.Percent = White.Students/Total.Students,
         Free.Lunch.Percent = Free.Lunch.Students/Total.Students) %>% 
  mutate(Full.ID = str_c(Agency.ID,School.ID)) %>% 
  mutate(Year = as.numeric(str_sub(Year, 1, 4)))#Grab first 4 characters to say the year 


# Filtering Year out ----
#Since we are exploring space in this script we will only look at one year for simplicity. 
df.2019 <- df %>% 
  filter(Year == 2019) %>% 
  filter(Total.Students != 0)

# Grabbing Spatial information ----


race.table.tract <-  get_acs(geography = "tract",
                         year=2019,
                         geometry = T,
                         output="wide",
                         table = "B03002",
                         cache_table = T,
                         state = "TX")

median.income.tract <- get_acs(geography = "tract",
                               year=2019,
                               geometry = T,
                               output="wide",
                               variables = "B06011_001E",
                               cache_table = T,
                               state = "TX")




