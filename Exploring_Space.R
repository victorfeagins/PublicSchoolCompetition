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

# Grabbing Tract Spatial information ----


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

### Cleaning Tract information ----

race.table.tract <- race.table.tract %>% 
  rename_with(.fn = ~str_c("Total", str_sub(.x, start = -1)), .cols = starts_with("B03002_001")) %>% 
  rename_with(.fn =  ~str_c("Not.Hispanic", str_sub(.x, start = -1)), .cols = starts_with("B03002_002")) %>% 
  rename_with(.fn =  ~str_c("NH.White", str_sub(.x, start = -1)), .cols = starts_with("B03002_003")) %>% 
  rename_with(.fn =  ~str_c("NH.Black", str_sub(.x, start = -1)), .cols = starts_with("B03002_004")) %>% 
  rename_with(.fn =  ~str_c("NH.American.Indian", str_sub(.x, start = -1)), .cols = starts_with("B03002_005")) %>% 
  rename_with(.fn =  ~str_c("NH.Asian", str_sub(.x, start = -1)), .cols = starts_with("B03002_006")) %>% 
  rename_with(.fn =  ~str_c("NH.Native.Hawaiian", str_sub(.x, start = -1)), .cols = starts_with("B03002_007")) %>% 
  rename_with(.fn =  ~str_c("NH.Other", str_sub(.x, start = -1)), .cols = starts_with("B03002_008")) %>% 
  rename_with(.fn =  ~str_c("NH.Two.or.More", str_sub(.x, start = -1)), .cols = starts_with("B03002_009")) %>% 
  rename_with(.fn =  ~str_c("NH.Two.Races.Other", str_sub(.x, start = -1)), .cols = starts_with("B03002_010")) %>% 
  rename_with(.fn =  ~str_c("NH.Two.Races.NOther", str_sub(.x, start = -1)), .cols = starts_with("B03002_011")) %>% 
  rename_with(.fn =  ~str_c("Hispanic", str_sub(.x, start = -1)), .cols = starts_with("B03002_012")) %>% 
  rename_with(.fn =  ~str_c("H.White", str_sub(.x, start = -1)), .cols = starts_with("B03002_013")) %>%
  rename_with(.fn =  ~str_c("H.Black", str_sub(.x, start = -1)), .cols = starts_with("B03002_014")) %>% 
  rename_with(.fn =  ~str_c("H.American.Indian", str_sub(.x, start = -1)), .cols = starts_with("B03002_015")) %>% 
  rename_with(.fn =  ~str_c("H.Asian", str_sub(.x, start = -1)), .cols = starts_with("B03002_016")) %>% 
  rename_with(.fn =  ~str_c("H.Native.Hawaiian", str_sub(.x, start = -1)), .cols = starts_with("B03002_017")) %>% 
  rename_with(.fn =  ~str_c("H.Other", str_sub(.x, start = -1)), .cols = starts_with("B03002_018")) %>% 
  rename_with(.fn =  ~str_c("H.Two.or.More", str_sub(.x, start = -1)), .cols = starts_with("B03002_019")) %>%
  rename_with(.fn =  ~str_c("H.Two.Races.Other", str_sub(.x, start = -1)), .cols = starts_with("B03002_020")) %>% 
  rename_with(.fn =  ~str_c("H.Two.Races.NOther", str_sub(.x, start = -1)), .cols = starts_with("B03002_021"))
  
# I really wanted to write a function to reduce that code but was struggling with how rename_with functions works.and getting a working codebook for ACS

median.income.tract <- median.income.tract %>% 
  rename_with(.fn =  ~str_c("Median.Income", str_sub(.x, start = -1)), .cols = starts_with("B06011_001"))

### Merging Tract Info ----

tract.information <- race.table.tract %>% 
  left_join(as.data.frame(median.income.tract))#since this not a spatial merge turn into a dataframe.


# School Spatial Information ----




