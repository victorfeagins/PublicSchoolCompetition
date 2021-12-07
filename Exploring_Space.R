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
#### Setting up ACS Codebook ----
rd <- "B03002001
Total
B03002002
Not Hispanic or Latino
B03002003
White Alone
B03002004
Black or African American Alone
B03002005
American Indian and Alaska Native Alone
B03002006
Asian Alone
B03002007
Native Hawaiian and Other Pacific Islander Alone
B03002008
Some Other Race Alone
B03002009
Two or More Races
B03002010
Two Races Including Some Other Race
B03002011
Two Races Excluding Some Other Race, and Three or More Races
B03002012
Hispanic or Latino
B03002013
White Alone
B03002014
Black or African American Alone
B03002015
American Indian and Alaska Native Alone
B03002016
Asian Alone
B03002017
Native Hawaiian and Other Pacific Islander Alone
B03002018
Some Other Race Alone
B03002019
Two or More Races
B03002020
Two Races Including Some Other Race
B03002021
Two Races Excluding Some Other Race, and Three or More Races"
#Copy and pasted from https://www.socialexplorer.com/data/ACS2015/metadata/?ds=ACS15&table=B03002

ACS.Codebook <- rd %>% 
  str_split("\n") %>% 
  unlist() %>% 
  matrix(ncol =2, byrow = TRUE,
         dimnames = list(c(),c("Key", "Label"))) %>% 
  as.data.frame() %>% 
  mutate(Label = str_replace_all(Label," ", ".")) %>% 
  mutate(Key = str_c(str_sub(Key, end=-4), "_", str_sub(Key, start = -3))) #Adding Underscore to match tidycensus



#### Renaming ACS variables ----
Rename_ACS <- function(name, ACS.Codebook)
{
  ACS.Codebook %>% 
    filter(Key %in% name) %>% 
    select(Label) %>% 
    as.vector(mode = "character")
}

race.table.tract %>% 
  rename_with(.fn = Rename_ACS, .cols = starts_with("B"), ACS.Codebook = ACS.Codebook)



