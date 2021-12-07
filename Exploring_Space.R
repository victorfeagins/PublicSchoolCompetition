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
#Since we are exploring space in this script we will only look at one year for simplicty. 
df.2019 <- df %>% 
  filter(Year == 2019) %>% 
  filter(Total.Students != 0)
