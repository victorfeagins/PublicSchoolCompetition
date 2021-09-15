# Packages ----
library(dplyr)
library(stringr)
library(ggplot2)


df <- read.csv("Data/2021_2015_Char.csv", 
               header = TRUE,
               skip = 6)

#Cleaning Data ----
df.clean <- df %>% 
  mutate(across(everything(), ~str_trim(str_replace(.,"â€", "NA")))) # Replace special characters with NA

# Plotting Charter vs Public ----




