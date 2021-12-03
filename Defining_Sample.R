#The script is defining the sample of our analysis

# Packages ----
library(dplyr)#For dataframe manipulation
library(tidyr) #For transposing data
library(purrr) # For effective iteration
library(stringr) #For handling of strings



# Reading in Data ----
df <- read.csv("Data2/Report_2019_1986.csv",  header = TRUE,
               skip = 6) %>% 
  head(-4)

df <- df %>% 
  mutate(across(everything(), ~ifelse(str_detect(.,pattern = "â€"), NA, .)))



# Calculating Years Active ----

df <- df %>% 
  rowwise() %>% 
  mutate(YearsActive = sum(str_count(c_across(starts_with("Start.of.Year")), "1|3"), na.rm = TRUE)) %>%  #Counting 1 open and 3 new schools
  ungroup() #removes grouping from rowwise

hist(df$YearsActive) # Most schools have the all the years open
  
table(df$YearsActive)

#It might be possible to actually get the ranges of 



# Finding the time frame the school is open or at least reported data -----

df <- df %>% 
  mutate(YearsReported = str_split(Years.School.Reported.Data..Public.School..Latest.available.year, " |-")) %>% 
  rowwise() %>% #Rowwise is nice for list columns
  mutate(FirstYear = min(as.numeric(YearsReported)),
         LastYear = max(as.numeric(YearsReported))) %>% 
  ungroup()



# Filtering schools 10 years or greater ----
#10 chosen out of simplicity

df.subset <- df %>%
  filter(YearsActive >= 10)








  
