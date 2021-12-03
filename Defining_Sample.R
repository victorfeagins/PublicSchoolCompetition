#The script is exploring the various observations in the Common Core
#Data set to find the ID's that are really IDs

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



# Looking at schools that have existed for more then 1 year ----

test <- df %>% 
  rowwise() %>% 
  mutate(YearsActive = sum(str_count(c_across(starts_with("Start.of.Year")), "1|3"), na.rm = TRUE))
  
  



#Problems of the same schools having multiple IDs in the dataset

sum(duplicated(df)) # No duplicates including ID's 

df %>% 
  select(-School.ID...NCES.Assigned..Public.School..Latest.available.year, 
         -Agency.ID...NCES.Assigned..Public.School..Latest.available.year) %>% 
  duplicated() %>% 
  sum()
#Without IDs 497 schools have the same information.


### Let's add new information and see if the duplicated changes

## Adding teacher and student numbers ----
df.t <- read.csv("Data2/Students_Teachers_2019_1986.csv", 
                 header = TRUE,
                 skip = 6) %>% 
  head(-4)

df.t <- df.t %>% 
  mutate(across(everything(), ~ifelse(str_detect(.,pattern = "â€"), NA, .)))

df.merged <- df %>% 
  left_join(df.t)


df.merged %>% 
  select(-School.ID...NCES.Assigned..Public.School..Latest.available.year, 
         -Agency.ID...NCES.Assigned..Public.School..Latest.available.year) %>% 
  duplicated() %>% 
  sum()
#Dang there are less duplication that means that adding student and teacher information is different across duplication

## Examining duplications by eye

old.duplicated<- df %>% 
  select(-School.ID...NCES.Assigned..Public.School..Latest.available.year, 
         -Agency.ID...NCES.Assigned..Public.School..Latest.available.year) %>% 
  duplicated()

test <- df.merged %>% 
  filter(old.duplicated) %>% 
  select(names(df.t))


## Adding Spatial information ----
df.spat <- read.csv("Data2/Long_Lat_2019_2000.csv", 
                 header = TRUE,
                 skip = 6) %>% 
  head(-4)


df.merged.2 <- df.merged %>% 
  left_join(df.spat)


df.merged.2 %>% 
  select(-School.ID...NCES.Assigned..Public.School..Latest.available.year, 
         -Agency.ID...NCES.Assigned..Public.School..Latest.available.year) %>% 
  duplicated() %>% 
  sum()

dup <- df.merged.2 %>% 
  select(-School.ID...NCES.Assigned..Public.School..Latest.available.year, 
         -Agency.ID...NCES.Assigned..Public.School..Latest.available.year) %>% 
  duplicated()
test <- df.merged.2 %>% 
  filter(dup) %>% 
  select(names(df.spat))
