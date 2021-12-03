#The script is defining the sample of our analysis

# Packages ----
library(dplyr)#For dataframe manipulation
library(tidyr) #For transposing data
library(purrr) # For effective iteration
library(stringr) #For handling of strings

read_american_core <- function(file)
{
  df <- read.csv(file,  header = TRUE,
           skip = 6) %>% 
    head(-4) %>% 
    mutate(across(everything(), ~ifelse(str_detect(.,pattern = "â€"), NA, .)))
  df
}


# Reading in Data ----
df  <-  read_american_core("Data2/Report_2019_1986.csv")

# Finding Subset of Data by Years Active ----
## Calculating Years Active ----

df <- df %>% 
  rowwise() %>% 
  mutate(YearsActive = sum(str_count(c_across(starts_with("Start.of.Year")), "1|3"), na.rm = TRUE)) %>%  #Counting 1 open and 3 new schools
  ungroup() #removes grouping from rowwise

hist(df$YearsActive) # Most schools have the all the years open
  
table(df$YearsActive)





## Finding the time frame the school is open or at least reported data -----

df <- df %>% 
  mutate(YearsReported = str_split(Years.School.Reported.Data..Public.School..Latest.available.year, " |-")) %>% 
  rowwise() %>% #Rowwise is nice for list columns
  mutate(FirstYear = min(as.numeric(YearsReported)),
         LastYear = max(as.numeric(YearsReported))) %>% 
  ungroup()

#This gets the last and first year reported but hides situations when schools leave and reenter the dataset 
#To explore this would require a bit more coding but is possible. By replacing "-" with ":" and " " with "," and 
#Evaluating the sequence would be each year school is active.



## Filtering schools 10 years or greater ----
#10 chosen out of simplicity

df.subset <- df %>%
  filter(YearsActive >= 10)



# Adding other Variables to Dataset ----

## Spatial Information ----

df.long.lat<- read_american_core("Data2/Long_Lat_2019_2000.csv")


### Does long lat vary over the years?

df.long.lat <- df.long.lat %>% 
  rowwise() %>% 
  mutate(sd.lat = sd(c_across(starts_with("Latitude")), na.rm = TRUE),
         sd.long = sd(c_across(starts_with("Longitude")), na.rm = TRUE))

hist(log(df.long.lat$sd.long))
hist(log(df.long.lat$sd.lat))
sd(df.long.lat$sd.lat, na.rm = TRUE)
sd(df.long.lat$sd.long, na.rm = TRUE)

#There is some variation in in the long lat over the years but most are very small. 
# I think it will be easier to see if the addresses change.
#For know let's take the average lat and long as the lat long for the school
#Taking note to explore this further.
















  
