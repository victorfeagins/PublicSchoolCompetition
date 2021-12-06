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
         sd.long = sd(c_across(starts_with("Longitude")), na.rm = TRUE)) %>% 
  ungroup()

hist(log(df.long.lat$sd.long))
hist(log(df.long.lat$sd.lat))
sd(df.long.lat$sd.lat, na.rm = TRUE)
sd(df.long.lat$sd.long, na.rm = TRUE)

#There is some variation in in the long lat over the years but most are very small. 
# I think it will be easier to see if the addresses change
#For know let's take the average lat and long as the lat long for the school
#Taking note to explore this further.

df.long.lat <- df.long.lat %>% 
  rowwise() %>% 
  mutate(Long.Avg = mean(as.numeric(c_across(starts_with("Longitude"))), na.rm = TRUE),
         Lat.Avg = mean(as.numeric(c_across(starts_with("Latitude"))), na.rm = TRUE)) %>% 
  ungroup()

df.long.lat.less.var <- df.long.lat %>% 
  select(Agency.ID...NCES.Assigned..Public.School..Latest.available.year,
         School.ID...NCES.Assigned..Public.School..Latest.available.year, 
         Long.Avg, 
         Lat.Avg)


### Merging Spatial information ----

df.subset <- df.subset %>% 
  left_join(df.long.lat.less.var)


sum(is.na(df.subset$Long.Avg))

sum(is.na(df.subset$Lat.Avg))

#Some schools do not have lat and long. The collection of that data goes back only to 2000
#For these schools we will need to geocode from the physical addresses if common core has these.


## Charter and Magnet School Status ----

df.charter.magnet<- read_american_core("Data2/Charter_Magnet_2019_1998.csv")


### Does Charter or Magnet Status change over the years? ----


df.charter.magnet <- df.charter.magnet %>% 
  rowwise() %>% 
  mutate(Status_CharterChange = n_distinct(c_across(starts_with("Charter")), na.rm = TRUE), #2 distinct groups mean yes and no exists in the same row
         Status_MagnetChange = n_distinct(c_across(starts_with("Magnet")), na.rm = TRUE)) %>% 
  ungroup()



table(df.charter.magnet$Status_CharterChange)
table(df.charter.magnet$Status_MagnetChange)

#Some schools became a charter school and some magnet schools became a magnet


df.changes<- df.charter.magnet %>% 
  filter(Status_CharterChange == 2 | Status_MagnetChange == 2)


### Can a school be a Magnet school and a Charter School? ----

df.charter.magnet <- df.charter.magnet %>% 
  rowwise() %>% 
  mutate(ListCharter = list(c_across(starts_with("Charter"))),
         ListMagnet = list(c_across(starts_with("Magnet"))),
         ListCM = list(paste(ListCharter,ListMagnet)), #Combines Charter and Magnet School Status together
         CM.SameTime = sum(str_detect(ListCM, "1-Yes 1-Yes"), na.rm = TRUE)) %>% #Checks if Charter Yes and Magnet yes at same year
  ungroup()

table(df.charter.magnet$CM.SameTime)

#Extremely small amount of schools are a magnet school and charter school


### Merging Charter School information

df.charter.less.var <- df.charter.magnet %>% 
  select(School.ID...NCES.Assigned..Public.School..Latest.available.year,
         Agency.ID...NCES.Assigned..Public.School..Latest.available.year, starts_with("Charter"))


df.subset <- df.subset %>% 
  left_join(df.charter.less.var)


## Student  Characteristics ----

### Number of Free Lunch Students ----
df.freelunch <- read_american_core("Data2/Free_Lunch_Eligible_2019_1987.csv")


##### Merging Free Lunch Students ----

df.subset <- df.subset %>% 
  left_join(df.freelunch)



### Total Number of Students ----

df.totalstudents <- read_american_core("Data2/Total_Students_2019_1986.csv")

##### Merging Total Students ----
df.subset <- df.subset %>% 
  left_join(df.totalstudents)


#There is another variable which is the number of students reported for ethic background ideally these numbers are the same. 

### Hispanic Black Students ----


df.hispanic.black <- read_american_core("Data2/Hispanic_Black_2019_1987.csv")

df.subset <- df.subset %>% 
  left_join(df.hispanic.black)


### White and Asian Students ----

df.white.asian <- read_american_core("Data2/White_Asian_2019_1987.csv")

df.subset <- df.subset %>% 
  left_join(df.white.asian)


#Creating Dataset for analysis 

RenameVar <- function(title, string){
  str_c(title, str_sub(string, -7)) #Combines the title and the last 7 characters (year)
}

df.final <- df.subset %>% 
  rename_with(~RenameVar("Status", .x), starts_with("Start")) %>% 
  select(sort(names(.))) #Sorts variables alphabetically



