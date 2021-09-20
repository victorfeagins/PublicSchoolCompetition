#Common Core of Data in their table generator they do not have an explicit school opened variable but it is
#Contained in the School Status 


#Packages ----
library(dplyr)
library(purrr)

#Data Files requested from Common Core ----
filevector <- c("Data/Status1989_1986.csv", "Data/Status1999_1900.csv", "Data/Status2009_2000.csv",
                "Data/Status2019_2010.csv")


#Reading in csv's
df<- lapply(filevector, read.csv, header = TRUE, skip = 6) %>%  #Read in all the csv's
  lapply(filter, (!is.na(School.ID...NCES.Assigned..Public.School..Latest.available.year))) %>% # In each csv file remove observations that have no school ID
  reduce(full_join) #Merge them all by common variable names


