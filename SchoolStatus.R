#Common Core of Data in their table generator they do not have an explicit school opened variable but it is
#Contained in the School Status 


#Packages ----
library(dplyr)#For dataframe manipulation
library(tidyr) #For transposing data
library(purrr) # For effective interation
library(stringr) #For handling of strings

#Data Files requested from Common Core ----
filevector <- c("Data/Status1989_1986.csv", "Data/Status1999_1900.csv", "Data/Status2009_2000.csv",
                "Data/Status2019_2010.csv")

# Data Processing ----
## Reading in csv's ----
df <- lapply(filevector, read.csv, header = TRUE, skip = 6) %>%  #Read in all the csv's
  lapply(filter, row_number() <= n()-4) %>% # In each csv file remove footer of 4 rows
  reduce(full_join)  #Merge them all by common variable names (name, state, ID)


## Renaming Column names ----

RenameVar <- function(title, string){
  str_c(title, str_sub(string, -7)) #Combines the title and the last 7 characters (year)
}

df.order <- df %>% 
  rename_with(~RenameVar("Status", .x), starts_with("Start")) %>% 
  select(sort(names(.))) #Sorts variables alphabetically


## Replacing special characters ----

df.order <- df.order %>% 
  mutate(across(everything(), ~str_trim(str_replace(.,"â€", "NA")))) %>%  # Replace special characters with "NA"
  na_if("NA") #making "NA" into real NA


## Transposing Data ----
df.trans <- df.order %>% 
  pivot_longer(starts_with("Status"))

df.trans.nm <- df.trans %>% 
  filter(!is.na(value))
