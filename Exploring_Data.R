#This script is the initial exploring of panel dataset created from Defining_Sample.R


#Packages ----
library(dplyr)
library(ggplot2)
library(stringr)
library(broom)

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

#Agency.ID is school district

table(df$Year) #no odd looking years

# Explore the number of schools every year ----

df %>% 
  group_by(Year) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(as.numeric(Year), count)) +
  geom_line()
#In 2010 school closed down and been on decline since curious why?


# Temporal quantity ----
df %>% 
  group_by(Full.ID) %>% 
  summarise(count = n())%>% 
  ggplot(mapping = aes(count)) +
  geom_histogram() +
  labs(x = "Years Length")

#Most schools have been around for more then 35 years.


#Explore Time if Vary ----

## Free.Lunch.Percent ----
Free.Lunch.Models <- df %>% 
  filter(!is.na(Free.Lunch.Percent)) %>% 
  nest_by(Full.ID) %>% #Groups data where each row is a dataframe for full.id
  mutate(model = list(lm(Free.Lunch.Percent ~ Year, data = data)))

Free.Lunch.Models.coeff <- Free.Lunch.Models %>% 
  summarise(tidy(model))

Free.Lunch.Models.coeff <- Free.Lunch.Models.coeff %>% 
  filter(term == "Year") %>% 
  mutate(sig = ifelse(p.value < .05, "Sig", "Not-Sig"))

hist(Free.Lunch.Models.coeff$estimate)
table(Free.Lunch.Models.coeff$sig)

#Since Free.Lunch is a percent should I use count regression with time? Something I am not familiar with.
#For most schools time is significant but can't be trusted since OLS might not make sense.