#This script explores space and not time of my outcomes


#Packages ----
library(dplyr)
library(ggplot2)
library(stringr)

## Spatial libraries ----
library(sf)
library(tidycensus)
library(spdep)
library(tigris)
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

tx.boundery <- states(cb = TRUE, year = 2019)

tx.boundery<- tx.boundery %>% 
  filter(STUSPS == "TX") %>%
  st_transform(crs = "EPSG:3081")
  
### Cleaning Tract information ----

race.table.tract <- race.table.tract %>% 
  rename_with(.fn = ~str_c("Total", str_sub(.x, start = -1)), .cols = starts_with("B03002_001")) %>% 
  rename_with(.fn =  ~str_c("Not.Hispanic", str_sub(.x, start = -1)), .cols = starts_with("B03002_002")) %>% 
  rename_with(.fn =  ~str_c("NH.White", str_sub(.x, start = -1)), .cols = starts_with("B03002_003")) %>% 
  rename_with(.fn =  ~str_c("NH.Black", str_sub(.x, start = -1)), .cols = starts_with("B03002_004")) %>% 
  rename_with(.fn =  ~str_c("NH.American.Indian", str_sub(.x, start = -1)), .cols = starts_with("B03002_005")) %>% 
  rename_with(.fn =  ~str_c("NH.Asian", str_sub(.x, start = -1)), .cols = starts_with("B03002_006")) %>% 
  rename_with(.fn =  ~str_c("NH.Native.Hawaiian", str_sub(.x, start = -1)), .cols = starts_with("B03002_007")) %>% 
  rename_with(.fn =  ~str_c("NH.Other", str_sub(.x, start = -1)), .cols = starts_with("B03002_008")) %>% 
  rename_with(.fn =  ~str_c("NH.Two.or.More", str_sub(.x, start = -1)), .cols = starts_with("B03002_009")) %>% 
  rename_with(.fn =  ~str_c("NH.Two.Races.Other", str_sub(.x, start = -1)), .cols = starts_with("B03002_010")) %>% 
  rename_with(.fn =  ~str_c("NH.Two.Races.NOther", str_sub(.x, start = -1)), .cols = starts_with("B03002_011")) %>% 
  rename_with(.fn =  ~str_c("Hispanic", str_sub(.x, start = -1)), .cols = starts_with("B03002_012")) %>% 
  rename_with(.fn =  ~str_c("H.White", str_sub(.x, start = -1)), .cols = starts_with("B03002_013")) %>%
  rename_with(.fn =  ~str_c("H.Black", str_sub(.x, start = -1)), .cols = starts_with("B03002_014")) %>% 
  rename_with(.fn =  ~str_c("H.American.Indian", str_sub(.x, start = -1)), .cols = starts_with("B03002_015")) %>% 
  rename_with(.fn =  ~str_c("H.Asian", str_sub(.x, start = -1)), .cols = starts_with("B03002_016")) %>% 
  rename_with(.fn =  ~str_c("H.Native.Hawaiian", str_sub(.x, start = -1)), .cols = starts_with("B03002_017")) %>% 
  rename_with(.fn =  ~str_c("H.Other", str_sub(.x, start = -1)), .cols = starts_with("B03002_018")) %>% 
  rename_with(.fn =  ~str_c("H.Two.or.More", str_sub(.x, start = -1)), .cols = starts_with("B03002_019")) %>%
  rename_with(.fn =  ~str_c("H.Two.Races.Other", str_sub(.x, start = -1)), .cols = starts_with("B03002_020")) %>% 
  rename_with(.fn =  ~str_c("H.Two.Races.NOther", str_sub(.x, start = -1)), .cols = starts_with("B03002_021"))
  
# I really wanted to write a function to reduce that code but was struggling with how rename_with functions works.and getting a working codebook for ACS

median.income.tract <- median.income.tract %>% 
  rename_with(.fn =  ~str_c("Median.Income", str_sub(.x, start = -1)), .cols = starts_with("B06011_001"))

### Merging Tract Info ----

tract.information <- race.table.tract %>% 
  left_join(as.data.frame(median.income.tract)) %>% #since this not a spatial merge turn into a dataframe.
  st_transform(crs = "EPSG:3081")


# School Spatial Information ----

### Turning School into Simple Feature ----
df.2019 <- df.2019 %>% 
  st_as_sf(coords = c("Long.Avg", "Lat.Avg"), crs = "WGS84") %>% #Will need to check the projection 
  st_transform(crs = "EPSG:3081")



### Plotting Spatial Properties----
df.2019 %>%
  ggplot()+
  geom_sf(data=tx.boundery)+
  geom_sf()

#There are some points not in Texas, One point in the middle of gulf of Mexico.
#The rest look pretty accurate.

df.2019 %>% 
  filter(!st_intersects(df.2019, tx.boundery, sparse = FALSE)) %>% 
  ggplot()+
  geom_sf(data=tx.boundery)+
  geom_sf(mapping = aes(col = School.Name))
  
#Need to further Examine these schools. It is probably a typo when they inputted the lat and long.
#Most of these schools seem to exist. For now I will put them aside.


df.2019.TX <- df.2019 %>% 
  filter(st_intersects(df.2019, tx.boundery, sparse = FALSE)) #Schools in Texas boundary



# Merging Tract information with Schools ----

df.2019.TX.Buffer <- df.2019.TX %>% 
  st_buffer(8046.72) #5 miles in meters


df.2019.TX.Buffer %>% 
  ggplot() +
  geom_sf(data = tract.information)+
  geom_sf()


df.2019.TX.Merged <- df.2019.TX.Buffer %>% 
  st_join(tract.information)


## Condensing Tract information ----
School.Tract.Info <- df.2019.TX.Merged %>% 
  as.data.frame() %>% #Removes space since it is sticky and it will slow things down
  group_by(Full.ID) %>% 
  select(Full.ID, ends_with("E", ignore.case = FALSE)) %>% 
  select_if(is.numeric) %>% 
  summarise(Median.Income.AvgE = mean(Median.IncomeE, na.rm = TRUE),
            across(.cols = ends_with("E", ignore.case = FALSE), .fns = sum)) %>% 
  select(-Median.IncomeE)

#Adds population numbers but averages median Income


### Joining condensed Tract info to data ----
df.2019.TX <- df.2019.TX %>% 
  left_join(School.Tract.Info)


saveRDS(df.2019.TX, "Data2/Data.For.Modeling.rds")

