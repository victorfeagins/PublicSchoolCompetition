#This script is some models of my data 

#Packages ----
library(dplyr)
library(ggplot2)
library(stringr)

## Spatial libraries ----
library(sf)
library(spdep)



# Reading in Data -----
df <- readRDS("Data2/Data.For.Modeling.rds")

df %>% 
  summarise_all(~(sum(is.na(.))))


# Constructing neighborhood -----


knn4 <- knearneigh(df, k = 4) %>% 
  knn2nb()

knn4.wts<-nb2listw(neighbours = knn4, style = "W")



# Assessing Moran's I ----

moran.test(df$Hispanic.Percent, knn4.wts)
#For Hispanic Percent we see a significant slight positive autocorrelation


moran.test(df$Black.Percent, knn4.wts)
#For Black Percent we see a significant slight positive autocorrelation

moran.test(df$Asian.Percent, knn4.wts)
#For Asian Percent we see a significant slight positive autocorrelation

moran.test(df$White.Percent, knn4.wts)
#For White Percent we see a significant slight positive autocorrelation


moran.test(df$Free.Lunch.Percent, knn4.wts, na.action	= na.omit, zero.policy = TRUE)
#For Free.Lunch.Percent Percent we see a significant slight positive autocorrelation


#

Local_Moran <- function(vector, listw)
{
  Local<- localmoran(vector, knn4.wts)
  Factor <- attr(Local, "quadr")$mean
  P.value <- Local[,5]
  
  data.frame(Factor, P.value) %>% 
    mutate(sig = as.factor(ifelse(P.value < .05, "Sig", "NotSig")))
}




Local.Hispanic <- Local_Moran(df$Hispanic.Percent, knn4.wts)
  
summary(Local.Hispanic)

table(Local.Hispanic$Factor, Local.Hispanic$sig)
