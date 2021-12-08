
#Packages ----
library(dplyr)
library(ggplot2)
library(stringr)

## Spatial libraries ----
library(sf)
library(spdep)
library(tigris)

## Modeling
library(car)
library(MASS)
library(spatialreg)


tx.boundery <- states(cb = TRUE, year = 2019)

tx.boundery<- tx.boundery %>% 
  filter(STUSPS == "TX") %>%
  st_transform(crs = "EPSG:3081")


df <- readRDS("Data2/Data.For.Modeling.rds")

df.model <- df %>% 
  mutate(across(.cols = ends_with("E", ignore.case = FALSE) & !c(TotalE,Median.Income.AvgE) , 
                .names = "{.col}.Percent",
                .fns = ~ .x/TotalE)) %>% 
  mutate(Charter.School.Status = ifelse(Charter.School.Status == "1-Yes", 1, 0))



#Statistics ----

table(df.model$Charter.School.Status)


### Plotting ----
df.model %>%
  ggplot()+
  geom_sf(data=tx.boundery)+
  geom_sf(mapping = aes(col = as.factor(Charter.School.Status))) +
  labs(title = "Charter School Locations in Texas",
       col = "Charter School Status")

# I don't know how to understand these plots.
ggplot(df.model) +
  geom_density(aes(x = Hispanic.Percent, fill = as.factor(Charter.School.Status)), alpha=0.6)

ggplot(df.model) +
  geom_density(aes(x = White.Percent, fill = as.factor(Charter.School.Status)), alpha=0.6)

ggplot(df.model) +
  geom_density(aes(x = Black.Percent, fill = as.factor(Charter.School.Status)), alpha=0.6)

#Modeling ----


# Checking Poisson Regression ----
Hispanic.formula <- Hispanic.Students ~  HispanicE.Percent + YearsActive + Median.Income.AvgE + offset(log(Total.Students))

Hispanic.model.poisson <- glm(formula = Hispanic.formula, 
    family = "poisson",
    data = df.model)

summary(Hispanic.model.poisson)

sqrt(Hispanic.model.poisson$deviance/Hispanic.model.poisson$df.residual) #Poisson process too over disperesed



# Negative Binomial ----
Hispanic.model.nb <- glm.nb(formula = Hispanic.formula, 
                      data = df.model)

summary(Hispanic.model.nb)

#Works Better
Hispanic.formula.charter.school <- Hispanic.Students ~  HispanicE.Percent*Charter.School.Status + YearsActive + Median.Income.AvgE + offset(log(Total.Students)) + Charter.School.Status


Hispanic.model.nb.c <- glm.nb(formula = Hispanic.formula.charter.school, 
                            data = df.model)

summary(Hispanic.model.nb.c) #AIC goes down

#Charter School interaction seems to influence how Hispanic tract information effects the enrollment looks negative


#Creating Auto covariate Variables -----

knn4 <- knearneigh(df.model, k = 4) %>% 
  knn2nb()

knn4.wts<-nb2listw(neighbours = knn4, style = "W")


df.model <- df.model %>% 
  mutate(Lag.Hispanic.Rate = lag.listw(x = knn4.wts, var = (Hispanic.Students/Total.Students)),
         Lag.Black.Rate = lag.listw(x = knn4.wts, var = (Black.Students/Total.Students)))



#Hispanic Students ----
Hispanic.formula.charter.school.auto <- Hispanic.Students ~  HispanicE.Percent*Charter.School.Status + YearsActive + Median.Income.AvgE + offset(log(Total.Students)) + Charter.School.Status + Lag.Hispanic.Rate

Hispanic.model.nb.c.auto <- glm.nb(formula = Hispanic.formula.charter.school.auto, 
                              data = df.model)

summary(Hispanic.model.nb.c.auto)



# Black Students ----

Black.formula.charter.school <- Black.Students ~  NH.BlackE.Percent*Charter.School.Status + YearsActive + Median.Income.AvgE + offset(log(Total.Students)) + Charter.School.Status

Black.model.nb.c <- glm.nb(formula = Black.formula.charter.school, 
                                   data = df.model)

summary(Black.model.nb.c)



Black.formula.charter.school.auto <- Black.Students ~  NH.BlackE.Percent*Charter.School.Status + YearsActive + Median.Income.AvgE + offset(log(Total.Students)) + Charter.School.Status + Lag.Black.Rate

Black.model.nb.c.auto <- glm.nb(formula = Black.formula.charter.school.auto, 
                           data = df.model)

summary(Black.model.nb.c.auto)


