
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



df <- readRDS("Data2/Data.For.Modeling.rds")

df.model <- df %>% 
  mutate(across(.cols = ends_with("E", ignore.case = FALSE) & !c(TotalE,Median.Income.AvgE) , 
                .names = "{.col}.Percent",
                .fns = ~ .x/TotalE)) %>% 
  mutate(Charter.School.Status = ifelse(Charter.School.Status == "1-Yes", 1, 0))



#Modeling ----


# Checking Poisson Regression ----
Hispanic.formula <- Hispanic.Students ~  HispanicE.Percent + YearsActive + Median.Income.AvgE + offset(log(Total.Students))

Hispanic.model.poisson <- glm(formula = Hispanic.formula, 
    family = "poisson",
    data = df.model)

summary(Hispanic.model.poisson)

sqrt(Hispanic.model.poisson$deviance/Hispanic.model.poisson$df.residual) #Poisson process too over disperesed



# Negative Binomial 
Hispanic.model.nb <- glm.nb(formula = Hispanic.formula, 
                      data = df.model)

summary(Hispanic.model.nb)

#Works Better
Hispanic.formula.charter.school <- Hispanic.Students ~  HispanicE.Percent*Charter.School.Status + YearsActive + Median.Income.AvgE + offset(log(Total.Students)) + Charter.School.Status


Hispanic.model.nb.c <- glm.nb(formula = Hispanic.formula.charter.school, 
                            data = df.model)

summary(Hispanic.model.nb.c)

#Charter School interaction seems to influence how Hispanic tract information effects the enrollment and negativelly





