library(readxl)
Assignmnet_1_2021 <- read_excel("C:/Users/16056/OneDrive/Desktop/Riaz Thesis/Assignmnet_1_2021.xlsx")
View(Assignmnet_1_2021)



library(afex)
library(car)
library(effects)
library(multcomp)
library(doBy)
library(lattice)
library(nlme)
library(MASS)
library(lsmeans)
library(ggplot2)
library(ggthemes)
library (tidyverse)
library(dplyr)

library(latticeExtra)
library(rockchalk)
library(carData)


library(readxl)
library(car)
library(psych)


mydata%>% 
  select(Season, Rainfall) %>% 
  group_by(Season)%>% 
  na.omit()%>%
  summarise(Avarage_rainfall = mean(Rainfall))   # dry = 36 and rainy = 90.542
Assignmnet_1_2021 <- na.omit(mydata)


Assignmnet_1_2021%>% 
  select(Season, Rainfall) %>% 
  group_by(Season)%>% 
  na.omit()%>%
  summarise(Avarage_rainfall = mean(Rainfall))   # dry = 36 and rainy = 90.542

mydata <- na.omit(Assignmnet_1_2021)


head(Assignmnet_1_2021)
mean(Assignmnet_1_2021$Rainfall) # mean 1



qplot(x=Season, data=subset(Assignmnet_1_2021,!is.na(Rainfall)),
      binwidth=10)+
  scale_x_continuous(lim=c(0, 1000), breaks = seq(0, 1000, 50))+
  facet_wrap(~Rainfall)


df1 <- mydata %>% 
  select(Season, Rainfall) %>% 
  na.omit()

      
