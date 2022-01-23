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

library(readxl)
library(car)
library(psych)
# save date to .csv format 
write.csv(BI300F_2021_Assignment1,file="mydata1.csv")


#effect of season on rainfall

# 1. Was there more rainfall in the months of the rainy season than in the months of the dry season?
# 2. Are there indications that GSI or total copepod biomass correlates with rainfall
#     or differs between the rainy and the dry season?
# 3. Is there a relationship between GSI and total copepod biomass?

# Question 1
mydata <- read.csv("mydata1.csv")


# Calculate mean of the rainfall in different seasons ####

mydata%>% 
  select(Season, Rainfall) %>% 
  group_by(Season)%>% 
  na.omit()%>%
  summarise(Avarage_rainfall = mean(Rainfall))   # dry = 36 and rainy = 90.542

# Data visualization ####
df1 <- mydata %>% 
  select(Season, Rainfall) %>% 
  na.omit()

boxplot(Rainfall ~ Season, data = df1)

#boxplot(Rainfall~Season,ylab="Rainfall (mm)",col="lightblue",data=mydata)


#####Statistical analysis to test if the mean difference between the dry and rainy is ####
#significance or not, for that we used t test and levenetest 

leveneTest(Rainfall~Season,data=mydata)


t.test(Rainfall~Season,var.equal=TRUE,alternative="two.sided",
       data=df1) # p-value = 0.0003882

by(df1$Rainfall,df1$Season,sd)

## this analysis indicates that there were more rainfall in rainy season than dry season 
##p value= 0.0003882 < 0.05 that means this differnce is statistically significance.

# Question No 2. Are there indications that GSI or total copepod biomass correlates with rainfall ####
#   or differs between the rainy and the dry season?

#make a data frame for rainfall, toatal copepod and season
df2 <- mydata %>% 
  select(Season, Rainfall,TotCop) %>% 
  na.omit()

df3 <- mydata[,c(8,9)]
df3 <- na.omit(df3)

fit0 = aov(Rainfall ~ TotCop+Season, data = df2)
summary(fit0)
plot(allEffects(mod=fit0))


df10 <- mydata[,c(3,4,5,6,7,9)]
df10 <- na.omit(df10)



fit1 = aov(StanMGSI ~ TotCop+Season, data = df10)
summary(fit1)
plot(allEffects(mod=fit1))

fit2 = aov(StanFGSI ~ TotCop+Season, data = df10)
summary(fit2)
plot(allEffects(mod=fit2))

fit3 = aov(LmioMGSI ~ TotCop+Season, data = df10)
summary(fit3)
plot(allEffects(mod=fit3))

fit4 = aov(LmioFGSI ~ TotCop+Season, data = df10)
summary(fit4)
plot(allEffects(mod=fit4))

#bwplot(StanMGSI ~ TotCop+Season, data=df10, fill="blue")

####### DATA VISUALIZATION
# Data visualisation for a difference
# Scatterplot matrix
pairs.panels(df4,lm=T,smooth=F,ellipses=F)

cor.test(df4$TotCop, df4$Rainfall)

cor=corr.test(df4, use="pairwise", method="pearson", adjust="none")
cor$ci



plot(Rainfall ~ TotCop, data = df2)

summary(lm(Rainfall ~ TotCop, data = df2))




df5 <- mydata[,c(4,5,6,7,9)]
df6 <- na.omit(df5)

# Question No 3####### DATA VISUALIZATION
# Data visualisation for a difference
# Scatterplot matrix
pairs.panels(df6,lm=T,smooth=F,ellipses=F)


cor=corr.test(df6, use="pairwise", method="pearson", adjust="none")
cor$ci





 
tapply(df2$Rainfall, df2$Season, mean)

tapply(df2$TotCop, df2$Season, mean)

boxplot(df2$TotCop ~ df2$Season, xlab="", ylab="Total Copped biomass") # Boxplot of total
#copped biomass in different season

# Data visualization 
df2 %>% 
  ggplot(aes(x=Rainfall,y=TotCop))+
  geom_point(alpha=0.03)+
  geom_smooth(method = lm)


df2 %>% 
  ggplot(aes(x=Rainfall,y=TotCop, col = Season))+
  geom_point(alpha=0.03)+
  geom_smooth(method = lm)+
  facet_wrap(~Season)



###### TESTING ASSUMPTIONS
### NORMALITY OF RESIDUALS


# We test if the rainfall between the seasons are normally distributed.
# Using the Shapiro-Wilk test, qqplot and a histogram

hist(df2$Rainfall[df2$Season=="rainy"],nclass=6,
     main = "",
     xlab = "Rainfall ") # Not too asymmetric and with outliers



hist(df2$Rainfall[df2$Season=="dry"],nclass=6,
     main = "",
     xlab = "Rainfall ") # Not too asymmetric and with outliers

shapiro.test(df2$Rainfall[df2$Season=="rainy"]) # W = 0.83 > 0.9
shapiro.test(df2$Rainfall[df2$Season=="dry"]) # W = 0.95 < 0.9

# The histograms are not asymmetric for rainy season 
# The histograms are not asymmetric for dry season 

### HOMOGENEITY OF VARIANCES

# Using Levene's test

leveneTest(df2$Rainfall ~ df2$Season) # Pvalue 0.0043 < 0.05, variances are
#significantly different from each other







bwplot(StanMGSI~Rainfall*Season, data=mydata, fill="Blue")

fit=aov(TotCop~Rainfall,data=mydata)
plot(allEffects(mod=fit))
summary(fit)

fit1=aov(StanMGSI~Rainfall,data=mydata)
plot(allEffects(mod=fit1))
summary(fit1)

fit2=aov(StanFGSI~Rainfall,data=mydata)
plot(allEffects(mod=fit2))
summary(fit2)

fit3=aov(LmioMGSI~Rainfall,data=mydata)
plot(allEffects(mod=fit3))
summary(fit3)

fit4=aov(LmioFGSI~Rainfall,data=mydata)
plot(allEffects(mod=fit4))
summary(fit4)

bwplot(mydata$TotCop~mydata, fill = "Blue", xlab ="Rain", ylab = "TotCop")



#boxplot(TotCop~Rainfall+Season, data=BI300F_2021_Assignment1, fill="Blue")
fit5=aov(TotCop~Rainfall+Season, data=df2)
summary(fit5)
plot(allEffects(mod=fit5))

cd=cooks.distance(fit1)
inflobs=which(cd>1)
inflobs # empty, so no outliers here
influenceIndexPlot(fit1,vars= "Cook")

# Posthoc tests
TukeyHSD(fit1) # No significant difference comparing means


bwplot(TotCop~Rainfall|Season, data=df2, fill="Blue")
