

#upload files with different temparature

library(readxl)
Length_weight_data_sampling <- read_excel("Length_weight_data_sampling.xlsx" , sheet = 1)
View(Length_weight_data_sampling) 


library(readxl)
thirtytwo <- read_excel("Length_weight_data_sampling.xlsx" , sheet = 1)
View(thirtytwo) 

library(readxl)
twentyeight <- read_excel("Length_weight_data_sampling.xlsx" , sheet = 2)
View(twentyeight)

library(readxl)
twentyfour <- read_excel("Length_weight_data_sampling.xlsx" , sheet = 3)
View(twentyfour)

library(readxl)
threegroupweight <- read_excel("Length_weight_data_sampling.xlsx" , sheet = 4)
View(threegroupweight)

library(readxl)
groups <- read_excel("Length_weight_data_sampling.xlsx" , sheet = 5)
View(groups)


rm(groups)

library(readxl)
Combine <- read_excel("Length_weight_data_sampling.xlsx" , sheet = 6)
View(Combine)

library(car)
install.packages("carData")

library(psych)


Combine # inspect data
head(Combine)
summary(Combine$Total_length_32) # mean 1
summary(Combine$Total_length_28) # mean 2
summary(Combine$Total_length_24) # mean 3


summary(Combine$Standard_length_32) # mean 1
summary(Combine$Standard_length_28) # mean 2
summary(Combine$Standard_Length_24) # mean 3



summary(Combine$Weight_32) # mean 1
summary(Combine$Weight_28) # mean 2
summary(Combine$Weight_24) # mean 3


t.test(Combine$Total_length_32, Combine$Total_length_28, paired = TRUE, alternative ="two.sided" )
t.test(Combine$Total_length_28, Combine$Total_length_24, paired = TRUE, alternative ="two.sided" )
t.test(Combine$Total_length_32, Combine$Total_length_24, paired = TRUE, alternative ="two.sided" )


t.test(Combine$Standard_length_32, Combine$Standard_length_28, paired = TRUE, alternative ="two.sided" )
t.test(Combine$Standard_length_28, Combine$Standard_Length_24, paired = TRUE, alternative ="two.sided" )
t.test(Combine$Standard_length_32, Combine$Standard_Length_24, paired = TRUE, alternative ="two.sided" )


t.test(Combine$Weight_32, Combine$Weight_28, paired = TRUE, alternative ="two.sided" )
t.test(Combine$Weight_28, Combine$Weight_24, paired = TRUE, alternative ="two.sided" )
t.test(Combine$Weight_32, Combine$Weight_24, paired = TRUE, alternative ="two.sided" )



shapiro.test(Combine$Total_length_32)  
shapiro.test(Combine$Total_length_28)  
shapiro.test(Combine$Total_length_24)  


shapiro.test(Combine$Standard_length_32)  
shapiro.test(Combine$Standard_length_28)  
shapiro.test(Combine$Standard_Length_24)  


shapiro.test(Combine$Weight_32)
shapiro.test(Combine$Weight_28)
shapiro.test(Combine$Weight_24)




####### DATA VISUALIZATION


tapply(groups$Weight, groups$Temperature, mean) 

#temprature 24 ??C    28 ??C    32 ??C 
# Mean      28.98598 33.31651 38.21393 
# There are no significant differences in the number of weights among the three different temparature

boxplot(groups$Weight ~ groups$Temperature, xlab="", ylab="Total Weight in (gm)")# Boxplot of total weight per level of Temparature


###### TESTING ASSUMPTIONS

### NORMALITY OF RESIDUALS
# We test if the weight in different temparature are normally distributed.
# Using the Shapiro-Wilk test, qqplot and a histogram.

hist(Combine$Weight_32,
     main = "",
     xlab = "Weight of 32 degree celsious temparature") # symmetric and with no outliers

hist(Combine$Weight_28,
     main = "",
     xlab = "Weight of 28 degree celsious temparature") # symmetric and with no outliers
     
hist(Combine$Weight_24,
     main = "",
     xlab = "Weight of 24 degree celsious temparature") # symmetric and with no outliers     


shapiro.test(Combine$Weight_32) # W = 0.994 > 0.90 OK Normally Distributed
shapiro.test(Combine$Weight_28) # W = 0.983 > 0.90 OK Normally Distributed
shapiro.test(Combine$Weight_24) # W = 0.992 > 0.90 OK Normally Distributed

# The histograms are not too asymmetric. considered normally distributed
# However, shapiro test suggest that the assumptions of normality is not violated for all the temparature group



### HOMOGENEITY OF VARIANCES
# We then test if the temparatue have an equal variance
# Equal variances means there is no significant differences between the variances in the population
# Using Levene's test

leveneTest(groups$Weight ~ groups$Temperature) # Pvalue < 0.05, variances are significantly different from each other

# Variance are equal referring to the H0: weight in different temeperature =0 and HA: weight in different temeperature???0
# Pvalue < 0.05, meaning the H0 of equal variances is rejected
# rule of thumb: if ratio of highest to lowest variance is < 5, variances are sufficiently close to equality 

max(by(groups$Weight, groups$Temperature,sd))^2/min(by(groups$Weight, groups$Temperature,sd))^2
by(groups$Weight, groups$Temperature,sd)^2


#groups$Temperature: 24 ??C
#[1] 41.86365
#groups$Temperature: 28 ??C
#[1] 58.92701
#groups$Temperature: 32 ??C
#[1] 79.20


# The ratio is 1.892071, so we can assume that these are approximately equal variances. sufficiently close to homogenity
# Concluded that there is no significant differences between the variances in the population

bartlett.test(groups$Weight ~ groups$Temperature)

#Bartlett test of homogeneity of variances
#data:  groups$Weight by groups$Temperature
#Bartlett's K-squared = 20.738, df = 2, p-value = 3.139e-05

# Concluded that there is no significant differences between the variances in the population

kruskal.test(groups$Weight ~ groups$Temperature)

#Kruskal-Wallis rank sum test

#data:  groups$Weight by groups$Temperature
#Kruskal-Wallis chi-squared = 116.37, df = 2, p-value < 2.2e-16


###### STATISTICAL TEST/MODEL
# We proceed by checking whether or not there is a difference in weight in different temperature 

t.test(Combine$Weight_32, Combine$Weight_28, paired = TRUE, alternative ="two.sided" )
t.test(Combine$Weight_28, Combine$Weight_24, paired = TRUE, alternative ="two.sided" )
t.test(Combine$Weight_32, Combine$Weight_24, paired = TRUE, alternative ="two.sided" )


# Pvalue < 0.05, meaning the Pvalue is significant and we reject the H0
# Concluded that there is significant differences in the weight of diferent temparature 

difftemperature <- Combine[,c(5, 11, 17)] # Extracted columns of four spines and length
difftemperature <- na.omit (difftemperature) # Missing data from extracted columns were omitted

difftemperature <- Combine[,c(5, 11, 17)] # Extracted columns of four spines and length
difftemperature1 <- Combine[,c(5, 11, 17)] # Extracted columns of four spines and length

####### DATA VISUALIZATION
# Data visualisation for a difference
# Scatterplot matrix
pairs.panels(difftemperature,lm=T,smooth=F,ellipses=F)


pairs.panels(difftemperature1,lm=T,smooth=F,ellipses=F)

cor.test(Combine$Weight_32, Combine$Weight_28)
cor.test(Combine$Weight_28, Combine$Weight_24)
cor.test(Combine$Weight_32, Combine$Weight_24)

cor=corr.test(difftemperature, use="pairwise", method="pearson", adjust="none")
cor$ci

# Conclusion: There is no significant positive correlation in differnt temperature level



tapply(groups$Weight, groups$Replicate, mean) 
#Replicates R1       R2       R3 
# Mean 33.00505 34.61792 32.61435 

boxplot(groups$Weight ~ groups$Replicate, xlab="", ylab="Total Weight in (gm)")# Boxplot of total weight in different Replicate

max(by(groups$Weight, groups$Replicate,sd))^2/min(by(groups$Weight, groups$Replicate,sd))^2
by(groups$Weight, groups$Replicate,sd)^2

tapply(Combine$Weight_32, Combine$Replicate_32, mean) 

boxplot(Combine$Weight_32 ~ Combine$Replicate_32, xlab="", ylab="Total Weight in (gm)")

tapply(Combine$Weight_28, Combine$Replicate_28, mean) 
boxplot(Combine$Weight_28 ~ Combine$Replicate_28, xlab="", ylab="Total Weight in (gm)")

tapply(Combine$Weight_24, Combine$Replicate_24, mean) 
boxplot(Combine$Weight_24 ~ Combine$Replicate_24, xlab="", ylab="Total Weight in (gm)")
