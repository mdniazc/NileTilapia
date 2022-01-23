library(readxl)
sampling6thsep2021 <- read_excel("C:/Users/16056/OneDrive/Desktop/Riaz Thesis/sampling6thsep2021.xlsx")
View(sampling6thsep2021)


library(car)
install.packages("carData")
library(psych)

library(readxl)
library(car)
library(psych)




head(sampling6thsep2021)
summary(sampling6thsep2021$Total_Length) # mean 1

library(gmodels)
CrossTable (sampling6thsep2021$Total_Length, sampling6thsep2021$Temperature )



fit=aov(Weight~Temparature, data=sampling6thsep2021)
summary.lm(fit)




# 3-Way Frequency Table
mytable <- xtabs(~Total_Length+Temperature, data=sampling6thsep2021)
ftable(mytable) # print table
summary(mytable) # chi-square test of indepedence


# 2-Way Frequency Table

attach(sampling6thsep2021)
mytable <- table(Total_Length,Temperature) # A will be rows, B will be columns
mytable # print table



# with(data, expression)
# example applying a t-test to a data frame mydata


library (tidyverse)


attach(sampling6thsep2021)
newdata <- sampling6thsep2021[ which(Temperature==24),]
detach(sampling6thsep2021)
View(newdata)



attach(sampling6thsep2021)
newdata28 <- sampling6thsep2021[ which(Temperature==28),]
detach(sampling6thsep2021)
View(newdata28)



attach(sampling6thsep2021)
newdata32 <- sampling6thsep2021[ which(Temperature==32),]
detach(sampling6thsep2021)
View(newdata32)

#Remove observations from the data set....

install.packages("dplyr")

library(dplyr)


newdata28[newdata28$ID !="F62", ]->newdata28A
newdata32[newdata32$ID !="F21", ]->newdata32A

sampling6thsep2021[sampling6thsep2021$ID !="F62" & sampling6thsep2021$ID !="F21" , ]->septexperiment



t.test(newdata32A$Weight, newdata28A$Weight, paired = TRUE, alternative ="two.sided")
t.test(newdata28A$Weight, newdata$Weight, paired = TRUE, alternative ="two.sided")
t.test(newdata32A$Weight, newdata$Weight, paired = TRUE, alternative ="two.sided")



shapiro.test(newdata$Weight)
shapiro.test(newdata28A$Weight)
shapiro.test(newdata32A$Weight)


leveneTest(newdata$Weight ~ newdata$Temperature)
leveneTest(newdata2A8$Weight ~ newdata28A$Temperature)
leveneTest(newdata32A$Weight ~ newdata32A$Temperature)


max(by(newdata$Weight, newdata$Temperature,sd))^2/min(by(newdata$Weight, newdata$Temperature,sd))^2
by(newdata$Weight, newdata$Temperature,sd)^2

max(by(newdata28A$Weight, newdata28A$Temperature,sd))^2/min(by(newdata28A$Weight, newdata28A$Temperature,sd))^2
by(newdata28A$Weight, newdata28A$Temperature,sd)^2

max(by(newdata32A$Weight, newdata32A$Temperature,sd))^2/min(by(newdata32A$Weight, newdata32A$Temperature,sd))^2
by(newdata32A$Weight, newdata32A$Temperature,sd)^2

Homogenity of Variance 

bartlett.test(sampling6thsep2021$Weight ~sampling6thsep2021$Temperature)

#### model omitted missing data###
newdata <- newdata24[,c(1, 2, 3, 4, 5 ,6 , 7)] # Extracted columns
newdata<- na.omit (newdata) # NA from extracted columns were omitted


newdata28 <- newdata28A[,c(1, 2, 3, 4, 5 ,6 , 7)] # Extracted columns
newdata28<- na.omit (newdata28) # NA from extracted columns were omitted


#Data Visuslization#

mean(newdata$Weight) #181.4467
mean(newdata28$Weight) #193.1613
mean(newdata32$Weight) #193.3226


boxplot(newdata$Weight, newdata28$Weight, names = c("wight 24 degree celsious","Weight in 28 degree celsious"), ylab=" gram (gm) ")

boxplot(newdata28A$Weight, newdata32A$Weight, names = c("wight 28 degree celsious","Weight in 32 degree celsious"), ylab=" gram (gm) ")


boxplot(newdata$Weight, newdata32A$Weight, names = c("wight 24 degree celsious","Weight in 32 degree celsious"), ylab=" gram (gm) ")



#Testing Assumptions #

# We test if the weight of the fish both 24 and 28 degress are normally distributed.

# Using the Shapiro-Wilk test, qqplot and a histogram.


hist(newdata$Weight,
     main = "",
     xlab = " weight fish for 24 degree celsious (gm)") # Not too asymmetric and with outliers


hist(newdata28$Weight,
     main = "",
     xlab = " weight fish for 28 degree celsious (gm)") # Not too asymmetric and with outliers


hist(newdata32$Weight,
     main = "",
     xlab = " weight fish for 32 degree celsious (gm)") # Not too asymmetric and with outliers


shapiro.test(newdata$Weight-mean(newdata$Weight)) # W = 0.96674> 0.90 OK Normally Distributed

shapiro.test(newdata28$Weight-mean(newdata28$Weight)) # W = 0.9622 > 0.90 OK Normally Distributed

shapiro.test(newdata32$Weight-mean(newdata32$Weight)) #W = 0.96389, > 0.90 OK Normally Distributed


# The histograms and qqplots are not too asymmetric, considered normally distributed
# Also, shapiro test suggest that the assumptions of normality is OK for both the left and right pelvic spine length.


### HOMOGENEITY OF VARIANCES

# We then test if the length of the pelvic spine of both left and right have an equal variance
# Equal variances means there is no significant mean differences between the variances in the population
# Using Levene's test

leveneTest(newdata$Weight, newdata28$Weight) #, significant deviation from equal variances


#Variance are equal referring to the H0: Mean difference = 0 and HA: Mean difference ??? 0
#Violated
#Pvalue < 0.05 (significantly different), meaning the H0 of equal variances is rejected
#Concluded that there is significant differences between the variances in the population


###### STATISTICAL TEST/MODEL
### PAIRED/DEPENDENT T-TEST (PARAMETRIC)
# Use Paired t-test to test if the pelvic spine length is significantly different between the symmetry


t.test(newdata$Weight, newdata28A$Weight, paired=TRUE, var.equal=FALSE, alternative="two.sided")
#Pvalue = 0.2922
#Pvalue > 0.05, meaning the Pvalue is not significant and we can accept the H0
# Conclusion: There is no significant difference in nite litapia weight between the symmetry (24 and 28) 
#sYMMETRICAL

t.test(newdata28A$Weight, newdata32A$Weight, paired=TRUE, var.equal=FALSE, alternative="two.sided")

#Pvalue = 0.24041
#Pvalue > 0.05, meaning the Pvalue is not significant and we can accept the H0
# Conclusion: There is no significant difference in nite litapia weight between the symmetry (28 and 32 degree celsius) 
#sYMMETRICAL


t.test(newdata32A$Weight, newdata$Weight, paired=TRUE, var.equal=FALSE, alternative="two.sided")

#Pvalue = 0.81195
#Pvalue > 0.05, meaning the Pvalue is not significant and we can accept the H0
# Conclusion: There is no significant difference in nite litapia weight between the symmetry (24 and 32 degree celsius) 
#sYMMETRICAL


# Ttest was still used since the assumptions of normality was met, however the homogeneity of variances is violated.
# In this case,Paired Samples T test with Unequal Variance was used where the var.equal is FALSE. If the assumptions
#of normality is violated, then we can use
# the non-parametric counterpart which is the Wilcoxon Paired Test






# 2.) Do males and females differ in Weight?

#UNPAIRED T-TEST
# H0: Number of plates (Males and Females) = 0
# HA: Number of plates (Males and Females) ??? 0



####### DATA VISUALIZATION


tapply(septexperiment$Weight, septexperiment$Sex, mean)




# There is no significant differences in weight between the two sexes (males and females).
# Data visualisation for a difference

boxplot(septexperiment$Weight ~ septexperiment$Sex, xlab="", ylab="Average Weight")
# Boxplot  of total plates per level of sex


###### TESTING ASSUMPTIONS
### NORMALITY OF RESIDUALS
# We test if the weight of both sexes are normally distributed.
# Using the Shapiro-Wilk test, qqplot and a histogram.


hist(septexperiment$Weight[septexperiment$Sex=="M"],nclass=6,
     main = "",
     xlab = "Total weight of males") # Not too symmetric and with no outliers


hist(septexperiment$Weight[septexperiment$Sex=="F"],nclass=6,
     main = "",
     xlab = "Total weight of females") # Not too symmetric and with no outliers


shapiro.test(septexperiment$Weight[septexperiment$Sex=="M"]) # W = 0.98322> 0.9#p-value = 0.7394


shapiro.test(septexperiment$Weight[septexperiment$Sex=="F"]) #W = 0.97683 #p-value = 0.5119

# The histograms are not symmetric
# However, shapiro test suggest that the assumptions of normality is not violated for both the males and females


### HOMOGENEITY OF VARIANCES

# We then test if the sexes have an equal variance
# Equal variances means there is no significant differences between the variances in the population
# Using Levene's test

leveneTest(septexperiment$Weight ~ septexperiment$Sex)

# Variance are equal referring to the 
#H0: average weight(males and females)=0 and HA: average of weight(males and females)???0
# Pvalue < 0.05, meaning the H0 of equal variances is rejected
# rule of thumb: if ratio of highest to lowest variance is < 5, variances are sufficiently close to equality

max(by(septexperiment$Weight,septexperiment$Sex,sd))^2/min(by(septexperiment$Weight,septexperiment$Sex,sd))^2
by(septexperiment$Weight,septexperiment$Se,sd)^2


# The ratio is 1.72, so we can assume that these are approximately equal variances

# Concluded that there is no significant differences between the variances in the population
# In summary, the assumption of normality is violated because
# there are both males and females with. In this case,non-parametric test
# is more fitted



###### STATISTICAL TEST/MODEL
# We proceed by checking whether or not there is a difference in weight between females and males with
# the non-parametric unpaired t-test, using the function wilcox.test().



### MANN-WHITNEY U-TEST (NONPARAMETRIC)
# Use Mann-Whitney Unpaired u-test

wilcox.test(Weight~Sex,paired=FALSE, var.equal=TRUE,alternative="two.sided",data=septexperiment)

#Wilcoxon rank sum test with continuity correction
#data:  Weight by Sex
#W = 145.5, p-value = 2.737e-12
#alternative hypothesis: true location shift is not equal to 0

# Median differences referring to the H0: Female-Male=0 and HA: Female-Male???0
# Pvalue 2.737e-12 <0.05, meaning the Pvalue is significant and we can not accept the H0
# Concluded that there is significant differences in the number of weight between the two sexes


#pearson correlation# 

cor.test(newdata$Weight, newdata28A$Weight)

cor.test(newdata28A$Weight, newdata32A$Weight)

cor.test(newdata32A$Weight, newdata$Weight)

# Conclusion: There is a significant positive correlation between the weight of different temaparature



# 3.) Do males and females differ in Weight?

#UNPAIRED T-TEST
# H0: Number of plates (Males and Females) = 0
# HA: Number of plates (Males and Females) ??? 0



####### DATA VISUALIZATION


tapply(newdata$Weight, newdata$Sex, mean)
tapply(newdata28A$Weight, newdata28A$Sex, mean)
tapply(newdata32A$Weight, newdata32A$Sex, mean)
tapply(septexperiment$Weight, septexperiment$Sex, mean)


# There is no significant differences in weight between the two sexes (males and females).
# Data visualisation for a difference

boxplot(newdata$Weight ~ newdata$Sex, xlab="", ylab="Average Weight of 24 tem")
# Boxplot  of weight per level of sex

boxplot(newdata28A$Weight ~ newdata28A$Sex, xlab="", ylab="Average Weight 28 deg tem")
# Boxplot  of weight per level of sex

boxplot(newdata32A$Weight ~ newdata32A$Sex, xlab="", ylab="Average Weight 32 deg tem")
# Boxplot  of weight per level of sex



###### TESTING ASSUMPTIONS
### NORMALITY OF RESIDUALS
# We test if the weight of both sexes are normally distributed.
# Using the Shapiro-Wilk test, qqplot and a histogram.


hist(septexperiment$Weight[septexperiment$Sex=="M"],nclass=6,
     main = "",
     xlab = "Total weight of males") # Not too symmetric and with no outliers


hist(septexperiment$Weight[septexperiment$Sex=="F"],nclass=6,
     main = "",
     xlab = "Total weight of females") # Not too symmetric and with no outliers


shapiro.test(septexperiment$Weight[septexperiment$Sex=="M"]) # W = 0.98322> 0.9#p-value = 0.7394


shapiro.test(septexperiment$Weight[septexperiment$Sex=="F"]) #W = 0.97683 #p-value = 0.5119



#24 degree tempara

hist(newdata$Weight[newdata$Sex=="M"],nclass=6,
     main = "",
     xlab = "Total weight of males 24") # Not too symmetric and with no outliers


hist(newdata$Weight[newdata$Sex=="F"],nclass=6,
     main = "",
     xlab = "Total weight of females 24") # Not too symmetric and with no outliers


shapiro.test(newdata$Weight[newdata$Sex=="M"]) # W = 0.94994, p-value = 0.4888


shapiro.test(newdata$Weight[newdata$Sex=="F"]) #W = 0.96533, p-value = 0.8088


#28 degree celsious temparature


hist(newdata28A$Weight[newdata28A$Sex=="M"],nclass=6,
     main = "",
     xlab = "Total weight of males 28") # Not too symmetric and with no outliers


hist(newdata28A$Weight[newdata28A$Sex=="F"],nclass=6,
     main = "",
     xlab = "Total weight of females 28") # Not too symmetric and with no outliers


shapiro.test(newdata28A$Weight[newdata28A$Sex=="M"]) # W = 0.92613, p-value = 0.1874


shapiro.test(newdata28A$Weight[newdata28A$Sex=="F"]) #W = 0.84459, p-value = 0.02427


#32 degree celsious temparature


hist(newdata32A$Weight[newdata32A$Sex=="M"],nclass=6,
     main = "",
     xlab = "Total weight of males 32") # Not too symmetric and with no outliers


hist(newdata32A$Weight[newdata28A$Sex=="F"],nclass=6,
     main = "",
     xlab = "Total weight of females 32") # Not too symmetric and with no outliers


shapiro.test(newdata32A$Weight[newdata32A$Sex=="M"]) # W = 0.97389, p-value = 0.936

shapiro.test(newdata32A$Weight[newdata32A$Sex=="F"]) #W = 0.97598, p-value = 0.9119



# The histograms are not symmetric
# However, shapiro test suggest that the assumptions of normality is not violated for both the males and females


### HOMOGENEITY OF VARIANCES

# We then test if the sexes have an equal variance
# Equal variances means there is no significant differences between the variances in the population
# Using Levene's test

leveneTest(newdata$Weight ~ newdata$Sex)
leveneTest(newdata28A$Weight ~ newdata28A$Sex)
leveneTest(newdata32A$Weight ~ newdata32A$Sex)


# Variance are equal referring to the 
#H0: average weight(males and females)=0 and HA: average of weight(males and females)???0
# Pvalue < 0.05, meaning the H0 of equal variances is rejected
# rule of thumb: if ratio of highest to lowest variance is < 5, variances are sufficiently close to equality

#24 degree temparature 

max(by(newdata$Weight,newdata$Sex,sd))^2/min(by(newdata$Weight,newdata$Sex,sd))^2
by(newdata$Weight,newdata$Sex,sd)^2

# The ratio is 1.25, so we can assume that these are approximately equal variances

#28 degree temparature 

max(by(newdata28A$Weight,newdata28A$Sex,sd))^2/min(by(newdata28A$Weight,newdata28A$Sex,sd))^2
by(newdata28A$Weight,newdata28A$Sex,sd)^2

# The ratio is 2.09, so we can assume that these are approximately equal variances

#32 degree temparature 

max(by(newdata28A$Weight,newdata28A$Sex,sd))^2/min(by(newdata28A$Weight,newdata28A$Sex,sd))^2
by(newdata28A$Weight,newdata28A$Sex,sd)^2

# The ratio is 2.09, so we can assume that these are approximately equal variances



# Concluded that there is no significant differences between the variances in the population
# In summary, the assumption of normality is violated because
# there are both males and females with. In this case,non-parametric test
# is more fitted



###### STATISTICAL TEST/MODEL
# We proceed by checking whether or not there is a difference in weight between females and males with
# the non-parametric unpaired t-test, using the function wilcox.test().



### MANN-WHITNEY U-TEST (NONPARAMETRIC)
# Use Mann-Whitney Unpaired u-test

wilcox.test(Weight~Sex,paired=FALSE, var.equal=TRUE,alternative="two.sided",data=newdata)




#Wilcoxon rank sum test with continuity correction
#data:  Weight by Sex
#W = 145.5, p-value = 2.737e-12
#alternative hypothesis: true location shift is not equal to 0

# Median differences referring to the H0: Female-Male=0 and HA: Female-Male???0
# Pvalue 2.737e-12 <0.05, meaning the Pvalue is significant and we can not accept the H0
# Concluded that there is significant differences in the number of weight between the two sexes

wilcox.test(Weight~Sex,paired=FALSE, var.equal=TRUE,alternative="two.sided",data=newdata28A)

#data:  Weight by Sex
#W = 15.5, p-value = 6.549e-05
#alternative hypothesis: true location shift is not equal to 0

# Median differences referring to the H0: Female-Male=0 and HA: Female-Male???0
# Pvalue 6.549e-05 <0.05, meaning the Pvalue is significant and we can not accept the H0
# Concluded that there is significant differences in the number of weight between the two 



wilcox.test(Weight~Sex,paired=FALSE, var.equal=TRUE,alternative="two.sided",data=newdata32A)

#data:  Weight by Sex
#W = 9.5, p-value = 2.582e-05
#alternative hypothesis: true location shift is not equal to 0

# Median differences referring to the H0: Female-Male=0 and HA: Female-Male???0
# Pvalue 2.582e-05 <0.05, meaning the Pvalue is significant and we can not accept the H0
# Concluded that there is significant differences in the number of weight between the two 





#pearson correlation# 

cor.test(newdata$Weight, newdata28A$Weight)

cor.test(newdata28A$Weight, newdata32A$Weight)

cor.test(newdata32A$Weight, newdata$Weight)

# Conclusion: There is a significant positive correlation between the weight of different temaparature




#Mean Calculation for different temparature (males and females)

CrossTable (newdata$Weight, newdata$Sex )

summary(newdata$Weight, newdata$Sex)
summary(Weight by Sex , data=newdata)


