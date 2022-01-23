#install.packages("dplyr")
# Install
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library(readxl)
library(dplyr)
library(ggpubr)
library(ggplot2)

library(readxl)
Data3Group <- read_excel("Length_weight_data_1_month_after_sampling.xlsx", sheet = 4)
View(Data3Group)

#ggwegght32(Data3Group$Weight_32_degree_Celcius,
#main = "weight 32", xlab= "weight in gram)

shapiro.test(Data3Group$Weight_32_degree_Celcius)

#Data3Group(Data3Group$Weight_32_degree_Celcius, 
         # main = "weight 32",
          #xlab = "weight in gram")

shapiro.test(Data3Group$Weight_28_degree_Celcius)
shapiro.test(Data3Group$Weight_24_degree_Celcius)


bartlett.test(list(Data3Group$Weight_32_degree_Celcius, Data3Group$Weight_28_degree_Celcius, Data3Group$Weight_24_degree_Celcius))


#Bartlett's Test of 32 degree sample,
#Bartlett test of homogeneity of variances

Data32 <- read_excel("Length_weight_data_1_month_after_sampling.xlsx", sheet = 1)

bartlett.test(Weight ~ Replicate, data = Data32)

#Bartlett's Test of 28 degree sample,


Data28 <- read_excel("Length_weight_data_1_month_after_sampling.xlsx", sheet = 2)

bartlett.test(Weight ~ Replicate, data = Data28)


#Bartlett's Test of 24 degree sample,


Data24 <- read_excel("Length_weight_data_1_month_after_sampling.xlsx", sheet = 3)

bartlett.test(Weight ~ Replicate, data = Data24)

bartlett.test(list(Data3Group$Weight_32_degree_Celcius, Data3Group$Weight_28_degree_Celcius, Data3Group$Weight_24_degree_Celcius))


result32 = bartlett.test(Weight ~ Replicate, Data32)
print(result32)

plot(Weight ~ Replicate, data = Data32)


cor.test(Data3Group$Weight_32_degree_Celcius, Data3Group$Weight_28_degree_Celcius)


library(psych)
data=Data3Group[,c("Weight_32_degree_Celcius","Data3Group$Weight_28_degree_Celcius","Data3Group$Weight_24_degree_Celcius")]
cor=corr.test(Data3Group, use="pairwise", method="pearson", adjust="none")
cor$ci


head(Data32)
levels(Data32$Replicate)

# Car

library(car)
library(carData)

boxplot(Weight ~ Replicate,ylab="weight of 32 (gm)",col="lightblue",
        data=Data32)

boxplot(Weight ~ Replicate,ylab="weight of 28 (gm)",col="lightblue",
        data=Data28)

boxplot(Weight ~ Replicate,ylab="weight of 24 (gm)",col="lightblue",
        data=Data24)


leveneTest(Weight~Replicate, Data32,center=mean)


kruskal.test(Weight ~ Replicate, data = Data32)
kruskal.test(Weight ~ Replicate, data = Data28)
kruskal.test(Weight ~ Replicate, data = Data24)

library(tidyverse)
library(ggpubr)
library(rstatix)
set.seed(345)


set.seed(345)
Data32 %>% sample_n_by(Replicate, size = 1)

Data28 %>% sample_n_by(Replicate, size = 1)
Data24 %>% sample_n_by(Replicate, size = 1)


Data32 %>%  
  group_by(Replicate) %>%
  get_summary_stats(Weight, type = "common")



Data28 %>%  
  group_by(Replicate) %>%
  get_summary_stats(Weight, type = "common")

Data24 %>%  
  group_by(Replicate) %>%
  get_summary_stats(Weight, type = "common")


bartlett.test(list(Data3Group$Weight_32_degree_Celcius, Data3Group$Weight_28_degree_Celcius, Data3Group$Weight_24_degree_Celcius))


ggboxplot(Data32, x = "Replicate", y = "Weight", fill="Replicate")
ggboxplot(Data28, x = "Replicate", y = "Weight", fill="Replicate")
ggboxplot(Data24, x = "Replicate", y = "Weight", fill="Replicate")

res.kruskal <- Data32 %>% kruskal_test(Weight ~ Replicate)
res.kruskal

res.kruskal <- Data28 %>% kruskal_test(Weight ~ Replicate)
res.kruskal
res.kruskal <- Data24 %>% kruskal_test(Weight ~ Replicate)
res.kruskal

library(psych)
data=Data3Group[,c("Weight_32_degree_Celcius","Data3Group$Weight_28_degree_Celcius","Data3Group$Weight_24_degree_Celcius")]
cor=corr.test(Data3Group, use="pairwise", method="pearson", adjust="none")
cor$ci

write.csv(cor$ci,file="correlations1.csv")
library(readxl)
library(car)
library(psych)

Data3.data <- Data3Group[,c(2)]
Data3.data <- na.omit(Data3Group$Weight_32_degree_Celcius)
mean(Data3.data)

Data32.data <- Data3Group[,c(3)]
Data32.data <- na.omit(Data32.data$Weight_28_degree_Celcius)
mean(Data32.data)

Data324.data <- Data3Group[,c(4)]
Data324.data <- na.omit(Data324.data$Weight_24_degree_Celcius)
mean(Data324.data)


sd(Data3.data)
sd(Data32.data)
sd(Data324.data)

AllData <- read_excel("Length_weight_data_1_month_after_sampling.xlsx", sheet = 5)
ggboxplot(AllData, x = "Temperature", y = "Weight", fill="Temperature")


bartlett.test(Weight ~ Temperature, data = AllData)

leveneTest(Weight ~ Temperature,data=AllData)

hist(Data32$Weight,
     main = " 32 degree celsius",
     xlab = "Weight (gm)")

hist(Data28$Weight,
     main = " 28 degree celsius",
     xlab = "Weight (gm)")

hist(Data24$Weight,
     main = " 24 degree celsius",
     xlab = "Weight (gm)")

#shapiro.test(Data3.data$PS_left-mean(pelvicspine.data$PS_left)) # W = 0.951 > 0.90 OK Normally Distributed


kruskal.test(Weight ~ Temperature, data = AllData)
