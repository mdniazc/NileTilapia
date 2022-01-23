
# Question No 2. Are there indications that GSI or total copepod ####
#biomass correlates with rainfall 
# or differs between the rainy and the dry season?

library(psych)

data2 = mydata[,c(8,9)]
cor=corr.test(data2, use="pairwise", method="pearson", adjust="none")
cor$ci

#alternatively

cor.test(mydata$TotCop, mydata$Rainfall)

# effect of season

df2 <- mydata %>% 
  select(Season, Rainfall,TotCop) %>% 
  na.omit()


fitQ2 = aov(Rainfall ~ TotCop+Season, data = df2)
summary(fitQ2)
plot(allEffects(mod=fitQ2))
    
# Test if variances are homogeneous or not:
leveneTest(TotCop ~ Season, data = mydata)

#Test for normally distributed differences

y <- mydata$TotCop
hist(y)
shapiro.test(y)

###corrlation test for GSI with rainfall ####



cor.test(mydata1$StanMGSI,mydata1$Rainfall,alternative="two.sided")
cor.test(mydata1$StanFGSI,mydata1$Rainfall,alternative="two.sided")
cor.test(mydata1$LmioMGSI,mydata1$Rainfall,alternative="two.sided")
cor.test(mydata1$LmioFGSI,mydata1$Rainfall,alternative="two.sided")


# alternatively

DStanMGSI= mydata1[,c(4,9)]
cor=corr.test(DStanMGSI, use="pairwise", method="pearson", adjust="none")
cor$ci

DStanFGSI= mydata1[,c(5,9)]
cor=corr.test(DStanFGSI, use="pairwise", method="pearson", adjust="none")
cor$ci

DLmioMGSI= mydata1[,c(6,9)]
cor=corr.test(DLmioMGSI, use="pairwise", method="pearson", adjust="none")
cor$ci

DLmioFGSI= mydata1[,c(7,9)]
cor=corr.test(DLmioFGSI, use="pairwise", method="pearson", adjust="none")
cor$ci
# data visualization 

df10 <- mydata[,c(4,5,6,7,9)]
df10 <- na.omit(df10)

pairs.panels(df10,lm=T,smooth=F,ellipses=F)

#cor.test(df10$TotCop, df10$Rainfall)
cor=corr.test(df10, use="pairwise", method="pearson", adjust="none")
cor$ci


#Total copped  differs between the rainy and the dry season?

boxplot(TotCop ~ Season, data = mydata1)


t.test(TotCop~Season,var.equal=TRUE,alternative="two.sided",
       data=mydata1) 

#Testing normally distributed residuals 

hist(mydata1$TotCop)
resid <- unlist(by(mydata1$TotCop, mydata1$Season, function(x) x-mean(x)))
shapiro.test(resid)

#Test if variances are homogeneous or not

leveneTest(mydata1$TotCop~mydata1$Season)


# GSI  differs between the rainy and the dry season?

boxplot(StanMGSI ~ Season, data = mydata1)
boxplot(StanFGSI ~ Season, data = mydata1)
boxplot(LmioMGSI ~ Season, data = mydata1)
boxplot(LmioFGSI ~ Season, data = mydata1)


t.test(StanMGSI~Season,var.equal=TRUE,alternative="two.sided",
       data=mydata1) 

t.test(StanFGSI~Season,var.equal=TRUE,alternative="two.sided",
       data=mydata1) 


t.test(LmioMGSI~Season,var.equal=TRUE,alternative="two.sided",
       data=mydata1) 


t.test(LmioFGSI~Season,var.equal=TRUE,alternative="two.sided",
       data=mydata1) 




#Question 3 ####

#Is there a relationship between GSI and total copepod biomass?

df11 <- mydata[,c(4,5,6,7,8)]

cor.test(mydata1$StanMGSI,mydata1$TotCop,alternative="two.sided")
cor.test(mydata1$StanFGSI,mydata1$TotCop,alternative="two.sided")
cor.test(mydata1$LmioMGSI,mydata1$TotCop,alternative="two.sided")
cor.test(mydata1$LmioFGSI,mydata1$TotCop,alternative="two.sided")

pairs.panels(df11,lm=T,smooth=F,ellipses=F)

#Create scatterplots for each pairwise comparison (correlation):
plot(mydata1$StanMGSI~mydata1$TotCop,
     main = "Scatterplot StanMGSI vx TotCOp",
     ylab = " GSI of male Stolothrissa tanganicae",
     xlab = "total copepod biomass ",
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16, cex = 1.2) 


plot(mydata1$StanFGSI~mydata1$TotCop,
     main = "Scatterplot StanFGSI vx TotCOp",
     ylab = " GSI of female Stolothrissa tanganicae",
     xlab = "total copepod biomass ",
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16, cex = 1.2)


plot(mydata1$LmioMGSI~mydata1$TotCop,
     main = "Scatterplot LmioMGSI vx TotCOp",
     ylab = " GSI of male Limnothrissa miodon",
     xlab = "total copepod biomass ",
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16, cex = 1.2)



plot(mydata1$LmioFGSI~mydata1$TotCop,
     main = "Scatterplot LmioFGSI vx TotCOp",
     ylab = " GSI of female Limnothrissa miodon",
     xlab = "total copepod biomass ",
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16, cex = 1.2)

library(psych)
cor <- corr.test(df11, use = "pairwise", method = "pearson", adjust = "none")
cor$ci


