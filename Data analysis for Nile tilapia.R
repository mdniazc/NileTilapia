#install.packages("dplyr")
# Install
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

library(readxl)

library(readxl)
Data3Group <- read_excel("Length_weight_data_1_month_after_sampling.xlsx", sheet = 4)
View(Data3Group)
