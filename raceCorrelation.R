
library(tidyverse)
library(dplyr)
library(corrplot)

data <- read.csv("290_All.csv")
data_byCourse <- split(data, f = data$Race)
data_race <- data_byCourse$"White"

sumColly <- apply(data_race[,c(4:9)], 1, sum)
data_race$sense_of_belonging <- sumColly
reduced <- data_race[10:15] #looking at only the columns we want to compare

#To make a correlogram, I followed this tutorial: 
#https://www.geeksforgeeks.org/visualize-correlation-matrix-using-correlogram-in-r-programming/

cor_mat <- cor(reduced, use = "pairwise.complete.obs")
head(cor_mat)

#There is some discussion about using this form of correlation matrix 
#which can be found here:
#https://stackoverflow.com/questions/7445639/dealing-with-missing-values-for-correlations-calculation
#I used this "pairwise.complete.obs" to deal with the presence of NAs in the data

#visualizing correlogram 
# as colour 
corrplot(cor_mat, method="color",addCoef.col = "black") 

#as circle 
#corrplot(cor_mat, method="circle") 
# as pie 
#corrplot(cor_mat, method="pie") 
# as number 
#corrplot(cor_mat, method="number")
