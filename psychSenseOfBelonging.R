
library(tidyverse)
library(dplyr)
library(corrplot)

data <- read.csv("290_All.csv")
data_byCourse <- split(data, f = data$Gender)
data_women <- data_byCourse$"Woman"

sumColly <- apply(data_women[,c(4:9)], 1, sum)
data_women$sense_of_belonging <- sumColly
reduced <- data_women[complete.cases(data_women[, 15]), ][10:15]


#correlation of sense of belonging with psych safety
correlation <- cor(reduced$sense_of_belonging, reduced$Conflict, method = 'pearson',  use = 'complete.obs')
print(correlation)

#To make a correlogram, I followed this tutorial: 
#https://www.geeksforgeeks.org/visualize-correlation-matrix-using-correlogram-in-r-programming/

cor_mat <- cor(reduced, use = "pairwise.complete.obs")
#There is some discussion about using this form of correlation matrix 
#which can be found here:
#https://stackoverflow.com/questions/7445639/dealing-with-missing-values-for-correlations-calculation
#I used this "pairwise.complete.obs" to deal with the presence of NAs in the data

#visualizing correlogram 
#as circle 
corrplot(cor_mat, method="circle") 
# as pie 
corrplot(cor_mat, method="pie") 
# as colour 
corrplot(cor_mat, method="color",addCoef.col = "black") 
# as number 
corrplot(cor_mat, method="number")
