
library(tidyverse)
library(dplyr)

data <- read.csv("290_All.csv")
data_byCourse <- split(data, f = data$Gender)
data_women <- data_byCourse$"Woman"

#correlation of sense of belonging with psych safety
correlation <- cor(data_women$sum_column, data_women$Conflict, method = 'pearson',  use = 'complete.obs')
print(correlation)
