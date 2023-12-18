library(tidyverse)
library(dplyr)

data <- read.csv("290_All.csv")
data_byCourse <- split(data, f = data$Gender)
data_women <- data_byCourse$"Woman"

#correlation of sense of belonging with psych safety
correlation <- cor(data_women, data$PS, method = 'pearson')