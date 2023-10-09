#Racial Analysis
library(tidyverse)
library(dplyr)


data <- read.csv("290_All.csv")

#splitting data by race
data <- data %>% 
  mutate(sum_column = data$SB1+data$SB2+data$SB3+data$SB4+data$SB5+data$SB6)
data_byrace <- split(data, f = data$Race)
data_white <- data_byrace$White
data_black <- data_byrace$Black
data_asian <- data_byrace$Asian
data_hispanic <- data_byrace$Hispanic
data_nonwhite <-subset(data, !(Race=="White"))

aggregate1<-data$SB1+data$SB2+data$SB3+data$SB4+data$SB5+data$SB6
aggreate2<-rowSums(data[ , c("SB1", "SB2", "SB3", "SB4", "SB5", "SB6")])



SBanalysis <- function(data1, data2, data_descriptor){
  # mean of sb col
  # na.rm = we are removing the "nas" which are all the students who did not fill out sb
  data1_mean <- mean(data1, na.rm = TRUE)
  data2_mean <- mean(data2, na.rm = TRUE)
  
  # sample size of sb (how many students filled out sb) - we remove the "nas"
  data1_n <- length(data1[!is.na(data1)])
  data2_n <- length(data2[!is.na(data2)])
  
  # standard deviation of sb
  data1_sd <- sd(data1, na.rm = TRUE)
  data2_sd <- sd(data2, na.rm = TRUE)
  
  # 95% confidence interval = mean +/- margin
  data1_margin <- qt(0.975,df=(data1_n)-1)*data1_sd/sqrt(data1_n)
  data1_lowerbound <- data1_mean - data1_margin
  data1_upperbound <- data1_mean + data1_margin
  
  data2_margin <- qt(0.975,df=(data2_n)-1)*data2_sd/sqrt(data2_n)
  data2_lowerbound <- data2_mean - data2_margin
  data2_upperbound <- data2_mean + data2_margin
  
  ttest <- t.test(data1, data2, var.equal=TRUE)
  
  my_list <- list("data_descriptor" = data_descriptor,
                  # "data1_mean" = data1_mean,
                  # "data2_mean" = data2_mean,
                  # "data1_sd" = data1_sd,
                  # "data2_sd" = data2_sd,
                  # "data1_lowerbound" = data1_lowerbound,
                  # "data1_upperbound" = data1_upperbound,
                  # "data2_lowerbound" = data2_lowerbound,
                  # "data2_upperbound" = data2_upperbound,
                  "ttest" = ttest["p.value"][[1]]
                  )
  return(my_list)
}

sb_whiteBlack <- SBanalysis(data_white$sum_column, data_black$sum_column, "sb_whiteBlack")
sb_whiteAsian <- SBanalysis(data_white$sum_column, data_asian$sum_column, "sb_whiteAsian")
sb_whiteHispanic <- SBanalysis(data_white$sum_column, data_hispanic$sum_column, "sb_whiteHispanic")
sb_blackAsian <- SBanalysis(data_black$sum_column, data_asian$sum_column, "sb_blackAsian")
sb_blackHispanic <- SBanalysis(data_black$sum_column, data_hispanic$sum_column, "sb_blackHispanic")
sb_AsianHispanic <- SBanalysis(data_asian$sum_column, data_hispanic$sum_column, "sb_AsianHispanic")
sb_whiteNonwhite <- SBanalysis(data_white$sum_column, data_nonwhite$sum_column, "sb_whiteNonwhite")
sbList = list(sb_whiteBlack,sb_whiteAsian, sb_whiteHispanic,sb_blackAsian,sb_blackHispanic, sb_AsianHispanic,sb_whiteNonwhite)


col_tibble <- ~tibble("data_descriptor" = .x[[1]],
                      # "data1_mean" = .x[[2]],
                      # "data2_mean" = .x[[3]],
                      # "data1_sd" = .x[[4]],
                      # "data2_sd" = .x[[5]],
                      # "data1_lowerbound" = .x[[6]],
                      # "data1_upperbound" = .x[[7]],
                      # "data2_lowerbound" = .x[[8]],
                      # "data2_upperbound" = .x[[9]],
                      "ttest" = .x[[2]]
)

df <- purrr::map_dfr(sbList, col_tibble)