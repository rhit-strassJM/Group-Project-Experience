#Racial Analysis
library(tidyverse)


data <- read.csv("290_All.csv")

#splitting data by race
data_byrace <- split(data, f = data$Race)
data_white <- data_byrace$White
data_black <- data_byrace$Black
data_asian <- data_byrace$Asian
data_hispanic <- data_byrace$Hispanic
data_nonwhite <-subset(data, !(Race=="White"))

#everyone but white men
data_notwhitemen <- subset(data, !(Gender == "Man" & Race == "White"))
data_whitemen <-subset(data_men, Race=="White")

master_list<- list()

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
                  "data1_mean" = data1_mean,
                  "data2_mean" = data2_mean,
                  "data1_sd" = data1_sd,
                  "data2_sd" = data2_sd,
                  "data1_lowerbound" = data1_lowerbound,
                  "data1_upperbound" = data1_upperbound,
                  "data2_lowerbound" = data2_lowerbound,
                  "data2_upperbound" = data2_upperbound,
                  "ttest" = ttest["p.value"][[1]])
  return(my_list)
}


#SB1

sb1_whiteBlack <- SBanalysis(data_white$SB1, data_black$SB1, "sb1_whiteBlack")
sb1_whiteAsian <- SBanalysis(data_white$SB1, data_asian$SB1, "sb1_whiteAsian")
sb1_whiteHispanic <- SBanalysis(data_white$SB1, data_hispanic$SB1, "sb1_whiteHispanic")
sb1_blackAsian <- SBanalysis(data_black$SB1, data_asian$SB1, "sb1_blackAsian")
sb1_blackHispanic <- SBanalysis(data_black$SB1, data_hispanic$SB1, "sb1_blackHispanic")
sb1_AsianHispanic <- SBanalysis(data_asian$SB1, data_hispanic$SB1, "sb1_AsianHispanic")
sb1_whiteNonwhite <- SBanalysis(data_white$SB1, data_nonwhite$SB1, "sb1_whiteNonwhite")
sb1List = list(sb1_whiteBlack,sb1_whiteAsian, sb1_whiteHispanic,sb1_blackAsian,sb1_blackHispanic, sb1_AsianHispanic,sb1_whiteNonwhite)

#SB2
sb2_whiteBlack <- SBanalysis(data_white$SB2, data_black$SB2, "sb2_whiteBlack")
sb2_whiteAsian <- SBanalysis(data_white$SB2, data_asian$SB2, "sb2_whiteAsian")
sb2_whiteHispanic <- SBanalysis(data_white$SB2, data_hispanic$SB2, "sb2_whiteHispanic")
sb2_blackAsian <- SBanalysis(data_black$SB2, data_asian$SB2, "sb2_blackAsian")
sb2_blackHispanic <- SBanalysis(data_black$SB2, data_hispanic$SB2, "sb2_blackHispanic")
sb2_AsianHispanic <- SBanalysis(data_asian$SB2, data_hispanic$SB2, "sb2_AsianHispanic")
sb2_whiteNonwhite <- SBanalysis(data_white$SB2, data_nonwhite$SB2, "sb2_whiteNonwhite")
sb2List = list(sb2_whiteBlack,sb2_whiteAsian, sb2_whiteHispanic,sb2_blackAsian,sb2_blackHispanic, sb2_AsianHispanic,sb2_whiteNonwhite)


#SB3
sb3_whiteBlack <- SBanalysis(data_white$SB3, data_black$SB3, "sb3_whiteBlack")
sb3_whiteAsian <- SBanalysis(data_white$SB3, data_asian$SB3, "sb3_whiteAsian")
sb3_whiteHispanic <- SBanalysis(data_white$SB3, data_hispanic$SB3, "sb3_whiteHispanic")
sb3_blackAsian <- SBanalysis(data_black$SB3, data_asian$SB3, "sb3_blackAsian")
sb3_blackHispanic <- SBanalysis(data_black$SB3, data_hispanic$SB3, "sb3_blackHispanic")
sb3_AsianHispanic <- SBanalysis(data_asian$SB3, data_hispanic$SB3, "sb3_AsianHispanic")
sb3_whiteNonwhite <- SBanalysis(data_white$SB3, data_nonwhite$SB3, "sb3_whiteNonwhite")
sb3List = list(sb3_whiteBlack,sb3_whiteAsian, sb3_whiteHispanic,sb3_blackAsian,sb3_blackHispanic, sb3_AsianHispanic,sb3_whiteNonwhite)

#SB4
sb4_whiteBlack <- SBanalysis(data_white$SB4, data_black$SB4, "sb4_whiteBlack")
sb4_whiteAsian <- SBanalysis(data_white$SB4, data_asian$SB4, "sb4_whiteAsian")
sb4_whiteHispanic <- SBanalysis(data_white$SB4, data_hispanic$SB4, "sb4_whiteHispanic")
sb4_blackAsian <- SBanalysis(data_black$SB4, data_asian$SB4, "sb4_blackAsian")
sb4_blackHispanic <- SBanalysis(data_black$SB4, data_hispanic$SB4, "sb4_blackHispanic")
sb4_AsianHispanic <- SBanalysis(data_asian$SB4, data_hispanic$SB4, "sb4_AsianHispanic")
sb4_whiteNonwhite <- SBanalysis(data_white$SB4, data_nonwhite$SB4, "sb4_whiteNonwhite")
sb4List = list(sb4_whiteBlack,sb4_whiteAsian, sb4_whiteHispanic,sb4_blackAsian,sb4_blackHispanic, sb4_AsianHispanic,sb4_whiteNonwhite)

#SB5
sb5_whiteBlack <- SBanalysis(data_white$SB5, data_black$SB5, "sb5_whiteBlack")
sb5_whiteAsian <- SBanalysis(data_white$SB5, data_asian$SB5, "sb5_whiteAsian")
sb5_whiteHispanic <- SBanalysis(data_white$SB5, data_hispanic$SB5, "sb5_whiteHispanic")
sb5_blackAsian <- SBanalysis(data_black$SB5, data_asian$SB5, "sb5_blackAsian")
sb5_blackHispanic <- SBanalysis(data_black$SB5, data_hispanic$SB5, "sb5_blackHispanic")
sb5_AsianHispanic <- SBanalysis(data_asian$SB5, data_hispanic$SB5, "sb5_AsianHispanic")
sb5_whiteNonwhite <- SBanalysis(data_white$SB5, data_nonwhite$SB5, "sb5_whiteNonwhite")
sb5List = list(sb5_whiteBlack,sb5_whiteAsian, sb5_whiteHispanic,sb5_blackAsian,sb5_blackHispanic, sb5_AsianHispanic,sb5_whiteNonwhite)

#SB6
sb6_whiteBlack <- SBanalysis(data_white$SB6, data_black$SB6, "sb6_whiteBlack")
sb6_whiteAsian <- SBanalysis(data_white$SB6, data_asian$SB6, "sb6_whiteAsian")
sb6_whiteHispanic <- SBanalysis(data_white$SB6, data_hispanic$SB6, "sb6_whiteHispanic")
sb6_blackAsian <- SBanalysis(data_black$SB6, data_asian$SB6, "sb6_blackAsian")
sb6_blackHispanic <- SBanalysis(data_black$SB6, data_hispanic$SB6, "sb6_blackHispanic")
sb6_AsianHispanic <- SBanalysis(data_asian$SB6, data_hispanic$SB6, "sb6_AsianHispanic")
sb6_whiteNonwhite <- SBanalysis(data_white$SB6, data_nonwhite$SB6, "sb6_whiteNonwhite")
sb6List = list(sb6_whiteBlack,sb6_whiteAsian, sb6_whiteHispanic,sb6_blackAsian,sb6_blackHispanic, sb6_AsianHispanic,sb6_whiteNonwhite)


df1 <- purrr::map_dfr(sb1List, col_List)
df2 <- purrr::map_dfr(sb2List, col_List)
df3 <- purrr::map_dfr(sb3List, col_List)
df4 <- purrr::map_dfr(sb4List, col_List)
df5 <- purrr::map_dfr(sb5List, col_List)
df6 <- purrr::map_dfr(sb6List, col_List)


