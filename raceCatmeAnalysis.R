library(tidyverse)
library(dplyr)


data <- read.csv("290_All.csv")

data_byrace <- split(data, f = data$Race)
data_white <- data_byrace$White
data_black <- data_byrace$Black
data_asian <- data_byrace$Asian
data_hispanic <- data_byrace$Hispanic
data_nonwhite <-subset(data, !(Race=="White"))

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


#SB1

conflict_whiteBlack <- SBanalysis(data_white$Conflict, data_black$Conflict, "sb1_whiteBlack")
conflict_whiteAsian <- SBanalysis(data_white$Conflict, data_asian$Conflict, "sb1_whiteAsian")
conflict_whiteHispanic <- SBanalysis(data_white$Conflict, data_hispanic$Conflict, "sb1_whiteHispanic")
conflict_blackAsian <- SBanalysis(data_black$Conflict, data_asian$Conflict, "sb1_blackAsian")
conflict_blackHispanic <- SBanalysis(data_black$Conflict, data_hispanic$Conflict, "sb1_blackHispanic")
conflict_AsianHispanic <- SBanalysis(data_asian$Conflict, data_hispanic$Conflict, "sb1_AsianHispanic")
conflict_whiteNonwhite <- SBanalysis(data_white$Conflict, data_nonwhite$Conflict, "sb1_whiteNonwhite")
sb1List = list(sb1_whiteBlack,sb1_whiteAsian, sb1_whiteHispanic,sb1_blackAsian,sb1_blackHispanic, sb1_AsianHispanic,sb1_whiteNonwhite)

#SB2
satisfaction_whiteBlack <- SBanalysis(data_white$Satisfaction, data_black$Satisfaction, "sb2_whiteBlack")
satisfaction_whiteAsian <- SBanalysis(data_white$Satisfaction, data_asian$Satisfaction, "sb2_whiteAsian")
satisfaction_whiteHispanic <- SBanalysis(data_white$Satisfaction, data_hispanic$Satisfaction, "sb2_whiteHispanic")
satisfaction_blackAsian <- SBanalysis(data_black$Satisfaction, data_asian$Satisfaction, "sb2_blackAsian")
satisfaction_blackHispanic <- SBanalysis(data_black$Satisfaction, data_hispanic$Satisfaction, "sb2_blackHispanic")
satisfaction_AsianHispanic <- SBanalysis(data_asian$Satisfaction, data_hispanic$Satisfaction, "sb2_AsianHispanic")
satisfaction_whiteNonwhite <- SBanalysis(data_white$Satisfaction, data_nonwhite$Satisfaction, "sb2_whiteNonwhite")
sb2List = list(sb2_whiteBlack,sb2_whiteAsian, sb2_whiteHispanic,sb2_blackAsian,sb2_blackHispanic, sb2_AsianHispanic,sb2_whiteNonwhite)


#SB3
ps_whiteBlack <- SBanalysis(data_white$PS, data_black$PS, "sb3_whiteBlack")
ps_whiteAsian <- SBanalysis(data_white$PS, data_asian$PS, "sb3_whiteAsian")
ps_whiteHispanic <- SBanalysis(data_white$PS, data_hispanic$PS, "sb3_whiteHispanic")
ps_blackAsian <- SBanalysis(data_black$PS, data_asian$PS, "sb3_blackAsian")
ps_blackHispanic <- SBanalysis(data_black$PS, data_hispanic$PS, "sb3_blackHispanic")
ps_AsianHispanic <- SBanalysis(data_asian$PS, data_hispanic$PS, "sb3_AsianHispanic")
ps_whiteNonwhite <- SBanalysis(data_white$PS, data_nonwhite$PS, "sb3_whiteNonwhite")
sb3List = list(sb3_whiteBlack,sb3_whiteAsian, sb3_whiteHispanic,sb3_blackAsian,sb3_blackHispanic, sb3_AsianHispanic,sb3_whiteNonwhite)

#SB4
cs_whiteBlack <- SBanalysis(data_white$ContributionSelf, data_black$ContributionSelf, "sb4_whiteBlack")
cs_whiteAsian <- SBanalysis(data_white$ContributionSelf, data_asian$ContributionSelf, "sb4_whiteAsian")
cs_whiteHispanic <- SBanalysis(data_white$ContributionSelf, data_hispanic$ContributionSelf, "sb4_whiteHispanic")
cs_blackAsian <- SBanalysis(data_black$ContributionSelf, data_asian$ContributionSelf, "sb4_blackAsian")
cs_blackHispanic <- SBanalysis(data_black$ContributionSelf, data_hispanic$ContributionSelf, "sb4_blackHispanic")
cs_AsianHispanic <- SBanalysis(data_asian$ContributionSelf, data_hispanic$ContributionSelf, "sb4_AsianHispanic")
cs_whiteNonwhite <- SBanalysis(data_white$ContributionSelf, data_nonwhite$ContributionSelf, "sb4_whiteNonwhite")
sb4List = list(sb4_whiteBlack,sb4_whiteAsian, sb4_whiteHispanic,sb4_blackAsian,sb4_blackHispanic, sb4_AsianHispanic,sb4_whiteNonwhite)

#SB5
cn_whiteBlack <- SBanalysis(data_white$ContributionNoSelf, data_black$ContributionNoSelf, "sb5_whiteBlack")
cn_whiteAsian <- SBanalysis(data_white$ContributionNoSelf, data_asian$ContributionNoSelf, "sb5_whiteAsian")
cn_whiteHispanic <- SBanalysis(data_white$ContributionNoSelf, data_hispanic$ContributionNoSelf, "sb5_whiteHispanic")
cn_blackAsian <- SBanalysis(data_black$ContributionNoSelf, data_asian$ContributionNoSelf, "sb5_blackAsian")
cn_blackHispanic <- SBanalysis(data_black$ContributionNoSelf, data_hispanic$ContributionNoSelf, "sb5_blackHispanic")
cn_AsianHispanic <- SBanalysis(data_asian$ContributionNoSelf, data_hispanic$ContributionNoSelf, "sb5_AsianHispanic")
cn_whiteNonwhite <- SBanalysis(data_white$ContributionNoSelf, data_nonwhite$ContributionNoSelf, "sb5_whiteNonwhite")
sb5List = list(sb5_whiteBlack,sb5_whiteAsian, sb5_whiteHispanic,sb5_blackAsian,sb5_blackHispanic, sb5_AsianHispanic,sb5_whiteNonwhite)


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

df1 <- purrr::map_dfr(sb1List, col_tibble)
df2 <- purrr::map_dfr(sb2List, col_tibble)
df3 <- purrr::map_dfr(sb3List, col_tibble)
df4 <- purrr::map_dfr(sb4List, col_tibble)
df5 <- purrr::map_dfr(sb5List, col_tibble)