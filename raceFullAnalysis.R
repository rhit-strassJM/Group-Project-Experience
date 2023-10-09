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

#whiteBlack
sb1_whiteBlack <- SBanalysis(data_white$SB1, data_black$SB1, "sb1_whiteBlack")
sb2_whiteBlack <- SBanalysis(data_white$SB2, data_black$SB2, "sb2_whiteBlack")
sb3_whiteBlack <- SBanalysis(data_white$SB3, data_black$SB3, "sb3_whiteBlack")
sb4_whiteBlack <- SBanalysis(data_white$SB4, data_black$SB4, "sb4_whiteBlack")
sb5_whiteBlack <- SBanalysis(data_white$SB5, data_black$SB5, "sb5_whiteBlack")
sb6_whiteBlack <- SBanalysis(data_white$SB6, data_black$SB6, "sb6_whiteBlack")
conflict_whiteBlack <- SBanalysis(data_white$Conflict, data_black$Conflict, "conflict_whiteBlack")
ps_whiteBlack <- SBanalysis(data_white$PS, data_black$PS, "ps_whiteBlack")
satisfaction_whiteBlack <- SBanalysis(data_white$Satisfaction, data_black$Satisfaction, "satisfaction_whiteBlack")
cs_whiteBlack <- SBanalysis(data_white$ContributionSelf, data_black$ContributionSelf, "cs_whiteBlack")
cn_whiteBlack <- SBanalysis(data_white$ContributionNoSelf, data_black$ContributionNoSelf, "cn_whiteBlack")
whiteBlackList = list(sb1_whiteBlack,sb2_whiteBlack, sb3_whiteBlack,
               sb4_whiteBlack,sb5_whiteBlack, sb6_whiteBlack,
               conflict_whiteBlack, ps_whiteBlack,
               satisfaction_whiteBlack, cs_whiteBlack, cn_whiteBlack)


#whiteAsian
sb1_whiteAsian <- SBanalysis(data_white$SB1, data_asian$SB1, "sb1_whiteAsian")
sb2_whiteAsian <- SBanalysis(data_white$SB2, data_asian$SB2, "sb2_whiteAsian")
sb3_whiteAsian <- SBanalysis(data_white$SB3, data_asian$SB3, "sb3_whiteAsian")
sb4_whiteAsian <- SBanalysis(data_white$SB4, data_asian$SB4, "sb4_whiteAsian")
sb5_whiteAsian <- SBanalysis(data_white$SB5, data_asian$SB5, "sb5_whiteAsian")
sb6_whiteAsian <- SBanalysis(data_white$SB6, data_asian$SB6, "sb6_whiteAsian")
conflict_whiteAsian <- SBanalysis(data_white$Conflict, data_asian$Conflict, "conflict_whiteAsian")
ps_whiteAsian <- SBanalysis(data_white$PS, data_asian$PS, "ps_whiteAsian")
satisfaction_whiteAsian <- SBanalysis(data_white$Satisfaction, data_asian$Satisfaction, "satisfaction_whiteAsian")
cs_whiteAsian <- SBanalysis(data_white$ContributionSelf, data_asian$ContributionSelf, "cs_whiteAsian")
cn_whiteAsian <- SBanalysis(data_white$ContributionNoSelf, data_asian$ContributionNoSelf, "cn_whiteAsian")
whiteAsianList = list(sb1_whiteAsian,sb2_whiteAsian, sb3_whiteAsian,
                      sb4_whiteAsian,sb5_whiteAsian, sb6_whiteAsian,
                      conflict_whiteAsian, ps_whiteAsian,
                      satisfaction_whiteAsian, cs_whiteAsian, cn_whiteAsian)

#whiteHispanic
sb1_whiteHispanic <- SBanalysis(data_white$SB1, data_hispanic$SB1, "sb1_whiteHispanic")
sb2_whiteHispanic <- SBanalysis(data_white$SB2, data_hispanic$SB2, "sb2_whiteHispanic")
sb3_whiteHispanic <- SBanalysis(data_white$SB3, data_hispanic$SB3, "sb3_whiteHispanic")
sb4_whiteHispanic <- SBanalysis(data_white$SB4, data_hispanic$SB4, "sb4_whiteHispanic")
sb5_whiteHispanic <- SBanalysis(data_white$SB5, data_hispanic$SB5, "sb5_whiteHispanic")
sb6_whiteHispanic <- SBanalysis(data_white$SB6, data_hispanic$SB6, "sb6_whiteHispanic")
conflict_whiteHispanic <- SBanalysis(data_white$Conflict, data_hispanic$Conflict, "conflict_whiteHispanic")
ps_whiteHispanic <- SBanalysis(data_white$PS, data_hispanic$PS, "ps_whiteHispanic")
satisfaction_whiteHispanic <- SBanalysis(data_white$Satisfaction, data_hispanic$Satisfaction, "satisfaction_whiteHispanic")
cs_whiteHispanic <- SBanalysis(data_white$ContributionSelf, data_hispanic$ContributionSelf, "cs_whiteHispanic")
cn_whiteHispanic <- SBanalysis(data_white$ContributionNoSelf, data_hispanic$ContributionNoSelf, "cn_whiteHispanic")
whiteHispanicList = list(sb1_whiteHispanic,sb2_whiteHispanic, sb3_whiteHispanic,
                      sb4_whiteHispanic,sb5_whiteHispanic, sb6_whiteHispanic,
                      conflict_whiteHispanic, ps_whiteHispanic,
                      satisfaction_whiteHispanic, cs_whiteHispanic, cn_whiteHispanic)

#blackAsian
sb1_blackAsian <- SBanalysis(data_black$SB1, data_asian$SB1, "sb1_blackAsian")
sb2_blackAsian <- SBanalysis(data_black$SB2, data_asian$SB2, "sb2_blackAsian")
sb3_blackAsian <- SBanalysis(data_black$SB3, data_asian$SB3, "sb3_blackAsian")
sb4_blackAsian <- SBanalysis(data_black$SB4, data_asian$SB4, "sb4_blackAsian")
sb5_blackAsian <- SBanalysis(data_black$SB5, data_asian$SB5, "sb5_blackAsian")
sb6_blackAsian <- SBanalysis(data_black$SB6, data_asian$SB6, "sb6_blackAsian")
conflict_blackAsian <- SBanalysis(data_black$Conflict, data_asian$Conflict, "conflict_blackAsian")
ps_blackAsian <- SBanalysis(data_black$PS, data_asian$PS, "ps_blackAsian")
satisfaction_blackAsian <- SBanalysis(data_black$Satisfaction, data_asian$Satisfaction, "satisfaction_blackAsian")
cs_blackAsian <- SBanalysis(data_black$ContributionSelf, data_asian$ContributionSelf, "cs_blackAsian")
cn_blackAsian <- SBanalysis(data_black$ContributionNoSelf, data_asian$ContributionNoSelf, "cn_blackAsian")
blackAsianList = list(sb1_blackAsian,sb2_blackAsian, sb3_blackAsian,
                         sb4_blackAsian,sb5_blackAsian, sb6_blackAsian,
                         conflict_blackAsian, ps_blackAsian,
                      satisfaction_blackAsian, cs_blackAsian, cn_blackAsian)

#blackHispanic
sb1_blackHispanic <- SBanalysis(data_black$SB1, data_hispanic$SB1, "sb1_blackHispanic")
sb2_blackHispanic <- SBanalysis(data_black$SB2, data_hispanic$SB2, "sb2_blackHispanic")
sb3_blackHispanic <- SBanalysis(data_black$SB3, data_hispanic$SB3, "sb3_blackHispanic")
sb4_blackHispanic <- SBanalysis(data_black$SB4, data_hispanic$SB4, "sb4_blackHispanic")
sb5_blackHispanic <- SBanalysis(data_black$SB5, data_hispanic$SB5, "sb5_blackHispanic")
sb6_blackHispanic <- SBanalysis(data_black$SB6, data_hispanic$SB6, "sb6_blackHispanic")
conflict_blackHispanic <- SBanalysis(data_black$Conflict, data_hispanic$Conflict, "conflict_blackHispanic")
ps_blackHispanic <- SBanalysis(data_black$PS, data_hispanic$PS, "ps_blackHispanic")
satisfaction_blackHispanic <- SBanalysis(data_black$Satisfaction, data_hispanic$Satisfaction, "satisfaction_blackHispanic")
cs_blackHispanic <- SBanalysis(data_black$ContributionSelf, data_hispanic$ContributionSelf, "cs_blackHispanic")
cn_blackHispanic <- SBanalysis(data_black$ContributionNoSelf, data_hispanic$ContributionNoSelf, "cn_blackHispanic")
blackHispanicList = list(sb1_blackHispanic,sb2_blackHispanic, sb3_blackHispanic,
                      sb4_blackHispanic,sb5_blackHispanic, sb6_blackHispanic,
                      conflict_blackHispanic, ps_blackHispanic,
                      satisfaction_blackHispanic, cs_blackHispanic, cn_blackHispanic)

#asianHispanic
sb1_asianHispanic <- SBanalysis(data_asian$SB1, data_hispanic$SB1, "sb1_asianHispanic")
sb2_asianHispanic <- SBanalysis(data_asian$SB2, data_hispanic$SB2, "sb2_asianHispanic")
sb3_asianHispanic <- SBanalysis(data_asian$SB3, data_hispanic$SB3, "sb3_asianHispanic")
sb4_asianHispanic <- SBanalysis(data_asian$SB4, data_hispanic$SB4, "sb4_asianHispanic")
sb5_asianHispanic <- SBanalysis(data_asian$SB5, data_hispanic$SB5, "sb5_asianHispanic")
sb6_asianHispanic <- SBanalysis(data_asian$SB6, data_hispanic$SB6, "sb6_asianHispanic")
conflict_asianHispanic <- SBanalysis(data_asian$Conflict, data_hispanic$Conflict, "conflict_asianHispanic")
ps_asianHispanic <- SBanalysis(data_asian$PS, data_hispanic$PS, "ps_asianHispanic")
satisfaction_asianHispanic <- SBanalysis(data_asian$Satisfaction, data_hispanic$Satisfaction, "satisfaction_asianHispanic")
cs_asianHispanic <- SBanalysis(data_asian$ContributionSelf, data_hispanic$ContributionSelf, "cs_asianHispanic")
cn_asianHispanic <- SBanalysis(data_asian$ContributionNoSelf, data_hispanic$ContributionNoSelf, "cn_asianHispanic")
asianHispanicList = list(sb1_asianHispanic,sb2_asianHispanic, sb3_asianHispanic,
                         sb4_asianHispanic,sb5_asianHispanic, sb6_asianHispanic,
                         conflict_asianHispanic, ps_asianHispanic,
                         satisfaction_asianHispanic, cs_asianHispanic, cn_asianHispanic)

#whiteNonwhite
sb1_whiteNonwhite <- SBanalysis(data_white$SB1, data_nonwhite$SB1, "sb1_whiteNonwhite")
sb2_whiteNonwhite <- SBanalysis(data_white$SB2, data_nonwhite$SB2, "sb2_whiteNonwhite")
sb3_whiteNonwhite <- SBanalysis(data_white$SB3, data_nonwhite$SB3, "sb3_whiteNonwhite")
sb4_whiteNonwhite <- SBanalysis(data_white$SB4, data_nonwhite$SB4, "sb4_whiteNonwhite")
sb5_whiteNonwhite <- SBanalysis(data_white$SB5, data_nonwhite$SB5, "sb5_whiteNonwhite")
sb6_whiteNonwhite <- SBanalysis(data_white$SB6, data_nonwhite$SB6, "sb6_whiteNonwhite")
conflict_whiteNonwhite <- SBanalysis(data_white$Conflict, data_nonwhite$Conflict, "conflict_whiteNonwhite")
ps_whiteNonwhite <- SBanalysis(data_white$PS, data_nonwhite$PS, "ps_whiteNonwhite")
satisfaction_whiteNonwhite <- SBanalysis(data_white$Satisfaction, data_nonwhite$Satisfaction, "satisfaction_whiteNonwhite")
cs_whiteNonwhite <- SBanalysis(data_white$ContributionSelf, data_nonwhite$ContributionSelf, "cs_whiteNonwhite")
cn_whiteNonwhite <- SBanalysis(data_white$ContributionNoSelf, data_nonwhite$ContributionNoSelf, "cn_whiteNonwhite")
whiteNonwhiteList = list(sb1_whiteNonwhite,sb2_whiteNonwhite, sb3_whiteNonwhite,
                         sb4_whiteNonwhite,sb5_whiteNonwhite, sb6_whiteNonwhite,
                         conflict_whiteNonwhite, ps_whiteNonwhite,
                         satisfaction_whiteNonwhite, cs_whiteNonwhite, cn_whiteNonwhite)

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

dfWhiteBlack <- purrr::map_dfr(whiteBlackList, col_tibble)
dfWhiteAsian <- purrr::map_dfr(whiteAsianList, col_tibble)
dfWhiteHispanic <- purrr::map_dfr(whiteHispanicList, col_tibble)
dfBlackAsian <- purrr::map_dfr(blackAsianList, col_tibble)
dfBlackHispanic <- purrr::map_dfr(blackHispanicList, col_tibble)
dfAsianHispanic <- purrr::map_dfr(asianHispanicList, col_tibble)
dfWhiteNonwhite <- purrr::map_dfr(whiteNonwhiteList, col_tibble)


