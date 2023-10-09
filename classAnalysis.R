library(tidyverse)
library(dplyr)


data <- read.csv("290_All.csv")

data_byCourse <- split(data, f = data$Course)
data_120 <- data_byCourse$"120"
data_220 <- data_byCourse$"220"
data_230 <- data_byCourse$"230"
data_intro <- subset(data, (Course=="120")|(Course=="220")|(Course=="230"))
data_other <- subset(data, (!(Course=="120"))&(!(Course=="220"))&(!(Course=="230")))

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

#120v220
sb1_120220 <- SBanalysis(data_120$SB1, data_220$SB1, "sb1_120220")
sb2_120220 <- SBanalysis(data_120$SB2, data_220$SB2, "sb2_120220")
sb3_120220 <- SBanalysis(data_120$SB3, data_220$SB3, "sb3_120220")
sb4_120220 <- SBanalysis(data_120$SB4, data_220$SB4, "sb4_120220")
sb5_120220 <- SBanalysis(data_120$SB5, data_220$SB5, "sb5_120220")
sb6_120220 <- SBanalysis(data_120$SB6, data_220$SB6, "sb6_120220")
# conflict_120220 <- SBanalysis(data_120$Conflict, data_220$Conflict, "conflict_120220")
# ps_120220 <- SBanalysis(data_120$PS, data_220$PS, "ps_120220")
# satisfaction_120220 <- SBanalysis(data_120$Satisfaction, data_220$Satisfaction, "satisfaction_120220")
# cs_120220 <- SBanalysis(data_120$ContributionSelf, data_220$ContributionSelf, "cs_120220")
# cn_120220 <- SBanalysis(data_120$ContributionNoSelf, data_220$ContributionNoSelf, "cn_120220")
list120220 = list(sb1_120220,sb2_120220, sb3_120220,
                      sb4_120220,sb5_120220, sb6_120220
                      # conflict_120220, ps_120220,
                      # satisfaction_120220, cs_120220, cn_120220
                  )
#120v230
sb1_120230 <- SBanalysis(data_120$SB1, data_230$SB1, "sb1_120230")
sb2_120230 <- SBanalysis(data_120$SB2, data_230$SB2, "sb2_120230")
sb3_120230 <- SBanalysis(data_120$SB3, data_230$SB3, "sb3_120230")
sb4_120230 <- SBanalysis(data_120$SB4, data_230$SB4, "sb4_120230")
sb5_120230 <- SBanalysis(data_120$SB5, data_230$SB5, "sb5_120230")
sb6_120230 <- SBanalysis(data_120$SB6, data_230$SB6, "sb6_120230")
# conflict_120230 <- SBanalysis(data_120$Conflict, data_230$Conflict, "conflict_120230")
# ps_120230 <- SBanalysis(data_120$PS, data_230$PS, "ps_120230")
# satisfaction_120230 <- SBanalysis(data_120$Satisfaction, data_230$Satisfaction, "satisfaction_120230")
# cs_120230 <- SBanalysis(data_120$ContributionSelf, data_230$ContributionSelf, "cs_120230")
# cn_120230 <- SBanalysis(data_120$ContributionNoSelf, data_230$ContributionNoSelf, "cn_120230")
list120230 = list(sb1_120230,sb2_120230, sb3_120230,
                      sb4_120230,sb5_120230, sb6_120230
                      # conflict_120230, ps_120230,
                      # satisfaction_120230, cs_120230, cn_120230
                  )
#120vOther
sb1_120other <- SBanalysis(data_120$SB1, data_other$SB1, "sb1_120other")
sb2_120other <- SBanalysis(data_120$SB2, data_other$SB2, "sb2_120other")
sb3_120other <- SBanalysis(data_120$SB3, data_other$SB3, "sb3_120other")
sb4_120other <- SBanalysis(data_120$SB4, data_other$SB4, "sb4_120other")
sb5_120other <- SBanalysis(data_120$SB5, data_other$SB5, "sb5_120other")
sb6_120other <- SBanalysis(data_120$SB6, data_other$SB6, "sb6_120other")
# conflict_120other <- SBanalysis(data_120$Conflict, data_other$Conflict, "conflict_120other")
# ps_120other<- SBanalysis(data_120$PS, data_other$PS, "ps_120other")
# satisfaction_120other <- SBanalysis(data_120$Satisfaction, data_other$Satisfaction, "satisfaction_120other")
# cs_120other <- SBanalysis(data_120$ContributionSelf, data_other$ContributionSelf, "cs_120other")
# cn_120other <- SBanalysis(data_120$ContributionNoSelf, data_other$ContributionNoSelf, "cn_120other")
list120other = list(sb1_120other,sb2_120other, sb3_120other,
                      sb4_120other,sb5_120other, sb6_120other
                      # conflict_120other, ps_120other,
                      # satisfaction_120other, cs_120other, cn_120other
                    )

#220v230
sb1_220230 <- SBanalysis(data_220$SB1, data_230$SB1, "sb1_220230")
sb2_220230 <- SBanalysis(data_220$SB2, data_230$SB2, "sb2_220230")
sb3_220230 <- SBanalysis(data_220$SB3, data_230$SB3, "sb3_220230")
sb4_220230 <- SBanalysis(data_220$SB4, data_230$SB4, "sb4_220230")
sb5_220230 <- SBanalysis(data_220$SB5, data_230$SB5, "sb5_220230")
sb6_220230 <- SBanalysis(data_220$SB6, data_230$SB6, "sb6_220230")
# conflict_220230 <- SBanalysis(data_220$Conflict, data_230$Conflict, "conflict_220230")
# ps_220230 <- SBanalysis(data_220$PS, data_230$PS, "ps_220230")
# satisfaction_220230 <- SBanalysis(data_220$Satisfaction, data_230$Satisfaction, "satisfaction_220230")
# cs_220230 <- SBanalysis(data_220$ContributionSelf, data_230$ContributionSelf, "cs_220230")
# cn_220230 <- SBanalysis(data_220$ContributionNoSelf, data_230$ContributionNoSelf, "cn_220230")
list220230 = list(sb1_220230,sb2_220230, sb3_220230,
                      sb4_220230,sb5_220230, sb6_220230
                      # conflict_220230, ps_220230,
                      # satisfaction_220230, cs_220230, cn_220230
                  )

#220vOther
sb1_220other <- SBanalysis(data_220$SB1, data_other$SB1, "sb1_220other")
sb2_220other <- SBanalysis(data_220$SB2, data_other$SB2, "sb2_220other")
sb3_220other <- SBanalysis(data_220$SB3, data_other$SB3, "sb3_220other")
sb4_220other <- SBanalysis(data_220$SB4, data_other$SB4, "sb4_220other")
sb5_220other <- SBanalysis(data_220$SB5, data_other$SB5, "sb5_220other")
sb6_220other <- SBanalysis(data_220$SB6, data_other$SB6, "sb6_220other")
# conflict_220other <- SBanalysis(data_220$Conflict, data_other$Conflict, "conflict_220other")
# ps_220other<- SBanalysis(data_220$PS, data_other$PS, "ps_220other")
# satisfaction_220other <- SBanalysis(data_220$Satisfaction, data_other$Satisfaction, "satisfaction_220other")
# cs_220other <- SBanalysis(data_220$ContributionSelf, data_other$ContributionSelf, "cs_220other")
# cn_220other <- SBanalysis(data_220$ContributionNoSelf, data_other$ContributionNoSelf, "cn_220other")
list220other = list(sb1_220other,sb2_220other, sb3_220other,
                      sb4_220other,sb5_220other, sb6_220other
                      # conflict_220other, ps_220other,
                      # satisfaction_220other, cs_220other, cn_220other
                    )


#230vOther
sb1_230other <- SBanalysis(data_230$SB1, data_other$SB1, "sb1_230other")
sb2_230other <- SBanalysis(data_230$SB2, data_other$SB2, "sb2_230other")
sb3_230other <- SBanalysis(data_230$SB3, data_other$SB3, "sb3_230other")
sb4_230other <- SBanalysis(data_230$SB4, data_other$SB4, "sb4_230other")
sb5_230other <- SBanalysis(data_230$SB5, data_other$SB5, "sb5_230other")
sb6_230other <- SBanalysis(data_230$SB6, data_other$SB6, "sb6_230other")
# conflict_230other <- SBanalysis(data_230$Conflict, data_other$Conflict, "conflict_230other")
# ps_230other<- SBanalysis(data_230$PS, data_other$PS, "ps_230other")
# satisfaction_230other <- SBanalysis(data_230$Satisfaction, data_other$Satisfaction, "satisfaction_230other")
# cs_230other <- SBanalysis(data_230$ContributionSelf, data_other$ContributionSelf, "cs_230other")
# cn_230other <- SBanalysis(data_230$ContributionNoSelf, data_other$ContributionNoSelf, "cn_230other")
list230other = list(sb1_230other,sb2_230other, sb3_230other,
                      sb4_230other,sb5_230other, sb6_230other
                      # conflict_230other, ps_230other,
                      # satisfaction_230other, cs_230other, cn_230other
                    )

#intro vs other
sb1_introother <- SBanalysis(data_intro$SB1, data_other$SB1, "sb1_introother")
sb2_introother <- SBanalysis(data_intro$SB2, data_other$SB2, "sb2_introother")
sb3_introother <- SBanalysis(data_intro$SB3, data_other$SB3, "sb3_introother")
sb4_introother <- SBanalysis(data_intro$SB4, data_other$SB4, "sb4_introother")
sb5_introother <- SBanalysis(data_intro$SB5, data_other$SB5, "sb5_introother")
sb6_introother <- SBanalysis(data_intro$SB6, data_other$SB6, "sb6_introother")
listintroother = list(sb1_introother,sb2_introother, sb3_introother,
                    sb4_introother,sb5_introother, sb6_introother
                    # conflict_230other, ps_230other,
                    # satisfaction_230other, cs_230other, cn_230other
)

# #For ones with catme, testing out 333 vs 232
# data_232 <- data_byCourse$"232"
# data_333 <- data_byCourse$"333"
# 
# sb1_232333 <- SBanalysis(data_230$SB1, data_other$SB1, "sb1_232333")
# sb2_232333 <- SBanalysis(data_230$SB2, data_other$SB2, "sb2_232333")
# sb3_232333 <- SBanalysis(data_230$SB3, data_other$SB3, "sb3_232333")
# sb4_232333 <- SBanalysis(data_230$SB4, data_other$SB4, "sb4_232333")
# sb5_232333 <- SBanalysis(data_230$SB5, data_other$SB5, "sb5_232333")
# sb6_232333 <- SBanalysis(data_230$SB6, data_other$SB6, "sb6_232333")
# conflict_232333 <- SBanalysis(data_230$Conflict, data_other$Conflict, "conflict_232333")
# ps_232333<- SBanalysis(data_230$PS, data_other$PS, "ps_232333")
# satisfaction_232333 <- SBanalysis(data_230$Satisfaction, data_other$Satisfaction, "satisfaction_232333")
# cs_232333 <- SBanalysis(data_230$ContributionSelf, data_other$ContributionSelf, "cs_232333")
# cn_232333 <- SBanalysis(data_230$ContributionNoSelf, data_other$ContributionNoSelf, "cn_232333")
# list232333 = list(sb1_232333,sb2_232333, sb3_232333,
#                   sb4_232333,sb5_232333, sb6_232333,
#                   conflict_232333, ps_232333,
#                   satisfaction_232333, cs_232333, cn_232333
# )


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

df120220 <- purrr::map_dfr(list120220, col_tibble)
df120230 <- purrr::map_dfr(list120230, col_tibble)
df120other <- purrr::map_dfr(list120other, col_tibble)
df220230 <- purrr::map_dfr(list220230, col_tibble)
df220other <- purrr::map_dfr(list220other, col_tibble)
df230other <- purrr::map_dfr(list230other, col_tibble)
dfintroother <- purrr::map_dfr(listintroother, col_tibble)
