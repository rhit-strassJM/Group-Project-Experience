# R Example

# Make sure your working directory is the same as where this R file and the CSV file are
data <- read.csv("290_All.csv")
View(data)

# example
# get the variable sb1
sb1 <- data$sb1
#
# # mean of sb1
# # na.rm = we are removing the "nas" which are all the students who did not fill out sb1
sb1_mean <- mean(sb1, na.rm = true)
#
# # sample size of sb1 (how many students filled out sb1) - we remove the "nas"
sb1_n <- length(sb1[!is.na(sb1)])
#
# # standard deviation of sb1
sb1_sd <- sd(sb1, na.rm = true)
#
# # 95% confidence interval = mean +/- margin
sb1_margin <- qt(0.975,df=(sb1_n)-1)*sb1_sd/sqrt(sb1_n)

sb1_lowerbound <- sb1_mean - sb1_margin
sb1_upperbound <- sb1_mean + sb1_margin

sb1_lowerbound # 4.1536
sb1_upperbound # 4.3626

# the 95% Confidence Interval of SB1 for all students who filled out SB1 is 4.15-4.36


# T-test example
# example groups
group1 <- c(8, 8, 9, 9, 9, 11, 12, 13, 13, 14, 15, 19)
group2 <- c(11, 12, 13, 13, 14, 14, 14, 15, 16, 18, 18, 19)

# perform two sample t-test
t.test(group1, group2, var.equal=TRUE)

# you can imagine having group 1 as men and group 2 as women

# splitting data by gender
data_bygender <- split(data, f = data$Gender)
data_men <- data_bygender$Man # just Men
data_women <- data_bygender$Woman # just Women
data_nonbinary <- data_bygender$Nonbinary # just Nonbinary

#splitting data by race
data_byrace <- split(data, f = data$Race)
data_white <- data_byrace$White
data_black <- data_byrace$Black
data_asian <- data_byrace$Asian
data_hispanic <- data_byrace$Hispanic
data_nonwhite <-subset(!(Race=="White"))

#everyone but white men
data_notwhitemen <- subset(data, !(Gender == "Man" & Race == "White"))
data_whitemen <-subset(data_men, Race=="White")

#Aggregated SB
agg = aggregate(data,by = list(data$Gender), FUN = mean)


# Get the variable SB1
sb1 <- data_notwhitemen$SB1

# Mean of SB1
# na.rm = we are removing the "NAs" which are all the students who did not fill out SB1
sb1_mean <- mean(sb1, na.rm = TRUE)

# Sample size of SB1 (how many students filled out SB1) - we remove the "NAs"
sb1_n <- length(sb1[!is.na(sb1)])

# Standard deviation of SB1
sb1_sd <- sd(sb1, na.rm = TRUE)

# 95% Confidence Interval = mean +/- margin
sb1_margin <- qt(0.975,df=(sb1_n)-1)*sb1_sd/sqrt(sb1_n)

sb1_lowerbound <- sb1_mean - sb1_margin
sb1_upperbound <- sb1_mean + sb1_margin

sb1_lowerbound # 3.7270
sb1_upperbound # 4.3419
