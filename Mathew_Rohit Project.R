rm(list = ls())
library(dplyr)
library(psych)

df1 <- read.csv("https://data.cityofnewyork.us/resource/9ct9-prf9.csv")
df2 <- read.csv("https://data.cityofnewyork.us/resource/vw9i-7mzq.csv")
df<-merge(x = df1, y = df2, by = "dbn")

# We are cleaning up the data
data = df %>% select(dbn, school_name.y, num_of_ap_test_takers, num_of_ap_total_exams_taken, num_of_ap_exams_passed, total_students, graduation_rate, attendance_rate, college_career_rate)  %>% filter(num_of_ap_test_takers != "s" & num_of_ap_total_exams_taken != "s" & num_of_ap_exams_passed != "s") 

# Convert string numbers into numeric
data$num_of_ap_test_takers = as.numeric(as.character(data$num_of_ap_test_takers))
data$num_of_ap_total_exams_taken = as.numeric(as.character(data$num_of_ap_total_exams_taken))
data$num_of_ap_exams_passed = as.numeric(as.character(data$num_of_ap_exams_passed))
data$total_students = as.numeric(as.character(data$total_students))
data$graduation_rate = as.numeric(as.character(data$graduation_rate))
data$attendance_rate = as.numeric(as.character(data$attendance_rate))
data$college_career_rate = as.numeric(as.character(data$college_career_rate))

# We are adding another metric to teh data frame based on already present data
data = data %>% mutate(pass_rate = num_of_ap_exams_passed/num_of_ap_total_exams_taken)
data = data %>% mutate(percent_student_AP = num_of_ap_test_takers/total_students)

# Data Summary of Variables

summary(data$pass_rate)
sd(data$pass_rate)
var(data$pass_rate)

summary(data$percent_student_AP)
sd(data$percent_student_AP)
var(data$percent_student_AP)

summary(data$graduation_rate)
sd(data$graduation_rate)
var(data$graduation_rate)

summary(data$attendance_rate)
sd(data$attendance_rate)
var(data$attendance_rate)

# Show plot of scatered variables to endure randomness
par(mfrow = c(1,1))
plot(data$pass_rate, ylab = "Pass Rate")
plot(data$graduation_rate, ylab = "Graduation Rate")
plot(data$attendance_rate, ylab = "Attendance Rate")

# Test to see how correlated variables are and if they are significant
cor.test(data$graduation_rate, data$pass_rate)
cor.test(data$attendance_rate, data$pass_rate)
cor.test(data$graduation_rate, data$percent_student_AP)
cor.test(data$attendance_rate, data$percent_student_AP)

# Development of linear model
imod = lm(graduation_rate ~ pass_rate + attendance_rate + total_students, data = data)
plot(imod)

# Use backward selection to find useful predictors
step(imod, direction='backward')
summary(imod)

'''
# Code below canbe used for futher analysis. Not used for our analysis
par(mfrow = c(1,2))


summary(data$num_of_ap_test_takers)
hist(data$num_of_ap_test_takers, xlab = "Number of AP Test Takers", main = "Number of AP Test Takers Histogram")
hist(log10(data$num_of_ap_test_takers), xlab = "Log 10 - Number of AP Test Takers", main = "Number of AP Test Takers Histogram")

summary(data$num_of_ap_total_exams_taken)
hist(data$num_of_ap_total_exams_taken, xlab = "Number of Total Exams Taken", main = "Number of Total Exams Taken Histogram")
hist(log10(data$num_of_ap_total_exams_taken), xlab = " Log 10 - Number of Total Exams Taken", main = "Number of Total Exams Taken Histogram")

summary(data$num_of_ap_exams_passed)
hist(data$num_of_ap_exams_passed, xlab = "Number of AP Exams Passed", main = "Number of AP Exams Passed Histogram")
hist(log10(data$num_of_ap_exams_passed), xlab = "Log 10 - Number of AP Exams Passed", main = "Number of AP Exams Passed Histogram")

summary(data$total_students)
hist(data$total_students, xlab = "Total Students", main = "Total Students Histogram")
hist(log10(data$total_students), xlab = "Log 10 - Total Students", main = "Total Students Histogram")

summary(data$graduation_rate)
hist(data$graduation_rate, xlab = "Graduation Rate", main = "Graduation Rate Histogram")
hist(log(data$graduation_rate), xlab = "Log 10 - Graduation Rate", main = "Graduation Rate Histogram")

summary(data$attendance_rate)
hist(data$attendance_rate, xlab = "Attendance Rate", main = "Attendance Rate Histogram")
# Log 10 transformation not needed here

summary(data$college_career_rate)
hist(data$college_career_rate, xlab = "College Career Rate", main = "College Career Rate Histogram")
# Log 10 transformation not needed here

par(mfrow = c(1,1))
'''