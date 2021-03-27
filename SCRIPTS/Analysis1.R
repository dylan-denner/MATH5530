###################################################################
#
#   MATH 5530
#   
#   Dylan Denner & Jackson Tucker
#   dd575213@ohio.edu & jt070017@ohio.edu
#   03/26/2021
#   
#   Overview:
#   
#   
###################################################################
# Working directory

setwd("C:/Users/dylan/Documents/R/MATH5530/Project/MATH5530/SCRIPTS/")

###################################################################
# Start program run time

start.time <- Sys.time()

###################################################################
# Libraries needed for analysis

library(dplyr)
library(tidycensus)
library(tidyverse)
library(tidyr)
library(readxl)
library(readr)
###################################################################
# Start program run time

start.time <- Sys.time()

###################################################################

file_path <- "../DATA/model_data_3_25.csv"
model_data <- read.csv(file_path)

model_data <- read_csv("C:/Users/jacks/Documents/School/MATH_4530/MATH5530/OUTPUT/model_data_3_25.csv")
###################################################################
# Checking linearity of independent variables to dependent variables

attach(model_data)

plot(poverty_percentage, HS_PLUS_percentage)

plot(mean_total_students_discipline, HS_PLUS_percentage)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)

plot(mean_attendence, HS_PLUS_percentage)

plot(mean_enrollment, HS_PLUS_percentage)
#####################################################################
# Same 5 outliers?

mod_model_data <- subset(model_data, model_data$HS_PLUS_percentage > 0.65)
attach(mod_model_data)

# Good
plot(poverty_percentage, HS_PLUS_percentage)
?abline
abline(lm(HS_PLUS_percentage~ poverty_percentage))
# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

#####################################################################
## Simple Linear Regression for poverty % and HS + %

poverty_percent_SLR <- lm(HS_PLUS_percentage ~ poverty_percentage, data = mod_model_data)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_SLR)
print(summary(poverty_percent_SLR))

#####################################################################
## Simple Linear Regression for mean_chronic_absenteesim % and HS + %

chronic_absenteesim_percent_SLR <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = mod_model_data)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_SLR)
print(summary(chronic_absenteesim_percent_SLR))


#####################################################################
## Simple Linear Regression for mean_attendence % and HS + %

attendence_percent_SLR <- lm(HS_PLUS_percentage ~ mean_attendence, data = mod_model_data)

plot(mean_attendence, HS_PLUS_percentage)
abline(attendence_percent_SLR)
print(summary(attendence_percent_SLR))

#####################################################################
##Assumption Checks for Normality
View(model_data)
library(ggpubr)
names(model_data)[names(model_data) == "mean_chronic_absenteesim"] <- "mean_chronic_absenteeism"
names(model_data)[names(model_data) == "mean_attendence"] <- "mean_attendance"

#ALL Significantly Different from normal 
shapiro.test(model_data$HS_PLUS_percentage)
ggqqplot(model_data$HS_PLUS_percentage, ylab = "Hs completion")
shapiro.test(model_data$mean_attendance)
ggqqplot(model_data$mean_attendance, ylab = "Attendance")
shapiro.test(model_data$poverty_percentage)
ggqqplot(model_data$poverty_percentage, ylab = "Poverty %")
shapiro.test(model_data$mean_chronic_absenteeism)
ggqqplot(model_data$mean_chronic_absenteeism, ylab = "chronic absenteeism")
shapiro.test(model_data$total_population)
ggqqplot(model_data$total_population, ylab = "total population")
shapiro.test(model_data$mean_enrollment)
ggqqplot(model_data$mean_enrollment, ylab = "Enrollment")

#####################################################################
## Assumption Checks for Multicollinearity

#Good
res <-cor.test(model_data$mean_enrollment,model_data$mean_chronic_absenteeism, method = "kendall")
res

# Multicollinearity
res <- cor.test(model_data$below_poverty_num ,model_data$mean_chronic_absenteeism, method = "kendall")
res

#Multicollinearity
res <- cor.test(model_data$below_poverty_num ,model_data$mean_enrollment, method = "kendall")
res

#Multicollinearity
res <- cor.test(model_data$total_population ,model_data$mean_enrollment, method = "kendall")
res

#Multicollinearity
res <- cor.test(model_data$poverty_percentage ,model_data$mean_enrollment, method = "kendall")
res
#####################################################################
## Multiple Linear Regression

full_MLR <- lm(HS_PLUS_percentage ~ poverty_percentage + mean_chronic_absenteesim + mean_attendence, data = mod_model_data)

print(summary(full_MLR))  #Interpretation is that mean-attendance


#####################################################################
## Multiple Linear Regression

dub_MLR <- lm(HS_PLUS_percentage ~ poverty_percentage + mean_chronic_absenteesim, data = mod_model_data)

print(summary(dub_MLR))

#####################################################################
## Poly Linear Regression

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_SLR)
print(summary(poverty_percent_SLR))

poly_regression <- lm(HS_PLUS_percentage ~ poverty_percentage + I(poverty_percentage^2), data = mod_model_data)

print(summary(poly_regression))

#####################################################################
## Region 1

region1 <- subset(mod_model_data, mod_model_data$Region == "Region 1")

attach(region1)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R1 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region1)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R1)
print(summary(poverty_percent_R1))



chronic_absenteesim_percent_R1 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region1)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R1)
print(summary(chronic_absenteesim_percent_R1))
#####################################################################
## Region 2

region2 <- subset(mod_model_data, mod_model_data$Region == "Region 2")

attach(region2)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R2 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region2)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R2)
print(summary(poverty_percent_R2))



chronic_absenteesim_percent_R2 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region2)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R2)
print(summary(chronic_absenteesim_percent_R2))

#####################################################################
## Region 3

region3 <- subset(mod_model_data, mod_model_data$Region == "Region 3")

attach(region3)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R3 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region3)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R3)
print(summary(poverty_percent_R3))



chronic_absenteesim_percent_R3 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region3)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R3)
print(summary(chronic_absenteesim_percent_R3))


#####################################################################
## Region 4

region4 <- subset(mod_model_data, mod_model_data$Region == "Region 4")

attach(region4)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R4 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region4)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R4)
print(summary(poverty_percent_R4))



chronic_absenteesim_percent_R4 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region4)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R4)
print(summary(chronic_absenteesim_percent_R4))

#####################################################################
## Region 5

region5 <- subset(mod_model_data, mod_model_data$Region == "Region 5")

attach(region5)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R5 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region5)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R5)
print(summary(poverty_percent_R5))



chronic_absenteesim_percent_R5 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region5)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R5)
print(summary(chronic_absenteesim_percent_R5))

#####################################################################
## Region 6

region6 <- subset(mod_model_data, mod_model_data$Region == "Region 6")

attach(region6)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R6 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region6)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R6)
print(summary(poverty_percent_R6))



chronic_absenteesim_percent_R6 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region6)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R6)
print(summary(chronic_absenteesim_percent_R6))

#####################################################################
## Region 7

region7 <- subset(mod_model_data, mod_model_data$Region == "Region 7")

attach(region7)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R7 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region7)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R7)
print(summary(poverty_percent_R7))



chronic_absenteesim_percent_R7 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region7)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R7)
print(summary(chronic_absenteesim_percent_R7))

#####################################################################
## Region 8

region8 <- subset(mod_model_data, mod_model_data$Region == "Region 8")

attach(region8)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R8 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region8)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R8)
print(summary(poverty_percent_R8))



chronic_absenteesim_percent_R8 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region8)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R8)
print(summary(chronic_absenteesim_percent_R8))

#####################################################################
## Region 9

region9 <- subset(mod_model_data, mod_model_data$Region == "Region 9")

attach(region9)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R9 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region9)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R9)
print(summary(poverty_percent_R9))



chronic_absenteesim_percent_R9 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region9)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R9)
print(summary(chronic_absenteesim_percent_R9))

#####################################################################
## Region 10

region10 <- subset(mod_model_data, mod_model_data$Region == "Region 10")

attach(region10)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R10 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region10)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R10)
print(summary(poverty_percent_R10))



chronic_absenteesim_percent_R10 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region10)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R10)
print(summary(chronic_absenteesim_percent_R10))

#####################################################################
## Region 11

region11 <- subset(mod_model_data, mod_model_data$Region == "Region 11")

attach(region11)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R11 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region11)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R11)
print(summary(poverty_percent_R11))



chronic_absenteesim_percent_R11 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region11)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R11)
print(summary(chronic_absenteesim_percent_R11))


#####################################################################
## Region 12

region12 <- subset(mod_model_data, mod_model_data$Region == "Region 12")

attach(region12)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R12 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region12)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R12)
print(summary(poverty_percent_R12))



chronic_absenteesim_percent_R12 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region12)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R12)
print(summary(chronic_absenteesim_percent_R12))

#####################################################################
## Region 13

region13 <- subset(mod_model_data, mod_model_data$Region == "Region 13")

attach(region13)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R13 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region13)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R13)
print(summary(poverty_percent_R13))



chronic_absenteesim_percent_R13 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region13)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R13)
print(summary(chronic_absenteesim_percent_R13))

#####################################################################
## Region 14

region14 <- subset(mod_model_data, mod_model_data$Region == "Region 14")

attach(region14)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R14 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region14)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R14)
print(summary(poverty_percent_R14))



chronic_absenteesim_percent_R14 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region14)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R14)
print(summary(chronic_absenteesim_percent_R14))

#####################################################################
## Region 15

region15 <- subset(mod_model_data, mod_model_data$Region == "Region 15")

attach(region15)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R15 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region15)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R15)
print(summary(poverty_percent_R15))



chronic_absenteesim_percent_R15 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region15)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R15)
print(summary(chronic_absenteesim_percent_R15))

#####################################################################
## Region 16

region16 <- subset(mod_model_data, mod_model_data$Region == "Region 16")

attach(region16)

# Good
plot(poverty_percentage, HS_PLUS_percentage)

# Bad
plot(mean_total_students_discipline, HS_PLUS_percentage)

# Good
plot(mean_chronic_absenteesim, HS_PLUS_percentage)

# Good
plot(mean_attendence, HS_PLUS_percentage)

# Bad
plot(mean_enrollment, HS_PLUS_percentage)

## Simple Linear Regression for poverty % and HS + %

poverty_percent_R16 <- lm(HS_PLUS_percentage ~ poverty_percentage, data = region16)

plot(poverty_percentage, HS_PLUS_percentage)
abline(poverty_percent_R16)
print(summary(poverty_percent_R16))



chronic_absenteesim_percent_R16 <- lm(HS_PLUS_percentage ~ mean_chronic_absenteesim, data = region16)

plot(mean_chronic_absenteesim, HS_PLUS_percentage)
abline(chronic_absenteesim_percent_R16)
print(summary(chronic_absenteesim_percent_R16))



#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)