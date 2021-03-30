###################################################################
#
#   MATH 5530
#   
#   Dylan Denner & Jackson Tucker
#   dd575213@ohio.edu & jt070017@ohio.edu
#   03/30/2021
#   
#   Overview:
#   Assumption checks and corrections
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
library(gridExtra)
library(ggpubr)
library(MASS)
library(car)
library(lmtest)



###################################################################

file_path <- "../DATA/model_data_3_30.csv"
model_data <- read.csv(file_path)

#model_data <- read_csv("C:/Users/jacks/Documents/School/MATH_4530/MATH5530/OUTPUT/model_data_3_25.csv")
###################################################################
# Potential Problems 3.3.3
# (1) Non-Linearity of the response-predictor relationships
# (2) Correlation of error terms
# (3) Non-constant variance of error terms
# (4) Outliers
# (5) High-leverage points
# (6) Collinearity



ggplot(model_data, aes(x=poverty_percentage, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Poverty % vs. Educational Attainment %") +
  xlab("Poverty %") + 
  ylab("Educational Attainment %")


ggplot(model_data, aes(x=mean_total_students_discipline, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Student Discipline vs. Educational Attainment %") +
  xlab("Mean Student Discipline") + 
  ylab("Educational Attainment %")


ggplot(model_data, aes(x=mean_chronic_absenteeism, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Chronic Absenteeism % vs. Educational Attainment %") +
  xlab("Mean Chronic Absenteeism %") + 
  ylab("Educational Attainment %")

ggplot(model_data, aes(x=mean_attendance, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Attendance % vs. Educational Attainment %") +
  xlab("Mean Attendance %") + 
  ylab("Educational Attainment %")

ggplot(model_data, aes(x=mean_enrollment, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Enrollment vs. Educational Attainment %") +
  xlab("Mean Enrollment") + 
  ylab("Educational Attainment %")


#####################################################################
# Removing Outliers

mod_model_data <- subset(model_data, model_data$HS_PLUS_percentage > 0.65)
require(gridExtra)

##############################################
# Poverty % vs. Educational Attainment %

plot1 <- ggplot(model_data, aes(x=poverty_percentage, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Poverty % vs. Educational Attainment %") +
  xlab("Poverty %") + 
  ylab("Educational Attainment %")

plot2 <- ggplot(mod_model_data, aes(x=poverty_percentage, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Modified: Poverty % vs. Educational Attainment %") +
  xlab("Poverty %") + 
  ylab("Educational Attainment %")

grid.arrange(plot1, plot2)

png("POV_EDU_MOD.png")
grid.arrange(plot1, plot2)
dev.off()

##############################################
# Mean Student Discipline vs. Educational Attainment %

plot1 <- ggplot(model_data, aes(x=mean_total_students_discipline, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Student Discipline vs. Educational Attainment %") +
  xlab("Mean Student Discipline") + 
  ylab("Educational Attainment %")

plot2 <- ggplot(mod_model_data, aes(x=mean_total_students_discipline, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Modified: Mean Student Discipline vs. Educational Attainment %") +
  xlab("Mean Student Discipline") + 
  ylab("Educational Attainment %")

grid.arrange(plot1, plot2)

png("DISCIPLINE_EDU_MOD.png")
grid.arrange(plot1, plot2)
dev.off()

##############################################
# Mean Chronic Absenteeism % vs. Educational Attainment %

plot1 <- ggplot(model_data, aes(x=mean_chronic_absenteeism, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Chronic Absenteeism % vs. Educational Attainment %") +
  xlab("Mean Student Discipline") + 
  ylab("Educational Attainment %")

plot2 <- ggplot(mod_model_data, aes(x=mean_chronic_absenteeism, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Modified: Mean Chronic Absenteeism % vs. Educational Attainment %") +
  xlab("Mean Chronic Absenteeism %") + 
  ylab("Educational Attainment %")

grid.arrange(plot1, plot2)

png("CRON_EDU_MOD.png")
grid.arrange(plot1, plot2)
dev.off()

##############################################
# Mean Attendance and Educational Attainment

plot1 <- ggplot(model_data, aes(x=mean_attendance, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Attendance % vs. Educational Attainment %") +
  xlab("Mean Attendance %") + 
  ylab("Educational Attainment %")

plot2 <- ggplot(mod_model_data, aes(x=mean_attendance, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Modified: Mean Attendance % vs. Educational Attainment %") +
  xlab("Mean Attendance %") + 
  ylab("Educational Attainment %")

grid.arrange(plot1, plot2)

png("ATTEN_EDU_MOD.png")
grid.arrange(plot1, plot2)
dev.off()

##############################################
# Mean Enrollment and Educational Attainment

ggplot(model_data, aes(x=mean_enrollment, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Enrollment vs. Educational Attainment %") +
  xlab("Mean Enrollment") + 
  ylab("Educational Attainment %")

plot1 <- ggplot(model_data, aes(x=mean_enrollment, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Enrollment vs. Educational Attainment %") +
  xlab("Mean Enrollment %") + 
  ylab("Educational Attainment %")

plot2 <- ggplot(mod_model_data, aes(x=mean_enrollment, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Modified: Mean Enrollment vs. Educational Attainment %") +
  xlab("Mean Enrollment") + 
  ylab("Educational Attainment %")

grid.arrange(plot1, plot2)

png("ENROLL_EDU_MOD.png")
grid.arrange(plot1, plot2)
dev.off()

##############################################
# Simple Linear Regression

POVERTY_lm <- lm(HS_PLUS_percentage ~ poverty_percentage, data = mod_model_data)
DISCIPLINE_lm <- lm(HS_PLUS_percentage ~ mean_total_students_discipline, data = mod_model_data)
CHRONIC_lm <- lm(HS_PLUS_percentage ~ mean_chronic_absenteeism, data = mod_model_data)
ATTENDANCE_lm <- lm(HS_PLUS_percentage ~ mean_attendance, data = mod_model_data)
ENROLLMENT_lm <- lm(HS_PLUS_percentage ~ mean_enrollment, data = mod_model_data)

##############################################
# (1) Diagnostics

png("POVERTY_dignose.png")
autoplot(POVERTY_lm)
dev.off()

png("DISCIPLINE_dignose.png")
autoplot(DISCIPLINE_lm)
dev.off()

png("CHRONIC_dignose.png")
autoplot(CHRONIC_lm)
dev.off()

png("ATTENDANCE_dignose.png")
autoplot(ATTENDANCE_lm)
dev.off()

png("ENROLLMENT_dignose.png")
autoplot(ENROLLMENT_lm)
dev.off()

#####################################################################
#  Shapiro-Wilk Normality Test

POVERTY_sresid <- studres(POVERTY_lm)
shapiro.test(POVERTY_sresid)

DISCIPLINE_sresid <- studres(DISCIPLINE_lm)
shapiro.test(DISCIPLINE_sresid)

CHRONIC_sresid <- studres(CHRONIC_lm)
shapiro.test(CHRONIC_sresid)

ATTENDANCE_sresid <- studres(ATTENDANCE_lm)
shapiro.test(ATTENDANCE_sresid)

ENROLLMENT_sresid <- studres(ENROLLMENT_lm)
shapiro.test(ENROLLMENT_sresid)

#####################################################################
# Testing the Homoscedasticity Assumption

ncvTest(POVERTY_lm)
ncvTest(DISCIPLINE_lm)
ncvTest(CHRONIC_lm)
ncvTest(ATTENDANCE_lm)
ncvTest(ENROLLMENT_lm)

bptest(POVERTY_lm)
bptest(DISCIPLINE_lm)
bptest(CHRONIC_lm)
bptest(ATTENDANCE_lm)
bptest(ENROLLMENT_lm)

#####################################################################
# Testing the Independence (Autocorrelation) Assumption



#####################################################################
# Transformations

mod_model_data[,"poverty_percentage"] <- sqrt(mod_model_data$poverty_percentage)
png("POVERTY_dignose_sqrt.png")
autoplot(lm(HS_PLUS_percentage ~ poverty_percentage, data = mod_model_data), label.size = 3)
dev.off()

## Drop Discipline, not at all linear

mod_model_data[,"mean_chronic_absenteeism"] <- sqrt(mod_model_data$mean_chronic_absenteeism)
png("CHRONIC_dignose_sqrt.png")
autoplot(lm(HS_PLUS_percentage ~ mean_chronic_absenteeism, data = mod_model_data), label.size = 3)
dev.off()


mod_model_data[,"mean_attendance"] <- sqrt(mod_model_data$mean_attendance)
png("ATTENDANCE_dignose_sqrt.png")
autoplot(lm(HS_PLUS_percentage ~ mean_attendance, data = mod_model_data), label.size = 3)
dev.off()

## Drop enrollment, not linear?

#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)