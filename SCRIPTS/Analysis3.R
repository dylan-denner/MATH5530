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

#model_data <- read_csv("C:/Users/jacks/Documents/School/MATH_4530/MATH5530/OUTPUT/model_data_3_30.csv")
###################################################################
# Data Transformation

mod_model_data <- subset(model_data, model_data$HS_PLUS_percentage > 0.65)

mod_model_data$HS_PLUS_percentage <- (((mod_model_data$HS_PLUS_percentage ^ 6) - 1) / 6)


###################################################################
library(gam)
library(mgcv)

gam_model_1 <- gam(HS_PLUS_percentage ~ s(poverty_percentage) + s(mean_chronic_absenteeism) + s(mean_attendance) + s(mean_enrollment) + s(mean_total_students_discipline), data = mod_model_data)
summary(gam_model_1)
gam.check(gam_model_1)

gam_model_2 <- gam(HS_PLUS_percentage ~ s(poverty_percentage) + s(mean_chronic_absenteeism) + s(mean_attendance) + s(mean_enrollment), data = mod_model_data)
summary(gam_model_2)
gam.check(gam_model_2)

gam_model_3 <- gam(HS_PLUS_percentage ~ s(poverty_percentage) + s(mean_chronic_absenteeism) + s(mean_enrollment) + s(mean_total_students_discipline), data = mod_model_data)
summary(gam_model_3)
gam.check(gam_model_3)

anova(gam_model_1, gam_model_2, gam_model_3, test = "F")


plot(gam_model, se=TRUE, col="blue")

plot.gam(gam_model, se=TRUE, col="red")
#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)