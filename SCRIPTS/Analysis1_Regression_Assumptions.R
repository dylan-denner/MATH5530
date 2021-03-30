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
library(ggplot2)
library(broom)
theme_set(theme_classic())

###################################################################
# Start program run time

start.time <- Sys.time()

###################################################################

file_path <- "../DATA/model_data_3_25.csv"
model_data <- read.csv(file_path)

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

plot(poverty_percent_SLR,1)

#par(mfrow = c(2, 2))
#plot(poverty_percent_SLR)
#plot(poverty_percent_SLR, 1)


poverty_percent_SLR_metrics <- augment(poverty_percent_SLR)

ggplot(poverty_percent_SLR_metrics, aes(poverty_percentage, HS_PLUS_percentage)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = poverty_percentage, yend = .fitted), color = "red", size = 0.3)

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
## Multiple Linear Regression

full_MLR <- lm(HS_PLUS_percentage ~ poverty_percentage + mean_chronic_absenteesim + mean_attendence, data = mod_model_data)

print(summary(full_MLR))

#####################################################################
## Multiple Linear Regression

dub_MLR <- lm(HS_PLUS_percentage ~ poverty_percentage + mean_chronic_absenteesim, data = mod_model_data)

print(summary(dub_MLR))

#####################################################################




#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)