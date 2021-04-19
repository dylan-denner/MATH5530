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
library(dplyr)
library(boot)
library(splines)
#library(tidycensus)
#library(tidyverse)
#library(tidyr)
#library(readxl)
#library(readr)
#library(gridExtra)
#library(ggpubr)
#library(MASS)
#library(car)
#library(lmtest)
library(ggplot2)


###################################################################

file_path <- "../DATA/model_data_3_30.csv"
model_data <- read.csv(file_path)

#model_data <- read_csv("C:/Users/jacks/Documents/School/MATH_4530/MATH5530/OUTPUT/model_data_3_30.csv")
###################################################################
# Data Transformation

mod_model_data <- subset(model_data, model_data$HS_PLUS_percentage > 0.65)
df <- mod_model_data %>% select(
  poverty_percentage,
  HS_PLUS_percentage
)

#####################################################################

plot(mod_model_data$poverty_percentage, mod_model_data$HS_PLUS_percentage)

ggplot(mod_model_data, aes(x=poverty_percentage, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Poverty % vs. Educational Attainment %") +
  xlab("Poverty %") + 
  ylab("Educational Attainment %")

#####################################################################
# Polynomial Regression

set.seed(100)
cv.poly <- rep(0,5)
for(i in 1:5){
  poly_fit <- glm(HS_PLUS_percentage~poly(poverty_percentage, i), data = df)
  cv.poly[i] <- cv.glm(df, poly_fit)$delta[1]
}

poly_ <- c(1,2,3,4,5)
plot(poly_, cv.poly)

#####################################################################
# Regression Splines

set.seed(100)
library(fabricatr)


# Cubic spline df has # knots plus 4
cv.spline <- rep(0,10)
for(i in 5:14){
  spline_fit <- glm(HS_PLUS_percentage~bs(poverty_percentage, df = i), data = df)
  cv.spline[i-4] <- cv.glm(df, spline_fit)$delta[1]
}


spine_ <- c(1,2,3,4,5,6,7,8,9,10)
plot(spine_, cv.spline)
#####################################################################
# Natural Cubic Spline

# Natrual Cubic spline df = # knots
cv.ns_spline <- rep(0,10)
for(i in 1:10){
  ns_spline_fit <- glm(HS_PLUS_percentage~ns(poverty_percentage, df = i), data = df)
  cv.ns_spline[i] <- cv.glm(df, ns_spline_fit)$delta[1]
}


spine_ <- c(1,2,3,4,5,6,7,8,9,10)
plot(spine_, cv.ns_spline)
#####################################################################
# Smoothing Spline
attach(df)

smooth_spline_poverty_cv <- smooth.spline(poverty_percentage, HS_PLUS_percentage, cv=TRUE)
smooth_spline_chron_cv <- smooth.spline(mod_model_data$mean_chronic_absenteeism, mod_model_data$HS_PLUS_percentage, cv=TRUE)

# Smooth Spline
cv.smooth_spline <- rep(0,16)
for(i in 4:20){
  smooth_fit <- smooth.spline(poverty_percentage, HS_PLUS_percentage, nknots = i)
  cv.smooth_spline[i-3] <- smooth_fit$cv.crit
}


smooth_ <- c(4:20)
plot(smooth_, cv.smooth_spline)

#####################################################################
# GAMs
attach(mod_model_data)
library(gam)
#library(gamclass)
gam1 <- gam(HS_PLUS_percentage~s(poverty_percentage,12)+s(mean_chronic_absenteeism, 12), data = mod_model_data)


#cv.gam <- CVgam(HS_PLUS_percentage~s(poverty_percentage,12)+s(mean_chronic_absenteeism, 12), data = mod_model_data, nfold = 10, debug.level = 0, method = "GCV.Cp",
#                printit = TRUE, cvparts = NULL, gamma = 1, seed = 29)

  
#Randomly shuffle the data
df_gam <- mod_model_data[sample(nrow(mod_model_data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(df_gam)),breaks=59,labels=FALSE)


cv.gam <- rep(0,59)

#Perform 10 fold cross validation
for(i in 1:59){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df_gam[testIndexes, ]
  trainData <- df_gam[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  gam_temp <- gam(HS_PLUS_percentage~s(poverty_percentage,12)+s(mean_chronic_absenteeism, 12), data = trainData)
  
  pred <- predict(gam_temp, testData)
  cv.gam[i] <- mean((pred - testData$HS_PLUS_percentage)^2)
  
}

print(mean(cv.gam))
  
#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)