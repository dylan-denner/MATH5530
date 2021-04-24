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

dev.off()


setwd("C:/Users/dylan/Documents/R/MATH5530/Project/MATH5530/SCRIPTS/")

###################################################################
# Start program run time

start.time <- Sys.time()

###################################################################
library(dplyr)
library(boot)
library(splines)
library(ggplot2)
library(reshape2)
library(fabricatr)
library(mgcv)

###################################################################

file_path <- "../DATA/model_data_4_23.csv"
model_data <- read.csv(file_path)

# model_data$HS_PLUS_percentage <- model_data$HS_PLUS_percentage*100
# model_data$poverty_percentage <- model_data$poverty_percentage*100
# 
# file_path_and_name = "../DATA/model_data_4_23.csv"
# write.csv(model_data, file_path_and_name)


#model_data <- read_csv("C:/Users/jacks/Documents/School/MATH_4530/MATH5530/OUTPUT/model_data_3_30.csv")
###################################################################
# Data Transformation

mod_model_data <- subset(model_data, model_data$HS_PLUS_percentage > 65.0)
mod_model_data <- subset(mod_model_data, mod_model_data$mean_enrollment < 2000)

df <- mod_model_data %>% select(
  poverty_percentage,
  HS_PLUS_percentage
)

#####################################################################

ggplot(mod_model_data, aes(x=poverty_percentage, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Poverty % vs. Educational Attainment %") +
  xlab("Poverty %") + 
  ylab("Educational Attainment %")

ggplot(mod_model_data, aes(x=mean_total_students_discipline, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Student Discipline vs. Educational Attainment %") +
  xlab("Mean Student Discipline") + 
  ylab("Educational Attainment %")

ggplot(mod_model_data, aes(x=mean_chronic_absenteeism, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Chronic Absenteeism % vs. Educational Attainment %") +
  xlab("Mean Chronic Absenteeism %") + 
  ylab("Educational Attainment %")

ggplot(mod_model_data, aes(x=mean_attendance, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Attendance % vs. Educational Attainment %") +
  xlab("Mean Attendance %") + 
  ylab("Educational Attainment %")

ggplot(mod_model_data, aes(x=mean_enrollment, y=HS_PLUS_percentage)) + geom_point() + 
  ggtitle("Mean Enrollment vs. Educational Attainment %") +
  xlab("Mean Enrollment") + 
  ylab("Educational Attainment %")

#####################################################################
# Polynomial Regression

set.seed(100)

# Initializing Test MSE arrays
cv.poly1 <- rep(0,5)
cv.poly5 <- rep(0,5)
cv.poly10 <- rep(0,5)


for(i in 1:5){
  poly_fit <- glm(HS_PLUS_percentage~poly(poverty_percentage, i), data = df)
  
  # This function calculates the estimated K-fold cross-validation prediction error for generalized linear models
  cv.poly1[i] <- cv.glm(df,poly_fit)$delta[1]
  cv.poly5[i] <- cv.glm(df,poly_fit, K=5)$delta[1]
  cv.poly10[i] <- cv.glm(df, poly_fit, K=10)$delta[1]
  
}

poly_ <- c(1,2,3,4,5)

dt_poly <- data.frame(
  PolyDegree = poly_,
  "K = 1" = cv.poly1,
  "K = 5" = cv.poly5,
  "K = 10" = cv.poly10
)

df.m <- melt(dt_poly, id.vars = "PolyDegree")

ggplot(df.m, aes(PolyDegree, value, colour = variable)) +
  geom_point() + 
  ggtitle("Polynomial Regression: K = 1, 5, 10") +
  xlab("Polynomial Degree") + 
  ylab("Test MSE")

poly_fit1 <- glm(HS_PLUS_percentage~poly(poverty_percentage, 1), data = df)
poly_fit2 <- glm(HS_PLUS_percentage~poly(poverty_percentage, 2), data = df)
poly_fit3 <- glm(HS_PLUS_percentage~poly(poverty_percentage, 3), data = df)
poly_fit4 <- glm(HS_PLUS_percentage~poly(poverty_percentage, 4), data = df)
poly_fit5 <- glm(HS_PLUS_percentage~poly(poverty_percentage, 5), data = df)

poly_pred1 <- predict(poly_fit1, type = "response")
poly_pred2 <- predict(poly_fit2, type = "response")
poly_pred3 <- predict(poly_fit3, type = "response")
poly_pred4 <- predict(poly_fit4, type = "response")
poly_pred5 <- predict(poly_fit5, type = "response")

predict_df <- cbind(df, poly_pred1)
predict_df <- cbind(predict_df, poly_pred2)
predict_df <- cbind(predict_df, poly_pred3)
predict_df <- cbind(predict_df, poly_pred4)
predict_df <- cbind(predict_df, poly_pred5)


ggplot(predict_df, aes(x = poverty_percentage)) +
  geom_point(aes(y=HS_PLUS_percentage))+
  geom_line(aes(y=poly_pred1),color="red", size=1)+
  geom_line(aes(y=poly_pred2),color="yellow", size=1)+
  geom_line(aes(y=poly_pred3),color="orange", size=1)+
  geom_line(aes(y=poly_pred4),color="blue", size=1)+
  geom_line(aes(y=poly_pred5),color="pink", size=1)



#####################################################################
# Regression Splines


# Cubic spline df = # knots plus 4
cv.spline <- rep(0,10)

for(i in 5:14){
  spline_fit <- glm(HS_PLUS_percentage~bs(poverty_percentage, df = i), data = df)
  cv.spline[i-4] <- cv.glm(df, spline_fit)$delta[1]
}

spline_ <- c(1,2,3,4,5,6,7,8,9,10)

dt_splines <- data.frame(
  spline_knot = spline_,
  "Spline_TMSE" = cv.spline
)

ggplot(dt_splines, aes(x=spline_knot, y=Spline_TMSE)) + geom_point() + 
  ggtitle("Cubic Regression Spline") +
  xlab("Number of Knots") + 
  ylab("Test MSE")
  
spline_1 <- glm(HS_PLUS_percentage~bs(poverty_percentage, df = 5), data = df)
spline_5 <- glm(HS_PLUS_percentage~bs(poverty_percentage, df = 10), data = df)

spline_pred1 <- predict(spline_1, type = "response")
spline_pred2 <- predict(spline_5, type = "response")

predict_df <- cbind(predict_df, spline_pred1)
predict_df <- cbind(predict_df, spline_pred2)


ggplot(predict_df, aes(x = poverty_percentage)) +
  geom_point(aes(y=HS_PLUS_percentage))+
  geom_line(aes(y=spline_pred1),color="red", size=1)+
  geom_line(aes(y=spline_pred2),color="yellow", size=1)

#####################################################################
# Natural Cubic Spline

# Natrual Cubic spline df = # knots
cv.ns_spline <- rep(0,10)

for(i in 1:10){
  ns_spline_fit <- glm(HS_PLUS_percentage~ns(poverty_percentage, df = i), data = df)
  cv.ns_spline[i] <- cv.glm(df, ns_spline_fit)$delta[1]
}

foo <- data.frame(
  NS_Spline_TMSE = cv.ns_spline
)

dt_splines <- cbind(dt_splines, foo)
df.s <- melt(dt_splines, id.vars = "spline_knot")

ggplot(df.s, aes(spline_knot, value, colour = variable)) +
  geom_point() + 
  ggtitle("Regression Spline: Natural vs Unnatrual") +
  xlab("Number of Knots") + 
  ylab("Test MSE")


ns_spline_1 <- glm(HS_PLUS_percentage~bs(poverty_percentage, df = 1), data = df)
ns_spline_8 <- glm(HS_PLUS_percentage~bs(poverty_percentage, df = 8), data = df)

ns_pred1 <- predict(ns_spline_1, type = "response")
ns_pred8 <- predict(ns_spline_8, type = "response")

predict_df <- cbind(predict_df, ns_pred1)
predict_df <- cbind(predict_df, ns_pred8)


ggplot(predict_df, aes(x = poverty_percentage)) +
  geom_point(aes(y=HS_PLUS_percentage))+
  geom_line(aes(y=ns_pred1),color="red", size=1)+
  geom_line(aes(y=ns_pred8),color="yellow", size=1)

#####################################################################
# Smoothing Spline
attach(df)

smooth_spline_poverty_cv <- smooth.spline(poverty_percentage, HS_PLUS_percentage, cv=TRUE)
#smooth_spline_chron_cv <- smooth.spline(mod_model_data$mean_chronic_absenteeism, mod_model_data$HS_PLUS_percentage, cv=TRUE)

# Smooth Spline
cv.smooth_spline <- rep(0,10)
for(i in 5:14){
  smooth_fit <- smooth.spline(poverty_percentage, HS_PLUS_percentage, nknots = i)
  cv.smooth_spline[i-4] <- smooth_fit$cv.crit
}

smooth_ <- c(5:14)

dt_smooth <- data.frame(
  smooth_knot = smooth_,
  "Smooth_TMSE" = cv.smooth_spline
)

ggplot(dt_smooth, aes(x=smooth_knot, y=Smooth_TMSE)) + geom_point() + 
  ggtitle("Smoothing Spline") +
  xlab("Number of Knots") + 
  ylab("Test MSE")

foo <- data.frame(
  "Smooth_TMSE" = cv.smooth_spline
)

dt_splines <- cbind(dt_splines, foo)

dt.ss <- melt(dt_splines, id.vars = "spline_knot")

ggplot(dt.ss, aes(spline_knot, value, colour = variable)) +
  geom_point() + 
  ggtitle("Regression Spline: Natural, Unnatrual, Smooth") +
  xlab("Number of Knots") + 
  ylab("Test MSE")

#####################################################################
# GAMs

#gam1 <- gam(HS_PLUS_percentage~s(poverty_percentage,12), data = mod_model_data, method = 'REML')
 
gam_poverty <- gam(HS_PLUS_percentage~s(poverty_percentage), data = mod_model_data, method = 'REML') 
gam_absenteeism <- gam(HS_PLUS_percentage~s(mean_chronic_absenteeism), data = mod_model_data, method = 'REML') 
gam_attendance <- gam(HS_PLUS_percentage~s(mean_attendance), data = mod_model_data, method = 'REML') 
gam_discipline <- gam(HS_PLUS_percentage~s(mean_total_students_discipline), data = mod_model_data, method = 'REML') 
gam_enrollment <- gam(HS_PLUS_percentage~s(mean_enrollment), data = mod_model_data, method = 'REML') 


plot(gam_poverty,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
plot(gam_absenteeism,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
plot(gam_attendance,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
plot(gam_discipline,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
plot(gam_enrollment,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)

gam_PAA <- gam(HS_PLUS_percentage~s(poverty_percentage)+s(mean_chronic_absenteeism)+s(mean_attendance), data = mod_model_data, method = 'REML') 
plot(gam_PAA,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)

gam_total <- gam(HS_PLUS_percentage~s(poverty_percentage)+s(mean_chronic_absenteeism)+s(mean_attendance)+s(mean_total_students_discipline)+s(mean_enrollment), data = mod_model_data, method = 'REML') 
plot(gam_total,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)

summary(gam_poverty)
summary(gam_PAA)
summary(gam_total)

gam.check(gam_poverty)

#Randomly shuffle the data
df_gam <- mod_model_data[sample(nrow(mod_model_data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(df_gam)),breaks=59,labels=FALSE)


cv.gam_poverty <- rep(0,59)
cv.gam_PAA <- rep(0,59)
cv.gam_total <- rep(0,59)


#Perform 10 fold cross validation
for(i in 1:59){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df_gam[testIndexes, ]
  trainData <- df_gam[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  cv_gam_poverty <- gam(HS_PLUS_percentage~s(poverty_percentage), data = trainData, method = 'REML') 
  cv_gam_PAA <- gam(HS_PLUS_percentage~s(poverty_percentage)+s(mean_chronic_absenteeism)+s(mean_attendance), data = trainData, method = 'REML') 
  cv_gam_total <- gam(HS_PLUS_percentage~s(poverty_percentage)+s(mean_chronic_absenteeism)+s(mean_attendance)+s(mean_total_students_discipline)+s(mean_enrollment), data = trainData, method = 'REML') 
  
  pred_poverty <- predict(cv_gam_poverty, testData)
  pred_PAA <- predict(cv_gam_PAA, testData)
  pred_total <- predict(cv_gam_total, testData)
  
  cv.gam_poverty[i] <- mean((pred_poverty - testData$HS_PLUS_percentage)^2)
  cv.gam_PAA[i] <- mean((pred_PAA - testData$HS_PLUS_percentage)^2)
  cv.gam_total[i] <- mean((pred_total - testData$HS_PLUS_percentage)^2)
}

print(mean(cv.gam_poverty))
print(mean(cv.gam_PAA))
print(mean(cv.gam_total))



  
#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)