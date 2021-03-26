###################################################################
#
#   MATH 5530
#   
#   Dylan Denner & Jackson Tucker
#   dd575213@ohio.edu & jt070017@ohio.edu
#   03/24/2021
#   
#   Overview:
#   Adding county and region from Ohio dataset
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

###################################################################
# Getting data

file_path <- "../DATA/BUILDING_OVERVIEW_1819.xlsx"
overview_data <- read_excel(file_path, sheet = "BUILDING_OVERVIEW")

file_path <- "../DATA/BUILDING_DISCIPLINE_1819.xlsx"
discipline_data <- read_excel(file_path, sheet = "DISCIPLINE")

###################################################################
# Formatting columns 

overview_data <- select(
  overview_data,
  "Building IRN",
  "Building Name",
  "District Name",
  "County",
  "Region",
  "Enrollment 2018-2019",
  "Attendance Rate 2018-2019",
  "Chronic Absenteeism Percent 2018-2019"
)

overview_data[,6:8] <- sapply(overview_data[,6:8],as.numeric)



discipline_data[ discipline_data == "<10" ] <- "0"
discipline_data[,8:26] <- sapply(discipline_data[,8:26],as.numeric)

discipline_data <- discipline_data[,-26]

discipline_data <- subset(discipline_data, discipline_data$`Discpline Reason Description` == "Disobedient/Disruptive Behavior")

discipline_data <- select(
  discipline_data,
  "Building IRN",
  "Students Disciplined - Expulsions",
  "Students Disciplined - Out-of-School Suspensions",
  "Students Disciplined - In-School Suspensions"
)

discipline_data <- discipline_data %>% mutate(
  "total_students_discipline" = (rowSums(discipline_data[,2:4])))

joined_df <- left_join(overview_data, discipline_data, by = "Building IRN")

###################################################################

grouped_df <- joined_df %>%
  group_by(`District Name`, County, Region) %>%
  summarise(mean_enrollment = mean(`Enrollment 2018-2019`, na.rm = TRUE),
            mean_attendence = mean(`Attendance Rate 2018-2019`, na.rm = TRUE),
            mean_chronic_absenteesim = mean(`Chronic Absenteeism Percent 2018-2019`, na.rm = TRUE),
            mean_expulsions = mean(`Students Disciplined - Expulsions`, na.rm = TRUE),
            mean_out_of_school_suspensions = mean(`Students Disciplined - Out-of-School Suspensions`, na.rm = TRUE),
            mean_in_school_suspensions = mean(`Students Disciplined - In-School Suspensions`, na.rm = TRUE),
            mean_total_students_discipline = mean(`total_students_discipline`, na.rm = TRUE))



#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
