###################################################################
#
#   MATH 5530
#   
#   Dylan Denner & Jackson Tucker
#   dd575213@ohio.edu & jt070017@ohio.edu
#   03/24/2021
#   
#   Overview:
#   Getting first dataset from ACS. 
#   Poverty by sex by edu attainment
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
# Getting data from ACS


poverty_by_sex_edu_attainment <- get_acs(
  geography = "school district (unified)", 
  variables = c(total_population = "B17003_001",
                below_poverty_num = "B17003_002",
                below_poverty_num_men = "B17003_003",
                M_below_poverty_less_than_HS = "B17003_004",
                M_below_poverty_HS = "B17003_005",
                M_below_poverty_some_college = "B17003_006",
                M_below_poverty_bachelor_plus = "B17003_007",
                below_poverty_num_women = "B17003_008",
                W_below_poverty_less_than_HS = "B17003_009",
                W_below_poverty_HS = "B17003_010",
                W_below_poverty_some_college = "B17003_011",
                W_below_poverty_bachelor_plus = "B17003_012",
                above_poverty_num = "B17003_013",
                above_poverty_num_men = "B17003_014",
                M_above_poverty_less_than_HS = "B17003_015",
                M_above_poverty_HS = "B17003_016",
                M_above_poverty_some_college = "B17003_017",
                M_above_poverty_bachelor_plus = "B17003_018",
                above_poverty_num_women = "B17003_019",
                W_above_poverty_less_than_HS = "B17003_020",
                W_above_poverty_HS = "B17003_021",
                W_above_poverty_some_college = "B17003_022",
                W_above_poverty_bachelor_plus = "B17003_023"),
  state = "OH", 
  year = 2019)

poverty_by_sex_edu_attainment <- select(
  poverty_by_sex_edu_attainment,
  -"moe",
  -"GEOID"
)

poverty_by_sex_edu_attainment <- spread(poverty_by_sex_edu_attainment, variable, estimate)

mod_poverty_by_sex_edu_attainment <- poverty_by_sex_edu_attainment %>% mutate(
  "tot_less_than_HS" = W_below_poverty_less_than_HS + M_below_poverty_less_than_HS + 
                      W_above_poverty_less_than_HS + M_above_poverty_less_than_HS,
  "tot_HS" = W_below_poverty_HS + M_below_poverty_HS + 
            W_above_poverty_HS + M_above_poverty_HS,
  "tot_some_college" = W_below_poverty_some_college + M_below_poverty_some_college + 
                    W_above_poverty_some_college + M_above_poverty_some_college,
  "tot_bachelor_plus" = W_below_poverty_bachelor_plus + M_below_poverty_bachelor_plus + 
                      W_above_poverty_bachelor_plus + M_above_poverty_bachelor_plus,
  "tot_HS_PLUS" = tot_HS + tot_some_college + tot_bachelor_plus,
  "poverty_percentage" = below_poverty_num/total_population,
  "HS_PLUS_percentage" = tot_HS_PLUS/total_population)


mod_poverty_by_sex_edu_attainment <- select(
  mod_poverty_by_sex_edu_attainment,
  NAME,
  total_population,
  below_poverty_num,
  above_poverty_num,
  poverty_percentage,
  tot_less_than_HS,
  tot_HS,
  tot_some_college,
  tot_bachelor_plus,
  HS_PLUS_percentage
)

file_path_and_name = "../DATA/mod_poverty_by_sex_edu_attainment.csv"
write.csv(mod_poverty_by_sex_edu_attainment, file_path_and_name)

#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
