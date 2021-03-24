###################################################################
#
#   MATH 5530
#   
#   Dylan Denner & Jackson Tucker
#   03/23/2021
#   dd575213@ohio.edu
#   
#   Overview:
#   ...
#   Hi Jackson! Now we don't have to send files via teams
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
  #-"moe",
  -"GEOID"
)

poverty_by_sex_edu_attainment <- spread(poverty_by_sex_edu_attainment, variable, c(estimate, moe))


poverty_by_sex_edu_attainment %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))


#####################################################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
