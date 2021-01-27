###########################################################################
#                                                                         #
# PROCESS GENDER INDEX DATA                                               #
#                                                                         #
# Author:  Jay Ware                                                       #
# Updated: 2020-05-13                                                     #
# Purpose: Take input datasets provided by analysts and standardise the   #
#          structure                                                      #
#                                                                         #
# Details: Creates dataset gender_index_data with following columns:      #
#          - variable: short name of indicator variable [CHR]             #
#          - ref_period: date/year of data points [CHR]                   #
#          - men: value to be used in gender equality score   [NUMERIC]   #
#          - women: value to be used in gender equality score [NUMERIC]   #
#          - overall: overall value for population [NUMERIC]              #
#                                                                         #
###########################################################################

library(openxlsx)
library(tidyverse)

####ADD EXCEL DATA####

gi_data <- 
  read.xlsx("Raw Data/GIWG dataset.xlsx") %>%
  select(variable,breakdown,ref_period,men,women,overall, men_n, women_n, overall_n)%>%
  mutate(ref_period=as.character(ref_period),
         men=as.numeric(men),
         women=as.numeric(women),
         overall=as.numeric(overall),
         men_n=as.numeric(men_n),
         women_n=as.numeric(women_n),
         overall_n=as.numeric(overall_n)
         )

gi_data <-
read.xlsx("Raw Data/Ministers.xlsx",startRow=3) %>%
  summarise(women = sum(women, na.rm = TRUE),
            men = sum(men, na.rm = TRUE)) %>%
  mutate(variable="ministers",
         ref_period="2018/19") %>%
  bind_rows(gi_data)

#add indicators from NPF dataset
npf_data <- 
  list.files("Raw Data", "NPF Data", full.names = TRUE) %>%
  last() %>%
  read.xlsx()  %>%
  as_tibble() %>%
  mutate(Figure = as.numeric(Figure)) %>%
  rename(breakdown = Breakdown)

gi_data <- npf_data %>%
  filter(grepl("Mental Wellbeing", Indicator, ignore.case = TRUE),
         Characteristic %in% c("Gender", "Total")) %>%
  mutate(variable = "mental_wellbeing",
         breakdown = case_when(breakdown %in% c("Female", "Women", "Woman") ~ "women",
                               breakdown %in% c("Male", "Men", "Man") ~ "men",
                               breakdown %in% c("Total") ~ "overall"
         )) %>%
  select(variable, ref_period = Year, breakdown, Figure) %>%
  spread(breakdown, Figure) %>%
  filter(!is.na(women), !is.na(men)) %>%
  bind_rows(gi_data)

gi_data <- npf_data %>%
  filter(grepl("Mental Wellbeing", Indicator, ignore.case = TRUE),
         !Characteristic %in% c("Gender", "Total", "None")) %>%
  mutate(variable = paste0("mental_wellbeing", "_", gsub("\\W", "_", Characteristic))) %>%
  select(variable, ref_period = Year, breakdown, overall = Figure) %>%
  bind_rows(gi_data)

gi_data <- npf_data %>%
  filter(grepl("Health Risk", Indicator, ignore.case = TRUE),
         Characteristic %in% c("Gender", "Total")) %>%
  mutate(variable = "health_risk",
         breakdown = case_when(breakdown %in% c("Female", "Women", "Woman") ~ "women",
                               breakdown %in% c("Male", "Men", "Man") ~ "men",
                               breakdown %in% c("Total") ~ "overall"
         )) %>%
  select(variable, ref_period = Year, breakdown, Figure) %>%
  spread(breakdown, Figure) %>%
  filter(!is.na(women), !is.na(men)) %>%
  bind_rows(gi_data)

gi_data <- npf_data %>%
  filter(grepl("Health Risk", Indicator, ignore.case = TRUE),
         !Characteristic %in% c("Gender", "Total", "None")) %>%
  mutate(variable = paste0("health_risk", "_", gsub("\\W", "_", Characteristic))) %>%
  select(variable, ref_period = Year, breakdown, overall = Figure) %>%
  bind_rows(gi_data)

#Healthy life expectancy from NRS tables 2016-18
nrs_data <- 
  read.xlsx("Raw Data/life-expectancy-16-18-tables.xlsx", sheet = 2, startRow = 2) %>%
  as_tibble()

nrs_dataSES <- 
  read.xlsx("Raw Data/life-expectancy-16-18-tables.xlsx", sheet = 5, startRow = 2)  %>%
  as_tibble()

hle <- nrs_data %>%
  filter(Age.band == "less than 1") %>%
  mutate(variable = "hle", ref_period = "2016-18",
         Sex = case_when(Sex %in% c("Female", "Women", "Woman", "female", "women", "woman") ~ "women",
                         Sex %in% c("Male", "Men", "Man", "male", "men", "man") ~ "men")) %>%
  select(variable, ref_period, Sex, Proportion = contains("Proportion")) %>%
  spread(Sex, Proportion)

hleSES <- nrs_dataSES %>%
  filter(Age.band == "less than 1") %>%
  mutate(variable = "hleSES", ref_period = "2016-18",
         Sex = case_when(Sex %in% c("Female", "Women", "Woman", "female", "women", "woman") ~ "women",
                         Sex %in% c("Male", "Men", "Man", "male", "men", "man") ~ "men")) %>%
  select(variable, ref_period, Sex, Proportion = contains("Proportion"), breakdown = contains("SIMD")) %>%
  spread(Sex, Proportion)

gi_data <- bind_rows(hle, hleSES, gi_data)

#Life satisfaction indicator

health_data <- map_dfr(c("W9", "W10", "W11", "W12"),
                         ~read.xlsx("Raw Data/General+Health+and+Wellbeing.xlsx", .x, startRow = 4, fillMergedCells = TRUE) %>% mutate(table = .x))
health_data <- health_data %>% 
  mutate(ref_period = "2018",
         gender = case_when(X1 %in% c("Female", "Women", "Woman", "female", "women", "woman") ~ "women",
                            X1 %in% c("Male", "Men", "Man", "male", "men", "man") ~ "men",
                            X1 %in% c("All adults") ~ "overall")) %>%
  filter(gender %in% c("women", "men", "overall"), grepl("9", X2)) %>%
  select(-X1, -X2) %>%
  gather("breakdown", "Figure", -gender, -table, -ref_period) %>%
  filter(!is.na(Figure)) %>%
  mutate(Figure = as.numeric(Figure))

health_dataG <- health_data %>%
  filter(breakdown == "X10") %>% #X10 is total - column name is missing on input data
  mutate(variable = "life_sat") %>%
  spread(gender, Figure)

health_dataA <- health_data %>%
  filter(table == "W9", breakdown != "X10") %>% #X10 is total - column name is missing on input data
  mutate(variable = "life_satA") %>%
  spread(gender, Figure)

health_dataI <- health_data %>%
  filter(table == "W10") %>% 
  mutate(variable = "life_satI",
         breakdown = gsub("\\.", " ", breakdown),
         breakdown = gsub("Bottom", "5th", breakdown),
         breakdown = gsub("Top", "1st", breakdown)) %>%
  spread(gender, Figure)

health_dataSES <- health_data %>%
  filter(table == "W11") %>% 
  mutate(variable = "life_satSES",
         breakdown = gsub("\\.", " ", breakdown),
         breakdown = gsub("\\(?(\\d)\\w+\\)?", "\\1", breakdown),
         breakdown = sub("( \\w)", " -\\1", breakdown)) %>%
  spread(gender, Figure)

health_dataD <- health_data %>%
  filter(table == "W12") %>% 
  mutate(variable = "life_satD",
         breakdown = gsub("\\.", " ", breakdown),
         breakdown = gsub("LI", "Long-term Condition", breakdown)) %>%
  spread(gender, Figure)

gi_data <- bind_rows(health_dataA, health_dataD, health_dataG, health_dataI, health_dataSES, gi_data)

#Remove unneeded columns
gi_data <- gi_data %>% select(variable,breakdown,ref_period,men,women,overall, men_n, women_n, overall_n)
  
####SAVE####

saveRDS(gi_data,file="gi_data.rds")





