###########################################################################
#                                                                         #
# LOAD GENDER INDEX DATA                                                  #
#                                                                         #
# Author:  Jay Ware                                                       #
# Updated: 2020-05-20                                                     #
# Purpose: Load the gender index data and add functions to calculate      #
#          gender equality scores                                         #
#                                                                         #
# Details: gi_data contains all data (includes all years and additional   #
#          variables)                                                     #
#          - variable: short name of indicator variable [CHR]             #
#          - ref_period: date/year of data points [CHR]                   #
#          - men: value to be used in gender equality score   [NUMERIC]   #
#          - women: value to be used in gender equality score [NUMERIC]   #
#          - overall: overall value for population [NUMERIC]              #
#                                                                         #
#          gi_meta contains metadata for variables                        #
#          - variable: short name of indicator variable [CHR]             #
#          - description: description of variable [CHR]                   #
#          - ref_population: reference population for the variable [CHR]  #
#          - source: [CHR]                                                #
#          - link: [CHR]                                                  #
#          - notes: [CHR]                                                 #
#                                                                         #
#          gi_2020 contains the domains and variables for 2020 index      #
#          - domain: [CHR]                                                #
#          - subdomain: [CHR]                                             #
#          - indicator: [CHR]                                             #
#          - variable: short name of indicator variable [CHR]             #
#          - ref_period: year(s) used for index (usually 2018) [CHR]      #
#          - reversed: ["no", "inverse", "complement"] method used to     #
#                      reverse the data so that scale has high values     #
#                      being the desirable outcome                        #
#                                                                         #
#          add_scores: function to calculate gender equality scores       #
#                      parameters:                                        #
#                      - ds: dataset containing columns for variable,     #
#                            men, women, reversed [REQUIRED]              #
#                                                                         #
###########################################################################

#setwd("//s0177a/datashare/Social_Justice/Equalities/Gender Index/Gender Index 2020")

#packages
library(tidyverse)
library(openxlsx)

#load data

gi_data <- readRDS("gi_data.rds")

gi_meta <- read.xlsx("Gender Index Variables.xlsx")

gi_2020 <- read.xlsx("Gender Index 2020.xlsx")

#functions

add_scores <- function(ds) {
  if(!"variable" %in% names(ds)) stop("dataset should contain a variable column")
  if(!"women" %in% names(ds)) stop("dataset should contain a women column")
  if(!"men" %in% names(ds)) stop("dataset should contain a men column")
  if(!"reversed" %in% names(ds)) {
    warning("dataset doesn't contain a reversed column. Assuming all variables are orientated correctly")
    ds <- ds %>% mutate(reversed = "no")
  }
  
  #add indicator equality scores
  ds %>%
    mutate(.women = ifelse(reversed == "inverse", 1/women, women),
           .men = ifelse(reversed == "inverse", 1/men, men),
           .overall = ifelse(reversed == "inverse", 1/overall, overall),
           .women = ifelse(reversed == "complement", 100 - .women, .women),
           .men = ifelse(reversed == "complement", 100 - .men, .men),
           .overall = ifelse(reversed == "complement", 100 - .overall, .overall),
           .overall = ifelse(is.na(.overall), (.women + .men)/2, .overall)
    )%>%
    mutate(score = 1 + 99 * (1 - abs((.women - .men) / (.women + .men)))) %>%
    #mutate(score = 1 + 99 * (1 - abs(1 - .women / .overall))) %>%
    select(-.women,-.men,-.overall)
}

geometric_mean <- function(x, weight = NULL, na.rm = FALSE) {
  if(is.null(weight)) weight <- rep(1, length(x))
  if(na.rm == TRUE) {
    weight <- weight[!is.na(x)]
    x <- x[!is.na(x)]
  }
  if(length(x) ==0) return(NA)
  
  weight <- weight / sum(weight)
  map2_dbl(x, weight, ~{ .x ^ .y}) %>%
    prod()
}

add_domain_scores <- function(ds, year) {
  if(!"domain" %in% names(ds)) stop("dataset should contain a domain column")
  if(!"subdomain" %in% names(ds)) stop("dataset should contain a subdomain column")
  if(!"score" %in% names(ds)) stop("dataset should contain a score column")
  
  #add subdomain scores (arithmetic mean)
  ds <- ds %>%
    group_by(domain,subdomain) %>%
    summarise(score = mean(score, na.rm=TRUE)) %>%
    ungroup %>%
    mutate(indicator = "Total", ref_period = year %>% as.character()) %>%
    bind_rows(ds)
  
  #add domain scores (geometic mean)
  ds <- ds %>%
    filter(indicator == "Total") %>%
    group_by(domain) %>%
    summarise(score = geometric_mean(score, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(indicator = "Total", subdomain = "Total", ref_period = year %>% as.character()) %>%
    bind_rows(ds)
    
  #add index score (weighted geometric mean)
  ds %>%
    filter(subdomain == "Total") %>%
    mutate(weight = case_when(domain == "Work" ~ 21, 
                              domain == "Money" ~ 18, 
                              domain == "Time" ~ 27, 
                              domain == "Knowledge" ~ 8, 
                              domain == "Power" ~ 19, 
                              domain == "Health" ~ 7, 
                              TRUE ~ 0)) %>%
    #summarise(score = weighted.mean(score, weight ,na.rm = TRUE)) %>%
    summarise(score = geometric_mean(score, weight, na.rm = TRUE)) %>%
    mutate(indicator = "Total", subdomain = "Total", domain = "Total", ref_period = year %>% as.character()) %>%
    bind_rows(ds)

}

#calculate scores

scores2020_unrounded <- scores_2020 <- left_join(gi_2020, gi_data, by=c("variable", "ref_period")) %>%
  add_scores() %>%
  add_domain_scores(2020)

scores_timeseries <- left_join(gi_2020, gi_data, by=c("variable")) %>%
  add_scores()


#round values for publication
scores_2020 <- scores_2020 %>%
  mutate(men = ifelse(variable %in% c("wealth_pension", "wealth_net"), men %>% round(-2), men),
         women = ifelse(variable %in% c("wealth_pension", "wealth_net"), women %>% round(-2), women),
         overall = ifelse(variable %in% c("wealth_pension", "wealth_net"), overall %>% round(-2), overall),
         men = ifelse(variable %in% c("attain_secondary"), men %>% round(1), men),
         women = ifelse(variable %in% c("attain_secondary"), women %>% round(1), women),
         overall = ifelse(variable %in% c("attain_secondary"), overall %>% round(1), overall),
         men = ifelse(variable %in% c("poverty"), men %>% round(0), men),
         women = ifelse(variable %in% c("poverty"), women %>% round(0), women),
         overall = ifelse(variable %in% c("poverty"), overall %>% round(0), overall)
  )

gi_data <- gi_data %>%
  mutate(men = ifelse(variable %in% c("wealth_pension", "wealth_net"), men %>% round(-2), men),
         women = ifelse(variable %in% c("wealth_pension", "wealth_net"), women %>% round(-2), women),
         overall = ifelse(variable %in% c("wealth_pension", "wealth_net"), overall %>% round(-2), overall),
         men = ifelse(variable %in% c("attain_secondary"), men %>% round(1), men),
         women = ifelse(variable %in% c("attain_secondary"), women %>% round(1), women),
         overall = ifelse(variable %in% c("attain_secondary"), overall %>% round(1), overall),
         men = ifelse(variable %in% c("poverty"), men %>% round(0), men),
         women = ifelse(variable %in% c("poverty"), women %>% round(0), women),
         overall = ifelse(variable %in% c("poverty"), overall %>% round(0), overall)
  )
  

save(gi_data, gi_meta, gi_2020, scores_2020, scores_timeseries, file="Publication/pub_data.rData")

##Examples to insert values in Rmd
# filter(scores_2020, variable == "msps")$score ##insert score
# filter(scores_2020, variable == "msps")$women ##insert value for women
# filter(scores_2020, variable == "msps")$men ##insert value for men
# filter(scores_2020, variable == "msps")$ref_period ##insert year data is based on
# filter(scores_2020, indicator == "Total", subdomain == "Political")$score ##insert score for subdomain
# filter(scores_2020, subdomain == "Total", domain == "Power")$score ##insert score for domain
# filter(scores_2020, domain == "Total")$score ##insert composite gender equality score



