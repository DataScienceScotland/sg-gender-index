knitr::opts_chunk$set(echo = FALSE, message = TRUE)

# Load some R packages ----
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(stringr)
library(tidyverse)
library(knitr)
#library(kableExtra)
library(scales)
#library(haven)
#library(treemap)
#library(labelled)
#library(ggiraph)
library(plotly)
library(htmltools)

#Load data
load("pub_data.rData")

#utility function to convert markdown text to html (outwith knitting)
create_html <- function(md) {
  htmltools::HTML(
    markdown::markdownToHTML(
      text = md, fragment.only = TRUE
    )
  )
}

bootstrap_tooltip <- function(text, title) {
  paste0('<span data-toggle="tooltip" style = "border-bottom: 1px dotted black;white-space: nowrap" title="', title, '">', text, '</span>')
}

simd_tooltip <- bootstrap_tooltip("SIMD", "Scottish Index of Multiple Deprivation (SIMD) is an area-based measure of deprivation.")

#convert to UTF-8 encoding for special characters, £, etc. to display properly 
utf_encoding <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}

#temporarily disable UTF-8 encoding, as causing issues with the knit button - 
utf_encoding <- identity

tolower_first <- function(x) {
  paste0(tolower(substr(x,1,1)), substr(x,2,nchar(x)))
}

gender_score_note <- function(description, reversed, ref_population) {
  paste0("This indicator measures the ", 
         description %>% tolower_first() %>% sub("\\.?$", ".", .) %>% gsub("%", "percentage", .),
         " The reference population was ",
         sub("\\.?$", ".", tolower_first(ref_population)),
         " The gender equality score was calculated by comparing the value for women with the mid-point of men and women. ",
         ifelse(reversed == "complement", "For consistency, indicator data was reversed before calculating the equality score so that high percentages correspond to the more desirable outcome. ", ""),
         "See the [Methodology chapter](#background-and-methodology) for further details.")
}
