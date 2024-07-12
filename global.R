library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(countrycode)
library(httr)

# Get country names and their ISO 3166 Alpha-2 codes
country_choices <- c("All", countrycode::codelist$country.name.en)