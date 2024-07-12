library(shiny)
library(shinythemes)
library(lubridate)
library(countrycode)

# Get country names and their ISO 3166 Alpha-2 codes
country_choices <- c("All", countrycode::codelist$country.name.en)

fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Rising items from Loughborough University's Research Repository"),
  HTML("<p>This widget was created by Lara Skelly for Loughborough University. It pulls the number of unique downloads from IRUS over a three-month period. </p>",
       "<p>You should specify the end of this three-month period by selecting any day in that month from the selector below. Note that you will not be able to select the current month as it hasn't finished yet. If it is before the 5th of the month, you will not be able to select the previous month either. You should also select the minimum total downloads for the three-month period and the percentage increase. </p>"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12,
               dateInput("selected_date", label = "Select a day:",
                         value = ifelse(day(Sys.Date()) <= 5, 
                                        ceiling_date(Sys.Date(), unit = "month") - months(3), 
                                        ceiling_date(Sys.Date(), unit = "month") - months(2)),
                         min = floor_date(Sys.Date(), unit = "month") - months(12),
                         max = ifelse(day(Sys.Date()) <= 5,
                                      ceiling_date(Sys.Date(), unit = "month") - months(2) - days(1),
                                      ceiling_date(Sys.Date(), unit = "month") - months(1) - days(1))),
               sliderInput("reporting_period", "Reporting period total:",
                           min = 1, max = 100, value = 20),
               sliderInput("increase", "Percentage increase:",
                           min = 1, max = 100, value = 20),
               checkboxGroupInput("item_types", "Select Item Types:",
                                  choices = c("All", "Article", "Book", "Book Section",
                                              "Conference Item", "Dataset", "Image",
                                              "Music/Musical Composition",
                                              "Performance", "Report", "Software",
                                              "Text", "Thesis or Dissertation","Other"),
                                  selected = "All"),
               selectInput("country", "Select Country:",
                           choices = country_choices,
                           selected = "All"),
               downloadButton("download_csv", "Download CSV")
        )
      ),
      
      fluidRow(
        column(12,
               HTML("<p > </p>",
                    "<p > </p>",
                    "<p style='font-size: 10px;' >The code for this app can be found at <a href='https://github.com/lboro-rdm/RisingItems.git'>GitHub</a>.</p>",
                    "<p style='font-size: 10px;' >To cite this item: Skelly, Lara (2024). Rising Items: a R/Shiny app that identifies the items which are gaining interest in the Loughborough Research Repository. Loughborough University. Online resource. https://doi.org/10.17028/rd.lboro.25696158 </p>",
                    "<p style='font-size: 10px;' >With thanks to David Campling for inspiring the idea, and ChatGPT for the coding coaching. </p>"),
        ))),
    mainPanel(
      uiOutput("items_title")
    )
  )
)
