library(shiny)
library(shinycssloaders)
library(lubridate)
library(plotly)
library(colourpicker)

ui <- tags$html(
  lang = "en",
  fluidPage(
    style = "padding: 0px; margin: 0px;",
    tags$head(
      tags$title("Rising Items"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # Black banner
    tags$div(
      class = "black-banner",
      tags$div(
        class = "banner-content",
        tags$a(
          href = "https://www.lboro.ac.uk",
          target = "_blank",
          tags$img(src = "logo.png", class = "uni-logo", alt = "University Logo")
        ),
        tags$a(
          href = "https://www.lboro.ac.uk/services/library/",
          target = "_blank",
          class = "return-link",
          "University Library"
        )
      )
    ),
    
    # Blue banner
    tags$div(
      class = "blue-banner",
      tags$div(
        class = "banner-content",
        tags$span("Open Research Services"),
        tags$a(
          href = "https://repository.lboro.ac.uk/",
          class = "return-link",
          "< Return to Research Repository"
        )
      )
    ),
    
    # Page title (using navbarPage only for the header style)
    navbarPage(
      title = span("Rising items from Loughborough University's Research Repository", class = "title"),
      id = "main_nav",
      fluid = TRUE
    ),
    
    # Description and sidebar layout
    HTML(
      "<p style='margin-left: 15px; margin-right: 15px;'>This widget was created by Lara Skelly for Loughborough University. 
      It pulls the number of unique downloads from IRUS over a three-month period.</p>",
      "<p style='margin-left: 15px; margin-right: 15px;'>You should specify the end of this three-month period by selecting any day in that month from the selector below. 
      Note that you will not be able to select the current month as it hasn't finished yet. 
      If it is before the 5th of the month, you will not be able to select the previous month either. 
      You should also select the minimum total downloads for the three-month period and the percentage increase.</p>",
      "<p style='color: red; font-weight: bold;margin-left: 15px; margin-right: 15px;'>
This app will be retired in January 2026 as the data provider (JISC) will no longer be supplying the necessary dataset.
</p>"
    ),
    
    sidebarLayout(
      sidebarPanel(
        style = "margin: 15px;",
        fluidRow(
          column(12,
                 dateInput("selected_date", label = "Select a day:",
                           value = ifelse(day(Sys.Date()) <= 5, 
                                          ceiling_date(Sys.Date(), unit = "month") - months(3), 
                                          ceiling_date(Sys.Date(), unit = "month") - months(2)),
                           min = floor_date(Sys.Date(), unit = "month") - months(12),
                           max = ifelse(day(Sys.Date()) <= 5,
                                        ceiling_date(Sys.Date(), unit = "month") - months(2) - days(1),
                                        ceiling_date(Sys.Date(), unit = "month") - months(1) - days(1))
                 ),
                 sliderInput("reporting_period", "Reporting period total:",
                             min = 1, max = 100, value = 20),
                 sliderInput("increase", "Percentage increase:",
                             min = 1, max = 100, value = 20),
                 checkboxGroupInput("item_types", "Select Item Types:",
                                    choices = c("All", "Article", "Book", "Book Section",
                                                "Conference Item", "Dataset", "Image",
                                                "Music/Musical Composition", "Performance",
                                                "Report", "Software", "Text",
                                                "Thesis or Dissertation", "Other"),
                                    selected = "All"),
                 selectInput("country", "Select Country:",
                             choices = country_choices,
                             selected = "All"),
                 downloadButton("download_csv", "Download CSV")
          )
        ),
        
        fluidRow(
          column(12,
                 HTML(
                   "<p></p>",
                   "<p style='font-size: 10px;'>The code for this app can be found at 
                   <a href='https://github.com/lboro-rdm/RisingItems.git'>GitHub</a>.</p>",
                   "<p style='font-size: 10px;'>
To cite this item: Skelly, Lara (2024). 
Rising Items: a R/Shiny app that identifies the items which are gaining interest 
in the Loughborough Research Repository. Loughborough University. Online resource. 
<a href='https://doi.org/10.17028/rd.lboro.25696158' target='_blank'>https://doi.org/10.17028/rd.lboro.25696158</a>
</p>",
                   "<p style='font-size: 10px;'>With thanks to David Campling for inspiring the idea, 
                   and ChatGPT for the coding coaching.</p>"
                 )
          )
        )
      ),
      
      mainPanel(
        uiOutput("items_title") %>% withSpinner(color = "#8D9C27")
      )
    ),
    
    # Footer
    tags$div(
      class = "footer",
      fluidRow(
        column(12,
               tags$a(href = "https://doi.org/10.17028/rd.lboro.28525481",
                      "Accessibility Statement")
        )
      )
    )
  )
)
