library(shiny)
library(readr)
library(lubridate)
library(shinythemes)
library(DT)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Rising items from Loughborough University's Research Repository"),
  HTML("<p>This widget was created by Lara Skelly for Loughborough University. It pulls the number of unique downloads from IRUS over a three-month period. </p>",
       "<p>You should specify the end of this three-month period by selecting any day in that month from the selector below. Note that you will not be able to select the current month as it hasn't finished yet. If it is before the 3rd of the month, you will not be able to select the previous month either. You should also select the minimum total downloads for the three-month period and the percentage increase. </p>"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12,
               dateInput("selected_date", label = "Select a day:",
                         value = ifelse(day(Sys.Date()) <= 3, 
                                    ceiling_date(Sys.Date(), unit = "month") - months(3), 
                                    ceiling_date(Sys.Date(), unit = "month") - months(2)),
                         min = floor_date(Sys.Date(), unit = "month") - months(12),
                         max = ifelse(day(Sys.Date()) <= 3,
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

# Define server logic
server <- function(input, output) {
  # Reactive expression for calculating the file_url
  file_url <- reactive({
    # Get the selected date from input
    selected_date <- input$selected_date
    # Calculate the starting point as 3 months before the selected date
    start_date <- selected_date %m-% months(2)
    formatted_start_date <- format(start_date, "%b+%Y")
    formatted_date <- format(selected_date, "%b+%Y")
    
    # Stitch the formatted start date and end date into the file_url
    file_url <- paste0("https://irus.jisc.ac.uk/r5/report/item/irus_ir_master/?sort_column=Reporting_Period_Total&sort_order=DESC&begin_date=",
                       formatted_start_date, "&end_date=", formatted_date,
                       "&items=100&report_requested=1&institution%5B0%5D=2&repository%5B0%5D=2&access_method%5B0%5D=1&access_type%5B0%5D=4&data_type%5B0%5D=12&item_type%5B0%5D=23&item_type%5B1%5D=0&item_type%5B2%5D=26&item_type%5B3%5D=1&item_type%5B4%5D=2&item_type%5B5%5D=3&item_type%5B6%5D=4&item_type%5B7%5D=5&item_type%5B8%5D=30&item_type%5B9%5D=6&item_type%5B10%5D=7&item_type%5B11%5D=28&item_type%5B12%5D=8&item_type%5B13%5D=9&item_type%5B14%5D=22&item_type%5B15%5D=10&item_type%5B16%5D=25&item_type%5B17%5D=27&item_type%5B18%5D=11&item_type%5B19%5D=12&item_type%5B20%5D=13&item_type%5B21%5D=14&item_type%5B22%5D=15&item_type%5B23%5D=16&item_type%5B24%5D=17&item_type%5B25%5D=18&item_type%5B26%5D=24&item_type%5B27%5D=29&item_type%5B28%5D=19&item_type%5B29%5D=20&item_type%5B30%5D=21&metric_type%5B0%5D=10&output%5B0%5D=13&format=csv")
    return(file_url)
  })
  
  # Reactive expression for downloading and processing CSV data
  data <- reactive({
    # Download the CSV file from the stitched file_url
    download.file(file_url(), destfile = "IRUS.csv", mode = "wb")
    
    # Read the downloaded CSV file, skipping the first 30 rows
    data <- read_csv("IRUS.csv", skip = 30)
    
    # Set column names
    colnames(data) <- c("Item", "URI", "Item_Type", "Metric_Type", "Reporting_Period_Total", "m1", "m2", "m3")
    return(data)
  })
  
  # Define the reactive expression for filtering data and rendering UI
  output$items_title <- renderUI({
    # Get the filtered data
    data_filtered <- data()
    # Filter by selected item types
    if ("All" %in% input$item_types) {
      # Do nothing, keep all data
    } else {
      data_filtered <- data_filtered[data_filtered$Item_Type %in% input$item_types, ]
    }
    # Calculate the percentage increase for each item from January to February and from February to March
    increase_m1_to_m2 <- ifelse(data_filtered$m1 == 0, NA, ((data_filtered$m2 - data_filtered$m1) / data_filtered$m1) * 100)
    increase_m2_to_m3 <- ifelse(data_filtered$m2 == 0, NA, ((data_filtered$m3 - data_filtered$m2) / data_filtered$m2) * 100)
    
    data_filtered <- data_filtered[data_filtered$Reporting_Period_Total >= input$reporting_period & increase_m1_to_m2 >= input$increase & increase_m2_to_m3 >= input$increase, ]
    
    data_filtered <- na.omit(data_filtered)
    
    # Create hyperlinked titles
    titles <- lapply(data_filtered$Item, function(item) {
      tags$a(href = data_filtered[data_filtered$Item == item, "URI"], target = "_blank", item)
    })
    
    # Display titles as a list
    tags$ul(
      lapply(titles, function(title) {
        tags$li(title)
      })
    )
  })
  
  # Function to generate CSV file
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("IRUS_UniqueDownloads", format(input$selected_date, "%b%Y"), ".csv", sep="_")
    },
    content = function(file) {
      # Write data to CSV file
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
