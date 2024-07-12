function(input, output, session) {
  
  data_reactive <- eventReactive(input$selected_date, {
    formatted_start_date <- format(as.Date(input$selected_date) - months(2), "%Y-%m-%d")
    formatted_date <- format(as.Date(input$selected_date), "%Y-%m-%d")
    
    country_code <- if (input$country == "All") {
      ""
    } else {
      countrycode(input$country, "country.name", "iso2c")
    }
    
    file_url <- if (input$country == "All") {
      paste0("https://irus.jisc.ac.uk/r5/report/item/irus_ir_master/?sort_column=Reporting_Period_Total&sort_order=DESC&begin_date=",
             formatted_start_date, "&end_date=", formatted_date,
             "&items=100&report_requested=1&institution%5B0%5D=2&repository%5B0%5D=2&access_method%5B0%5D=1&access_type%5B0%5D=4&data_type%5B0%5D=12&item_type%5B0%5D=23&item_type%5B1%5D=0&item_type%5B2%5D=26&item_type%5B3%5D=1&item_type%5B4%5D=2&item_type%5B5%5D=3&item_type%5B6%5D=4&item_type%5B7%5D=5&item_type%5B8%5D=30&item_type%5B9%5D=6&item_type%5B10%5D=7&item_type%5B11%5D=28&item_type%5B12%5D=8&item_type%5B13%5D=9&item_type%5B14%5D=22&item_type%5B15%5D=10&item_type%5B16%5D=25&item_type%5B17%5D=27&item_type%5B18%5D=11&item_type%5B19%5D=12&item_type%5B20%5D=13&item_type%5B21%5D=14&item_type%5B22%5D=15&item_type%5B23%5D=16&item_type%5B24%5D=17&item_type%5B25%5D=18&item_type%5B26%5D=24&item_type%5B27%5D=29&item_type%5B28%5D=19&item_type%5B29%5D=20&item_type%5B30%5D=21&metric_type%5B0%5D=10&output%5B0%5D=13&format=json")
    } else {
      paste0("https://irus.jisc.ac.uk/r5/report/item/irus_ir_master/?sort_column=Reporting_Period_Total&sort_order=DESC&begin_date=",
             formatted_start_date, "&end_date=", formatted_date,
             "&items=100&report_requested=1&institution%5B0%5D=2&repository%5B0%5D=2&country%5B0%5D=", country_code,
             "&access_method%5B0%5D=1&access_type%5B0%5D=4&data_type%5B0%5D=12&item_type%5B0%5D=23&item_type%5B1%5D=0&item_type%5B2%5D=26&item_type%5B3%5D=1&item_type%5B4%5D=2&item_type%5B5%5D=3&item_type%5B6%5D=4&item_type%5B7%5D=5&item_type%5B8%5D=30&item_type%5B9%5D=6&item_type%5B10%5D=7&item_type%5B11%5D=28&item_type%5B12%5D=8&item_type%5B13%5D=9&item_type%5B14%5D=22&item_type%5B15%5D=10&item_type%5B16%5D=25&item_type%5B17%5D=27&item_type%5B18%5D=11&item_type%5B19%5D=12&item_type%5B20%5D=13&item_type%5B21%5D=14&item_type%5B22%5D=15&item_type%5B23%5D=16&item_type%5B24%5D=17&item_type%5B25%5D=18&item_type%5B26%5D=24&item_type%5B27%5D=29&item_type%5B28%5D=19&item_type%5B29%5D=20&item_type%5B30%5D=21&metric_type%5B0%5D=10&output%5B0%5D=13&format=json")
    }
    
    # Print the URL for debugging purposes
    print(file_url)
    
    # Make the GET request
    response <- fromJSON(file_url)
    
    # Check if the request was successful
    if (http_type(response) == "application/json" && status_code(response) == 200) {
      IRUS_data <- fromJSON(content(response, "text"))
      
      if (!is.null(IRUS_data$Statistics)) {
        IRUS_df <- IRUS_data$Statistics
        names(IRUS_df)[6:8] <- c("m1", "m2", "m3")
        return(IRUS_df)
      } else {
        return(data.frame()) # return an empty data frame if no data
      }
    } else {
      print(content(response, "text"))  # Print the error response for debugging
      stop("Failed to retrieve JSON data")
    }
  })
  
  # Reactive expression for downloading and processing CSV data
  data <- reactive({
    IRUS_df <- data_reactive()
    return(IRUS_df)
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