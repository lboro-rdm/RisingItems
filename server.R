function(input, output, session) {
  
  data_reactive <- eventReactive(list(input$selected_date, input$country), {
    formatted_start_date <- format(as.Date(input$selected_date) - months(2), "%b+%Y")
    formatted_date <- format(as.Date(input$selected_date), "%b+%Y")
    country_code <- if (input$country == "All") "" else countrycode(input$country, "country.name", "iso2c")
    
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
    
    response <- fromJSON(file_url)
    IRUS_df <- response$Statistics
    names(IRUS_df)[6:8] <- c("m1", "m2", "m3")
    IRUS_df$m1 <- as.numeric(IRUS_df$m1)
    IRUS_df$m2 <- as.numeric(IRUS_df$m2)
    IRUS_df$m3 <- as.numeric(IRUS_df$m3)
    return(IRUS_df)
  })
  
  data <- reactive({
    data_reactive()
  })
  
  # 🔹 NEW: Shared filtered reactive
  filtered_data <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    
    # Filter by item type
    if (!("All" %in% input$item_types)) {
      df <- df[df$Item_Type %in% input$item_types, ]
    }
    
    # Compute increases
    increase_m1_to_m2 <- ifelse(df$m1 == 0, NA, ((df$m2 - df$m1) / df$m1) * 100)
    increase_m2_to_m3 <- ifelse(df$m2 == 0, NA, ((df$m3 - df$m2) / df$m2) * 100)
    
    # Apply filters
    df <- df[df$Reporting_Period_Total >= input$reporting_period &
               increase_m1_to_m2 >= input$increase &
               increase_m2_to_m3 >= input$increase, ]
    
    df <- na.omit(df)
    df
  })
  
  # 🔹 Use filtered_data() in the UI
  output$items_title <- renderUI({
    df <- filtered_data()
    if (is.null(df) || nrow(df) == 0) {
      tags$p("There are no items with these parameters.", style = "font-weight: bold;")
    } else {
      tags$ul(
        lapply(seq_len(nrow(df)), function(i) {
          tags$li(
            tags$a(href = df$URI[i], target = "_blank", df$Item[i])
          )
        })
      )
    }
  })
  
  # 🔹 Use filtered_data() for CSV download too
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("IRUS_UniqueDownloads", format(input$selected_date, "%b%Y"), ".csv", sep="_")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}
