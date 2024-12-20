# modules/incrTriModule.R

incrTriUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    bs4Card(
      title = "Incremental Triangle Analysis",
      status = "white",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 4,
          selectInput(ns("statutory_class"), "Select Statutory Class", choices = NULL)
        ),
        column(
          width = 4,
          selectInput(
            ns("time_scale"),
            "Select Time Scale",
            choices = c("Yearly", "Quarterly", "Monthly"),
            selected = "Yearly"
          )
        )
      ),
      uiOutput(ns("loss_year_range_ui")),
      fluidRow(
        column(
          width = 4,
          actionButton(ns("generate_triangle"), "Generate Incremental Triangle", class = "btn btn-primary btn-primary-custom")
        ),
        column(
          width = 4,
          downloadButton(ns("download_triangle"), "Download Triangle CSV", class = "btn btn-primary btn-primary-custom")
        )
      ),
      tags$div(
        class = "triangle-output-container",
        verbatimTextOutput(ns("incremental_triangle_output"))
      )
    )
  )
}

incrTriServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update the statutory class choices based on data
    observeEvent(data(), {
      req(data())
      updateSelectInput(session, "statutory_class", choices = unique(data()$Statutory_Class))
    })
    
    # Generate the UI for the loss year range slider based on the selected statutory class
    output$loss_year_range_ui <- renderUI({
      req(data())
      req(input$statutory_class)
      
      available_years <- data() %>%
        filter(Statutory_Class == input$statutory_class) %>%
        pull(Loss_Date) %>%
        year() %>%
        unique() %>%
        sort()
      
      sliderInput(
        ns("loss_year_range"),
        "Select Loss Year Range",
        min = min(available_years),
        max = max(available_years),
        value = c(min(available_years), max(available_years)),
        step = 1,
        sep = ""  
      )
    })

    # Compute the incrulative triangle when the button is clicked
    triangle_data  <- eventReactive(input$generate_triangle, {
      req(data())
      req(input$statutory_class, input$loss_year_range, input$time_scale)
      
      df <- data() %>%
        filter(
          Statutory_Class == input$statutory_class,
          year(Loss_Date) >= input$loss_year_range[1],
          year(Loss_Date) <= input$loss_year_range[2]
        )
      
      # Depending on the time scale, define origin and dev periods
      if (input$time_scale == "Yearly") {
        # Yearly: origin by year, dev in years
        df <- df %>%
          mutate(
            Loss_Origin = year(Loss_Date),
            Dev_period = year(Paid_Date) - year(Loss_Date)
          )
        
        origin_col <- "Loss_Origin"
        dev_col <- "Dev_period"
        
      } else if (input$time_scale == "Quarterly") {
        # Quarterly: origin by year-quarter, dev in quarters
        # Define an origin quarter index and dev quarter index
        df <- df %>%
          mutate(
            Loss_Year = year(Loss_Date),
            Loss_Q = quarter(Loss_Date),
            # Create a label or factor for the origin, e.g. "2017-Q1"
            Loss_Origin = factor(paste0(Loss_Year, "-Q", Loss_Q)),
            
            Dev_period = floor(
              ((year(Paid_Date)*12 + month(Paid_Date)) - (year(Loss_Date)*12 + month(Loss_Date))) / 3
            )
          )
        
        origin_col <- "Loss_Origin"
        dev_col <- "Dev_period"
        
      } else {
        # Monthly: origin by year-month, dev in months
        df <- df %>%
          mutate(
            Loss_Year = year(Loss_Date),
            Loss_M = month(Loss_Date),
            # Create a label or factor for the origin month, e.g. "2017-01"
            # Ensure consistent formatting with sprintf
            Loss_Origin = factor(paste0(Loss_Year, "-", sprintf("%02d", Loss_M))),
            
            Dev_period = (year(Paid_Date)*12 + month(Paid_Date)) - (year(Loss_Date)*12 + month(Loss_Date))
          )
        
        origin_col <- "Loss_Origin"
        dev_col <- "Dev_period"
      }

      # Aggregate and form incremental triangle
      filtered_data <- df %>%
        group_by(!!sym(origin_col), !!sym(dev_col)) %>%
        summarise(Gross_Amount = sum(Gross_Paid, na.rm = TRUE), .groups = "drop")
      
      # Convert to triangle
      inc_tri <- ChainLadder::as.triangle(
        filtered_data,
        origin = origin_col,
        dev = dev_col,
        value = "Gross_Amount"
      )

      inc_tri
    })

    # Render the incrulative triangle as text
    output$incremental_triangle_output <- renderText({
      req(triangle_data())
      # Capture printed output
      output_text <- capture.output(print(triangle_data()))
      paste(output_text, collapse = "\n")
    })

    # Download handler for the triangle data
    output$download_triangle <- downloadHandler(
      filename = function() {
        paste("incrulative_triangle_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(triangle_data())
        # triangle_data() should be a matrix or data frame-like structure
        write.csv(triangle_data(), file, row.names = TRUE)
      }
    )

  })
}