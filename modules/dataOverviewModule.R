# UI function for data overview
dataOverviewUI <- function(id) {
  ns <- NS(id)
  tagList(
      fluidRow(
          hr(),
      div(
        column(12,
        br(),
        br(),
        br(),
        class = "upload-container",
        fileInput(ns("file"), 
          label = tags$span("Upload Claims Data as an Excel or CSV File", class = "upload-label"),
          accept = c(".xlsx", ".xls", ".csv")
        ),
        br(),
        br())
        ),
        hr(),
        br(), 
      div(
          class = "upload-container",
          tags$p(class = "instruction-header", "How to Prepare Data before Upload:"),
          tags$ul(
              class = "list-item",
          tags$li(
              class = "custom-list-item", "Ensure the data format is either a CSV or Excel file."),
          tags$li(
              class = "custom-list-item", "Dataset Variable Definition:-"),
          tags$ul(
              class = "sub-list-item" ,
              tags$li(class = "custom-list-item", icon("tag"), tags$b("Claim_No: - "), " Unique identifier for each claim."),
              tags$li(class = "custom-list-item", icon("calendar-day"), tags$b("Loss_Date: - "), " Date when the loss occurred."),
              tags$li(class = "custom-list-item", icon("calendar-check"), tags$b("Paid_Date: - "), " Date when the claim payment was made."),
              tags$li(class = "custom-list-item", icon("money-bill-wave"), tags$b("Gross_Paid: - "), " Total amount paid for the claim (KES)."),
              tags$li(class = "custom-list-item", icon("layer-group"), tags$b("Statutory_Class: - "), " The Class of Business.")
                  )
              )
          ),
        hr(),
        br(),
          bs4Card(
            title = "Data Overview",
            status = "white",
            solidHeader = TRUE,
            width = 12,
            DTOutput(ns("data_table"))
          )          
        )
  )
}



# Server function for data overview
dataOverviewServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive value for storing the uploaded data with validation
  # Reactive data loading
    data <- reactive({
      req(input$file)
      inFile <- input$file

    withProgress(message = 'Reading and validating data...', {
      setProgress(0.2)

      # Attempt to read data
      file_extension <- tools::file_ext(inFile$name)
      tryCatch({
      if (file_extension %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(inFile$datapath) %>%
        mutate(
          Gross_Paid = as.numeric(Gross_Paid),
          Claim_No = as.character(Claim_No),
          Statutory_Class = as.character(Statutory_Class)
        )
      } else if (file_extension == "csv") {
        df <- read_csv(inFile$datapath, 
                       col_types = cols(
                         Claim_No = col_character(),
                         Paid_Date = col_character(),
                         Loss_Date = col_character(),
                         Gross_Paid = col_number(), 
                         Statutory_Class = col_character()))
      } else {
        stop("Unsupported file type. Please upload a CSV or Excel file.")
      }
        
        # Validate necessary columns
        requiredColumns <- c("Claim_No", "Paid_Date", "Loss_Date", "Gross_Paid", "Statutory_Class")
        if (!all(requiredColumns %in% names(df))) {
          stop("Data must contain the following columns: ", paste(requiredColumns, collapse=", "))
        }

        # Parse dates using lubridate to allow multiple formats
        # orders = c("dmy", "ymd", "mdy") means it will try day-month-year, then year-month-day, then month-day-year
        df <- df %>%
            mutate(
            Paid_Date = lubridate::parse_date_time(Paid_Date, orders = c("dmy", "ymd", "mdy")),
            Loss_Date  = lubridate::parse_date_time(Loss_Date,  orders = c("dmy", "ymd", "mdy"))
            )

        # Check if any of the date columns failed to parse
        if (any(is.na(df$Paid_Date)) || any(is.na(df$Loss_Date))) {
            warning("Some date values could not be parsed. Please ensure dates are in a recognized format.")
        }

        setProgress(1)
        return(df) 

      }, error = function(e) {
        # Handle errors in data format
        showModal(modalDialog(
          title = "Error in data format",
          paste("Please check your CSV file for the correct columns and data formats. Details: ", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
    })
  })
    

    output$data_table <- renderDT({
      req(data())
      datatable(data(),
                options = list(scrollX = TRUE, 
                              pageLength = 20,
                              autoWidth = TRUE,
                              paging = TRUE,
                              searching = FALSE,
                              info = FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "  $(this.api().table().header()).css({",
                                "    'background-color': '#FFFFFF',", 
                                "    'color': '#000000'",  
                                "  });",
                                "}"
                              )))
    })
    
  return( data = reactive({ data() }))

  })
}

