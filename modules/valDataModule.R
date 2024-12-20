# UI for Valuation Data Module
valDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        bs4Card(
        title = "Valuation Data Analysis",
        status = "white",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        fluidRow(
            hr(),
            hr(),
            width = 4,
            selectInput(
              ns("val_type"),
              "Valuation Type",
              choices = c("Quarterly", "Monthly"),
              selected = "Quarterly"
          ),
            hr(), hr(),
          ),
          fluidRow(
            width = 4,
            # Add a download button
            downloadButton(ns("download_val_data"), "Download Data", class = "btn btn-primary btn-primary-custom")
          ),
          br(),
        DTOutput(ns("val_data_table"))
        )
    )
  )
}


# Server logic for Valuation Data Module
valDataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    val_data_reactive <- reactive({
      req(data())
      df <- data()
      
      if (input$val_type == "Quarterly") {
        # Quarterly logic
        val_data <- df %>%
          mutate(
            Acc_Quarters = paste0("Q", quarter(Loss_Date), "-", year(Loss_Date)),
            Dev_period = floor(
              ((year(Paid_Date) * 12 + month(Paid_Date)) -
               (year(Loss_Date) * 12 + month(Loss_Date))) / 3
            )
          ) %>%
          select(Acc_Quarters, Gross_Paid, Dev_period)
      } else {
        # Monthly logic
        val_data <- df %>%
          mutate(
            Acc_Months_ = as.yearmon(Loss_Date),
            Dev_period = floor(difftime(Paid_Date, Loss_Date, units = "days") / (365.25/12)) + 1,
            Acc_Year = as.integer(format(Acc_Months_, "%Y")),
            Acc_Month = as.integer(format(Acc_Months_, "%m")),
            Acc_Months = (Acc_Year - min(Acc_Year, na.rm = TRUE)) * 12 +
                         Acc_Month - min(Acc_Month, na.rm = TRUE) + 1
          ) %>%
          select(Acc_Months, Gross_Paid, Dev_period)
      }

      val_data
    })

        # Render the datatable
    output$val_data_table <- renderDT({
      datatable(
        val_data_reactive(),
        options = list(pageLength = 15)
      )
    })

    # Download handler for the val_data
    output$download_val_data <- downloadHandler(
      filename = function() {
        if (input$val_type == "Quarterly") {
          "valuation_data_quarterly.csv"
        } else {
          "valuation_data_monthly.csv"
        }
      },
      content = function(file) {
        write.csv(val_data_reactive(), file, row.names = FALSE)
      }
    )


  })
}
