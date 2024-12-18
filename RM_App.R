library(shiny)
library(bs4Dash)
library(tidyverse)
library(bslib)
library(DT)
library(scales)
library(lubridate)
library(zoo)
library(ChainLadder)

# Increase max file size to 100 MB
options(shiny.maxRequestSize = 2000 * 1024^2)

# UI
ui <- bs4DashPage(
  dark = FALSE,
  title = "Claims Data Dashboard",
  header = bs4DashNavbar(title = dashboardBrand("Claims Dashboard", color = "primary")),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      bs4SidebarMenuItem("Data Display", tabName = "data_display", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Valuation Data", tabName = "valuation_data", icon = icon("calculator")),
      bs4SidebarMenuItem("Cumulative Triangles", tabName = "cumulative_triangles", icon = icon("shapes")),
      bs4SidebarMenuItem("RM Bootstrapping Results", tabName = "rm_bootstrap", icon = icon("cogs"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      # Data Overview Tab
      bs4TabItem(
        tabName = "data_overview",
        fluidRow(
          bs4Card(
            title = "Variable Definitions",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            htmlOutput("variable_definitions")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Upload and Preview Data",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            fileInput("file", "Upload CSV File", accept = ".csv"),
            DTOutput("data_table")
          )
        )
      ),
      
      # Data Display Tab
      bs4TabItem(
        tabName = "data_display",
        fluidRow(
          bs4Card(
            title = "Claim Count by Statutory Class",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("claim_count_plot", height = "400px")
          ),
          bs4Card(
            title = "Sum of Gross Paid by Statutory Class (KES)",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("gross_paid_plot", height = "400px")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Statistical Summary of Gross Paid (KES 'Million)",
            status = "info",
            solidHeader = TRUE,
            DTOutput("stat_summary_table")
          )
        )
      ),
      
      # Valuation Data Tab
      bs4TabItem(
        tabName = "valuation_data",
        fluidRow(
          bs4Card(
            title = "Valuation Data Analysis",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("val_data_table")
          )
        )
      ),
      
      # Cumulative Triangles Tab
      bs4TabItem(
        tabName = "cumulative_triangles",
        fluidRow(
          bs4Card(
            title = "Cumulative Triangle Analysis",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("statutory_class", "Select Statutory Class", choices = NULL),
            sliderInput("loss_year_range", "Select Loss Year Range", min = 2000, max = 2024, value = c(2017, 2024)),
            plotOutput("triangle_plot", height = "600px")
          )
        )
      ),
      
      # RM Bootstrapping Results Tab
      bs4TabItem(
        tabName = "rm_bootstrap",
        fluidRow(
          bs4Card(
            title = "Bootstrapping Results",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("bootstrap_class", "Select Statutory Class", choices = NULL),
            sliderInput("bootstrap_loss_year", "Select Loss Year Range", min = 2000, max = 2024, value = c(2017, 2024)),
            actionButton("run_bootstrap", "Run Bootstrapping"),
            DTOutput("bootstrap_results_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive data loading
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath, 
             col_types = cols(
               Paid_Date = col_date(format = "%d/%m/%Y"),
               Loss_Date = col_date(format = "%d/%m/%Y"),
               Gross_Paid = col_number()
             ))
  })
  
  # Populate statutory class choices dynamically
  observeEvent(data(), {
    req(data())
    classes <- unique(data()$Statutory_Class)
    updateSelectInput(session, "statutory_class", choices = classes)
    updateSelectInput(session, "bootstrap_class", choices = classes)
  })
  
  # Cumulative Triangle Plot
  output$triangle_plot <- renderPlot({
    req(data(), input$statutory_class, input$loss_year_range)
    
    filtered_data <- data() %>%
      filter(
        Statutory_Class == input$statutory_class,
        year(Loss_Date) >= input$loss_year_range[1],
        year(Loss_Date) <= input$loss_year_range[2]
      ) %>%
      mutate(
        Loss_Year = year(Loss_Date),
        Dev_Period = year(Paid_Date) - year(Loss_Date)
      ) %>%
      group_by(Loss_Year, Dev_Period) %>%
      summarise(Gross_Amount = sum(Gross_Paid, na.rm = TRUE), .groups = "drop")
    
    inc_tri <- as.triangle(filtered_data, origin = "Loss_Year", dev = "Dev_Period", value = "Gross_Amount")
    cum_tri <- incr2cum(inc_tri, na.rm = TRUE)
    
    plot(cum_tri, main = paste("Cumulative Triangle for", input$statutory_class), cex = 0.8)
  })
  
  # Run Bootstrapping and Display Results
  observeEvent(input$run_bootstrap, {
    req(data(), input$bootstrap_class, input$bootstrap_loss_year)
    
    filtered_data <- data() %>%
      filter(
        Statutory_Class == input$bootstrap_class,
        year(Loss_Date) >= input$bootstrap_loss_year[1],
        year(Loss_Date) <= input$bootstrap_loss_year[2]
      ) %>%
      mutate(
        Loss_Year = year(Loss_Date),
        Dev_Period = year(Paid_Date) - year(Loss_Date)
      ) %>%
      group_by(Loss_Year, Dev_Period) %>%
      summarise(Gross_Amount = sum(Gross_Paid, na.rm = TRUE), .groups = "drop")
    
    inc_tri <- as.triangle(filtered_data, origin = "Loss_Year", dev = "Dev_Period", value = "Gross_Amount")
    cum_tri <- incr2cum(inc_tri, na.rm = TRUE)
    
    Boot_Method <- BootChainLadder(cum_tri, R = 999, process.distr = "od.pois")
    summary_Boot_Method <- summary(Boot_Method)$Totals
    
    final_results <- data.frame(
      Metric = c("Mean IBNR", "Total IBNR (75%)", "Risk Margin"),
      Value = c(
        round(summary_Boot_Method["IBNR"], 2),
        round(summary_Boot_Method["IBNR"] * 1.25, 2),
        round(summary_Boot_Method["IBNR"] * 0.25, 2)
      )
    )
    
    output$bootstrap_results_table <- renderDT({
      datatable(final_results, options = list(pageLength = 5, dom = 't'))
    })
  })
}

# Run the app
shinyApp(ui, server)
