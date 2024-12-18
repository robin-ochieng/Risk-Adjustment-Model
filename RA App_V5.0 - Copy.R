# Load necessary libraries
library(shiny)
library(bs4Dash)
library(tidyverse)
library(bslib)
library(DT)
library(scales)
library(lubridate)
library(zoo)
library(ChainLadder)

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
      bs4SidebarMenuItem("Bootstrapping Results", tabName = "bootstrapping_results", icon = icon("sync-alt")),
      bs4SidebarMenuItem("Risk Margin Download", tabName = "risk_margin_download", icon = icon("download"))
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
            width = 12,
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
            status = "white",
            solidHeader = TRUE,
            plotOutput("claim_count_plot", height = "400px")
          ),
          bs4Card(
            title = "Sum of Gross Paid by Statutory Class (KES)",
            status = "white",
            solidHeader = TRUE,
            plotOutput("gross_paid_plot", height = "400px")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Statistical Summary of Gross Paid (KES 'Million)",
            status = "white",
            solidHeader = TRUE,
            width = 12,
            DTOutput("stat_summary_table")
          )
        )
      ),
      
      # Valuation Data Tab
      bs4TabItem(
        tabName = "valuation_data",
        fluidRow(
          bs4Card(
            title = "Valuation Data Analysis (Quarterly)",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DTOutput("val_data_table")
          )
        )
      ),
      
      # Cumulative Triangles Tab
      bs4TabItem(
        tabName = "cumulative_triangles",
        fluidRow(
          bs4Card(
            title = "Cumulative Triangle Analysis (Yearly)",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            selectInput("statutory_class", "Select Statutory Class", choices = NULL),
            uiOutput("loss_year_range_ui"),
            actionButton("generate_triangle", "Generate Triangle"),
            tags$div(
              style = "font-size: 10px; max-width: 100%; overflow-x: auto; white-space: pre-wrap;",
              verbatimTextOutput("cumulative_triangle_output")
            )
          )
        )
      ),
      
      # Bootstrapping Results Tab
      bs4TabItem(
        tabName = "bootstrapping_results",
        fluidRow(
          bs4Card(
            title = "Bootstrapping Analysis",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            selectInput("boot_statutory_class", "Select Statutory Class", choices = NULL),
            selectInput("loss_year_boot", "Select Loss Year", choices = 2000:2030, selected = 2017),
            selectInput("outlier_option", "Outlier Handling", 
                        choices = c("Remove Outliers" = "remove", "Use Whole Data" = "whole")),
            actionButton("run_bootstrap", "Run Bootstrapping"),
            br(), br(),
            h4("Bootstrap Summary"),
            DTOutput("bootstrap_summary"),
            br(),
            h4("Confidence Levels"),
            DTOutput("bootstrap_confidence"),
            br(),
            h4("Risk Margin Calculations"),
            DTOutput("risk_margin_table"),
            br(),
            plotOutput("bootstrap_plot")
          )
        )
      ),
      
      # Risk Margin Download Tab
      bs4TabItem(
        tabName = "risk_margin_download",
        fluidRow(
          bs4Card(
            title = "Risk Margin Results",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DTOutput("risk_margin_download_table"),
            downloadButton("download_risk_margin", "Download Risk Margin Results")
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
               Claim_No = col_character(),
               Paid_Date = col_date(format = "%d/%m/%Y"),
               Loss_Date = col_date(format = "%d/%m/%Y"),
               Gross_Paid = col_number(),
               Statutory_Class = col_character()
             ))
  })
  
  # Variable Definitions
  output$variable_definitions <- renderUI({
    HTML(
      "<h4>Dataset Variables</h4>
      <ul>
        <li><b>Claim_No:</b> Unique identifier for each claim.</li>
        <li><b>Loss_Date:</b> Date when the loss occurred.</li>
        <li><b>Paid_Date:</b> Date when the claim payment was made.</li>
        <li><b>Gross_Paid:</b> Total amount paid for the claim (KES).</li>
        <li><b>Statutory_Class:</b> Classification of the claim based on regulatory requirements.</li>
      </ul>"
    )
  })
  
  # Display Data Table
  output$data_table <- renderDT({
    req(data())
    datatable(head(data(), 15), options = list(pageLength = 15))
  })
  
  # Claim Count Plot
  output$claim_count_plot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = Statutory_Class, fill = Statutory_Class)) +
      geom_bar(show.legend = FALSE) +
      labs(title = "Claim Count by Statutory Class", x = "Statutory Class", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Sum of Gross Paid Plot
  output$gross_paid_plot <- renderPlot({
    req(data())
    plot_data <- data() %>%
      filter(!is.na(Gross_Paid)) %>%
      group_by(Statutory_Class) %>%
      summarise(Total_Gross_Paid = sum(Gross_Paid, na.rm = TRUE))
    ggplot(plot_data, aes(x = Statutory_Class, y = Total_Gross_Paid, fill = Statutory_Class)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Sum of Gross Paid by Statutory Class", x = "Statutory Class", y = "Gross Paid (KES)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Statistical Summary Table
  output$stat_summary_table <- renderDT({
    req(data())
    summary_data <- data() %>%
      filter(!is.na(Gross_Paid)) %>%
      group_by(Statutory_Class) %>%
      summarise(
        Mean = mean(Gross_Paid, na.rm = TRUE),
        Median = median(Gross_Paid, na.rm = TRUE),
        Min = min(Gross_Paid, na.rm = TRUE),
        Max = max(Gross_Paid, na.rm = TRUE),
        "75th Percentile" = quantile(Gross_Paid, 0.75, na.rm = TRUE)
      ) %>%
      mutate(across(where(is.numeric), ~ round(. / 1e6, 2))) # Convert to millions
    datatable(summary_data, options = list(pageLength = 10))
  })
  
  # Valuation Data Table
  output$val_data_table <- renderDT({
    req(data())
    val_data <- data() %>%
      mutate(
        Acc_Quarters = paste0("Q", quarter(Loss_Date), "-", year(Loss_Date)),
        Dev_period = floor(((year(Paid_Date) * 12 + month(Paid_Date)) - 
                              (year(Loss_Date) * 12 + month(Loss_Date))) / 3)
      ) %>%
      select(Acc_Quarters, Gross_Paid, Dev_period)
    datatable(val_data, options = list(pageLength = 15))
  })
  
  # Populate Statutory Class Choices
  observeEvent(data(), {
    req(data())
    updateSelectInput(session, "statutory_class", choices = unique(data()$Statutory_Class))
    updateSelectInput(session, "boot_statutory_class", choices = unique(data()$Statutory_Class))
  })
  
  # Loss Year Range UI for Cumulative Triangles
  output$loss_year_range_ui <- renderUI({
    req(input$statutory_class)
    available_years <- data() %>%
      filter(Statutory_Class == input$statutory_class) %>%
      pull(Loss_Date) %>%
      year() %>%
      unique() %>%
      sort()
    
    sliderInput("loss_year_range", "Select Loss Year Range",
                min = min(available_years),
                max = max(available_years),
                value = c(min(available_years), max(available_years)),
                step = 1)
  })
  
  # Display Data Table (repeated)
  output$data_table <- renderDT({
    req(data())
    datatable(head(data(), 15), options = list(pageLength = 15))
  })
  
  # Populate Statutory Class Choices (repeated)
  observeEvent(data(), {
    req(data())
    updateSelectInput(session, "boot_statutory_class", choices = unique(data()$Statutory_Class))
  })
  
  # Bootstrapping Results
  bootstrap_results <- eventReactive(input$run_bootstrap, {
    req(data())
    req(input$boot_statutory_class)
    
    # Filter data by selected class and chosen loss year
    filtered_data <- data() %>%
      filter(Statutory_Class == input$boot_statutory_class,
             year(Loss_Date) >= input$loss_year_boot)
    
    # Handle outliers based on user selection
    if (input$outlier_option == "remove") {
      Q1 <- quantile(filtered_data$Gross_Paid, 0.15, na.rm = TRUE)
      Q3 <- quantile(filtered_data$Gross_Paid, 0.85, na.rm = TRUE)
      IQR <- Q3 - Q1
      filtered_data <- filtered_data %>%
        filter(Gross_Paid >= (Q1 - 1.5 * IQR) & Gross_Paid <= (Q3 + 1.5 * IQR))
    }
    
    # Create cumulative triangle
    filtered_data <- filtered_data %>%
      mutate(
        Loss_Year = year(Loss_Date),
        Dev_period = year(Paid_Date) - year(Loss_Date)
      ) %>%
      group_by(Loss_Year, Dev_period) %>%
      summarise(Gross_Amount = sum(Gross_Paid, na.rm = TRUE), .groups = "drop")
    
    inc_tri <- as.triangle(filtered_data, origin = "Loss_Year", dev = "Dev_period", value = "Gross_Amount")
    cum_tri <- incr2cum(inc_tri, na.rm = TRUE)
    
    Boot_Method <- BootChainLadder(cum_tri, R = 999, process.distr = "od.pois")
    list(Boot_Method = Boot_Method, Cum_Tri = cum_tri)
  })
  
  # Reactive expression for risk margin data
  risk_margin_data <- reactive({
    req(bootstrap_results())
    
    # Extract Bootstrap Summary and Confidence Levels
    boot_summary <- summary(bootstrap_results()$Boot_Method)$Totals
    confidence_level <- quantile(bootstrap_results()$Boot_Method, c(0.75))
    confidence_df <- as.data.frame(confidence_level$ByOrigin)
    
    # Mean IBNR from Bootstrap Summary
    mean_ibnr <- boot_summary["Mean IBNR", "Totals"]
    
    # Total IBNR at 75% CI from Bootstrap Summary
    total_ibnr_75 <- boot_summary["Total IBNR 75%", "Totals"]
    
    # Sum of IBNR at 75% CI by Origin from Confidence Levels
    sum_ibnr_75 <- sum(confidence_df$`IBNR 75%`, na.rm = TRUE)
    
    # Calculate Risk Margins
    risk_margin_75_ci <- total_ibnr_75 - mean_ibnr
    risk_margin_sum <- sum_ibnr_75 - mean_ibnr
    
    # Create Data Frame for Display
    risk_margin_df <- data.frame(
      Description = c("Risk Margin @ 75% CI", "Risk Margin from Summing"),
      Value = c(risk_margin_75_ci, risk_margin_sum)
    )
    
    risk_margin_df
  })
  
  # Display Bootstrap Summary
  output$bootstrap_summary <- renderDT({
    req(bootstrap_results())
    boot_summary <- summary(bootstrap_results()$Boot_Method)$Totals
    datatable(
      boot_summary %>% mutate_all(~formatC(.x, format = "f", big.mark = ",", digits = 2)),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Display Confidence Levels
  output$bootstrap_confidence <- renderDT({
    req(bootstrap_results())
    confidence_level <- quantile(bootstrap_results()$Boot_Method, c(0.75))
    confidence_df <- as.data.frame(confidence_level$ByOrigin)
    datatable(
      confidence_df %>% mutate_all(~formatC(.x, format = "f", big.mark = ",", digits = 2)),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Display Risk Margin Table in Bootstrapping Results Tab
  output$risk_margin_table <- renderDT({
    req(risk_margin_data())
    datatable(
      risk_margin_data() %>% mutate(Value = formatC(Value, format = "f", big.mark = ",", digits = 2)),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  # Plot Bootstrap Results
  output$bootstrap_plot <- renderPlot({
    req(bootstrap_results())
    plot(bootstrap_results()$Boot_Method)
  })
  
  # Risk Margin Download Table
  output$risk_margin_download_table <- renderDT({
    req(risk_margin_data())
    datatable(
      risk_margin_data() %>% mutate(Value = formatC(Value, format = "f", big.mark = ",", digits = 2)),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  # Download Handler for Risk Margin Results
  output$download_risk_margin <- downloadHandler(
    filename = function() {
      paste("risk_margin_results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(risk_margin_data())
      write_csv(risk_margin_data(), file)
    }
  )
}

# Run the application
shinyApp(ui, server)
