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
library(shinycssloaders)
library(bslib)
library(plotly)

options(shiny.maxRequestSize = 10000 * 1024^2)  # 10,000 MB



source("modules/dataOverviewModule.R", local = TRUE)[1]
source("modules/dataInsightsModule.R", local = TRUE)[1]
source("modules/valDataModule.R", local = TRUE)[1]
source("modules/cummulativeTriangleModule.R", local = TRUE)[1]
source("modules/incrementalTriangleModule.R", local = TRUE)[1]

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  # Darker background for the navbar for contrast
  navbar_fg = "#ffffff"  # White text color for readability
)


# UI
ui <- bs4DashPage(
  title = "Risk Adjustment Model",
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  header = bs4DashNavbar(
    status = "white",
    skin = "dark",
    sidebarIcon = NULL,
    controlbarIcon = NULL,
    tags$li(
      class = "text-center header-title-container",  # Added a new class for more specific styling
      tags$h4("Risk Adjustment Dashboard", class = "header-title")
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    tags$div(
      class = "menu-container",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      bs4SidebarMenuItem("Data Insights", tabName = "data_insights", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Valuation Data", tabName = "valuation_data", icon = icon("calculator")),
      bs4SidebarMenuItem("Incremental Triangles", tabName = "incremental_triangles", icon = icon("shapes")),
      bs4SidebarMenuItem("Cumulative Triangles", tabName = "cumulative_triangles", icon = icon("shapes")),
      bs4SidebarMenuItem("Bootstrapping Results", tabName = "bootstrapping_results", icon = icon("sync-alt")),
      bs4SidebarMenuItem("Risk Margin Download", tabName = "risk_margin_download", icon = icon("download"))
    )),
    div(class = "sidebar-logo",
        img(src = "images/kenbright.png")
    )
  ),
  body = bs4DashBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      tags$link(href = "https://fonts.googleapis.com/css?family=Mulish", rel = "stylesheet"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon")
    ),
    bs4TabItems(
      # Data Overview Tab
      bs4TabItem(
        tabName = "data_overview",
        dataOverviewUI("data_overview")
      ),
      # Data Display Tab
      bs4TabItem(
        tabName = "data_insights",
        dataInsightsUI("data_insights_id")
      ),
      
      # Valuation Data Tab
      bs4TabItem(
        tabName = "valuation_data",
        valDataUI("valuation_data_id")
      ),
            # Cumulative Triangles Tab
      bs4TabItem(
        tabName = "incremental_triangles",
        incrTriUI("incremental_triangles_id")
      ),

      # Cumulative Triangles Tab
      bs4TabItem(
        tabName = "cumulative_triangles",
        cumTriUI("cumulative_triangles_id")
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
  
   data <- dataOverviewServer("data_overview") 

   dataInsightsServer("data_insights_id", data)
  
   valDataServer("valuation_data_id", data)
   
   incrTriServer("incremental_triangles_id", data)
  
   cumTriServer("cumulative_triangles_id", data)

  
  

  
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
