library(shiny)
library(bs4Dash)
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(zoo)
library(ggplot2)
library(scales)
library(bslib)
library(DT)

source("modules/dataOverviewModule.R", local = TRUE)[1]
source("modules/uprSummaries.R", local = TRUE)[1]
source("modules/gepResultsModule.R", local = TRUE)[1]
source("modules/helperFunctions.R")


options(shiny.maxRequestSize = 1000 * 1024^2)  # 100 MB

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

# Define the User Interface for the Application
ui <- bs4DashPage(
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
      tags$h4("UPR and GEP Model", class = "header-title")
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    tags$div(
      class = "menu-container",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data Overview", tabName = "dataOverview", icon = icon("table")),
      bs4SidebarMenuItem("UPR Summaries", tabName = "uprSummaries", icon = icon("chart-bar")),
      bs4SidebarMenuItem("GEP Results", tabName = "gepResults", icon = icon("chart-line"))
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
      bs4TabItem(tabName = "dataOverview",
        dataOverviewUI("data_overview")
      ),
      bs4TabItem(tabName = "uprSummaries",
        uprSummariesUI("upr_summaries")                 
      ),
      bs4TabItem(tabName = "gepResults",
        gepResultsUI("gep_results")
      )
    )
  )
)



# Define the server logic required to read the input and calculate outputs
server <- function(input, output, session) {
  
  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("controlbar")
  })

  overviewResults  <- dataOverviewServer("data_overview")
  
  processedData <- overviewResults$data
  
  cutoffYear <- overviewResults$cutoffYear

  uprSummariesServer("upr_summaries", processedData)

  gepResultsServer("gep_results", processedData, cutoffYear)

}

# Run the application
shinyApp(ui = ui, server = server)