# DataInsightsUI.R

dataInsightsUI <- function(id) {
  ns <- NS(id)
    tagList(
        fluidRow(
            valueBoxOutput(ns("total_gross_paid_box")),
            valueBoxOutput(ns("avg_days_to_pay_box")),
            valueBoxOutput(ns("total_claims_box"))
        ),
        fluidRow(
            bs4Card(
                title = "Statistical Summary of Gross Paid (KES 'Million)",
                status = "white",
                solidHeader = TRUE,
                width = 12,
                DTOutput(ns("stat_summary_table"))
            )
        ),
        fluidRow(
            tabBox( 
                solidHeader = TRUE,
                selected = "Sum of Gross Paid by Statutory Class",
                status = "white",
                type = "tabs",
                height = "500px",
                width = 12,
                id = ns("grosspaid_tabs"),
                tabPanel("Sum of Gross Paid by Statutory Class", plotlyOutput(ns("gross_paid_plot")) %>% withSpinner(type = 6)),
                tabPanel("Count of Gross Paid by Statutory Class", plotOutput(ns("claim_count_plot")) %>% withSpinner(type = 6))
            )
        )
    )
}


# DataInsightsServer.R

dataInsightsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate Total Gross Paid
    output$total_gross_paid_box <- renderValueBox({
      total_gross_paid <- sum(data()$Gross_Paid, na.rm = TRUE)
      valueBox(
        formatC(total_gross_paid, format = "f", big.mark = ",", digits = 0),
        "Total Gross Paid (KES)",
        icon = icon("money-bill-alt"),
        color = "white"
      )
    })

    # Calculate Average Days to Pay with formatting (although unusual for days)
    output$avg_days_to_pay_box <- renderValueBox({
        req(data())
        avg_days <- mean(data()$Paid_Date - data()$Loss_Date, na.rm = TRUE)
        formatted_avg_days <- formatC(avg_days, format = "f", big.mark = ",", digits = 1)
        valueBox(
            formatted_avg_days,
            "Average Days to Pay",
            icon = icon("calendar-alt"),
            color = "white"
        )
    })


   
    # Calculate Total Number of Claims
    output$total_claims_box <- renderValueBox({
        req(data())
        total_claims <- nrow(data())
        formatted_claims <- formatC(total_claims, format = "f", big.mark = ",", digits = 0)  # No decimal points
        valueBox(
            formatted_claims,
            "Total Claims",
            icon = icon("clipboard-list"),
            color = "white"
        )
    })

    
    # Render the claim count plot
    output$claim_count_plot <- renderPlot({
        req(data())
        data <- data() %>%
        group_by(Statutory_Class) %>%
        dplyr::summarize(statutoryClassCount = n()) %>% 
        arrange(desc(statutoryClassCount))
        statutoryClass_ordered <- factor(data$Statutory_Class, levels = rev(data$Statutory_Class))
        # Creating the lollipop plot
        ggplot(data, aes(x = statutoryClass_ordered, y = statutoryClassCount)) +
        geom_segment(aes(xend = statutoryClass_ordered, yend = 0), color = "#0d6efd", size = 0.7) +  # Lines for lollipop sticks
        geom_point(color = "#00acc1", size = 2.5) +  # Points as lollipop heads
        geom_text(aes(label = scales::comma(statutoryClassCount),
                        y = statutoryClassCount + 0.02 * max(statutoryClassCount)), 
                    hjust = -0.05, vjust = 0.5, color = "black", size = 3.0) +
        coord_flip() +
        theme_minimal() +
        theme(
            text = element_text(family = "Mulish", face = "plain"),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "lightgrey", size = 0.5), # Add vertical major grid lines
            panel.grid.minor.x = element_line(color = "lightgrey", size = 0.25),
            axis.title.x = element_text(angle = 0, hjust = 0.5, size = 12),
            axis.title.y = element_text(angle = 90, vjust = 0.5, size = 12),
            axis.text.x = element_text(color = "black", size = 10),  # Increased font size for X-axis text
            axis.text.y = element_text(color = "black", size = 10),
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
        ) +
        labs(
            x = "Statutory Class",
            y = "Count",
            title = "Claim Count by Statutory Class"
        )
    })
    
    # Render the sum of gross paid plot
    output$gross_paid_plot <- renderPlotly({
        req(data())
        data <- data() %>%
        filter(!is.na(Gross_Paid)) %>%
        group_by(Statutory_Class) %>%
        summarize(Total_Gross_Paid = sum(Gross_Paid, na.rm = TRUE)) %>%
        mutate(TotalGrossPaidMillions = Total_Gross_Paid / 1e6) %>%
        mutate(Statutory_Class = fct_reorder(Statutory_Class, TotalGrossPaidMillions)) %>%  # Reorder factor levels
        arrange((TotalGrossPaidMillions))
        
        plot_ly(data, x = ~TotalGrossPaidMillions, y = ~Statutory_Class, type = 'bar', orientation = 'h',
                marker = list(color = '#0d6efd')) %>%
        layout(
            title = "Sum of Gross Paid by Statutory Class",
            yaxis = list(title = "Statutory Class", tickangle = 0, size = 6, tickfont = list(size = 8, color = "#333333")),
            xaxis = list(title = "Total Gross Paid (Million KES)", size = 6, tickfont = list(size = 8, color = "#333333")),
            font = list(family = "Mulish", color = "#333333"),
            margin = list(b = 100), # To accommodate rotated x-axis labels
            plot_bgcolor = "white",
            paper_bgcolor = "white"
        ) %>%
        add_annotations(
            x = ~TotalGrossPaidMillions,
            y = ~Statutory_Class,
            text = ~formatC(TotalGrossPaidMillions, format = "f", big.mark = ",", digits = 1, decimal.mark = ".") %>% paste0(" M"),
            showarrow = FALSE,
            xshift = 25,
            font = list(size = 9, color = "#333333")
        )
    })
    
    # Render the statistical summary table
    output$stat_summary_table <- renderDT({
      req(data())
      summary_data <- data() %>%
        filter(!is.na(Gross_Paid)) %>%
        group_by(Statutory_Class) %>%
        summarise(
          Mean = mean(Gross_Paid, na.rm = TRUE),
          Median = median(Gross_Paid, na.rm = TRUE),
          Minimum = min(Gross_Paid, na.rm = TRUE),
          Maximum = max(Gross_Paid, na.rm = TRUE),
          "75th Percentile" = quantile(Gross_Paid, 0.75, na.rm = TRUE)
        ) %>%
        mutate(across(where(is.numeric), ~ round(. / 1e6, 2))) # Convert to millions
      datatable(summary_data,
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
  })
}


