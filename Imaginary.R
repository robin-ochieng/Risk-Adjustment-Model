    div(
        class = "upload-container",
        tags$p(class = "instruction-header", "How to Prepare Data before Upload:"),
        tags$ul(
            class = "list-item",
        tags$li(
            class = "custom-list-item", "Ensure the data format is CSV or Excel file."),
        tags$li(
            class = "custom-list-item", "Dataset Variable Definition:-"),
        tags$ul(
            class = "sub-list-item" ,
            tags$li(class = "custom-list-item", icon("calendar"), tags$b("Claim_No: - "), " Unique identifier for each claim."),
            tags$li(class = "custom-list-item", icon("calendar-alt"), tags$b("Loss_Date: - "), " Date when the loss occurred."),
            tags$li(class = "custom-list-item", icon("clipboard-check"), tags$b("Paid_Date: - "), " Date when the claim payment was made."),
            tags$li(class = "custom-list-item", icon("briefcase"), tags$b("Gross_Paid: - "), " Total amount paid for the claim (KES)."),
            tags$li(class = "custom-list-item", icon("dollar-sign"), tags$b("Statutory_Class: - "), " The Class of Business.")
                )
            )
        ),