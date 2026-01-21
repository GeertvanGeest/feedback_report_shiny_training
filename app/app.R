library(shiny)
library(readxl)
library(jsonlite)
library(htmltools)
library(munsell)

source("generate_html_report.R")

# UI Definition
ui <- fluidPage(
  # Custom CSS for branding
  tags$head(
    tags$link(href = "https://fonts.cdnfonts.com/css/cabinet-grotesk", rel = "stylesheet"),
    tags$link(href = "https://fonts.googleapis.com/css?family=Inter", rel = "stylesheet"),
    tags$style(HTML("
      body, .form-control, .btn, p, li, td, th, .selectize-input {
        font-family: 'Inter', sans-serif;
      }
      
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Cabinet Grotesk', sans-serif;
      }
      
      a {
        color: #009ee3;
      }
      
      a:hover {
        color: #007bb5;
      }
      
      .btn-primary {
        background-color: #ae191a;
        border-color: #ae191a;
      }
      
      .btn-primary:hover {
        background-color: #8a1416;
        border-color: #8a1416;
      }
    ")),
    tags$script(HTML("
      // JavaScript function to download HTML content as file
      function downloadHTMLFile(content, filename) {
        const blob = new Blob([content], { type: 'text/html' });
        const url = window.URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        window.URL.revokeObjectURL(url);
        document.body.removeChild(a);
      }
      
      // Shiny custom message handler
      Shiny.addCustomMessageHandler('downloadHTML', function(message) {
        downloadHTMLFile(message.content, message.filename);
      });
    "))
  ),
  
  titlePanel(
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      "Course Feedback Report Generator",
      tags$a(
        href = "https://github.com/GeertvanGeest/feedback_report_shiny_training",
        target = "_blank",
        icon("github", lib = "font-awesome"),
        "GitHub",
        style = "font-size: 16px; text-decoration: none;"
      )
    ),
    windowTitle = "Feedback report generator"
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$img(src = "logo.svg", style = "width: 100%; max-width: 200px; margin-bottom: 20px;"),
      h4("Configuration"),
      
      # Dropdown for question metadata versions
      selectInput(
        "metadata_version",
        "Question Metadata Version:",
        choices = NULL  # Will be populated dynamically
      ),
      
      # File upload for Excel feedback file
      fileInput(
        "feedback_file",
        "Upload Feedback Excel File:",
        accept = c(".xlsx", ".xls")
      ),
      
      hr(),
      
      # Generate report button
      actionButton(
        "generate",
        "Generate Report",
        class = "btn-primary",
        width = "100%"
      ),
      
      br(), br(),
      
      # Download button (initially hidden)
      uiOutput("download_ui"),
      
      br(),
      
      # Status messages
      uiOutput("status_message")
    ),
    
    mainPanel(
      h4("Instructions"),
      tags$ol(
        tags$li("Select the appropriate question metadata version from the dropdown"),
        tags$li("Upload your feedback Excel file"),
        tags$li("Click 'Generate Report' to create the HTML report"),
        tags$li("Download the generated report when ready")
      ),
      
      hr(),
      
      h4("Available Metadata Versions"),
      tableOutput("metadata_info")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive values to store state
  rv <- reactiveValues(
    html_content = NULL,
    report_filename = NULL,
    report_ready = FALSE
  )
  
  # Scan for available metadata files on startup
  observe({
    metadata_files <- list.files("metadata", pattern = "\\.json$", full.names = TRUE)
    
    if (length(metadata_files) == 0) {
      showNotification("No metadata files found in metadata/ directory", type = "error")
      return()
    }
    
    # Create friendly names (remove path and extension)
    names(metadata_files) <- tools::file_path_sans_ext(basename(metadata_files))
    
    updateSelectInput(session, "metadata_version", choices = metadata_files)
  })
  
  # Display metadata information
  output$metadata_info <- renderTable({
    metadata_files <- list.files("metadata", pattern = "\\.json$", full.names = TRUE)
    
    if (length(metadata_files) == 0) return(NULL)
    
    # Read descriptions from JSON files
    metadata_info <- lapply(metadata_files, function(file) {
      json_content <- fromJSON(file)
      data.frame(
        Name = tools::file_path_sans_ext(basename(file)),
        Description = if (!is.null(json_content$description)) json_content$description else "No description",
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, metadata_info)
  })
  
  # Generate report when button is clicked
  observeEvent(input$generate, {
    # Validate inputs
    req(input$feedback_file)
    req(input$metadata_version)
    
    # Show progress
    withProgress(message = 'Generating report...', value = 0, {
      
      tryCatch({
        # Get uploaded file path
        feedback_path <- input$feedback_file$datapath
        original_filename <- input$feedback_file$name
        
        incProgress(0.2, detail = "Loading data")
        
        # Create output filename based on original filename
        output_basename <- paste0(tools::file_path_sans_ext(original_filename), ".html")
        output_path <- file.path(tempdir(), output_basename)
        
        incProgress(0.4, detail = "Generating visualizations")
        
        # Generate HTML report programmatically
        html_doc <- generate_html_report(
          feedback_file = feedback_path,
          metadata_file = input$metadata_version,
          original_filename = original_filename
        )
        
        incProgress(0.8, detail = "Saving report")
        
        # Convert HTML to string with full document structure (WASM-compatible)
        temp_file <- tempfile(fileext = ".html")
        save_html(html_doc, file = temp_file)
        rv$html_content <- paste(readLines(temp_file, warn = FALSE), collapse = "\n")
        unlink(temp_file)
        
        rv$report_filename <- output_basename
        rv$report_ready <- TRUE
        
        incProgress(1.0, detail = "Complete!")
        
        showNotification(
          "Report generated successfully!",
          type = "message", duration = 5
        )
        
      }, error = function(e) {
        showNotification(paste("Error generating report:", e$message), type = "error", duration = 10)
        rv$report_ready <- FALSE
      })
    })
  })
  
  # Show download button only when report is ready
  output$download_ui <- renderUI({
    if (rv$report_ready) {
      actionButton("download_report", "Download Report", 
                   class = "btn-success", width = "100%",
                   icon = icon("download"))
    }
  })
  
  # Status message
  output$status_message <- renderUI({
    if (rv$report_ready) {
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        " Report is ready for download!"
      )
    }
  })
  
  # Download handler using JavaScript (WASM-compatible for Chrome)
  observeEvent(input$download_report, {
    if (!is.null(rv$html_content) && !is.null(rv$report_filename)) {
      session$sendCustomMessage(
        type = "downloadHTML",
        message = list(
          content = rv$html_content,
          filename = rv$report_filename
        )
      )
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
