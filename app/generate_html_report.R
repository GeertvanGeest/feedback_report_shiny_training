# Generate HTML report programmatically (WASM-compatible)
library(htmltools)
library(ggplot2)
library(dplyr)
library(readxl)
library(jsonlite)
library(base64enc)
library(lubridate)

source("plot_functions.R")

# Function to convert ggplot to base64 PNG
ggplot_to_base64 <- function(plot, width = 6, height = 4.5, dpi = 150) {
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, plot, width = width, height = height, dpi = dpi)
  img_data <- base64enc::base64encode(tmp)
  unlink(tmp)
  paste0("data:image/png;base64,", img_data)
}

# Function to generate HTML table from data frame
df_to_html_table <- function(df, caption = NULL) {
  table_html <- tags$table(
    class = "table table-striped table-hover table-condensed",
    style = "max-width: 800px;",
    if (!is.null(caption)) tags$caption(caption),
    tags$thead(
      tags$tr(
        lapply(colnames(df), function(col) tags$th(col))
      )
    ),
    tags$tbody(
      lapply(1:nrow(df), function(i) {
        tags$tr(
          lapply(df[i, ], function(cell) tags$td(as.character(cell)))
        )
      })
    )
  )
  table_html
}

# Main function to generate HTML report
generate_html_report <- function(feedback_file, metadata_file, original_filename = NULL) {
  
  # Load data
  feedback_results <- read_xlsx(feedback_file)
  question_metadata <- fromJSON(metadata_file)
  
  # Get filename for title
  if (is.null(original_filename)) {
    file_basename <- basename(feedback_file) |> tools::file_path_sans_ext()
  } else {
    file_basename <- tools::file_path_sans_ext(original_filename)
  }
  
  # Build HTML content
  content_sections <- list()
  
  # Add course title
  content_sections[[1]] <- h2(file_basename)
  
  # Calculate response statistics
  n_responses <- nrow(feedback_results)
  content_sections[[length(content_sections) + 1]] <- p(strong(paste0("Number of responses: ", n_responses)))
  
  # Process Start time if available
  if ("Start time" %in% colnames(feedback_results)) {
    start_times <- feedback_results %>%
      filter(!is.na(`Start time`)) %>%
      mutate(
        datetime = as.POSIXct(`Start time`)
      ) %>%
      filter(!is.na(datetime))
    
    if (nrow(start_times) > 0) {
      # Calculate time spread
      min_time <- min(start_times$datetime)
      max_time <- max(start_times$datetime)
      time_spread <- paste0(
        "Survey period: ",
        format(min_time, "%d.%m.%Y %H:%M"),
        " to ",
        format(max_time, "%d.%m.%Y %H:%M")
      )
      content_sections[[length(content_sections) + 1]] <- p(time_spread)
      
      # Create histogram showing distribution of start times
      if (nrow(start_times) > 1) {
        # Add date for grouping
        start_times <- start_times %>%
          mutate(date = as.Date(datetime))
        
        # Create histogram binned by day
        time_plot <- ggplot(start_times, aes(x = datetime)) +
          geom_histogram(fill = "#4E79A7", alpha = 1, color = "white", binwidth = 86400) +
          labs(x = "Date", y = "Count") +
          scale_x_datetime(date_breaks = "1 day", date_labels = "%d.%m.%Y") +
          theme_minimal() +
          theme(
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
        
        img_base64 <- ggplot_to_base64(time_plot, width = 8, height = 3)
        content_sections[[length(content_sections) + 1]] <- tags$img(
          src = img_base64,
          style = "max-width: 800px; width: 100%; height: auto;"
        )
        content_sections[[length(content_sections) + 1]] <- br()
      }
    }
  }
  
  content_sections[[length(content_sections) + 1]] <- hr()
  
  # Process each question
  for (i in 1:nrow(question_metadata$questions)) {
    question <- question_metadata$questions[i, ]
    variable <- question$question_text
    
    # Check if column exists
    if (variable %in% colnames(feedback_results)) {
      
      # Add question header
      content_sections[[length(content_sections) + 1]] <- h3(question$question_text)
      
      # Generate appropriate visualization
      if (question$question_type == "open") {
        # Create table for open questions
        table_data <- feedback_results %>%
          select(all_of(variable)) %>%
          filter(!is.na(.data[[variable]]), .data[[variable]] != "") %>%
          mutate(Response_ID = row_number()) %>%
          select(Response_ID, Answer = all_of(variable))
        
        content_sections[[length(content_sections) + 1]] <- df_to_html_table(
          table_data, 
          caption = question$question_text
        )
        
      } else if (question$question_type == "multiple_select" || 
                 (question$question_type == "multiple_choice" && question$is_ordinal)) {
        # Bar chart
        plot <- bar_chart(feedback_results, variable, question_metadata)
        img_base64 <- ggplot_to_base64(plot, width = 6, height = 4.5)
        content_sections[[length(content_sections) + 1]] <- tags$img(
          src = img_base64,
          style = "max-width: 600px; width: 100%; height: auto;"
        )
        
      } else if (question$question_type == "multiple_choice" && !question$is_ordinal) {
        # Pie chart
        plot <- pie_chart(feedback_results, variable, question_metadata)
        img_base64 <- ggplot_to_base64(plot, width = 6, height = 4.5)
        content_sections[[length(content_sections) + 1]] <- tags$img(
          src = img_base64,
          style = "max-width: 600px; width: 100%; height: auto;"
        )
      }
      
      # Add spacing
      content_sections[[length(content_sections) + 1]] <- br()
    }
  }
  
  # Build complete HTML document
  html_doc <- tagList(
    tags$html(
      tags$head(
        tags$meta(charset = "UTF-8"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
        tags$title("Course Feedback Summary"),
        tags$style(HTML("
          body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            max-width: 1200px;
            margin: 40px auto;
            padding: 20px;
            line-height: 1.6;
            color: #000;
          }
          h1 { color: #000; margin-bottom: 20px; }
          h2 { color: #000; margin-top: 30px; margin-bottom: 15px; }
          h3 { color: #000; margin-top: 25px; margin-bottom: 10px; }
          .table {
            width: 100%;
            margin-bottom: 20px;
            border-collapse: collapse;
          }
          .table th, .table td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #ddd;
          }
          .table-striped tbody tr:nth-of-type(odd) {
            background-color: #f9f9f9;
          }
          .table-hover tbody tr:hover {
            background-color: #f5f5f5;
          }
          .table th {
            background-color: #e0e0e0;
            color: #000;
            font-weight: bold;
          }
          img {
            display: block;
            margin: 20px auto;
          }
        "))
      ),
      tags$body(
        h1("Course Feedback Summary"),
        content_sections
      )
    )
  )
  
  return(html_doc)
}

# Wrapper function to save HTML report
save_html_report <- function(feedback_file, metadata_file, output_file = NULL, original_filename = NULL) {
  
  # Generate output filename if not provided
  if (is.null(output_file)) {
    if (!is.null(original_filename)) {
      output_file <- paste0(tools::file_path_sans_ext(original_filename), ".html")
    } else {
      output_file <- paste0(tools::file_path_sans_ext(feedback_file), ".html")
    }
  }
  
  # Generate HTML
  html_doc <- generate_html_report(feedback_file, metadata_file, original_filename)
  
  # Save to file
  save_html(html_doc, file = output_file)
  
  message("Report generated: ", output_file)
  return(invisible(output_file))
}
