# Generate HTML report programmatically (WASM-compatible)
library(htmltools)
library(ggplot2)
library(dplyr)
library(readxl)
library(jsonlite)
library(base64enc)
library(lubridate)
library(stringr)
library(here)

# Source plot functions using here package for robust path resolution
source(here::here("app", "plot_functions.R"))

# Function to convert ggplot to base64 PNG
ggplot_to_base64 <- function(plot, width = 6, height = 4.5, dpi = 150) {
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, plot, width = width, height = height, dpi = dpi)
  img_data <- base64enc::base64encode(tmp)
  unlink(tmp)
  paste0("data:image/png;base64,", img_data)
}

# Function to convert base graphics plot (like wordcloud) to base64 PNG
base_plot_to_base64 <- function(plot_function, width = 8, height = 6, dpi = 150) {
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = width * dpi, height = height * dpi, res = dpi)
  plot_function()
  dev.off()
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
generate_html_report <- function(feedback_file, metadata_file, original_filename = NULL, viz_preferences = NULL) {
  
  # Default visualization preferences if not provided
  if (is.null(viz_preferences)) {
    viz_preferences <- list(
      mc_ordinal = "bar",
      mc_not_ordinal = "pie",
      multiple_select = "bar",
      open = "table",
      mc_number_display = "numbers"
    )
  }
  
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
  
  # Add Survey Overview section header
   content_sections[[1]] <- h2("Survey Overview")

  # Add course title
   content_sections[[length(content_sections) + 1]] <- p(tags$strong("Feedback of:"), " ", file_basename)
  
  # Calculate response statistics
  n_responses <- nrow(feedback_results)
  content_sections[[length(content_sections) + 1]] <- p(
    tags$strong("Number of responses:"), " ", n_responses
  )
  
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
      time_spread_text <- paste0(
        format(min_time, "%d.%m.%Y %H:%M"),
        " to ",
        format(max_time, "%d.%m.%Y %H:%M")
      )
      content_sections[[length(content_sections) + 1]] <- p(
        tags$strong("Survey period:"), " ", time_spread_text
      )
      
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
  
  # Helper function to normalize text for matching (handle different dash/ampersand encodings)
  normalize <- function(x) {
    x %>%
      str_replace_all("[\u2013\u2014\u2212\\-]", "-") %>%  # normalize dashes
      str_replace_all("\u0026|&", "&") %>%  # normalize ampersands
      str_squish()  # normalize whitespace
  }
  
  # Helper to add content sections
  add_content <- function(element) {
    content_sections[[length(content_sections) + 1]] <<- element
  }
  
  # Process each section
  for (section_idx in seq_along(question_metadata$sections$title)) {
    section_title <- question_metadata$sections$title[section_idx]
    section_description <- question_metadata$sections$description[section_idx]
    section_questions <- question_metadata$sections$questions[[section_idx]]
    
    # Add section header
    add_content(h2(section_title))
    
    # Add section description if present and not empty
    if (!is.null(section_description) && section_description != "") {
      add_content(p(tags$em(section_description)))
    }
    
    # Process each question in the section
    for (q_idx in seq_len(nrow(section_questions))) {
      question_text <- section_questions$question_text[q_idx]
      question_type <- section_questions$question_type[q_idx]
      is_ordinal <- section_questions$is_ordinal[q_idx]
      
      # Find matching column
      norm_question <- normalize(question_text)
      norm_columns <- normalize(colnames(feedback_results))
      matched_idx <- which(norm_columns == norm_question)
      
      # If no exact match, try prefix match for truncated columns (min 30 chars)
      if (length(matched_idx) == 0 && nchar(norm_question) > 30) {
        matched_idx <- which(str_starts(norm_columns, str_sub(norm_question, 1, 30)))
      }
      
      # Check if we found a matching column
      if (length(matched_idx) > 0) {
        matched_column <- colnames(feedback_results)[matched_idx[1]]
        
        # Add question header
        add_content(h3(question_text))
        
        # Generate appropriate visualization based on question type and user preferences
        if (question_type == "open") {
          # Open question: table or word cloud
          if (viz_preferences$open == "wordcloud") {
            plot_func <- word_cloud(feedback_results, matched_column)
            add_content(tags$img(
              src = base_plot_to_base64(plot_func, width = 8, height = 6),
              style = "max-width: 800px; width: 100%; height: auto;"
            ))
          } else {
            # Table (default)
            table_data <- feedback_results %>%
              select(all_of(matched_column)) %>%
              filter(!is.na(.data[[matched_column]]), .data[[matched_column]] != "") %>%
              mutate(Response_ID = row_number()) %>%
              select(Response_ID, Answer = all_of(matched_column))
            
            add_content(df_to_html_table(table_data, caption = question_text))
          }
          
        } else if (question_type == "multiple_select") {
          # Multiple select: always bar chart
          plot <- bar_chart(feedback_results, matched_column, question_metadata, question_text)
          add_content(tags$img(
            src = ggplot_to_base64(plot, width = 6, height = 4.5),
            style = "max-width: 600px; width: 100%; height: auto;"
          ))
          
        } else if (question_type == "multiple_choice" && is_ordinal) {
          # Multiple choice ordinal: bar or pie based on preference
          if (viz_preferences$mc_ordinal == "pie") {
            plot <- pie_chart(feedback_results, matched_column, question_metadata, question_text)
          } else {
            # Pass number_display preference only for bar charts
            number_display <- if (!is.null(viz_preferences$mc_number_display)) viz_preferences$mc_number_display else "numbers"
            plot <- bar_chart(feedback_results, matched_column, question_metadata, question_text, number_display)
          }
          add_content(tags$img(
            src = ggplot_to_base64(plot, width = 6, height = 4.5),
            style = "max-width: 600px; width: 100%; height: auto;"
          ))
          
        } else if (question_type == "multiple_choice" && !is_ordinal) {
          # Multiple choice not ordinal: bar or pie based on preference
          if (viz_preferences$mc_not_ordinal == "pie") {
            plot <- pie_chart(feedback_results, matched_column, question_metadata, question_text)
          } else {
            # Pass number_display preference only for bar charts
            number_display <- if (!is.null(viz_preferences$mc_number_display)) viz_preferences$mc_number_display else "numbers"
            plot <- bar_chart(feedback_results, matched_column, question_metadata, question_text, number_display)
          }
          add_content(tags$img(
            src = ggplot_to_base64(plot, width = 6, height = 4.5),
            style = "max-width: 600px; width: 100%; height: auto;"
          ))
        }
        
        # Add spacing
        add_content(br())
      } else {
        # Warn about missing column
        warning(sprintf("Column not found: '%s'", question_text))
      }
    }
    
    # Add separator between sections (except after the last section)
    if (section_idx < length(question_metadata$sections$title)) {
      add_content(hr())
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
save_html_report <- function(feedback_file, metadata_file, output_file = NULL, original_filename = NULL, viz_preferences = NULL) {
  
  # Generate output filename if not provided
  if (is.null(output_file)) {
    if (!is.null(original_filename)) {
      output_file <- paste0(tools::file_path_sans_ext(original_filename), ".html")
    } else {
      output_file <- paste0(tools::file_path_sans_ext(feedback_file), ".html")
    }
  }
  
  # Generate HTML
  html_doc <- generate_html_report(feedback_file, metadata_file, original_filename, viz_preferences)
  
  # Save to file
  save_html(html_doc, file = output_file)
  
  message("Report generated: ", output_file)
  return(invisible(output_file))
}
