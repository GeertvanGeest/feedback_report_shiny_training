#' Helper function to find best matching question from JSON
#' @param header Character. Column header from Excel file
#' @param questions Character vector. Expected questions from JSON
#' @param threshold Numeric. Similarity threshold (0-1)
#' @return Character. Best matching question or original header if no good match
match_header <- function(header, questions, threshold) {
  if (header %in% questions) {
    return(header)  # Exact match
  }
  
  # Calculate Jaro-Winkler similarity (good for text comparison)
  similarities <- stringdist::stringsim(header, questions, method = "jw")
  best_match_idx <- which.max(similarities)
  best_similarity <- similarities[best_match_idx]
  
  if (best_similarity >= threshold) {
    message("  Mapping '", substr(header, 1, 50), 
            if (nchar(header) > 50) "..." else "",
            "' -> '", substr(questions[best_match_idx], 1, 50),
            if (nchar(questions[best_match_idx]) > 50) "..." else "",
            "' (similarity: ", round(best_similarity, 2), ")")
    return(questions[best_match_idx])
  } else {
    message("  Keeping original header: '", substr(header, 1, 60),
            if (nchar(header) > 60) "..." else "", "'")
    return(header)
  }
}

#' Standardize Excel column names based on JSON metadata
#'
#' @param df Data frame. Excel data with original column names
#' @param expected_questions Character vector. Expected question texts from JSON
#' @param similarity_threshold Numeric. Threshold for fuzzy matching (0-1)
#'
#' @return Data frame with standardized column names
#' @keywords internal
standardize_excel_columns <- function(df, expected_questions, similarity_threshold = 0.8) {
  
  # Load required package
  require(stringdist)
  require(dplyr)
  
  # Get column names from the Excel file
  excel_headers <- names(df)
  
  # Map headers to standardized names using fuzzy matching
  message("  Standardizing column names...")
  new_headers <- sapply(excel_headers, function(h) {
    match_header(h, expected_questions, similarity_threshold)
  }, USE.NAMES = FALSE)
  
  # Rename columns
  names(df) <- new_headers
  
  # Check for duplicate column names after mapping
  if (any(duplicated(names(df)))) {
    dup_cols <- names(df)[duplicated(names(df))]
    message("  Warning: Duplicate columns detected after mapping: ", 
            paste(unique(dup_cols), collapse = ", "))
    message("  Merging duplicate columns by keeping first non-NA value...")
    
    # For each duplicate, merge the columns
    for (col in unique(dup_cols)) {
      dup_indices <- which(names(df) == col)
      # Combine values: take first non-NA value across duplicates
      merged_col <- df[[dup_indices[1]]]
      for (i in 2:length(dup_indices)) {
        merged_col <- ifelse(is.na(merged_col), df[[dup_indices[i]]], merged_col)
      }
      df[[dup_indices[1]]] <- merged_col
      # Remove other duplicates
      df <- df[-dup_indices[-1]]
    }
  }
  
  return(df)
}

#' Process Feedback Excel Files
#'
#' This function searches for Excel files starting with 'Feedback -' in subdirectories,
#' standardizes column names based on JSON metadata using fuzzy matching, and concatenates them.
#'
#' @param directory Character. Path to the directory containing subdirectories with Excel files
#' @param json_path Character. Path to the JSON metadata file containing question definitions
#' @param similarity_threshold Numeric. Threshold for fuzzy matching (0-1). Default 0.8.
#'
#' @return A data frame containing all concatenated feedback data with added columns:
#'         - file_name: Name of the source Excel file (without extension)
#'         - directory_name: Name of the subdirectory where the file was found
#'
#' @export
process_feedback_files <- function(directory, json_path, similarity_threshold = 0.8) {
  
  # Load required packages
  require(readxl)
  require(jsonlite)
  require(dplyr)
  require(stringdist)
  
  # Read JSON metadata
  metadata <- jsonlite::fromJSON(json_path)
  
  # Extract all expected question texts from the JSON
  # The sections contain questions as nested data frames
  expected_questions <- do.call(rbind, lapply(metadata$sections$questions, function(q) {
    data.frame(question_text = q$question_text, stringsAsFactors = FALSE)
  })) %>%
    pull(question_text)
  
  # Get all subdirectories (non-recursive)
  subdirs <- list.dirs(directory, recursive = FALSE, full.names = TRUE)
  
  if (length(subdirs) == 0) {
    warning("No subdirectories found in ", directory)
    return(data.frame())
  }
  
  # Initialize list to store data frames
  all_data <- list()
  
  # Process each subdirectory
  for (subdir in subdirs) {
    
    # Get directory name
    dir_name <- basename(subdir)
    
    # Find Excel files starting with 'Feedback' (case insensitive, with various formats)
    # Matches: "Feedback -", "Feedback-", "Feedback ", "Feedback2", etc.
    excel_files <- list.files(
      path = subdir,
      pattern = "^[Ff]eedback.*\\.(xlsx|xls)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(excel_files) == 0) {
      message("No feedback files found in ", dir_name)
      next
    }
    
    # Check for multiple feedback files in same directory
    if (length(excel_files) > 1) {
      stop("Multiple feedback files found in ", dir_name, ":\n  ",
           paste(basename(excel_files), collapse = "\n  "),
           "\n\nPlease ensure only one feedback file exists per course directory.")
    }
    
    # Process each Excel file
    for (excel_file in excel_files) {
      
      file_name_no_ext <- tools::file_path_sans_ext(basename(excel_file))
      
      message("Processing: ", excel_file)
      
      tryCatch({
        # Read Excel file
        df <- readxl::read_excel(excel_file)
        
        # Standardize column names using helper function
        df <- standardize_excel_columns(df, expected_questions, similarity_threshold)
        
        # Add metadata columns
        df$file_name <- file_name_no_ext
        df$directory_name <- dir_name
        
        # Add to list
        all_data[[length(all_data) + 1]] <- df
        
      }, error = function(e) {
        warning("Error processing file ", excel_file, ": ", e$message)
      })
    }
  }
  
  # Check if any data was collected
  if (length(all_data) == 0) {
    warning("No data was successfully processed")
    return(data.frame())
  }
  
  # Concatenate all data frames
  # Using bind_rows to handle different column sets gracefully
  combined_data <- bind_rows(all_data)
  
  message("\nSuccessfully processed ", length(all_data), " file(s)")
  message("Total rows: ", nrow(combined_data))
  
  return(combined_data)
}


#' Generate HTML Reports for All Feedback Files
#'
#' This function recursively searches for Excel files starting with 'Feedback -',
#' standardizes column names based on JSON metadata, and generates an HTML report
#' for each file using the report generation functions.
#'
#' @param directory Character. Path to the root directory to search (recursive)
#' @param json_path Character. Path to the JSON metadata file containing question definitions
#' @param similarity_threshold Numeric. Threshold for fuzzy matching (0-1). Default 0.8.
#' @param viz_preferences List. Visualization preferences for the reports. Default NULL uses defaults.
#' @param output_dir Character. Optional. Directory to save HTML reports. If NULL, saves alongside Excel files.
#'
#' @return Invisibly returns a vector of generated HTML file paths
#' 
#' @note This function must be run from the project root directory as it sources
#'       files from the app/ directory.
#'
#' @export
generate_feedback_reports <- function(directory, json_path, similarity_threshold = 0.8, 
                                      viz_preferences = NULL, output_dir = NULL) {
  
  # Load required packages
  require(readxl)
  require(jsonlite)
  require(dplyr)
  require(stringdist)
  require(here)
  
  # Source the report generation functions using here package
  source(here::here("app", "generate_html_report.R"))
  
  # Read JSON metadata
  metadata <- jsonlite::fromJSON(json_path)
  
  # Extract all expected question texts from the JSON
  expected_questions <- do.call(rbind, lapply(metadata$sections$questions, function(q) {
    data.frame(question_text = q$question_text, stringsAsFactors = FALSE)
  })) %>%
    pull(question_text)
  
  # Find all Excel files recursively starting with 'Feedback' (case insensitive)
  # Matches: "Feedback -", "Feedback-", "Feedback ", "Feedback2", etc.
  excel_files <- list.files(
    path = directory,
    pattern = "^[Ff]eedback.*\\.(xlsx|xls)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(excel_files) == 0) {
    warning("No feedback files found in ", directory)
    return(invisible(character(0)))
  }
  
  message("Found ", length(excel_files), " feedback file(s)")
  
  # Initialize vector to store output paths
  generated_reports <- character(length(excel_files))
  
  # Process each Excel file
  for (i in seq_along(excel_files)) {
    excel_file <- excel_files[i]
    file_name_no_ext <- tools::file_path_sans_ext(basename(excel_file))
    
    message("\n[", i, "/", length(excel_files), "] Processing: ", excel_file)
    
    tryCatch({
      # Read Excel file
      df <- readxl::read_excel(excel_file)
      
      # Standardize column names using helper function
      df <- standardize_excel_columns(df, expected_questions, similarity_threshold)
      
      # Save to temporary file with standardized columns
      temp_excel <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(df, temp_excel)
      
      # Determine output path
      if (!is.null(output_dir)) {
        # Save to specified output directory
        dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
        output_file <- file.path(output_dir, paste0(file_name_no_ext, ".html"))
      } else {
        # Save alongside original Excel file
        output_file <- file.path(dirname(excel_file), paste0(file_name_no_ext, ".html"))
      }
      
      # Generate HTML report
      message("  Generating HTML report...")
      html_doc <- generate_html_report(
        feedback_file = temp_excel,
        metadata_file = json_path,
        original_filename = basename(excel_file),
        viz_preferences = viz_preferences
      )
      
      # Save HTML to file
      htmltools::save_html(html_doc, file = output_file)
      
      # Clean up temp file
      unlink(temp_excel)
      
      generated_reports[i] <- output_file
      message("  Report saved: ", output_file)
      
    }, error = function(e) {
      warning("Error processing file ", excel_file, ": ", e$message)
      generated_reports[i] <- NA_character_
    })
  }
  
  # Summary
  successful <- sum(!is.na(generated_reports))
  message("\n========================================")
  message("Report generation complete!")
  message("Successfully generated ", successful, " out of ", length(excel_files), " report(s)")
  message("========================================")
  
  return(invisible(generated_reports[!is.na(generated_reports)]))
}


#' Example usage:
#' 
#' # Source the functions (from project root)
#' source('R/process_feedback_files.R')
#' 
#' # === Process and concatenate feedback files ===
#' # Process all feedback files with default similarity threshold (0.8)
#' result <- process_feedback_files(
#'   directory = "~/path/to/courses",
#'   json_path = "app/metadata/202601.json"
#' )
#' 
#' # Use stricter matching (only very similar headers will be merged)
#' result <- process_feedback_files(
#'   directory = "~/path/to/courses",
#'   json_path = "app/metadata/202601.json",
#'   similarity_threshold = 0.9
#' )
#' 
#' # View the combined data
#' View(result)
#' 
#' # Save to Excel if needed
#' writexl::write_xlsx(result, "combined_feedback.xlsx")
#' 
#' 
#' # === Generate HTML reports for all feedback files ===
#' # Generate reports for all feedback files (saved alongside Excel files)
#' generate_feedback_reports(
#'   directory = "~/path/to/courses",
#'   json_path = "app/metadata/202601.json"
#' )
#' 
#' # Generate reports with custom visualization preferences
#' viz_prefs <- list(
#'   mc_ordinal = "bar",
#'   mc_not_ordinal = "pie",
#'   multiple_select = "bar",
#'   open = "wordcloud"
#' )
#' generate_feedback_reports(
#'   directory = "~/path/to/courses",
#'   json_path = "app/metadata/202601.json",
#'   viz_preferences = viz_prefs
#' )
#' 
#' # Generate reports and save to specific output directory
#' generate_feedback_reports(
#'   directory = "~/path/to/courses",
#'   json_path = "app/metadata/202601.json",
#'   output_dir = "reports_output"
#' )
