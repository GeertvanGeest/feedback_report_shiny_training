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
    
    # Find Excel files starting with 'Feedback -'
    excel_files <- list.files(
      path = subdir,
      pattern = "^Feedback -.*\\.(xlsx|xls)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(excel_files) == 0) {
      message("No feedback files found in ", dir_name)
      next
    }
    
    # Process each Excel file
    for (excel_file in excel_files) {
      
      file_name_no_ext <- tools::file_path_sans_ext(basename(excel_file))
      
      message("Processing: ", excel_file)
      
      tryCatch({
        # Read Excel file
        df <- readxl::read_excel(excel_file)
        
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


#' Example usage:
#' 
#' # Process all feedback files with default similarity threshold (0.8)
#' result <- process_feedback_files(
#'   directory = "app/metadata",
#'   json_path = "app/metadata/202601.json"
#' )
#' 
#' # Use stricter matching (only very similar headers will be merged)
#' result <- process_feedback_files(
#'   directory = "app/metadata",
#'   json_path = "app/metadata/202601.json",
#'   similarity_threshold = 0.9
#' )
#' 
#' # View the combined data
#' View(result)
#' 
#' # Save to CSV if needed
#' write.csv(result, "combined_feedback.csv", row.names = FALSE)
