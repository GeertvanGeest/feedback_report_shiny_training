#!/usr/bin/env Rscript

# Render feedback report with custom input files
# Usage: Rscript render_feedback_report.R <feedback_file> <metadata_file>
# Or from R: source("render_feedback_report.R"); render_report("path/to/feedback.xlsx", "data/question_metadata.json")

render_report <- function(feedback_file, metadata_file = "data/question_metadata.json") {
  library(quarto)
  
  # Check if files exist
  if (!file.exists(feedback_file)) {
    stop("Feedback file not found: ", feedback_file)
  }
  if (!file.exists(metadata_file)) {
    stop("Metadata file not found: ", metadata_file)
  }
  
  # Create output filename (same as feedback file but .html extension)
  output_file <- paste0(tools::file_path_sans_ext(feedback_file), ".html")
  output_basename <- basename(output_file)
  
  # Render the document with parameters (output_file must be basename only)
  quarto_render(
    input = "summary_feedback_form.qmd",
    execute_params = list(
      feedback_file = feedback_file,
      metadata_file = metadata_file,
      original_filename = basename(feedback_file)
    ),
    output_file = output_basename
  )
  
  # Move the rendered file to the desired location if needed
  if (output_file != output_basename) {
    file.rename(output_basename, output_file)
  }
  
  cat("Report generated:", output_file, "\n")
  return(invisible(output_file))
}

# If run from command line
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    cat("Usage: Rscript render_feedback_report.R <feedback_file> [<metadata_file>]\n")
    cat("Example: Rscript render_feedback_report.R 'data/My Course.xlsx' 'data/question_metadata.json'\n")
    quit(status = 1)
  }
  
  feedback_file <- args[1]
  metadata_file <- if (length(args) >= 2) args[2] else "data/question_metadata.json"
  
  render_report(feedback_file, metadata_file)
}
