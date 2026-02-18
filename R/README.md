# R Helper Functions

This directory contains R functions for processing feedback data outside of the Shiny app.

## Files

- **process_feedback_files.R**: Main helper functions for processing feedback Excel files
  - `process_feedback_files()`: Concatenates multiple feedback Excel files into a single data frame
  - `generate_feedback_reports()`: Recursively generates HTML reports for all feedback files
  - `standardize_excel_columns()`: Standardizes column names using fuzzy matching
  - `match_header()`: Fuzzy matching helper for column names

## Usage

Source these functions from the project root:

```r
source('R/process_feedback_files.R')
```

See the documentation in the file for detailed usage examples.
