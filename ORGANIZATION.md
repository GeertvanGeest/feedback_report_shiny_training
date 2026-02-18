# Project Organization Summary

The project has been reorganized for better maintainability and clarity.

## Directory Structure

### `app/` - Web Application
Contains all Shiny app components that run in the browser via shinylive:
- `app.R` - Main Shiny application interface
- `generate_html_report.R` - Report generation engine
- `plot_functions.R` - Visualization functions (charts, wordclouds, tables)
- `metadata/` - Question metadata JSON files
- `www/` - Static assets (logo, images)

### `R/` - Helper Functions
Contains R functions for local batch processing (outside the web app):
- `process_feedback_files.R` - Main helper functions:
  - `process_feedback_files()` - Concatenate multiple feedback files
  - `generate_feedback_reports()` - Generate HTML reports for all files
  - `standardize_excel_columns()` - Standardize column names
  - `match_header()` - Fuzzy matching helper

### Root Directory
- `analysis_feedback_2025.qmd` - Example analysis notebook showing how to use the helper functions
- `README.md` - Project documentation
- `renv.lock` - R package dependencies

## Path References

All paths are relative to the **project root**:
- QMD files source: `source('R/process_feedback_files.R')`
- Helper functions reference app: `source_path <- file.path("app", "generate_html_report.R")`
- Metadata files: `json_path = "app/metadata/202601.json"`

## Usage Pattern

**For web-based single-file processing:**
Use the Shiny app at https://YOUR-SITE/

**For local batch processing:**
```r
# From project root
source('R/process_feedback_files.R')

# Concatenate files
df <- process_feedback_files(
  directory = "~/courses/2025",
  json_path = "app/metadata/202601.json"
)

# Generate reports
generate_feedback_reports(
  directory = "~/courses/2025",
  json_path = "app/metadata/202601.json"
)
```

## Key Benefits

✅ Clear separation between web app and local analysis tools
✅ All paths work from project root
✅ Helper functions can be reused across different analyses
✅ App remains self-contained for shinylive deployment
✅ Easy to add new analysis notebooks
