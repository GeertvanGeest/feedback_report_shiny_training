# Shinylive Feedback Form Generator

Course feedback report generator that runs entirely in your browser using WebAssembly.

## Features

- Upload Excel feedback files
- Select from multiple question metadata versions
- Generate and download HTML reports
- No server required - everything runs in your browser
- Your data never leaves your device

## Live Demo

Visit the app at: `https://YOUR-USERNAME.github.io/YOUR-REPO-NAME/`

## Local Development

```r
# Run the app locally
shiny::runApp("app.R")
```

## Deployment

This app automatically deploys to GitHub Pages when you push to the main branch.

## Project Structure

```
.
├── app/                          # Shiny web application
│   ├── app.R                    # Main Shiny app
│   ├── generate_html_report.R   # HTML report generation
│   ├── plot_functions.R         # Visualization functions
│   ├── metadata/                # Question metadata JSON files
│   └── www/                     # Static assets (logo, etc.)
│
├── R/                           # R helper functions for local analysis
│   └── process_feedback_files.R # Batch processing functions
│
├── site/                        # Shinylive deployment (auto-generated)
│
├── analysis_feedback_2025.qmd   # Example analysis notebook
│
└── .github/workflows/           # CI/CD configuration
    └── deploy.yml
```

## Local Analysis

For batch processing feedback files locally (outside the web app):

```r
# Source helper functions
source('R/process_feedback_files.R')

# Concatenate all feedback files
combined_df <- process_feedback_files(
  directory = "~/path/to/courses",
  json_path = "app/metadata/202601.json"
)

# Generate HTML reports for all feedback files
generate_feedback_reports(
  directory = "~/path/to/courses",
  json_path = "app/metadata/202601.json"
)
```

See `analysis_feedback_2025.qmd` for a complete example.

## Adding New Metadata Versions

Add new JSON files to the `app/metadata/` folder. The app will automatically detect and list them.
