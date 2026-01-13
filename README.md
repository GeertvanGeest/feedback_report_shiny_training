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

- `app.R` - Main Shiny application
- `generate_html_report.R` - HTML report generation functions
- `plot_functions.R` - Visualization functions (pie charts, bar charts, tables)
- `data/question_metadata.json` - Question configuration
- `.github/workflows/deploy.yml` - Automated deployment workflow

## Adding New Metadata Versions

Add new JSON files to the `data/` folder with the pattern `question_metadata*.json`.
The app will automatically detect and list them.
