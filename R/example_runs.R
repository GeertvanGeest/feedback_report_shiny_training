source("app/generate_html_report.R")

html <- generate_html_report(feedback_file = "Feedback - Missing Data and Imputation Methods - June 2026(1-12).xlsx",
metadata_file = "app/metadata/202601.json")

htmltools::save_html(html, file = "output.html")

html <- generate_html_report(feedback_file = "Feedback - Intro to Spatial Transcriptomics Data Analysis - June 2026(1-20).xlsx",
metadata_file = "app/metadata/202601.json")

htmltools::save_html(html, file = "output1.html")

html <- generate_html_report(feedback_file = "Feedback - Introduction to Spatial Transcriptomics - December 2025(1-16).xlsx",
metadata_file = "app/metadata/202601.json")

htmltools::save_html(html, file = "output2.html")