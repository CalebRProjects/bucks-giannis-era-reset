# scripts/05_render_report.R

# Purpose:
# Render the Quarto report.

source("scripts/03_build_summary_tables.R")
source("scripts/04_create_visuals.R")

report_file <- file.path(dir_report, "bucks_giannis_reset_report.qmd")

if (!file.exists(report_file)) {
  stop("Report file does not exist: report/bucks_giannis_reset_report.qmd")
}

if (file.info(report_file)$size == 0) {
  stop("Report file exists but is empty. Add content to report/bucks_giannis_reset_report.qmd before rendering.")
}

if (!requireNamespace("quarto", quietly = TRUE)) {
  install.packages("quarto")
}

quarto::quarto_render(
  input = report_file,
  output_format = "html"
)

html_file <- file.path(dir_report, "bucks_giannis_reset_report.html")

if (file.exists(html_file)) {
  browseURL(normalizePath(html_file))
} else {
  warning("Render finished, but HTML file was not found where expected.")
}

message("Report rendered.")
