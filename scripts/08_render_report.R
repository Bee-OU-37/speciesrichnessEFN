# Render the final project report from R Markdown.
# The report consolidates workflow outputs into a reproducible document.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()
config <- load_project_config(project_root)

rmarkdown::render(
  input = file.path(config$paths$reports, "speciesrichness_workflow_report.Rmd"),
  output_format = "html_document",
  output_dir = config$paths$reports,
  clean = TRUE,
  envir = new.env(parent = globalenv())
)

log_step("Report rendering complete.")
