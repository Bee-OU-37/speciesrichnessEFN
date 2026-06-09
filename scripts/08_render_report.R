# Render the final project report from R Markdown.
# The report consolidates workflow outputs into a reproducible document.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()
config <- load_project_config(project_root)

rmarkdown::render(
  input = config$reporting$report_template,
  output_format = config$reporting$report_output_format,
  output_dir = config$paths$reports,
  clean = TRUE,
  envir = new.env(parent = globalenv())
)

log_step("Report rendering complete.")
