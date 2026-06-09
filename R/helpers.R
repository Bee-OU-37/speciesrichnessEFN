# Helper functions for the reproducible species richness workflow.
# These functions standardize path handling, logging, and script execution.

# Locate the project root by searching for the .Rproj file.
find_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    rproj_files <- list.files(current, pattern = "\\.Rproj$", full.names = TRUE)
    if (length(rproj_files) > 0) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find project root containing an .Rproj file.")
    }
    current <- parent
  }
}

# Print a timestamped workflow message.
log_step <- function(message_text) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s\n", timestamp, message_text))
}

# Create a directory tree when it does not exist.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# Load and return centralized project configuration.
load_project_config <- function(project_root = find_project_root()) {
  source(file.path(project_root, "config", "project_config.R"), local = TRUE)
  get_project_config(project_root)
}

# Source a script from its own working directory for compatibility with legacy paths.
source_with_working_dir <- function(script_path, project_root = find_project_root()) {
  absolute_script <- file.path(project_root, script_path)

  if (!file.exists(absolute_script)) {
    stop(sprintf("Script not found: %s", absolute_script))
  }

  log_step(sprintf("Running %s", script_path))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(dirname(absolute_script))
  source(absolute_script, local = new.env(parent = .GlobalEnv))
  invisible(TRUE)
}

# Execute a vector of scripts in sequence.
run_script_sequence <- function(script_paths, project_root = find_project_root()) {
  for (script_path in script_paths) {
    source_with_working_dir(script_path, project_root = project_root)
  }
  invisible(TRUE)
}
