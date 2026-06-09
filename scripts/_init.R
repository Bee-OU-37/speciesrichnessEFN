# Shared initialization for orchestration scripts.
# This resolver supports execution from repository root or scripts/.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)
