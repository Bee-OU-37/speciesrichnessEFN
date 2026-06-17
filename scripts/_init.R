# Shared initialization for orchestration scripts.
# This resolver supports execution from repository root or scripts/.

helpers_path <- if (file.exists("scripts/helpers.R")) "scripts/helpers.R" else "../scripts/helpers.R"
source(helpers_path)
