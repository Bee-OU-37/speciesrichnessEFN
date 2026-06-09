# Shared initialization for orchestration scripts.
helpers_path <- if (file.exists("scripts/helpers.R")) "scripts/helpers.R" else "helpers.R"
source(helpers_path)
