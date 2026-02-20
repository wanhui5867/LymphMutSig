# =============================================================================
# Step 1: Create renv.lock for Docker build (SciLifeLab Serve)
# Run from LymphMutSig project root:  Rscript init_renv.R
# =============================================================================

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}
library(renv)

if (!dir.exists("app") || !file.exists("app/app.R")) {
  stop("Run this script from the LymphMutSig project root (folder containing app/ and app/app.R).")
}

# Create renv.lock inside app/ so it is shipped with the app
dir_orig <- setwd("app")
on.exit(setwd(dir_orig), add = TRUE)
renv::init(bare = TRUE)
renv::snapshot(type = "implicit", prompt = FALSE)
setwd(dir_orig)

message("renv.lock created in: ", normalizePath("app/renv.lock", mustWork = TRUE))
message("Next: docker build (see DOCKER_DEPLOY.md)")
