# Set Project Options
options(
  mc.cores = parallel::detectCores(), # Multicore processing
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"],
  brms.backend = "cmdstanr",
  knitr.kable.NA = ''
)

# Load the helper functions on project start
.helpers <- lapply(
  list.files(
    path = "functions/", 
    pattern = ".*.R", 
    full.names = TRUE
    ), source
  )

# Base Directory for the moderate prior models
main_priors_dir <- "output/fits/main-priors/"

# Base Directory for the good alternative prior models
alt_priors_dir <- "output/fits/alt-priors/"

# Base Directory for the model diagnostics
diags_dir <- "output/diagnostics/"

# Base Directory for the model predictions
preds_dir <- "output/predictions/marginal-effects/"

# Base Directory for the model comparisons
comparisons_dir <- "output/predictions/comparisons/"
