#' Fitting Models in Parallel with brms via cmdstanr
#'
#' @param formula A `brmsformula` object for the model.
#' 
#' @param treat A string argument containing the name of the treatment
#' or a regular expression to match against the columns in `data`.
#' 
#' @param prior A `brmsprior` object containing the priors for the model.
#' 
#' @param data A data frame containing the response and inputs named in
#' `formula`.
#' 
#' @param cores The maximum number of MCMC chains to run in parallel.
#' 
#' @param chains The number of Markov chains to run.
#' 
#' @param warmup The number of warmup iterations to run per chain.
#' 
#' @param sampling The number of post-warmup iterations to run per chain.
#' 
#' @param stan_models The compiled Stan program(s) to be passed to 
#' `cmdstanr::sample`.
#' 
#' @param save_file The file path where the brmsfit object should be saved.
#' 
#' @param ... Additional arguments passed to `brms::brm`
#'
#' @return An object of class `brmsfit`
#' 
#' @export cmdstan_logit
#'
cmdstan_logit <- function(formula,
                          treat,
                          prior,
                          data,
                          cores,
                          chains,
                          warmup,
                          sampling,
                          stan_models,
                          save_file,
                          ...) {
  # Check if the file already exists
  file_check <- file.exists(save_file)
  
  if (isTRUE(file_check)) {
    # Read in the file if it already exists
    out <- readr::read_rds(file = save_file)
  } else {
    
    # Total iterations for brms
    niter <- sampling + warmup
    
    # Initialize an empty brmsfit object
    brms_obj <- brm(
      formula = formula,
      family = brmsfamily("bernoulli", link = "logit"),
      prior = prior,
      data = data,
      cores = cores,
      chains = chains,
      iter = niter,
      warmup = warmup,
      empty = TRUE,
      seed = 123456,
      sample_prior = "yes",
      ...
    )
    
    # Get the data for the stan model
    stan_data <- standata(brms_obj)
    
    # Check whether the model contains the treatment
    stan_data$treat_pos <- .match_treat_args(stan_data, treat)
    
    # Select the Stan model to use based on the data
    if (!is.null(stan_data$treat_pos)) {
      cmdstan_model <- stan_models[["gformula"]]
    } else {
      cmdstan_model <- stan_models[["non-gformula"]]
    }
    
    # Fit the brms model
    out <- .fit_cmdstan_to_brms(cmdstan_model, brms_obj, stan_data, 
                                cores, chains, warmup, sampling)
    
    # Write the brmsfit object to an rds file
    readr::write_rds(out, file = save_file, "xz", compression = 9L)
    
  }
  
  # Return the model object
  return(out)
}

# Function for detecting the treatment vector
.match_treat_args <- function(stan_data, treat) {
  
  # Check if any of the columns in X match the treatment
  check_treat <- grepl(treat, colnames(stan_data$X))
  
  # Get the position of the treatment if present
  if (any(check_treat)) {
    treat_pos <- which(check_treat)
  } else {
    treat_pos <- NULL
  }
  
  return(treat_pos)
}

# Function to fit the models with cmdstanr and store them in the empty brmsfit
.fit_cmdstan_to_brms <- function(cmdstan_model,
                                 brms_obj,
                                 stan_data,
                                 cores,
                                 chains,
                                 warmup,
                                 sampling) {
  
  # Set the initial number of draws to 0
  min_draws <- 0
  
  # Repeat the run if any of the chains stop unexpectedly
  while (min_draws < (sampling * chains)) {
    
    # Fit the Stan Model
    fit <- cmdstan_model$sample(
      data = stan_data,
      seed = 123456,
      refresh = 1000,
      sig_figs = 5,
      parallel_chains = cores,
      chains = chains,
      iter_warmup = warmup,
      iter_sampling = sampling,
      max_treedepth = 13
    )
    
    # Update the check
    min_draws <- posterior::ndraws(fit$draws())
  }
  
  # Convert the environment to a stanfit object and add it to the brmsfit
  brms_obj$fit <- rstan::read_stan_csv(fit$output_files())
  
  # Rename the parameters
  brms_obj <- brms::rename_pars(brms_obj)
  
  # Store the predictions inside the brmsfit object
  if (!is.null(stan_data$treat_pos)) {
    
    # Extract the predictions and contrasts
    brms_obj$predictions <- fit$draws(
      variables = c("EYX0", "EYX1", "AME"), 
      format = "draws_df"
    )
  }
  
  # Store run times
  brms_obj$times <- fit$time()
  
  # Return the brmsfit object
  return(brms_obj)
}

# A Function  for creating a field containing the meta data from a brms object----
stan_metadata <- function(x, ...) {
  # Construct a field with relevant metadata
  meta_data <- purrr::map_dfr(
    .x = x$fit@stan_args,
    .f = ~ tibble::tibble(
      warmup = str_remove_all(.x$time_info[1], "[^[:digit:]|\\.]"),
      sampling = str_remove_all(.x$time_info[2], "[^[:digit:]|\\.]"),
      total = str_remove_all(.x$time_info[3], "[^[:digit:]|\\.]"),
      misc = str_remove_all(.x$time_info[4], "[^[:digit:]|\\.]"),
      metadata = list(
        str_c("stanc_version:", .x$stanc_version[1], sep = " "),
        str_c("opencl_device_name:", .x$opencl_device_name[1], sep = " "),
        str_c("opencl_platform_name", .x$opencl_platform_name[1], sep = " "),
        str_c("date", .x$start_datetime[1], sep = " ")
      )
    ),
    .id = "chain"
  )
  # Return the metadata field
  return(meta_data)
}
