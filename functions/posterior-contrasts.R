#' Posterior Contrasts for Bayesian Models fit with {brms}
#'
#' @param models A named list of model objects of class `brmsfit` for which to 
#' generate predictions.
#' 
#' @param X A string indicating the name of the treatment in the models.
#' 
#' @param data Data used to fit the model objects.
#' 
#' @param contrasts Values of `X` at which to generate predictions. Defaults to
#' `c(0, 1)` but could also be set to factor levels.
#' 
#' @param cores Number of cores to use for parallel computation. If cores > 1,
#' future::multisession is used to speed computation via `{furrr}`. Requires
#' the `{furrr}` package.
#' 
#' @param ... Additional arguments passed down to `brms::posterior_predict`
#'
#' @return A list of posterior contrasts for each model in `models`
#' 
#' @export posterior_contrasts
#'
posterior_contrasts <- function(models, 
                                X, 
                                data,
                                contrasts = c(0, 1),
                                cores = 1,
                                ...) {
  
  # Check that all elements of models are of class brmsfit
  for (i in seq_along(models)) {
    brms_check <- brms::is.brmsfit(models[[i]])
    stopifnot(
      "All elements in models must be of class brmsfit" = brms_check, 
      brms_check
      )
  }
  
  # Y_i(X = 0, Z)
  lo <- data |> mutate("{X}" := contrasts[1])
  
  # Y_i(X = 1, Z)
  hi <- data |> mutate("{X}" := contrasts[2])
  
  # Check if cores > 1
  if (cores > 1) {
    
    # Requires {furrr}
    require(furrr)
    
    # Fit models in parallel via future
    plan(tweak(multisession, workers = cores))
    
    # Posterior predictions for each data set
    predictions <- furrr::future_map(
      .x = models,
      ~ .get_posterior_predictions(hi, lo, model = .x, ...),
      .options = furrr::furrr_options(
        scheduling = 1,
        seed = TRUE,
        prefix = "prefix"
      ),
      .progress = TRUE
    )
    
    # Close the future session
    plan(sequential)
  }
  
  if (cores == 1) {
    # Posterior predictions for each data set
    predictions <- purrr::map(
      .x = models,
      ~ .get_posterior_predictions(hi, lo, model = .x, ...)
    )
  }
  
  # Append the list of data tables into one
  predictions <- rbindlist(
    predictions,
    fill = TRUE,
    idcol = "model"
  )
  
  # Return the list of predictions
  return(predictions)
}

# Helper function to get posterior prediction contrasts
.get_posterior_predictions <- function(hi, lo, model, ...) {
  
  # Predictions for Y_i(X = 1, Z)
  EYX1 <- brms::posterior_predict(model, newdata = hi, ...)
  
  # Predictions for Y_i(X = 0, Z)
  EYX0 <- brms::posterior_predict(model, newdata = lo, ...)
  
  # Average over the observations for each draw in the prediction matrix
  out <- data.table(
    EYX1 = rowMeans(EYX1),
    EYX0 = rowMeans(EYX0),
    AME = rowMeans(EYX1 - EYX0),
    .draw = 1:ndraws(model)
  )
  
  # Return just the average contrast
  return(out)
}
