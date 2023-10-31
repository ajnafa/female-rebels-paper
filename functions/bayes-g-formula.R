model_averaged_ame <- function(x, weights, ndraws, summary = TRUE, ...) {
  
  # Nest the draws data frame
  draws <- x[, list(x.yz = list(.SD)), by=model]
  
  # If weights are a matrix, calculate the draws by row
  if (is.matrix(weights)) {
    
    # Construct a matrix of draw weights
    weighted_draws <- apply(
      weights, 
      MARGIN = 1, 
      FUN = function(x) {
        brms:::round_largest_remainder(x * ndraws)
      })
    
    # Initialize a list to store the draws in
    out <- list()
    
    # Loop over each column in the matrix
    for (i in 1:ncol(weighted_draws)) {
      
      # Randomly sample n rows for each model based on the weights
      draw_ids <- lapply(weighted_draws[, i], function(x){
        sample(sum(weighted_draws[,i]), size = x)
      })
      
      # Randomly sample n draws proportional to the weights
      out[[i]] <- lapply(seq_along(draws[, x.yz]), function(id) {
        draws[, x.yz[[id]][draw_ids[[id]]]]
      })
      
      # Bind the draws into a single data table per repetition
      out[[i]] <- rbindlist(out[[i]], idcol = "model")
    }
    
    # Combine everything into a single data table
    out <- rbindlist(out, idcol = "bridge_rep")
    
    # Average over the bridge sampling repetitions by draw
    if (isTRUE(summary)) {
      out = out[
        , keyby = .(.draw, model),
        lapply(.SD, mean),
        .SDcols = patterns("EY|AME")
      ]
    }
    
  } else if (is.vector(weights)) {
    # Construct a vector of draw weights
    weighted_draws <- round_largest_remainder(weights * ndraws)
    
    # Randomly sample n rows for each model based on the weights
    draw_ids <- lapply(weighted_draws, function(x){
      sample(sum(weighted_draws), size = x)
    })
    
    # Randomly sample n draws proportional to the weights
    out <- lapply(seq_along(draws[, x.yz]), function(id) {
      draws[, x.yz[[id]][draw_ids[[id]]]]
    })
    
    # Combine everything into a single data table
    out <- rbindlist(out, idcol = "model")
  }
  
  # Return the model averaged draws
  return(out)
  
}
