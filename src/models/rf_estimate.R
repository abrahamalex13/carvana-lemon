#rf_estimate.R

estimate_rangers <- function(X, Y, grid_tune, filename_out = "models/rf.rds") {

  mdl.l <- lapply(1:nrow(grid_tune), function(x) {
    
    mdl <- ranger(x = X, y = factor(Y, levels = c(0, 1)), 
                  importance = "permutation", oob.error = TRUE, 
                  num.trees = grid_tune[["B"]][x],
                  mtry = grid_tune[["mtry"]][x],
                  class.weights = c(1, grid_tune[["wgt_event"]][x]),
                  
                  write.forest = TRUE,
                  # respect.unordered.factors = "order",
                  
                  probability = TRUE,
                  
                  num.threads = detectCores() - 1, 
                  verbose = TRUE)
    
    out <- list("mdl" = mdl
                , "tune" = grid_tune[x, ])
    
    return(out)
    
  })
  
  saveRDS(mdl.l, file = filename_out)
  
}