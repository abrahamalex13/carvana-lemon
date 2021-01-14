#gbm_estimate.R

estimate_gbms <- function(X, Y, grid_tune, filename_out = "models/gbm.rds") {
  
  #parallel enviro not already defined
  do_parallel <- TRUE
  
  if (do_parallel) {
  
    cl <- makeCluster(detectCores() - 2)
    clusterExport(cl, c("grid_tune", "X", "Y"))
    clusterEvalQ(cl, {
  
      library(gbm)
  
    })
  }
  
  
  mdl.l <-
    parLapply(cl, 1:nrow(grid_tune), function(x) {
      # lapply(1:nrow(grid_tune), function(x) {
      
      mdl <- gbm.fit(x = X, y = Y
                     
                     , w = ifelse(Y == 1, grid_tune[["factor_w_event"]][x], 1)
                     , distribution = grid_tune[["loss"]][x]
                     , n.trees = grid_tune[["n.trees"]][x]
                     , interaction.depth = grid_tune[["interaction.depth"]][x]
                     , shrinkage = grid_tune[["shrinkage"]][x]
                     , bag.fraction = grid_tune[["bag.fraction"]][x]
                     
                     , keep.data = FALSE)
      
      out <- list("mdl" = mdl
                  , "tune" = grid_tune[x, ])
      
      return(out)
      
    })
  
  if (do_parallel) {
    stopCluster(cl)
    registerDoSEQ()
  }
  
  # do_augment_gbm_trees <- TRUE
  # if (!do_augment_gbm_trees) { 
  #   invisible( estimate_gbms(X = X, Y = Y, grid_tune = grid_tune) )
  # } else if (do_augment_gbm_trees) {
  #   
  #   n.trees_add <- c(500, 1000)
  #   mdl.l <- c(mdl.l, 
  #              lapply(mdl.l, function(x) lapply(n.trees_add, mdl = x, function(x, mdl) gbm.more(mdl, n.new.trees = x)))
  #   )
  
  saveRDS(mdl.l, file = filename_out)
  
}