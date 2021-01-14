#glm_estimate.R

estimate_glms <- function(X, Y, grid_tune, filename_out = "models/glm.rds") {
 
  #parallel enviro not already defined
  do_parallel <- TRUE
  
  if (do_parallel) {
    
    cl <- makeCluster(detectCores() - 2)
    clusterExport(cl, c("grid_tune", "X", "Y"))
    clusterEvalQ(cl, {
      
      library(glmnet)
      
    })
    
  }
    
    mdl.l <-
      parLapply(cl, 1:nrow(grid_tune), function(x) {
        # lapply(1:nrow(grid_tune), function(x) {
        
        mdl <- glmnet(x = X, y = factor(Y, levels = c(0, 1)), 
                      alpha = grid_tune[["alpha"]][x],
                      weights = ifelse(Y == 1, grid_tune[["factor_w_event"]][x], 1), 
                      family = "binomial", standardize = FALSE, intercept = FALSE)
        
        out <- list("mdl" = mdl
                    , "tune" = bind_cols(
                      data.frame("s" = colnames(mdl$beta), "lambda" = mdl$lambda),
                      grid_tune[x, ])
        )
        
        return(out)
        
      })
    
    if (do_parallel) {
      stopCluster(cl)
      registerDoSEQ()
    }
  
  saveRDS(mdl.l, file = filename_out)
  
}