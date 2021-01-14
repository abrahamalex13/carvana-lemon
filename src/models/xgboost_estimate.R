#xgboost_estimate.R

estimate_xgboosts <- function(X, Y, grid_tune, filename_out = "models/xgboost.rds") {
  
  dtrain <- xgb.DMatrix(X, label = Y)
  
  mdl.l <- lapply(1:nrow(grid_tune), grid_tune = grid_tune, data = data, 
                  function(x, grid_tune, data) {
    
    param <- list(objective = grid_tune[["objective"]][x],
                  max_depth = grid_tune[["max_depth"]][x],
                  eta = grid_tune[["eta"]][x],
                  subsample = grid_tune[["subsample"]][x],
                  colsample_bytree = grid_tune[["colsample_bytree"]][x],
                  gamma = grid_tune[["gamma"]][x],
                  min_child_weight = grid_tune[["min_child_weight"]][x],
                  lambda = grid_tune[["lambda"]][x],
                  tree_method = grid_tune[["tree_method"]][x])
    nrounds <- grid_tune[["nrounds"]][x]
    
    mdl <- xgb.train(params = param, data = dtrain, nrounds = nrounds)
    
    list("mdl" = mdl, "tune" = grid_tune[x, ])
    
  })
  
  saveRDS(mdl.l, file = filename_out)
  
}