#model_main.R


#data load -----------------

if (run_type_train) {
  
  X <- readRDS(filename_X_train)
  if (model_type %in% c("xgboost", "gbm", "glm")) X <- as.matrix(X)
  if (to_sparse) X <- Matrix(as.matrix(X), sparse = TRUE)
  
}

if (run_type_test) {
  
  X_test <- readRDS(filename_X_test)
  if (model_type %in% c("xgboost", "gbm", "glm")) X_test <- as.matrix(X_test)
  if (to_sparse) X_test <- Matrix(as.matrix(X_test), sparse = TRUE)
  
}

#Y only possibly available under training run
if (run_type_train) {
    
  Y <- readRDS(filename_Y_train)
  if (run_type_test) Y_test <- readRDS(filename_Y_test)
   
}

#end data load ---------------



time_start <- Sys.time()
#individual model estimation ---------

if (run_type_train) {

  if (model_type == "rf") {
    
    grid_B <- 1000 #c(1000, 2000) #c(100, 250, 400) #500
    grid_wgt_event <- 1
    p <- ncol(X)
    grid_mtry <- c(floor(.02 * p), 10, sqrt(p)) #sqrt(p) #floor(.1 * p) #c(.5 * sqrt(p), sqrt(p), floor(.5 * p))
    
    grid_tune <- expand.grid("B" = grid_B, "wgt_event" = grid_wgt_event, "mtry" = grid_mtry)
    
    source("src/models/rf_estimate.R")
    invisible( estimate_rangers(
      X = X, Y = Y, grid_tune = grid_tune, filename_out = paste(directory_mdl_save, "rf.rds", sep = "")
      ) )
    
  } else if (model_type == "gbm") {
    
    grid_loss <- "adaboost" #c("bernoulli", "adaboost") #c("bernoulli", "adaboost")
    grid_n.trees <- 2500 #seq(100, 3000, by = 250) #1000 #500
    grid_interaction.depth <- 40
    grid_shrinkage <- c(1e-2) #c(1e-1, .25) #c(1e-1, .5)
    grid_bag.fraction <- .5
    
    factor_w_event <- 1
    
    grid_tune <- expand.grid("loss" = grid_loss
                             , "n.trees" = grid_n.trees
                             , "interaction.depth" = grid_interaction.depth
                             , "shrinkage" = grid_shrinkage
                             , "bag.fraction" = grid_bag.fraction
                             , "factor_w_event" = factor_w_event
                             , KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    
    source("src/models/gbm_estimate.R")
    invisible( estimate_gbms(
      X = X, Y = Y, grid_tune = grid_tune, filename_out = paste(directory_mdl_save, "gbm.rds", sep = "")
    ) )
    
  } else if (model_type == "xgboost") {
    
    grid_objective <- "binary:logitraw"
    grid_nrounds <- 4000
    grid_max_depth <- 3 #c(1, 2, 3) #c(3, 5)
    grid_eta <- 1e-2
    grid_subsample <- .5
    grid_colsample_bytree <- 1 #c(1, .5)
    grid_gamma <- 0
    grid_min_child_weight <- 1
    grid_lambda <- 0
    grid_alpha <- 0 #c(0, .5)
    grid_tree_method <- "exact"
    
    factor_w_event <- 1
    
    grid_tune <- expand.grid("objective" = grid_objective
                             , "nrounds" = grid_nrounds
                             , "max_depth" = grid_max_depth
                             , "eta" = grid_eta
                             , "subsample" = grid_subsample
                             , "colsample_bytree" = grid_colsample_bytree
                             , "gamma" = grid_gamma
                             , "min_child_weight" = grid_min_child_weight
                             , "lambda" = grid_lambda
                             , "alpha" = grid_alpha
                             , "tree_method" = grid_tree_method
                             , KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    
    source("src/models/xgboost_estimate.R")
    invisible( estimate_xgboosts(
      X = X, Y = Y, grid_tune = grid_tune, filename_out = paste(directory_mdl_save, "xgboost.rds", sep = "")
    ))
    
  } else if (model_type == "glm") {
    
    grid_alpha <- c(0, .5) #c(.5, 1) #c(0, .5, 1)
    grid_w_event <- c(1, 2)
    grid_tune <- expand.grid("alpha" = grid_alpha, 
                             "factor_w_event" = grid_w_event)
    
    source("src/models/glm_estimate.R")
    invisible( estimate_glms(X = X, Y = Y, grid_tune = grid_tune) )
    
  }
  
}
  
#end model estimation ---------
time_end <- Sys.time()
print(paste("model estimation time elapsed: ", time_end - time_start, sep = ""))
  


#model predictions, validation if requested -----------

if (run_type_test) {
  
  mdl.l <- readRDS(paste(directory_mdl_save, model_type, ".rds", sep = ""))
  
  Yhat_test.l <- lapply(
    mdl.l, newdata = X_test, function(x, newdata) predict(x$mdl, newdata = newdata)
  )
  
  #extract standard-form predictions, from variable 'predict' output.
  if (model_type == "rf") Yhat_test.l <- lapply(Yhat_test.l, function(x) x$predictions[, 2])
  if (model_type == "xgboost" & do_transform_logit) {
    Yhat_test.l <- lapply(Yhat_test.l, function(x) exp(x) / (1 + exp(x)))
  }

  
  #(validate under test & train type-execution)
  
  if (run_type_train) {
  
    scores <- useValidateModels::validate_tunes_classifier(
      mdl.l = mdl.l, X_train = X, Y_train = Y, X_test = X_test, Y_test = Y_test, scores = "Gini", 
      filename_out = paste(directory_mdl_save, "scores_", model_type, ".rds", sep = "")
    )
  
  }
  
}
  
#end model pred & validate --------------------------



#ensemble methods



#output predictions along with attributes set (identifiers, etc)
if (run_type_test && !run_type_train) {
  
  Yhat_test <- useValidateModels::output_pred_attr_set(
    pred = Yhat_test.l[[1]], attr_set = identifiers_test, filename_out = filename_pred
  )
  
}