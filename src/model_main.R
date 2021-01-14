#model_main.R

time_start <- Sys.time()


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



#individual model estimation and/or predictions ----------

if (model_type == "rf") {
  
  if (run_type_train) {
    
    grid_B <- 1000 #c(1000, 2000) #c(100, 250, 400) #500
    grid_wgt_event <- 1
    p <- ncol(X)
    grid_mtry <- c(floor(.02 * p), 10, sqrt(p)) #sqrt(p) #floor(.1 * p) #c(.5 * sqrt(p), sqrt(p), floor(.5 * p))
    
    grid_tune <- expand.grid("B" = grid_B, "wgt_event" = grid_wgt_event, "mtry" = grid_mtry)
    
    source("src/models/rf_estimate.R")
    invisible( estimate_rangers(
      X = X, Y = Y, grid_tune = grid_tune, filename_out = paste(directory_mdl_save, "rf.rds", sep = "")
      ) )
    
  }
  
  if (run_type_test) {
    
    mdl.l <- readRDS(paste(directory_mdl_save, "rf.rds", sep = ""))
    source("src/models/rf_use.R")
    
    if (run_type_test) Yhat_test.l <- lapply(
      mdl.l, newx = X_test, function(x, newx) extract_preds_ranger(mdl = x$mdl, newx = newx)
      )
    
    if (run_type_train && do_validate) {
      p <- validate_rangers(mdl.l, X_train = X, Y_train = Y, X_test = X_test, Y_test = Y_test)
    }
    
  }
  
}



if (model_type == "gbm") {
  
  if (run_type_train) {
  
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
      
    }
  
  if (run_type_test) {
    
    mdl.l <- readRDS(paste(directory_mdl_save, "gbm.rds", sep = ""))
    source("src/models/gbm_use.R")
    
    Yhat_test.l <- lapply(
      mdl.l, newx = X_test, function(x, newx) extract_preds_gbm(mdl = x$mdl, newx = newx)
    )
    
    if (run_type_train && do_validate) {
      scores <- validate_gbms(mdl.l, X_train = X, Y_train = Y, X_test = X_test, Y_test = Y_test)
      saveRDS(scores, file = paste(directory_mdl_save, "scores_gbm.rds", sep = ""))
    }
    
  }
  
  
}



if (model_type == "xgboost") {
  
  if (run_type_train) {
    
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
    
  }
  
  if (run_type_test) {
    
    mdl.l <- readRDS(paste(directory_mdl_save, "xgboost.rds", sep = ""))
    source("src/models/xgboost_use.R")
    
    Yhat_test.l <- lapply(
      mdl.l, newx = X_test, function(x, newx) extract_preds_xgb(mdl = x$mdl, newx = newx, transform_logit = TRUE)
    )
    
    if (run_type_train && do_validate) {
      scores <- validate_xgboosts(mdl.l, X_train = X, Y_train = Y, X_test = X_test, Y_test = Y_test)
      saveRDS(scores, file = paste(directory_mdl_save, "scores_xgboost.rds", sep = ""))
      
    }
    
  }
  
}


if (model_type == "glm") {
  
  if (run_type_train) {
    
    grid_alpha <- c(0, .5) #c(.5, 1) #c(0, .5, 1)
    grid_w_event <- c(1, 2)
    grid_tune <- expand.grid("alpha" = grid_alpha, 
                             "factor_w_event" = grid_w_event)
    
    source("src/models/glm_estimate.R")
    invisible( estimate_glms(X = X, Y = Y, grid_tune = grid_tune) )
    
  }
  
}

#end indiv mdl est, pred ----------------------
  
time_end <- Sys.time()
print(paste("time elapsed: ", time_end - time_start, sep = ""))


#ensemble methods



#predictions output
if (run_type_test && !do_validate) {
  
  source("src/data/output_pred.R")
  Yhat_test <- output_pred(Yhat = Yhat_test.l[[1]], identifiers = identifiers_test, filename_out = filename_pred)
  
}