#standardize.R

do_standardize <- function(X, varnames_standardize, filename_operator, run_type) {
  
  if (run_type == "train") {
    
    invisible(doPrepExplore::derive_standardize_operator(
      df = X[, varnames_standardize], filename_operator = filename_operator
    ))
    
  }
  
  transformer_std <- readRDS(filename_operator)
  X_std <- caret:::predict.preProcess(transformer_std, X)
  
  X_std
  
}