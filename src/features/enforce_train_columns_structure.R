#enforce_train_columns_structure

enforce_train_columns_structure <- function(X, filename_save_X, filename_operator, run_type) {

  if (run_type == "train") {
    
    saveRDS(X, file = filename_save_X)
    invisible(doPrepExplore::check_columns_order(
      df = X, 
      filename_operator = filename_operator,
      run_type = run_type
    ))   
    
  } else {
    
    #new factor levels in test would imply, test columns do not match train
    operator <- readRDS(filename_operator)
    
    #columns in test, but not in train, lack clear applicability in modeling; drop
    X <- X[, intersect(colnames(X), operator)]
    
    #columns in train, but not in test, reflect 'train' factor levels not observed in test.
    #soundly populate as zero.
    varnames_train_fill <- setdiff(operator, colnames(X))
    blanks <- matrix(0, ncol = length(varnames_train_fill), nrow = nrow(X))
    colnames(blanks) <- varnames_train_fill
    X <- cbind(X, blanks)
    X <- X[, operator]
    
    saveRDS(X, file = filename_save_X)
    
  }
  
  X
  
}