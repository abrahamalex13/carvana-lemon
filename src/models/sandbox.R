#sandbox (for models)


fitControl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
method <- 'glmnet'
n <- nrow(X_std)

if (method == 'glmnet') {
  cl <- makeCluster(8)
  registerDoParallel(cl)

mdl <- caret::train(x = X_std[1:n, ],
                    y = factor(train1_keep[[varname_y]][1:n], levels = c(0, 1)),
                    trControl = fitControl,
                    # method = 'ranger'
                    # method = 'xgbDART'
                    method = method

                    )
  
mdl <- cv.glmnet(x = X_std[1:n, ],
                 y = factor(train1_keep[[varname_y]][1:n], levels = c(0, 1)),
                 family = "binomial", nfolds = 10, 
                 parallel = TRUE)

  stopCluster(cl)
  registerDoSEQ()
}

