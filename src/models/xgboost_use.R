#xgboost_use


#predict
extract_preds_xgb <- 
  function(mdl, newx, transform_logit, ...) {
    
    out <- predict(mdl, newdata = newx, ...)
    if (transform_logit) out <- exp(out) / (1 + exp(out))
    out
    
  }



validate_xgboosts <- function(mdl.l, X_train, Y_train, X_test, Y_test) {
  
  #predictions
  Yhat_train.l <- lapply(
    mdl.l, newx = X_train, function(x, newx) extract_preds_xgb(mdl = x$mdl, newx = newx, transform_logit = TRUE)
  )  
  
  Yhat_test.l <- lapply(
    mdl.l, newx = X_test, function(x, newx) extract_preds_xgb(mdl = x$mdl, newx = newx, transform_logit = TRUE)
  )  
  
  
  #score
  score_Gini.l <- lapply(
    1:length(mdl.l), Y = Y_test, 
    FUN = function(x, Y) Gini_preds_tune(tune = mdl.l[[x]]$tune, Yhat = Yhat_test.l[[x]], Y = Y)
  )
  score_Gini <- bind_rows(score_Gini.l)
  
  
  confMat_train.l <- bind_rows( 
    lapply(1:length(mdl.l), mdl.l = mdl.l, Y = Y_train, Yhat.l = Yhat_train.l, 
           function(x, mdl.l, Y, Yhat.l) confMat_preds_tune(tune = mdl.l[[x]]$tune, Y = Y, Yhat = Yhat.l[[x]])
    )
  ) %>% mutate(id_dataset = "train")
  
  confMat_test.l <- bind_rows( 
    lapply(1:length(mdl.l), mdl.l = mdl.l, Y = Y_test, Yhat.l = Yhat_test.l, 
           function(x, mdl.l, Y, Yhat.l) confMat_preds_tune(tune = mdl.l[[x]]$tune, Y = Y, Yhat = Yhat.l[[x]])
    )
  ) %>% mutate(id_dataset = "test")
  
  confMat <- bind_rows(confMat_train.l, confMat_test.l)
  
  
  list("Gini" = score_Gini, "confMat" = confMat)
  
}