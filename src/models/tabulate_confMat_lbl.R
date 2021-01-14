#calc_scores

tabulate_confMat_lbl <- function(pred, actual) {
  
  if (!is.factor(pred)) pred <- round(pred, 0)
  
  compare <- data.frame("actual" = actual, "predicted" = pred)
  confMat <- compare %>% 
    group_by(actual, predicted) %>% 
    summarize(n = n())

  confMat <- confMat %>% 
    group_by(actual) %>% 
    mutate(denom_by_actual = sum(n)) %>% 
    ungroup()
  
  confMat[["rate"]] <- confMat[["n"]] / confMat[["denom_by_actual"]]
  
  confMat <- confMat %>% 
    mutate(measure = case_when(
      
      actual == 1 & predicted == 1 ~ "True positive",
      actual == 1 & predicted == 0 ~ "False negative",
      actual == 0 & predicted == 1 ~ "False positive",
      actual == 0 & predicted == 0 ~ "True negative"
      
    ))
  
  return(confMat)
  
}

confMat_preds_tune <- function(tune, Yhat, Y) {
  
  out <- tabulate_confMat_lbl(pred = Yhat, actual = Y)
  out <- bind_cols(out, tune)
  out
  
}


calc_Gini <- function(actual, pred) {
  
  #DESCENDING by prediction
  results <- data.frame(actual = actual, pred = pred, idx = 1:length(actual) )
  results <- results %>% arrange(desc(pred, idx)) #dictates how ties sort
  
  sum_events <- sum(actual)
  popn_increment <- 1 / length(actual)
  
  #meaningful because of prediction sort
  accum_loss_percentage <- cumsum(results[["actual"]] / sum_events)
  
  accum_popn_percentage <- cumsum(popn_increment)
  
  sum_gini <- sum(accum_loss_percentage - accum_popn_percentage)
  sum_gini / length(actual)
  
}

calc_Gini_norm <- function(actual, pred) Gini(actual, pred) / Gini(actual, actual)