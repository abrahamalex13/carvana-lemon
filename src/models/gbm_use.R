#gbm_use


#predict
extract_preds_gbm <- function(mdl, newx, ...) gbm:::predict.gbm(mdl, newdata = newx, type = "response", ...)



validate_gbms <- function(mdl.l, X_train, Y_train, X_test, Y_test) {
  
  #predictions
  Yhat_train.l <- lapply(
    mdl.l, newx = X_train, function(x, newx) extract_preds_gbm(mdl = x$mdl, newx = newx)
  )  
  
  Yhat_test.l <- lapply(
    mdl.l, newx = X_test, function(x, newx) extract_preds_gbm(mdl = x$mdl, newx = newx)
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

  p <- 
    ggplot(confMat) +
      geom_line(aes(interaction.depth, rate,
                    group = interaction(id_dataset, loss, n.trees, shrinkage, bag.fraction),
                    linetype = loss)) +
      
      geom_point(aes(interaction.depth, rate,
                    color = interaction(shrinkage, bag.fraction),
                    shape = id_dataset), size = 4) +
      
      facet_wrap(~measure, scales = "free_y") +
      scale_y_continuous(label = scales::percent) +
      theme(legend.position = "top")
  
  list("Gini" = score_Gini, "confMat" = confMat, "p" = p)

}