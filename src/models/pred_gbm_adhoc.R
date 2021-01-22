Yhat_test.l <- lapply(
  seq(100, 4000, by = 50), mdl = mdl.l[[1]]$mdl, newx = X_test, 
  function(x, mdl, newx) extract_preds_xgb(mdl, newx, ntreelimit = x, transform_logit = TRUE )#extract_preds_gbm(mdl = mdl, newx = newx, n.trees = x)
)

chk <- unlist( lapply(Yhat_test.l, actual = Y_test, FUN = calc_Gini) )
plot(chk)
