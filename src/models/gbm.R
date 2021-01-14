#gbm



#model estimation -----------------

if (do_estimate) {
  
  time_start <- Sys.time()
 
  grid_loss <- c("adaboost", "bernoulli")
  grid_n.trees <- 100 #1000 #c(500, 1000)
  grid_interaction.depth <- c(3, 4)
  grid_shrinkage <- 1e-1
  grid_bag.fraction <- c(.25, .75)
  
  factor_w_event <- 1

  grid_tune <- expand.grid("loss" = grid_loss
                          , "n.trees" = grid_n.trees
                          , "interaction.depth" = grid_interaction.depth
                          , "shrinkage" = grid_shrinkage
                          , "bag.fraction" = grid_bag.fraction
                          , "factor_w_event" = factor_w_event
                          , KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  
  #parallel enviro not already defined
  do_parallel <- TRUE

  if (do_parallel) {

    cl <- makeCluster(detectCores() - 2)
    clusterExport(cl, c("grid_tune", "X", "Y"))
    clusterEvalQ(cl, {

      library(gbm)

    })
  }
  
  
  mdl.l <-
    parLapply(cl, 1:nrow(grid_tune), function(x) {
      # lapply(1:nrow(grid_tune), function(x) {
      
      mdl <- gbm.fit(x = X, y = Y
                     
                     , w = ifelse(Y == 1, grid_tune[["factor_w_event"]][x], 1)
                     , distribution = grid_tune[["loss"]][x]
                     , n.trees = grid_tune[["n.trees"]][x]
                     , interaction.depth = grid_tune[["interaction.depth"]][x]
                     , shrinkage = grid_tune[["shrinkage"]][x]
                     , bag.fraction = grid_tune[["bag.fraction"]][x]
                     
                     , keep.data = FALSE)
      
      out <- list("mdl" = mdl
                  , "tune" = grid_tune[x, ])
      
      return(out)
      
    })
  
  if (do_parallel) {
    stopCluster(cl)
    registerDoSEQ()
  }
  
  saveRDS(mdl.l, file = "models/gbm.rds")
  
} else mdl.l <- readRDS("models/gbm.rds")
  
#end model est -----------------



#predict -------------

Yhat_test.l <- lapply(mdl.l, newx = X_test, function(x, newx) predict.gbm(x$mdl, newdata = newx, type = "response"))

if (run_type == "train") Yhat_train.l <- lapply(mdl.l, newx = X, function(x, newx) predict.gbm(x$mdl, newdata = newx, type = "response"))

#end predict -------



#assess performance -----------

if (run_type == "train") {

  confMat_train.l <- bind_rows( 
    lapply(1:length(mdl.l), Y = Y, Yhat.l = Yhat_train.l, function(x, Y, Yhat.l) {
      
      out <- tabulate_confMat_lbl(pred = Yhat.l[[x]], actual = Y)
      bind_cols(out, mdl.l[[x]]$tune) %>% 
        mutate(id_dataset = "train")
      
    })
  )
  
  confMat_test.l <- bind_rows( 
    lapply(1:length(mdl.l), Y = Y_test, Yhat.l = Yhat_test.l, function(x, Y, Yhat.l) {
      
      out <- tabulate_confMat_lbl(pred = Yhat.l[[x]], actual = Y)
      bind_cols(out, mdl.l[[x]]$tune) %>% 
        mutate(id_dataset = "test")
      
    })
  )

  confMat <- bind_rows(confMat_train.l, confMat_test.l)

  ggplot(confMat) +
    geom_line(aes(n.trees, rate,
                  group = interaction(id_dataset, interaction.depth, bag.fraction),
                  color = interaction(factor(interaction.depth), bag.fraction),
                  linetype = id_dataset)) +
    
    geom_point(aes(n.trees, rate,
                  group = interaction(id_dataset, interaction.depth, bag.fraction),
                  color = interaction(factor(interaction.depth), bag.fraction),
                  linetype = id_dataset)) +
    
    facet_wrap(~measure, scales = "free_y") +
    scale_y_continuous(label = scales::percent) +
    theme(legend.position = "top")

}

#end assess perform ---------