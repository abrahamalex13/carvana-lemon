#penalized_linear



#model estimation -----------------

#interestingly, pure LASSO struggles to converge under more basis functions
grid_alpha <- c(0, .5) #c(.5, 1) #c(0, .5, 1)
grid_w_event <- c(1, 2)
grid_tune <- expand.grid("alpha" = grid_alpha, 
                         "factor_w_event" = grid_w_event)

if (do_estimate) {
  
  time_start <- Sys.time()
  
  #parallel enviro not already defined
  do_parallel <- TRUE

  if (do_parallel) {
    
    cl <- makeCluster(detectCores() - 2)
    clusterExport(cl, c("grid_tune", "X", "Y"))
    clusterEvalQ(cl, {
      
      library(glmnet)
      library(tidyverse)
      
    })
  }
  
  
  mdl.l <-
    parLapply(cl, 1:nrow(grid_tune), function(x) {
      # lapply(1:nrow(grid_tune), function(x) {
      
      mdl <- glmnet(x = X, y = factor(Y, levels = c(0, 1)), 
                   alpha = grid_tune[["alpha"]][x],
                   weights = ifelse(Y == 1, grid_tune[["factor_w_event"]][x], 1), 
                   family = "binomial", standardize = FALSE, intercept = FALSE)
      
      out <- list("mdl" = mdl
                  , "tune" = bind_cols(
                    data.frame("s" = colnames(mdl$beta), "lambda" = mdl$lambda),
                    grid_tune[x, ])
      )
      
      return(out)
      
    })
  
  if (do_parallel) {
    stopCluster(cl)
    registerDoSEQ()
  }
  
  saveRDS(mdl.l, file = "models/penalized_linear.rds")
  
  time_end <- Sys.time()
  print(time_end - time_start)

} else mdl.l <- readRDS("models/penalized_linear.rds")

#end model est -------------------



#predict --------

#for each model, a suite of predictions (many lambda values)
Yhat_test.l <- 
  lapply(mdl.l, newx = X_test,
         function(x, newx) as.data.frame(predict(x$mdl, newx = newx, type = "response"))
  )

if (run_type == "train") {
  
  Yhat_train.l <-
    lapply(mdl.l, newx = X,
           function(x, newx) as.data.frame(predict(x$mdl, newx = newx, type = "response"))
    )
}

# ----------



#assess performance -----------

if (run_type == "train") {

  #Yhat is a suite of predictions (data.frame)
  #one row of tune_details contains: 
    # 's' (glmnet-encoded lambda), lambda, and custom grid_tune columns.
  assess_pred_glmnet_suite <- function(Yhat, tune_details, Y) {
    
    confMat.l <- lapply(Yhat, actual = Y, FUN = tabulate_confMat_lbl)
    
    #one column of predictions has one corresponding row in tune_details.
    confMat.l <- 
      lapply(1:nrow(tune_details), 
             function(x) bind_cols(confMat.l[[x]], tune_details[x, ]))
    
    confMat <- bind_rows(confMat.l)
  
    confMat
    
  }
  
  confMat_train <- lapply(
    1:length(mdl.l), mdl.l = mdl.l, Yhat.l = Yhat_train.l, 
    Y = Y, 
    function(x, mdl.l, Yhat.l, Y) {
    
    assess_pred_glmnet_suite(Yhat = Yhat.l[[x]], tune_details = mdl.l[[x]]$tune, Y = Y) %>% 
      mutate(id_dataset = "train")
    
  })
  
  confMat_test <- lapply(
    1:length(mdl.l), mdl.l = mdl.l, Yhat.l = Yhat_test.l, 
    Y = Y_test, 
    function(x, mdl.l, Yhat.l, Y) {
      
      assess_pred_glmnet_suite(Yhat = Yhat.l[[x]], tune_details = mdl.l[[x]]$tune, Y = Y) %>% 
        mutate(id_dataset = "test")
      
    })
  
  confMat <- bind_rows(confMat_train, confMat_test)
  
  
  
  ggplot(confMat %>% dplyr::filter(lambda < .1)) +
    geom_line(aes(lambda, rate,
                  group = interaction(id_dataset, alpha, factor_w_event),
                  color = interaction(factor(alpha), factor_w_event),
                  linetype = id_dataset), alpha = .75) +
    facet_wrap(~measure, scales = "free_y") +
    scale_y_continuous(label = scales::percent) +
    scale_x_continuous(label = scales::scientific) + 
    theme(legend.position = "top")

}

#end assess perform ----------



#reduced-penalty refit  ---------------

# get_tp_based_lambda <- function()
# 
# idx_mdl_best <- 2
# mdl_fnl <- mdl.l[[idx_mdl_best]]
# summ_tp <- confMat %>% 
#   dplyr::filter(alpha == grid_tune[idx_mdl_best, "alpha"] & 
#                 factor_w_event == grid_tune[idx_mdl_best, "factor_w_event"] & 
#                   id_dataset == "valid" & 
#                   measure == "True positive")
# max_tp <- max(summ_tp[["rate"]])
# se_tp <- sd(summ_tp[["rate"]])
# which_tp_1se_grid <- which.min( (1 - summ_tp[["rate"]]) - (1 - max_tp + se_tp) )
# lambda_1se <- summ_tp[which_tp_1se_grid, "lambda"]
# 
# s_fnl <- as.character( summ_tp[which_tp_1se_grid, "s"] )
# lambda_fnl <- lambda_1se
# 
# coefs_suite_mdl <- coef(mdl_fnl$mdl)
# coefs <- data.frame("term" = rownames(coefs_suite_mdl), 
#                     "value" = as.numeric(coefs_suite_mdl[, s_fnl]))
# terms_nz <- coefs[abs( coefs[["value"]] ) > 1e-16, "term"][-1]
# which_terms_nz <- which(terms_nz %in% colnames(X))
# 
# 
# 
# mdl <- glmnet(x = X[, which_terms_nz], y = factor(Y, levels = c(0, 1)),
#               alpha = 0, weights = ifelse(Y == 1, grid_tune[["factor_w_event"]][idx_mdl_best], 1),
#               family = "binomial", standardize = FALSE, intercept = FALSE, 
#               trace.it = 1)
# 
# pred_valid0 <- as.data.frame( predict(mdl, newx = X_test[, which_terms_nz], type = "response") )
# pred_valid <- pred_valid0[, ncol(pred_valid0), drop = FALSE]
# colnames(pred_valid) <- "pred_prb"
# pred_valid[["actual"]] <- Y_test
# pred_valid[["pred"]] <- round(pred_valid[[1]])
# pred_valid[["is_wrong"]] <- pred_valid[["actual"]] != pred_valid[["pred"]]
# ggplot(pred_valid) + 
#   geom_density(aes(pred_prb, group = is_wrong, color = is_wrong)) 
# 
# 
# 
# summ_valid <- summarize_pred_binary(pred_valid[[1]], actual = Y_test)
# summ_valid