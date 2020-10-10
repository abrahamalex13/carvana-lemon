#present_plot_features_distr_by_outcome.R
#make chart variable names 'presentable'.


#discrete vars ------------

p_distr_discrete_by_y_pres.l <- 
  lapply(names(p_distr_discrete_by_y.l), df_relabels = tbl_pres_pred, FUN = function(x, df_relabels) {
    
    p <- p_distr_discrete_by_y.l[[x]]
    lab_y <- "Rate"
    title_prefix <- "Lemon Rate by "
    
    p_out <- 
      doPrepExplore:::relabel_plot_present(p = p, varname_x0 = x, df_relabels = df_relabels, 
                                           title_prefix = title_prefix, lab_y = lab_y)
    
    return(p_out)
    
  })
names(p_distr_discrete_by_y_pres.l) <- names(p_distr_discrete_by_y.l)

saveRDS(p_distr_discrete_by_y_pres.l, paste(path_plots, "supervised/discrete_var_bars_by_y.rds", sep = ""))

#end discrete vars -------




#conti vars -------

p_sub.l <- p_distr_conti_by_y.l[c("VehOdo", "VehBCost",
                                "RatioCostAuctionAverage", "RatioCostRetailAverage",
                                "RatioCostAuctionClean", "RatioCostRetailClean",
                                "RatioWarrantyVehBCost")]

p_distr_conti_by_y_pres.l <- 
  lapply(names(p_sub.l), df_relabels = tbl_pres_pred, FUN = function(x, df_relabels) {
    
    p <- p_sub.l[[x]]
    lab_y <- "Probability Density"
    title_prefix <- "Distribution by Outcome: "
    
    p_out <- 
      doPrepExplore:::relabel_plot_present(p = p, df_relabels = df_relabels, 
                                           varname_x0 = x, title_prefix = title_prefix, lab_y = lab_y)
    
    return(p_out)
    
  })
names(p_distr_conti_by_y_pres.l) <- names(p_sub.l)

saveRDS(p_distr_conti_by_y_pres.l, paste(path_plots, "supervised/price_quantities_by_y.rds", sep = ""))
