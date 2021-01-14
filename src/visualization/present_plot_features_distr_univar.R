#present_plot_features_distr_univar.R
#make chart variable names 'presentable'.


#discrete vars ------------

p_distr_discrete_pres.l <- 
  lapply(names(p_distr_discrete.l), df_relabels = tbl_pres_pred, FUN = function(x, df_relabels) {
           
           p <- p_distr_discrete.l[[x]]
           lab_y <- "Frequency"
           title_prefix <- "Distribution: "
           
           p_out <- 
             doPrepExplore:::relabel_plot_present(p = p, varname_x0 = x, df_relabels = df_relabels, 
                                                  title_prefix = title_prefix, lab_y = lab_y) + 
             scale_y_continuous(labels = comma)
           
           return(p_out)
           
  })
names(p_distr_discrete_pres.l) <- names(p_distr_discrete.l)

saveRDS(p_distr_discrete_pres.l, paste(path_plots, "unsupervised/discrete_var_bars.rds", sep = ""))

#end discrete vars -------



#conti vars -----------

# p_sub.l <- p_distr_conti.l[c("VehBCost",
#                              "RatioCostAuctionAverage", "RatioCostRetailAverage",
#                              "RatioCostAuctionClean", "RatioCostRetailClean",
#                              "RatioWarrantyVehBCost")]
# p_distr_conti_pres.l <- 
#   lapply(names(p_sub.l), plots.l = p_sub.l, 
#          df_relabels = tbl_pres_pred, FUN = relabel_plot_pres)
# names(p_distr_conti_pres.l) <- names(p_sub.l)
# 
# saveRDS(p_distr_conti_pres.l, paste(path_plots, "unsupervised/price_quantities_kde.rds", sep = ""))

#end conti vars -------
