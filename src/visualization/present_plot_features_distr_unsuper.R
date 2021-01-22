#make chart variable names 'presentable'.


#discrete vars ------------

p_distr_discrete_pres.l <- 
  lapply(names(p_distr_discrete.l), varnames_dict = varnames_dict_present, FUN = function(x, varnames_dict) {
           
           p <- p_distr_discrete.l[[x]]
           lab_y <- "Frequency"
           title_prefix <- "Distribution: "
           
           p_out <- doPrepExplore:::relabel_univar_plot(
             p = p, varname_x0 = x, varnames_dict = varnames_dict, 
             title_prefix = title_prefix, lab_y = lab_y) + 
             scale_y_continuous(labels = comma)
           
           return(p_out)
           
  })
names(p_distr_discrete_pres.l) <- names(p_distr_discrete.l)

saveRDS(p_distr_discrete_pres.l, "reports/figures/unsupervised/discrete_var_bars.rds")

#end discrete vars -------



#conti vars -----------

# p_sub.l <- p_distr_conti.l[c("VehBCost",
#                              "RatioCostAuctionAverage", "RatioCostRetailAverage",
#                              "RatioCostAuctionClean", "RatioCostRetailClean",
#                              "RatioWarrantyVehBCost")]
# p_distr_conti_pres.l <-
#   lapply(names(p_sub.l), plots.l = p_sub.l, varnames_dict = tbl_pres_pred, 
#          FUN = doPrepExplore:::relabel_univar_plot)
# names(p_distr_conti_pres.l) <- names(p_sub.l)
# 
# saveRDS(p_distr_conti_pres.l, "reports/figures/unsupervised/price_quantities_kde.rds")

#end conti vars -------
