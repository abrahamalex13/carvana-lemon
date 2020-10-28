#plot_features_distr_univar.R
#don't peek at outcomes! prepare inputs independently.


#conti vars ---------------------

varnames_plot <- colnames(train1)[grepl("MMR", colnames(train1))]
varnames_plot <- c(varnames_plot, "VehBCost",
                   "RatioCostAuctionAverage", "RatioCostRetailAverage",
                   "RatioCostAuctionClean", "RatioCostRetailClean",
                   "RatioWarrantyVehBCost")
p_distr_conti.l <- lapply(varnames_plot, df = train1, path_filename_print = NULL, 
                          FUN = doPrepExplore:::ggplot_density_x)
names(p_distr_conti.l) <- varnames_plot
filename_out <- paste(path_plots, "unsupervised/", "price_quantities_kde", ".pdf", sep = "")
pdf(filename_out, width = 13, height = 8)
invisible(lapply(p_distr_conti.l, FUN = print))
dev.off()

#end conti vars -------------



#discrete vars --------

varnames_tab <- c("PurchDate_year", "PurchDate_month", "PurchDate_day", 
                  
                  "Auction", 
                  
                  "VehYear", "VehicleAge", "Color", 
                  "Transmission", "WheelType", 
                  
                  "Nationality", "Make", "Size", 
                  
                  "VNST", "PRIMEUNIT", "AUCGUART", "IsOnlineSale")

p_distr_discrete.l <- lapply(varnames_tab, df = train1, order_x = TRUE,
                             path_filename_print = NULL, FUN = doPrepExplore:::ggplot_bar_x)
names(p_distr_discrete.l) <- varnames_tab

p_distr_discrete.l[["Make"]] <- p_distr_discrete.l[["Make"]] + theme(axis.text.x = element_text(angle = 90))
p_distr_discrete.l[["VehYear"]] <- doPrepExplore:::ggplot_bar_x(train1, "VehYear", order_x = FALSE)

filename_out <- paste(path_plots, "unsupervised/", "discrete_var_bars", ".pdf", sep = "")
pdf(filename_out, width = 13, height = 8)
invisible(lapply(p_distr_discrete.l, FUN = print))
dev.off()

# end discrete ----------