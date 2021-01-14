#plot_features_distr_by_outcome.R

value_avg_baseline <- 
  nrow(train1_keep %>% dplyr::filter(IsBadBuy == 1)) / nrow(train1_keep)
  
train1_keep <- train1_keep %>% 
  mutate(Lemon = ifelse(IsBadBuy == 1, "Yes", "No"))
  

#conti vars ---------------------

varnames_plot <- colnames(train1)[grepl("MMR", colnames(train1))]
varnames_plot <- c(varnames_plot, "VehBCost",
                   "RatioCostAuctionAverage", "RatioCostRetailAverage",
                   "RatioCostAuctionClean", "RatioCostRetailClean",
                   "RatioWarrantyVehBCost", "VehOdo")
p_distr_conti_by_y.l <-
  lapply(varnames_plot, df = train1_keep, group = "Lemon", color = "Lemon", 
         path_filename_print = NULL, FUN = doPrepExplore:::ggplot_density_x)
names(p_distr_conti_by_y.l) <- varnames_plot

filename_out <- paste(path_plots, "supervised/", "price_quantities_by_y", ".pdf", sep = "")
pdf(filename_out, width = 13, height = 8)
invisible(lapply(p_distr_conti_by_y.l, FUN = print))
dev.off()

#end conti vars -------------




#discrete vars --------

varnames_tab <- c("PurchDate_year", "PurchDate_month", "PurchDate_day", 
                  
                  "Auction", 
                  
                  "VehYear", "VehicleAge", "Color", 
                  "Transmission", "WheelType", 
                  
                  "Nationality", "Make", "Size", 
                  
                  "VNST", "PRIMEUNIT", "AUCGUART", "IsOnlineSale")

#share
p_distr_discrete_by_y.l <- 
  lapply(varnames_tab, df = train1_keep, group = "Lemon", fill = "Lemon",
         units_share = TRUE, order_x = TRUE, 
         path_filename_print = NULL, FUN = doPrepExplore:::ggplot_bar_x)
names(p_distr_discrete_by_y.l) <- varnames_tab

p_distr_discrete_by_y.l[["Make"]] <- 
  p_distr_discrete_by_y.l[["Make"]] + 
    theme(axis.text.x = element_text(angle = 90))
p_distr_discrete_by_y.l[["VehYear"]] <- 
  doPrepExplore:::ggplot_bar_x(df = train1_keep, varname_x = "VehYear", 
                               group = "Lemon", fill = "Lemon", 
                               order_x = FALSE, units_share = TRUE)


p_distr_discrete_by_y.l <- 
  lapply(p_distr_discrete_by_y.l, 
         function(x) x + geom_hline(yintercept = value_avg_baseline, linetype = 2, color = 'red'))


filename_out <- paste(path_plots, "supervised/", "discrete_var_bars_share_by_y", ".pdf", sep = "")
pdf(filename_out, width = 13, height = 8)
invisible(lapply(p_distr_discrete_by_y.l, FUN = print))
dev.off()

# end discrete ----------