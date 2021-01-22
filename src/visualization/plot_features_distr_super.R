#plot_features_distr_super.R


#re-code supervisor for clearer presentation.
value_avg_baseline <- 
  nrow(df %>% dplyr::filter(IsBadBuy == 1)) / nrow(df)
df <- df %>% 
  mutate(Lemon = ifelse(IsBadBuy == 1, "Yes", "No"))
  

#conti vars ---------------------

varnames_plot <- colnames(df)[grepl("MMR", colnames(df))]
varnames_plot <- c(varnames_plot, "VehBCost",
                   "RatioCostAuctionAverage", "RatioCostRetailAverage",
                   "RatioCostAuctionClean", "RatioCostRetailClean",
                   "RatioWarrantyVehBCost", "VehOdo")
p_distr_conti_by_y.l <-
  lapply(varnames_plot, df = df, group = "Lemon", color = "Lemon", FUN = function(x, df, group, color) {
           
           ggplot(df) + 
             geom_density(aes(x = !!rlang::sym(x), 
                              group = !!rlang::sym(group),
                              color = !!rlang::sym(color)))

})
names(p_distr_conti_by_y.l) <- varnames_plot

invisible(
  doPrepExplore:::save_plot_fmt_pdf(p_distr_conti_by_y.l, "reports/figures/supervised/price_quantities_by_y.pdf")
)

#end conti vars -------------



#discrete vars --------
source("src/visualization/plot_functions.R")

varnames_tab <- c(#"PurchDate_year", 
                  "PurchDate_month", "PurchDate_day", 
                  
                  "Auction", 
                  
                  "VehYear", "VehicleAge", "Color", 
                  "Transmission", "WheelType", 
                  
                  "Nationality", "Make", "Size", 
                  
                  "VNST", "PRIMEUNIT", "AUCGUART", "IsOnlineSale")

#share
p_distr_discrete_by_y.l <- 
  lapply(varnames_tab, df = df, fill = "Lemon", order_x = TRUE, 
         FUN = ggplot_bar_prop_simple)
names(p_distr_discrete_by_y.l) <- varnames_tab

p_distr_discrete_by_y.l[["Make"]] <- 
  p_distr_discrete_by_y.l[["Make"]] + 
    theme(axis.text.x = element_text(angle = 90))
p_distr_discrete_by_y.l[["VehYear"]] <- ggplot_bar_prop_simple(df = df, varname_x = "VehYear", fill = "Lemon", order_x = FALSE)

#all plots need overall average prb baseline
p_distr_discrete_by_y.l <- 
  lapply(p_distr_discrete_by_y.l, 
         function(x) x + geom_hline(yintercept = value_avg_baseline, linetype = 2, color = 'red'))

invisible(
  doPrepExplore:::save_plot_fmt_pdf(p_distr_discrete_by_y.l, "reports/figures/supervised/discrete_var_bars_share_by_y.pdf")
)

# end discrete ----------