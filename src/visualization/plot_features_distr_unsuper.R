#don't peek at outcomes! prepare inputs independently.
source("src/visualization/plot_functions.R")


#conti vars
varnames_plot <- colnames(df)[grepl("MMR", colnames(df))]
varnames_plot <- c(varnames_plot, "VehBCost", "RatioWarrantyVehBCost")
p_distr_conti.l <- lapply(varnames_plot, df = df, filename_print = NULL, FUN = doPrepExplore:::ggplot_density_simple)
names(p_distr_conti.l) <- varnames_plot
invisible(doPrepExplore:::save_plot_fmt_pdf(
  p_distr_conti.l, filename_print = "reports/figures/unsupervised/price_quantities_kde.pdf"
  ))



#discrete vars --------

varnames_tab <- c(#"PurchDate_year", 
                  "PurchDate_month", "PurchDate_day", 
                  
                  "Auction", 
                  
                  "VehYear", "VehicleAge", "Color", 
                  "Transmission", "WheelType", 
                  
                  #"Nationality", 
                  "Make", "Size", 
                  
                  "VNST", "PRIMEUNIT", "AUCGUART", "IsOnlineSale")

p_distr_discrete.l <- lapply(varnames_tab, df = df, order_x = TRUE, filename_print = NULL, FUN = doPrepExplore:::ggplot_bar_simple)
names(p_distr_discrete.l) <- varnames_tab

p_distr_discrete.l[["Make"]] <- p_distr_discrete.l[["Make"]] + theme(axis.text.x = element_text(angle = 90))
p_distr_discrete.l[["VehYear"]] <- doPrepExplore:::ggplot_bar_simple(df, "VehYear", order_x = FALSE)

invisible(doPrepExplore:::save_plot_fmt_pdf(
  p_distr_discrete.l, filename_print = "reports/figures/unsupervised/discrete_var_bars.pdf"
))

# end discrete ----------