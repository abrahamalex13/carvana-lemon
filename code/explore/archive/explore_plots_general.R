#explore_plots_general.R

#plots of interest: 
#(0) density of y, (a) y vs x, (b) distr of x train and distr of x test.


#overall y
p_y_overall <- 
  ggplot(train1) + 
    geom_bar(aes(factor(!!varname_y.sym)))

p_y_overall_pct <- 
  y_tab %>% 
  ggplot(.) + geom_col(aes(factor(!!varname_y.sym), share))







#FUNCTION DEFINITIONS ---------------------------------------

plot_x2_x1 <- function(varname_x1, varname_x2 = NULL, varname_grp = NULL, do_fwrap = NULL, data=train1) {
  
  varname_x1.sym <- rlang::sym(varname_x1)
  if (!is.null(varname_x2)) varname_x2.sym <- rlang::sym(varname_x2)
  if (!is.null(varname_grp)) varname_grp.sym <- rlang::sym(varname_grp)
  
  if (!is.null(varname_x2) & !is.null(varname_grp)) {
    
    p <- ggplot(data) + 
      geom_point(aes(!!varname_x1.sym, !!varname_x2.sym, color = factor(!!varname_grp.sym))) + 
      labs(title = paste(varname_x2, " vs ", varname_x1, sep=""))
    
  } else if (is.null(varname_x2) & !is.null(varname_grp)) {
    
    
    p <- ggplot(data) + 
      geom_density(aes(!!varname_x1.sym, color = factor(!!varname_grp.sym))) + 
      geom_rug(aes(!!varname_x1.sym)) + 
      labs(title = paste(varname_x1, " by ", varname_grp, sep=""))
    
    if (do_fwrap == TRUE) p <- p + facet_wrap(varname_grp, ncol = 2)
    
  }
  
  
  
  return(p)
  
}




#plotting x by y, must specify what kind of plot you want.
plot_x_by_y <- function(data=train1, data_ref_instr = plot_types_x_by_y, 
                        bar_pct = FALSE, varname_x, varname_y_plot) {
  
  
  #SETUP FOR NAME-BASED FUNCTIONALIZATION
  varname_x.sym <- rlang::sym(varname_x)
  varname_y_plot.sym <- rlang::sym(varname_y_plot)
  
  #extract plot instructions for particular varname_x
  #varname_x must exist in plotting instructions table,
  #or be a log-counterpart of a varname that exists in plot instruct table
  varname_x_i <- varname_x
  if (varname_x_i %in% ren_varnames_x_to_log) {
    instr <- data.frame("plot_type" = "density", "facet_wrap_is_best" = "FALSE")
  } else instr <- data_ref_instr %>% dplyr::filter(varname_x == varname_x_i)

  
  
  
  
  #plot according to plotting instructions
  p <- NULL
  
  if (instr$plot_type == "density") {
    
    if (instr$facet_wrap_is_best == "TRUE") {
      
      p <- ggplot(data) + 
        geom_density(aes(!!varname_x.sym)) + 
        facet_wrap(c(varname_y)) + 
        geom_rug(aes(!!varname_x.sym))
      
    } else if (instr$facet_wrap_is_best == "FALSE") {
      
      p <- ggplot(data) + 
        geom_density(aes(!!varname_x.sym, group = !!varname_y.sym, color = factor(!!varname_y.sym))) + 
        geom_rug(aes(!!varname_x.sym))
      
    }
    
  } else if (instr$plot_type == "bar") {
    
    if (bar_pct == FALSE) {
      p <- ggplot(data) + 
        geom_bar(aes(!!varname_x.sym, group = !!varname_y_plot.sym, fill = factor(!!varname_y_plot.sym)), 
                 position = "dodge")
    } else {
      
      #want to compute proportion with (x count) as denominator
      data_tab1 <- data %>% 
        group_by(!!varname_x.sym, !!varname_y_plot.sym) %>% 
        summarize(n = n())
      data_tab_denom <- data %>% 
          group_by(!!varname_x.sym) %>% 
          summarize(denom = n() )
      
      data_tab <- left_join(data_tab1, data_tab_denom) %>% 
        mutate(share = n / denom)
      
      p <- ggplot(data_tab) + 
        geom_col(aes(!!varname_x.sym, y = share, 
                     group = !!varname_y_plot.sym, 
                     fill = factor(!!varname_y_plot.sym)))      
      
    }
    
  }
  
  
  
  
  if (!is.null(p)) p <- p + labs(title = paste(varname_y_plot, " vs ", varname_x, sep=""))
  print(paste("Plot for ", varname_x_i, " complete.", sep=""))
  
  return(p)
  
}

#END DEFINITIONS -------------------------------------








#CALLS 

#x by y ---------------------

#sample sizes
plots_x_by_y.l <- lapply(c(varnames_x, varnames_price_ratios), function(x) {

                          varname_x_i <- x
                          p <- plot_x_by_y(varname_x = varname_x_i,
                                        varname_y_plot = varname_y)
                          p <- p + theme_grey(base_size = 16)
                            
                          return(p)

                        })
names(plots_x_by_y.l) <- c(varnames_x, varnames_price_ratios)

#manually rotate x-axis labels, where there are many
plots_x_by_y.l[["Make"]] <- plots_x_by_y.l[["Make"]] + 
  theme(axis.text.x = element_text(angle = 90))



#percentages
plots_x_by_y_bar_pct.l <- lapply(c(varnames_x, ren_varnames_x_to_log), function(x) {
  
  varname_x_i <- x
  p <- plot_x_by_y(varname_x = varname_x_i,
                   varname_y_plot = varname_y, bar_pct = TRUE)
  p <- p + theme_grey(base_size = 16)
  return(p)
  
})
names(plots_x_by_y_bar_pct.l) <- varnames_x

#manually rotate x-axis labels, where there are many
plots_x_by_y_bar_pct.l[["Make"]] <- plots_x_by_y_bar_pct.l[["Make"]] + 
  theme(axis.text.x = element_text(angle = 90))


names_plots_keep <- c("Auction", "VehYear", "VehicleAge", "Transmission", 
                      "WheelType", "VehOdo", "Size", "IsOnlineSale", "PurchDate_year",
                      "Make",
                      varnames_price_ratios)
plots_x_by_y_sub.l <- lapply(names_plots_keep, FUN = function(x) plots_x_by_y.l[[x]])
names(plots_x_by_y_sub.l) <- names_plots_keep
save(plots_x_by_y_sub.l, file = paste(path_data_processed, "plots_x_by_y.RData", sep=""))


plots_x_by_y_bar_pct_sub.l <- lapply(names_plots_keep, FUN = function(x) plots_x_by_y_bar_pct.l[[x]])
names(plots_x_by_y_bar_pct_sub.l) <- names_plots_keep
save(plots_x_by_y_bar_pct_sub.l, file = paste(path_data_processed, "plots_x_by_y_bar_pct.RData", sep=""))



#output
pdf(paste(path_plots, "plots_x_by_y.pdf", sep=""), width = 16, height = 8.5)
invisible(lapply(plots_x_by_y.l, FUN = function(x) if (!is.null(x)) print(x)))
dev.off()

pdf(paste(path_plots, "plots_x_by_y_bar_pct.pdf", sep=""), width = 18, height = 8.5)
invisible(lapply(plots_x_by_y_bar_pct.l, FUN = function(x) if (!is.null(x)) print(x)))
dev.off()

#end x by y ---------------------







#x2 v x1 --------------------

#overall
p_VehOdo_by_VehAge <- plot_x2_x1(varname_x1 = "VehOdo", varname_grp = "VehicleAge", do_fwrap = TRUE)
p_VehOdo_by_VehAge_y <- ggplot() + 
  geom_density(data = train1, aes(VehOdo, group = !!varname_y.sym, color = factor(!!varname_y.sym))) + 
  geom_rug(data = train1, aes(VehOdo)) + 
  facet_wrap(~VehicleAge, ncol = 2)

p_RatioPrice_VehAge <- ggplot(train1) + 
  geom_density(aes(RatioAuctionRetailAveragePrice, group = !!varname_y.sym, color = factor(!!varname_y.sym))) + 
  geom_rug(aes(RatioAuctionRetailAveragePrice, group = !!varname_y.sym, color = factor(!!varname_y.sym))) + 
  
  facet_wrap(~VehicleAge, ncol = 2, scales = "free_y")



save(p_VehOdo_by_VehAge, p_VehOdo_by_VehAge_y, p_RatioPrice_VehAge,
     file = paste(path_data_processed, "plots_x2_x1.RData", sep=""))



#subset to Honda Accords, examine price variables' behavior.
sub1 <- train1 %>% 
  dplyr::filter(Make == "HONDA" & Model_hmnz == "ACCORD")

p_Accords_Price_VehAge <- ggplot(sub1) + 
  geom_density(aes(MMRAcquisitionAuctionAveragePrice, color = "AuctionAverage")) + 
  geom_rug(aes(MMRAcquisitionAuctionAveragePrice, color = "AuctionAverage")) + 
  
  geom_density(aes(MMRAcquisitionRetailAveragePrice, color = "RetailAverage")) + 
  geom_rug(aes(MMRAcquisitionRetailAveragePrice, color = "RetailAverage")) + 
  
  facet_wrap(~VehicleAge, ncol = 2, scales = "free_y") + labs(x = "Price")

p_Accords_Price_Mileage <- ggplot(sub1) + 
  geom_point(aes(VehOdo, MMRAcquisitionAuctionAveragePrice, color = "AuctionAverage")) + 
  geom_point(aes(VehOdo, MMRAcquisitionRetailAveragePrice, color = "RetailAverage")) + 
  facet_wrap(~VehicleAge, ncol = 2) + labs(y = "Price")




save(p_Accords_Price_VehAge, p_Accords_Price_Mileage,
     file = paste(path_data_processed, "Accords_plots_Price_x1.RData", sep=""))


#end x2 v x1 --------------










#CASE ANALYSIS: for pt cruiser ----------

# plots_x_by_y_pt_cruiser.l <- lapply(c(varnames_x, ren_varnames_x_to_log), function(x) {
#   
#   varname_x_i <- x
#   p <- plot_x_by_y(data = left_join(train1, make_model_hmnz %>% dplyr::select(-n)) %>%  
#                      dplyr::filter(Make == "CHRYSLER" & 
#                                      Model_hmnz == "PT CRUISER" & VehYear == 2008 & Auction == "OTHER"),
#                    varname_x = varname_x_i,
#                    varname_y_plot = varname_y, bar_pct = FALSE)
#   return(p)
#   
# })
# names(plots_x_by_y_pt_cruiser.l) <- varnames_x
# 
# 
# plots_x_by_y_bar_pct_pt_cruiser.l <- lapply(c(varnames_x, ren_varnames_x_to_log), function(x) {
#   
#   varname_x_i <- x
#   p <- plot_x_by_y(data = left_join(train1, make_model_hmnz %>% dplyr::select(-n)) %>%  
#                             dplyr::filter(Make == "CHRYSLER" & 
#                                           Model_hmnz == "PT CRUISER" & VehYear == 2008 & Auction == "OTHER"),
#                    varname_x = varname_x_i,
#                    varname_y_plot = varname_y, bar_pct = TRUE)
#   return(p)
#   
# })
# names(plots_x_by_y_bar_pct_pt_cruiser.l) <- varnames_x


#end CASE ANALYSIS: for pt cruiser ----------

# pdf(paste(path_plots, "plots_x_by_y_pt_cruiser.pdf", sep=""), width = 18, height = 8.5)
# invisible(lapply(plots_x_by_y_pt_cruiser.l, FUN = function(x) if (!is.null(x)) print(x)))
# dev.off()
# 
# pdf(paste(path_plots, "plots_x_by_y_bar_pct_pt_cruiser.pdf", sep=""), width = 18, height = 8.5)
# invisible(lapply(plots_x_by_y_bar_pct_pt_cruiser.l, FUN = function(x) if (!is.null(x)) print(x)))
# dev.off()






#price alternatives scatter plots
# avg_auction_v_retail <- 
#   p_scatter("MMRAcquisitionRetailAveragePrice", "MMRAcquisitionAuctionAveragePrice", 
#             varname_grp = varname_y, data = train1)
