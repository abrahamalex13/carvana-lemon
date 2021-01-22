#plot_functions.R

ggplot_bar_prop_simple <- function(df, varname_x, fill = "Lemon", order_x = TRUE) {
  
  if (order_x) df <- doPrepExplore:::declare_factor_order_freq(df, varname_x)
  
  ggplot(df) + 
    geom_bar(aes(!!rlang::sym(varname_x), group = !!rlang::sym(fill), fill = !!rlang::sym(fill)),
             position = "fill") + 
    scale_y_continuous(labels = scales::percent)
  
}
