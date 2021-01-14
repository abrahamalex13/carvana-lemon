derive_indic_varnames_rare_operator <- function(df, varnames_cat, thresh_frac_obs, directory_operator) {
  
  varnames_levels_drop <- unlist(
    lapply(varnames_cat, df = df, thresh_frac_obs = thresh_frac_obs, 
           FUN = function(x, df, thresh_frac_obs) {
             
             levels_drop <- 
               doPrepExplore::get_categories_rare(df = df, varname_cat = x, thresh_frac_obs = thresh_frac_obs)
             varnames_levels_drop <- paste(x, levels_drop, sep = "")
             varnames_levels_drop
             
             })
  )
  
  saveRDS(varnames_levels_drop, 
          file = paste(directory_operator, "varnames_rare_drop.rds", sep = ""))
  
  varnames_levels_drop
  
}