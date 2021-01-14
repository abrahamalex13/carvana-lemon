#transforms_conti_derived.R


#b-spline basis -------------

filename_operator <- "data/processed/bspline_operators.rds"

if (run_type == "train") {
  
  bs.l <- lapply(varnames_bs, bs_df = bs_df, Xdf = X_wide, FUN = function(x, bs_df, Xdf) {
    
    if (grepl(":", x)) {
    
      comp <- doPrepExplore:::get_interact_components(varname_interact = x, df = Xdf)
      
      doPrepExplore::bs_interaction_subset(
        indic_interaction = comp[["indic_interaction"]], 
        x = comp[["interaction"]], 
        df = bs_df
        )
      
    } else splines::bs(Xdf[[x]], df = bs_df)
    
  })
  names(bs.l) <- varnames_bs
  
  saveRDS(bs.l, filename_operator)
  
} 



bs.l <- readRDS(filename_operator)

values_bs <- purrr::map_dfc(intersect(names(bs.l), colnames(X_wide)), 
                            bs.l = bs.l, Xdf = X_wide, function(x, bs.l, Xdf) {
  
  if (grepl(":", x)) {
    
    comp <- doPrepExplore:::get_interact_components(varname_interact = x, df = Xdf)
    
    out <- predict_bs_interaction_subset(
        object = bs.l[[x]], 
        indic_interaction = comp[["indic_interaction"]],
        newx = comp[["interaction"]]
        )
    
    
  } else out <- as.data.frame( splines:::predict.bs(bs.l[[x]], newx = Xdf[[x]]) )

  colnames(out) <- paste(x, "_bs", 1:ncol(out), sep = "")
  
  out

})

#end b-spline basis --------