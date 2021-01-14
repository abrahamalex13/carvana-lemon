#factor_interactions.R
vars_interact <- 
  purrr::map_dfc(varnames_pairs_interact, df = df, .f = doPrepExplore::construct_factor_interactions)