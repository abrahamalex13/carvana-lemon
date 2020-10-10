#explore_tabs_cases

tab_pt <- tab_vars(
  df = left_join(train1, make_model_hmnz %>% dplyr::select(-n)) %>% 
          dplyr::filter(Model_hmnz == "PT CRUISER" & VehYear == 2008), 
  varnames = c(varname_y, "VNZIP1", "VNST") )
