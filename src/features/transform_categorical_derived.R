#transform_categorical_derived.R

transform_categorical_derived <- 
  function(df, run_type, thresh_nobs_consol, thresh_nobs_indic_other, directory_operators) {

  
#numeric feature requires non-default value_consol argument
if (run_type == "train") {
  
  op_VehYear <- 
    doPrepExplore::derive_consolidate_categories_operator(
      df = df, varname_cat = "VehYear", 
      thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, value_consol = 1,
      directory_operator = directory_operators)
  
} else op_VehYear <- list("path" = doPrepExplore:::mk_filename_op_consol(directory_operators, "VehYear"))

df <- doPrepExplore::join_consolidate_categories_operator(
  filename_operator = op_VehYear[["path"]], df = df, 
  varname_cat = "VehYear", value_consol = 1)
  


#uninterested in 0/1 'other' indic for engine.
varnames_engine <- c("engine_type", "engine_vol", 
                     "engine_type_engine_vol", "Make_engine_type_engine_vol")
for (var in varnames_engine) {
  
  if (run_type == "train") {
  
    op <- 
      doPrepExplore::derive_consolidate_categories_operator(
        df = df, varname_cat = var, 
        thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = NULL, 
        value_consol = "OTHER", directory_operator = directory_operators)
    
  } else {
    
    op <- list(
      "path" = doPrepExplore:::mk_filename_op_consol(directory_operators, var)
    )
    
  }
  
  df <- doPrepExplore::join_consolidate_categories_operator(
    filename_operator = op[["path"]], df = df, 
    varname_cat = var, value_consol = "OTHER")  
  
}



#shared workflow structure for remaining vars
varnames_Make_details <- 
  c("Make", "Make_Model", "Make_VehYear", 
    "Make_Model_SubModel", "Make_Model_VehYear", 
    "Make_Model_SubModel_VehYear")
varnames_tran <- c("VNST", "VNZIP1", "BYRNO", 
                   "PurchDate_month_PurchDate_wday")
varnames_misc <- c("Color", "Size")

for (var in c(varnames_Make_details, varnames_tran, varnames_misc)) {
  
  if (run_type == "train") {
    
    op <- 
      doPrepExplore::derive_consolidate_categories_operator(
        df = df, varname_cat = var, 
        thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, 
        value_consol = "OTHER", directory_operator = directory_operators)
    
  } else {
    
    op <- list(
      "path" = doPrepExplore:::mk_filename_op_consol(directory_operators, var)
    )
    
  }
  
  df <- doPrepExplore::join_consolidate_categories_operator(
    filename_operator = op[["path"]], df = df, 
    varname_cat = var, value_consol = "OTHER")  
  
}



df

}