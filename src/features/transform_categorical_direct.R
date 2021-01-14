#transforms_categorical_direct.R

#workflow-specific function defns -----

construct_engine_type <- function(df) {
  
  df <- df %>% 
    mutate(engine_type = case_when(
      
      grepl("V6", Model) ~ "V6",
      grepl("V8", Model) ~ "V8",
      grepl("6C", Model) ~ "6C",
      grepl("4C", Model) ~ "4C",
      TRUE ~ "NA"
      
    ))
  
  return(df)
}
construct_engine_volume <- function(df) {
  
  engine_vol_model <- str_extract(df[["Model"]], "[\\d].[\\d]L")
  engine_vol_submodel <- str_extract(df[["SubModel"]], "[\\d].[\\d]L")
  
  which_na <- which(is.na(engine_vol_submodel))
  engine_vol_submodel[which_na] <- engine_vol_model[which_na]
  
  df[["engine_vol"]] <- engine_vol_submodel
  df[["engine_vol"]][which(is.na(df[["engine_vol"]]))] <- "NA"
  
  return(df)
  
}

#looping over variable names with shared structure, only new var needs return
extract_var_interact <- function(df, varnames_interact) {
  
  varname_new <- paste(varnames_interact, collapse = "_")
  df <- doPrepExplore::construct_interact_char(df, varnames_interact)
  df[, varname_new, drop = FALSE]
  
}

#end workflow-specific fun defns ----




transform_categorical_direct <- function(df) {

  #for seasonal patterns
  df <- df %>% 
    mutate(across(.cols = "PurchDate",
                  .fns = list(month = lubridate::month, 
                              wday = lubridate::wday),
                  label = TRUE,
                  .names = "{.col}_{.fn}"))
  
  df <- df %>% 
    mutate(PurchDate_day = lubridate::day(PurchDate))
  df <- doPrepExplore::construct_interact_char(df, c("PurchDate_month", "PurchDate_wday"))
  
  #general time trend
  df <- df %>% 
    mutate(PurchDate_day_int = as.integer(PurchDate))
  
  
  
  #engine
  df <- construct_engine_type(df)
  df <- construct_engine_volume(df)
  df <- doPrepExplore::construct_interact_char(df, c("engine_type", "engine_vol"))
  df <- doPrepExplore::construct_interact_char(df, c("Make", "engine_type", "engine_vol"))
  
  
  #Make/Model/Submodel/Trim
  varnames_main_fx_Make <- c("Model", "VehYear")
  varnames_main_fx_Make <- lapply(varnames_main_fx_Make, function(x) c("Make", x))
  vars_interact <- purrr::map_dfc(varnames_main_fx_Make, df = df, .f = extract_var_interact)
  df <- bind_cols(df, vars_interact)
  
  
  
  varnames_2fx_Make_Model <- c("SubModel" 
                               #, "Trim" 
                               , "VehYear")
  varnames_2fx_Make_Model <- lapply(varnames_2fx_Make_Model, function(x) c("Make_Model", x))
  vars_interact <- purrr::map_dfc(varnames_2fx_Make_Model, df = df, .f = extract_var_interact)
  df <- bind_cols(df, vars_interact)
  
  
  varnames_3fx_Make_Model_SubModel <- c("VehYear")
  varnames_3fx_Make_Model_SubModel <- 
    lapply(varnames_3fx_Make_Model_SubModel, function(x) c("Make_Model_SubModel", x))
  vars_interact <- purrr::map_dfc(varnames_3fx_Make_Model_SubModel, df = df, .f = extract_var_interact)
  df <- bind_cols(df, vars_interact)
  
  return(df)

}