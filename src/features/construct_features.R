#construct_features.R


#individual month, day, year components of date may be informative.
df1 <- doPrepExplore:::construct_vars_mdy(df1, "PurchDate")


varnames_x_to_log <- c(
  
  "VehOdo", "MMRAcquisitionAuctionAveragePrice",
  "MMRAcquisitionAuctionCleanPrice", "MMRAcquisitionRetailAveragePrice",
  "MMRAcquisitionRetailCleanPrice", "MMRCurrentAuctionAveragePrice",
  "MMRCurrentAuctionCleanPrice", "MMRCurrentRetailAveragePrice", 
  "MMRCurrentRetailCleanPrice", "VehBCost", "WarrantyCost"
  
)
df1 <- df1 %>% 
  mutate_at(varnames_x_to_log, list("log" = log))




#price ratios ----

#on average - cost of purchase at auction vs retail
#sale-specific - (sale cost vs typical auction/retail)
df1 <- df1 %>% 
  mutate(MMRRatioAuctionRetailAveragePrice = MMRAcquisitionAuctionAveragePrice / MMRAcquisitionRetailAveragePrice,
         MMRRatioAuctionRetailCleanPrice = MMRAcquisitionAuctionCleanPrice / MMRAcquisitionRetailCleanPrice,
         
         RatioCostAuctionAverage = VehBCost / MMRAcquisitionAuctionAveragePrice,
         RatioCostRetailAverage = VehBCost / MMRAcquisitionRetailAveragePrice,
         
         RatioCostAuctionClean = VehBCost / MMRAcquisitionAuctionCleanPrice,
         RatioCostRetailClean = VehBCost / MMRAcquisitionRetailCleanPrice,
         
         RatioWarrantyVehBCost = WarrantyCost / VehBCost)
        
# ----------
















# categorical feature treatment -----------


#workflow-specific function defns -----

do_consolidations <- 
  function(df, varname_cat, thresh_nobs_consol, thresh_nobs_indic_other, 
           value_consol = "OTHER") {
    
    #construct consolidated var
    df <- 
      doPrepExplore:::consolidate_sparse_other_cat(df, varname_cat, threshold = thresh_nobs_consol / nrow(df), 
                                                   value_consol = value_consol, construct_indic_other = FALSE)
    
    #construct strictly an indicator of 'other'
    df <- 
      doPrepExplore:::consolidate_sparse_other_cat(df, varname_cat, threshold = thresh_nobs_indic_other / nrow(df), 
                                                   value_consol = value_consol, construct_indic_other = TRUE)   
    
    return(df)
    
  }

thresh_nobs_consol <- 30
thresh_nobs_indic_other <- 100


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

#end workflow-specific fun defns ----





#vehicle characteristics ------


train1 <- 
  do_consolidations(df = train1, varname_cat = "Make", 
                    thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, 
                    value_consol = "OTHER")
train1 <- 
  do_consolidations(df = train1, varname_cat = "VehYear", 
                    thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, 
                    value_consol = 1)


#engine
train1 <- construct_engine_type(train1)
train1 <- construct_engine_volume(train1)
train1 <- doPrepExplore:::consolidate_sparse_other_cat(
  train1, "engine_vol", threshold = thresh_nobs_indic_other / nrow(train1), 
  value_consol = "OTHER", construct_indic_other = FALSE)   

train1 <- 
  doPrepExplore:::construct_interact_char(train1, c("Make", "engine_type", "engine_vol"))
train1 <- doPrepExplore:::consolidate_sparse_other_cat(
  train1, "Make_engine_type_engine_vol", threshold = thresh_nobs_indic_other / nrow(train1), 
  value_consol = "OTHER", construct_indic_other = FALSE)   




#Make/Model/SubModel/Trim
varnames_main_fx_Make <- c("Model", "VehYear")
invisible(lapply(varnames_main_fx_Make, varname_pre = "Make", function(x, varname_pre) {
  
  varname_iact <- paste(varname_pre, x, sep = "_")
  train1 <<- doPrepExplore:::construct_interact_char(train1, c(varname_pre, x))
  train1 <<- 
    do_consolidations(df = train1, varname_cat = varname_iact, 
                      thresh_nobs_consol = thresh_nobs_consol, 
                      thresh_nobs_indic_other = thresh_nobs_indic_other, 
                      value_consol = "OTHER")    
  
  return(NULL)
  
}))


varnames_2fx_Make_Model <- c("SubModel", 
                             #"Trim", 
                             "VehYear")
invisible(lapply(varnames_2fx_Make_Model, varname_pre = "Make_Model", function(x, varname_pre) {
  
  varname_iact <- paste(varname_pre, x, sep = "_")
  train1 <<- doPrepExplore:::construct_interact_char(train1, c(varname_pre, x))
  train1 <<- 
    do_consolidations(df = train1, varname_cat = varname_iact, 
                      thresh_nobs_consol = thresh_nobs_consol, 
                      thresh_nobs_indic_other = thresh_nobs_indic_other, 
                      value_consol = "OTHER")        
    
  return(NULL)
  
}))


varnames_3fx_Make_Model_SubModel <- c(#"Trim", 
                                      "VehYear")
invisible(lapply(varnames_3fx_Make_Model_SubModel, varname_pre = "Make_Model_SubModel", function(x, varname_pre) {
  
  varname_iact <- paste(varname_pre, x, sep = "_")
  train1 <<- doPrepExplore:::construct_interact_char(train1, c(varname_pre, x))
  train1 <<- 
    do_consolidations(df = train1, varname_cat = varname_iact, 
                      thresh_nobs_consol = thresh_nobs_consol, 
                      thresh_nobs_indic_other = thresh_nobs_indic_other, 
                      value_consol = "OTHER")    
  
  return(NULL)
  
}))

train1 <- doPrepExplore:::construct_interact_char(train1, c("Make_Model_SubModel", 
                                                            #"Trim", 
                                                            "VehYear"))
# train1 <- 
#   do_consolidations(df = train1, varname_cat = "Make_Model_SubModel_Trim_VehYear", 
#                     thresh_nobs_consol = thresh_nobs_consol*.75, 
#                     thresh_nobs_indic_other = thresh_nobs_indic_other, 
#                     value_consol = "OTHER")    
  


train1 <-
  do_consolidations(df = train1, varname_cat = "Size", 
                    thresh_nobs_consol = thresh_nobs_consol, 
                    thresh_nobs_indic_other = thresh_nobs_indic_other, 
                    value_consol = "OTHER")    
  
train1 <- 
  do_consolidations(df = train1, varname_cat = "Color", 
                    thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, 
                    value_consol = "OTHER")    
  
#end vehicle char ---------



#transaction
train1 <- 
  do_consolidations(df = train1, varname_cat = "VNST", 
                    thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, 
                    value_consol = "OTHER")    

train1 <- 
  do_consolidations(df = train1, varname_cat = "VNZIP1", 
                    thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, 
                    value_consol = "OTHER")    
  
train1 <- 
  do_consolidations(df = train1, varname_cat = "BYRNO", 
                    thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, 
                    value_consol = "OTHER")    











# test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Make", threshold = thresh_nobs / nrow(test1))
# test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Model_agg", threshold = thresh_nobs / nrow(test1))
# test1 <- test1 %>% 
#   mutate(Make_Model_agg_consol = case_when(
#     
#     Model_agg_consol == "OTHER" ~ Model_agg_consol,
#     TRUE ~ paste(Make, "_", Model_agg_consol, sep = "")
#     
#   ))
# test1 <- test1 %>% 
#   mutate(Make_Model_agg_Year = paste(Make_Model_agg_consol, "_", VehYear, sep = ""))
# test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Make_Model_agg_Year", threshold = thresh_nobs / nrow(test1))
# 
# test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "VehYear", threshold = thresh_nobs / nrow(test1), value_consol = 1)


#end cat feature dim reduction --------------

