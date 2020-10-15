#construct_features.R
#calls to construct_features_FN functions.


#individual month, day, year components of date may be informative.
train1 <- doPrepExplore:::construct_vars_mdy(train1, "PurchDate")
test1 <- doPrepExplore:::construct_vars_mdy(test1, "PurchDate")



varnames_x_to_log <- c(
  
  "VehOdo", "MMRAcquisitionAuctionAveragePrice",
  "MMRAcquisitionAuctionCleanPrice", "MMRAcquisitionRetailAveragePrice",
  "MMRAcquisitionRetailCleanPrice", "MMRCurrentAuctionAveragePrice",
  "MMRCurrentAuctionCleanPrice", "MMRCurrentRetailAveragePrice", 
  "MMRCurrentRetailCleanPrice", "VehBCost", "WarrantyCost"
  
)

train1 <- train1 %>% 
  mutate_at(varnames_x_to_log, list("log" = log))
test1 <- test1 %>% 
  mutate_at(varnames_x_to_log, list("log" = log))




#price ratios -----------

#on average - cost of purchase at auction vs retail
#sale-specific - (sale cost vs typical auction/retail)
train1 <- train1 %>% 
  mutate(MMRRatioAuctionRetailAveragePrice = MMRAcquisitionAuctionAveragePrice / MMRAcquisitionRetailAveragePrice,
         MMRRatioAuctionRetailCleanPrice = MMRAcquisitionAuctionCleanPrice / MMRAcquisitionRetailCleanPrice,
         
         RatioCostAuctionAverage = VehBCost / MMRAcquisitionAuctionAveragePrice,
         RatioCostRetailAverage = VehBCost / MMRAcquisitionRetailAveragePrice,
         
         RatioCostAuctionClean = VehBCost / MMRAcquisitionAuctionCleanPrice,
         RatioCostRetailClean = VehBCost / MMRAcquisitionRetailCleanPrice,
         
         RatioWarrantyVehBCost = WarrantyCost / VehBCost)
         

test1 <- test1 %>% 
  mutate(MMRRatioAuctionRetailAveragePrice = MMRAcquisitionAuctionAveragePrice / MMRAcquisitionRetailAveragePrice,
         MMRRatioAuctionRetailCleanPrice = MMRAcquisitionAuctionCleanPrice / MMRAcquisitionRetailCleanPrice,
         
         RatioWarrantyVehBCost = WarrantyCost / VehBCost)

# ----------------




# categorical feature dimension reduction -----------

thresh_nobs <- 50


#vehicle characteristics ---------

train1 <- doPrepExplore:::consolidate_sparse_other_cat(train1, "Make", threshold = thresh_nobs / nrow(train1))
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "VehYear", threshold = thresh_nobs / nrow(train1), value_consol = 1)


#Make/Model/SubModel/Trim
varnames_main_fx_Make <- c("Model", "VehYear")
invisible(lapply(varnames_main_fx_Make, varname_pre = "Make", function(x, varname_pre) {
  
  varname_iact <- paste(varname_pre, x, sep = "_")
  train1 <<- doPrepExplore:::construct_interact_char(train1, c(varname_pre, x))
  train1 <<- 
    doPrepExplore:::consolidate_sparse_other_cat(train1, varname_iact, threshold = thresh_nobs / nrow(train1))
  
  return(NULL)
  
}))
train1 <- doPrepExplore:::construct_interact_char(train1, c("Make_Model", "SubModel"))
train1 <- doPrepExplore:::consolidate_sparse_other_cat(train1, "Make_Model_SubModel", threshold = thresh_nobs / nrow(train1))



varnames_2fx_Make_Model <- c("SubModel", "Trim", "VehYear")
invisible(lapply(varnames_2fx_Make_Model, varname_pre = "Make_Model", function(x, varname_pre) {
  
  varname_iact <- paste(varname_pre, x, sep = "_")
  train1 <<- doPrepExplore:::construct_interact_char(train1, c(varname_pre, x))
  train1 <<- 
    doPrepExplore:::consolidate_sparse_other_cat(train1, varname_iact, threshold = thresh_nobs / nrow(train1))
  
  return(NULL)
  
}))


varnames_3fx_Make_Model_SubModel <- c("Trim", "VehYear")
invisible(lapply(varnames_3fx_Make_Model_SubModel, varname_pre = "Make_Model_SubModel", function(x, varname_pre) {
  
  varname_iact <- paste(varname_pre, x, sep = "_")
  train1 <<- doPrepExplore:::construct_interact_char(train1, c(varname_pre, x))
  train1 <<- 
    doPrepExplore:::consolidate_sparse_other_cat(train1, varname_iact, threshold = thresh_nobs / nrow(train1))
  
  return(NULL)
  
}))

train1 <- doPrepExplore:::construct_interact_char(train1, c("Make_Model_SubModel", "Trim", "VehYear"))
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "Make_Model_SubModel_Trim_VehYear", threshold = thresh_nobs / nrow(train1))





train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "Size", threshold = thresh_nobs / nrow(train1))

train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "Color", threshold = thresh_nobs / nrow(train1))


#end vehicle char ------------





#transaction
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "VNST", threshold = thresh_nobs / nrow(train1))
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "VNZIP1", threshold = thresh_nobs / nrow(train1))
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "BYRNO", threshold = thresh_nobs / nrow(train1), value_consol = "OTHER")







test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Make", threshold = thresh_nobs / nrow(test1))
test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Model_agg", threshold = thresh_nobs / nrow(test1))
test1 <- test1 %>% 
  mutate(Make_Model_agg_consol = case_when(
    
    Model_agg_consol == "OTHER" ~ Model_agg_consol,
    TRUE ~ paste(Make, "_", Model_agg_consol, sep = "")
    
  ))
test1 <- test1 %>% 
  mutate(Make_Model_agg_Year = paste(Make_Model_agg_consol, "_", VehYear, sep = ""))
test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Make_Model_agg_Year", threshold = thresh_nobs / nrow(test1))

test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "VehYear", threshold = thresh_nobs / nrow(test1), value_consol = 1)


#end cat feature dim reduction -----