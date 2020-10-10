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




# categorical feature dimension reduction ----

#vehicle
train1 <- doPrepExplore:::consolidate_sparse_other_cat(train1, "Make", threshold = 100 / nrow(train1))
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "VehYear", threshold = 100 / nrow(train1), value_consol = 1)

train1 <- train1 %>% 
  mutate(Make_Model_agg = paste(Make, "_", Model_agg, sep = ""))
train1 <- train1 %>% 
  mutate(Make_Model_agg_Trim = paste(Make_Model_agg, "_", Trim, sep = ""),
         Make_Model_agg_Year = paste(Make_Model_agg, "_", VehYear, sep = ""),
         Make_Model_agg_Trim_Year = paste(Make_Model_agg, "_", Trim, "_", VehYear, sep = ""))

train1 <- doPrepExplore:::consolidate_sparse_other_cat(train1, "Make_Model_agg", threshold = 100 / nrow(train1))
train1 <- doPrepExplore:::consolidate_sparse_other_cat(train1, "Make_Model_agg_Trim", threshold = 100 / nrow(train1))
train1 <- doPrepExplore:::consolidate_sparse_other_cat(train1, "Make_Model_agg_Year", threshold = 100 / nrow(train1))
train1 <- doPrepExplore:::consolidate_sparse_other_cat(train1, "Make_Model_agg_Trim_Year", threshold = 100 / nrow(train1))

train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "Size", threshold = 100 / nrow(train1))

train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "Color", threshold = 100 / nrow(train1))



#transaction
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "VNST", threshold = 100 / nrow(train1))
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "VNZIP1", threshold = 100 / nrow(train1))
train1 <- 
  doPrepExplore:::consolidate_sparse_other_cat(train1, "BYRNO", threshold = 100 / nrow(train1), value_consol = 1)






test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Make", threshold = 100 / nrow(test1))
test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Model_agg", threshold = 100 / nrow(test1))
test1 <- test1 %>% 
  mutate(Make_Model_agg_consol = case_when(
    
    Model_agg_consol == "OTHER" ~ Model_agg_consol,
    TRUE ~ paste(Make, "_", Model_agg_consol, sep = "")
    
  ))
test1 <- test1 %>% 
  mutate(Make_Model_agg_Year = paste(Make_Model_agg_consol, "_", VehYear, sep = ""))
test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "Make_Model_agg_Year", threshold = 200 / nrow(test1))

test1 <- doPrepExplore:::consolidate_sparse_other_cat(test1, "VehYear", threshold = 100 / nrow(test1), value_consol = 1)


#end cat feature dim reduction -----