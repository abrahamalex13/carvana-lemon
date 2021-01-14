#transform_conti_direct.R

transform_conti_direct <- function(df, varnames_to_log) {

  df <- df %>% 
    mutate_at(varnames_to_log, list("log" = log))
  
  #price ratios
  #on average - cost of purchase at auction vs retail
  #sale-specific - (sale cost vs typical auction/retail)
  df <- df %>% 
    mutate(MMRRatioAuctionRetailAveragePrice = MMRAcquisitionAuctionAveragePrice / MMRAcquisitionRetailAveragePrice,
           MMRRatioAuctionRetailCleanPrice = MMRAcquisitionAuctionCleanPrice / MMRAcquisitionRetailCleanPrice,
           
           RatioCostAuctionAverage = VehBCost / MMRAcquisitionAuctionAveragePrice,
           # RatioCostRetailAverage = VehBCost / MMRAcquisitionRetailAveragePrice,
           # SpreadRatioARAverage = RatioCostAuctionAverage - RatioCostRetailAverage,
           
           RatioCostAuctionClean = VehBCost / MMRAcquisitionAuctionCleanPrice,
           # RatioCostRetailClean = VehBCost / MMRAcquisitionRetailCleanPrice,
           # SpreadRatioARClean = RatioCostAuctionClean - RatioCostRetailClean,
           
           SpreadRatioCostAuctionAC = RatioCostAuctionAverage - RatioCostAuctionClean,
           
           RatioWarrantyVehBCost = WarrantyCost / VehBCost)
  
  df 
  
}