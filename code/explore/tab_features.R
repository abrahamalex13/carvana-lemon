#tab_features.R

#univariate -----------------------------

#frequency tabulations for discrete explanatory variables
varnames_tab <- c("PurchDate", "PurchDate_year", "PurchDate_month", "PurchDate_day", 
                  
                  "Auction", 
                  
                  "VehYear", "VehicleAge", 
                  "Make", "Model", "Trim", "SubModel", "Color", 
                  
                  "Transmission", "WheelTypeID", "WheelType", 
                  "Nationality", "Size", 
                  
                  "PRIMEUNIT", "AUCGUART", "BYRNO", "VNZIP1", "VNST", "IsOnlineSale")

freq_tab.l <- lapply(varnames_tab, df = train1, rename_generic = TRUE, FUN = doPrepExplore:::tab_var_discrete)
names(freq_tab.l) <- varnames_tab
freq_tab_missing <- bind_rows( lapply(freq_tab.l, function(x) x %>% dplyr::filter(is.na(value))) )

freq_tab_discrete.l <- freq_tab.l
freq_tab_discrete_missing <- freq_tab_missing


#for conti explanatory vars - how many values missing?
varnames_tab <- c("VehOdo", "VehBCost",
                  colnames(train1)[grepl("MMR", colnames(train1))], 
                  colnames(train1)[grepl("Ratio", colnames(train1))],
                  "WarrantyCost")

freq_tab_discrete.l <- lapply(varnames_tab, df = train1, rename_generic = TRUE, FUN = doPrepExplore:::tab_var_discrete)
names(freq_tab_discrete.l) <- varnames_tab
freq_tab_missing <- bind_rows( lapply(freq_tab_discrete.l, 
                                      function(x) x %>% dplyr::filter(is.na(value) | is.nan(value))) )
freq_tab_conti_missing <- freq_tab_missing

# VIM::matrixplot(train1[, c("MMRAcquisitionAuctionAveragePrice_log", "MMRAcquisitionRetailAveragePrice_log")],
#                 sortby = c('MMRAcquisitionAuctionAveragePrice_log'))


#end univariate ------------- 