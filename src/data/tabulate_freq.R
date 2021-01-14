#tabulate_freq.R

if (run_type == "train") {

#frequency tabulations most natural for discrete vars
varnames_tab <- c("PurchDate", "PurchDate_day", #"PurchDate_year", "PurchDate_month", "PurchDate_day", 
                  
                  "Auction", 
                  
                  "VehYear", "VehicleAge", 
                  "Make", "Model", "Trim", "SubModel", "Color", 
                  
                  #"Transmission", 
                  "WheelTypeID", "WheelType", 
                  "Nationality", "Size", 
                  
                  "PRIMEUNIT", "AUCGUART",
                  "BYRNO", "VNZIP1", "VNST"
                  #"IsOnlineSale"
                  
                  , "engine_type_engine_vol"
                  )

freq_tab.l <- lapply(varnames_tab, df = df, FUN = doPrepExplore::tabulate_var_discrete)
names(freq_tab.l) <- varnames_tab

tab <- doPrepExplore::tabulate_var_discrete(df = df, var = c("Make", "Model"))
freq_tab.l[["Make_Model"]] <- tab

tab <- doPrepExplore::tabulate_var_discrete(df = df, var = c("Make", "Model", "SubModel"))
freq_tab.l[["Make_Model_SubModel"]] <- tab

tab <- doPrepExplore::tabulate_var_discrete(df = df, var = c("Make", "Model", "engine_type_engine_vol"))
freq_tab.l[["Make_Model_engine_type_engine_vol"]] <- tab

tab <- doPrepExplore::tabulate_var_discrete(df = df, var = c("VNST", "VNZIP1"))
freq_tab.l[["VNST_VNZIP1"]] <- tab

saveRDS(freq_tab.l, "data/processed/frequency_tabs_train.rds")

}