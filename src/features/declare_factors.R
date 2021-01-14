#declare_factors.R

#ordering also guides modeling treatment.
declare_factors_custom <- function(df) {
  
  df[["WheelType"]] <- 
    factor(df[["WheelType"]], c("NULL", "Special", "Covers", "Alloy")) %>% 
    droplevels()
  
  df[["Color_consol"]] <- factor(df[["Color_consol"]]) %>% droplevels()
  
  df[["VehYear_consol"]] <- 
    factor(df[["VehYear_consol"]], unique(c(1, unique(df[["VehYear_consol"]])))) %>% 
    droplevels()
  
  df[["Make_consol"]] <- 
    factor(df[["Make_consol"]], unique( c("HONDA", unique(df[["Make_consol"]])))) %>% 
    droplevels()
  
  df[["Make_Model_consol"]] <- factor(df[["Make_Model_consol"]]) %>% droplevels()
  df[["Make_Model_SubModel_consol"]] <- factor(df[["Make_Model_SubModel_consol"]]) %>% droplevels()
  
  df[["Size_consol"]] <-
    factor(df[["Size_consol"]], unique( c("MEDIUM", unique(df[["Size_consol"]]))) ) %>% droplevels()
  
  
  
  df[["Auction"]] <- 
    factor(df[["Auction"]], c("MANHEIM", "OTHER", "ADESA")) %>% droplevels()
  df[["AUCGUART"]] <- factor(df[["AUCGUART"]])
  df[["PRIMEUNIT"]] <- factor(df[["PRIMEUNIT"]])
  
  df[["VNST_consol"]] <- 
    factor(df[["VNST_consol"]], unique( c("TX", unique(df[["VNST_consol"]])))) %>% 
    droplevels()
  df[["VNZIP1_consol"]] <- factor(df[["VNZIP1_consol"]])
  
  
  
  df[["engine_type_consol"]] <- factor(df[["engine_type_consol"]])
  df[["engine_vol_consol"]] <- factor(df[["engine_vol_consol"]])
  df[["engine_type_engine_vol_consol"]] <- factor(df[["engine_type_engine_vol_consol"]])
  df[["Make_engine_type_engine_vol_consol"]] <- factor(df[["Make_engine_type_engine_vol_consol"]])
  
  
 
  df[["BYRNO_consol"]] <- factor(df[["BYRNO_consol"]]) %>% droplevels()
  
  
  
  df[["PurchDate_month"]] <- factor(df[["PurchDate_month"]], ordered = FALSE) %>% droplevels()
  df[["PurchDate_day"]] <- factor(df[["PurchDate_day"]])
  df[["PurchDate_wday"]] <- factor(df[["PurchDate_wday"]], ordered = FALSE) %>% droplevels()
  df[["PurchDate_month_PurchDate_wday_consol"]] <- factor(df[["PurchDate_month_PurchDate_wday_consol"]])
  
  return(df)
  
}