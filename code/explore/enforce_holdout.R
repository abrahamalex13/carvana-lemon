#enforce_holdout.R
#before proceeding with any supervised-type work, 
#mitigate overfitting risk.

set.seed(1)
index_row_keep <- createDataPartition(train1[[varname_y]], p = .75, list = F)
train1_keep <- train1[index_row_keep[, 1], ]
train1_excl <- train1[-index_row_keep[, 1], ]




declare_factors_custom <- function(df) {
  
  df[["WheelType"]] <- 
    factor(df[["WheelType"]], c("NULL", "Special", "Covers", "Alloy"))
  
  df[["VehYear_consol"]] <- 
    factor(df[["VehYear_consol"]], c(2001:2010, 1))
  
  df[["Make_consol"]] <- 
    factor(df[["Make_consol"]], unique( c("HONDA", unique(train1[["Make_consol"]]))) )
  
  df[["Size_consol"]] <- 
    factor(df[["Size_consol"]], unique( c("MEDIUM", unique(train1[["Size_consol"]]))) )
  
  df[["Auction"]] <- 
    factor(df[["Auction"]], c("MANHEIM", "OTHER", "ADESA"))
  
  df[["VNST_consol"]] <- 
    factor(df[["VNST_consol"]], unique( c("TX", unique(train1[["VNST_consol"]]))) )
  
  
  return(df)
  
}

train1_keep <- declare_factors_custom(train1_keep)
train1_excl <- declare_factors_custom(train1_excl)