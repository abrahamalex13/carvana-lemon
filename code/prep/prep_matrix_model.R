#prep_matrix_model.R



#order factors for modeling treatment -------

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

#end order factors -----------------





#factor var to multiple features -------------

#construct matrix of factor variables.
expand_factor_features <- function(df, varnames, add_intercept = F) {
  
  if (!add_intercept) X <- model.matrix(~ . - 1, df[, varnames])
  else X <- model.matrix(~ ., df[, varnames])
    
  return(X)
  
}

varnames_x <- c("Make_consol", "VehYear_consol", "Size_consol", "WheelType",
                "RatioWarrantyVehBCost", "VehOdo_log",
                
                "Auction", "AUCGUART", "VNST_consol", "VNZIP1_consol", "BYRNO_consol",
                
                "Make_Model_consol", "Make_VehYear_consol",
                
                "Make_Model_SubModel_consol", "Make_Model_Trim_consol", "Make_Model_VehYear_consol",
                
                "Make_Model_SubModel_Trim_consol", "Make_Model_SubModel_VehYear_consol",
              
                "Make_Model_SubModel_Trim_VehYear_consol")

varnames_conti <- c("RatioWarrantyVehBCost", "VehOdo_log")
varnames_discrete <- varnames_x[-which(varnames_x %in% varnames_conti)]

X_train1 <- expand_factor_features(train1_keep, c(varnames_conti, varnames_discrete))
X_valid1 <- expand_factor_features(train1_excl, c(varnames_conti, varnames_discrete))

#end factor var to multiple features -----------






#standardize conti ----------

transformer_std <- caret::preProcess(X_train1[, varnames_conti], method = c("center", "scale"))

X_train_std <- predict(transformer_std, X_train1)
X_valid_std <- predict(transformer_std, X_valid1)

# end std ---------

X_train_std_sx <- Matrix(X_train_std, sparse = TRUE)
X_valid_std_sx <- Matrix(X_valid_std, sparse = T)
