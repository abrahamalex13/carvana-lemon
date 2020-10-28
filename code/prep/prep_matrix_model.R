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





#single factor var to multiple features -------------

#construct matrix of factor variables.
expand_factor_features <- function(df, varnames, add_intercept = F) {
  
  if (!add_intercept) X <- model.matrix(~ . - 1, df[, varnames])
  else X <- model.matrix(~ ., df[, varnames])
    
  return(X)
  
}

varnames_discrete <- c("Make_consol", "MakeOTHER", "VehYear_consol", "VehYear1", 
                       "Size_consol", "SizeOTHER", "WheelType",
                
                "Auction", "AUCGUART", 
                
                "VNST_consol", "VNSTOTHER", "VNZIP1_consol", "VNZIP1OTHER", "BYRNO_consol", "BYRNOOTHER",
                
                "Make_Model_consol", "Make_ModelOTHER", "Make_VehYear_consol", "Make_VehYearOTHER",
                
                "Make_Model_SubModel_consol", "Make_Model_SubModelOTHER",
                "Make_Model_Trim_consol", "Make_Model_TrimOTHER",
                "Make_Model_VehYear_consol", "Make_Model_VehYearOTHER",
                
                "Make_Model_SubModel_Trim_consol", "Make_Model_SubModel_TrimOTHER",
                "Make_Model_SubModel_VehYear_consol", "Make_Model_SubModel_VehYearOTHER",
              
                "Make_Model_SubModel_Trim_VehYear_consol", "Make_Model_SubModel_Trim_VehYearOTHER")

varnames_conti <- c("RatioWarrantyVehBCost", 
                    colnames(train1_keep)[grepl("RatioWarrantyVehBCost_bs", colnames(train1_keep))],
                    
                    "VehOdo", colnames(train1_keep)[grepl("VehOdo_bs", colnames(train1_keep))])

X_train1 <- expand_factor_features(train1_keep, c(varnames_conti, varnames_discrete))
X_valid1 <- expand_factor_features(train1_excl, c(varnames_conti, varnames_discrete))

#ensure identical columns with training data.
varnames_train_fill <- setdiff(colnames(X_train1), colnames(X_valid1))
blanks <- matrix(0, ncol = length(varnames_train_fill), nrow = nrow(X_valid1))
colnames(blanks) <- varnames_train_fill
X_valid1 <- cbind(X_valid1, blanks)

#end single factor var to multiple features ---------









#factor - conti var interactions --------------------

construct_conti_factor_interact <- function(values_conti, col_binary_numeric) {
  
  values_conti_iact <- values_conti * as.numeric(col_binary_numeric)
  colnames(values_conti_iact) <- 
    paste(colnames(col_binary_numeric), "_", colnames(values_conti), sep = "")
  
  return(values_conti_iact)
  
}

#only certain factor vars to enter interactions.
varnames_factor_Make_Model_Sub_VehYear <- 
  colnames(X_train1)[grepl(c("Make_Model_VehYear"), colnames(X_train1))]

varnames_factor_VNZIP1 <- 
  colnames(X_train1)[grepl(c("VNZIP1"), colnames(X_train1))]




#train ---------------------

varnames_factor_select <- 
  c(varnames_factor_Make_Model_Sub_VehYear, varnames_factor_VNZIP1)

splines_iact.l <- 
  lapply(varnames_factor_select, 
         values_conti = X_train1[, grepl("VehOdo", colnames(X_train1))], 
         function(x, values_conti) {
  
  col_binary_numeric <- X_train1[, x, drop = F]
  splines_iact <- construct_conti_factor_interact(values_conti, col_binary_numeric)
  return(splines_iact)
  
  
})
splines_iact <- do.call(cbind, splines_iact.l)
X_train1 <- cbind(X_train1, splines_iact)


varnames_factor_select <- varnames_factor_Make_Model_Sub_VehYear
  
splines_iact.l <-
  lapply(varnames_factor_select,
         values_conti = X_train1[, grepl("RatioWarrantyVehBCost", colnames(X_train1))],
         function(x, values_conti) {

           col_binary_numeric <- X_train1[, x, drop = F]
           splines_iact <- construct_conti_factor_interact(values_conti, col_binary_numeric)
           return(splines_iact)


         })
splines_iact <- do.call(cbind, splines_iact.l)
X_train1 <- cbind(X_train1, splines_iact)

#end train ------------------




#validate -------------------

varnames_factor_select <- 
  c(varnames_factor_Make_Model_Sub_VehYear, varnames_factor_VNZIP1)

splines_iact.l <- 
  lapply(varnames_factor_select, 
         values_conti = X_valid1[, grepl("VehOdo", colnames(X_valid1))], 
         function(x, values_conti) {
           
           col_binary_numeric <- X_valid1[, x, drop = F]
           splines_iact <- construct_conti_factor_interact(values_conti, col_binary_numeric)
           return(splines_iact)
           
           
         }
        )
splines_iact <- do.call(cbind, splines_iact.l)
X_valid1 <- cbind(X_valid1, splines_iact)


varnames_factor_select <- varnames_factor_Make_Model_Sub_VehYear

splines_iact.l <-
  lapply(varnames_factor_select,
         values_conti = X_valid1[, grepl("RatioWarrantyVehBCost", colnames(X_valid1))],
         function(x, values_conti) {

           col_binary_numeric <- X_valid1[, x, drop = F]
           splines_iact <- construct_conti_factor_interact(values_conti, col_binary_numeric)
           return(splines_iact)


         })
splines_iact <- do.call(cbind, splines_iact.l)
X_valid1 <- cbind(X_valid1, splines_iact)

#end validate ---------------


#end factor - conti var interactions ------------------------------









#standardize conti ----------

varnames_conti <- c("RatioWarrantyVehBCost", colnames(X_train1)[grepl("RatioWarrantyVehBCost_bs", colnames(X_train1))],
                    "VehOdo", colnames(X_train1)[grepl("VehOdo_bs", colnames(X_train1))])
transformer_std <- caret::preProcess(X_train1[, varnames_conti], method = c("center", "scale"))

X_train_std <- predict(transformer_std, X_train1)
X_valid_std <- predict(transformer_std, X_valid1)

# end std ---------

X_train_std_sx <- Matrix(as.matrix(X_train_std), sparse = TRUE)
rm(X_train1)
X_valid_std_sx <- Matrix(as.matrix(X_valid_std), sparse = TRUE)
rm(X_valid1)
