#CONFIG.R

rm(list=ls())
library(tidyverse)
library(data.table)
library(lubridate)
library(VIM)

library(caret)
library(doPrepExplore)
library(scales)
library(doParallel)
library(parallel)

library(glmnet)
library(ranger)
library(gbm)
library(xgboost)



#data preparation -----------

set.seed(1)

filename_raw_in <- "training.csv"
data_source_is_static_augmented <- grepl("csv", filename_raw_in)

varname_y <- "IsBadBuy"
directory_data_mdl_in <- "data/processed/enter_model/full/"
directory_operators_prep <- "data/processed/operators_prep/"


run_type <- "train"
  do_split_data <- FALSE
    fraction_train_split <- .75

  
#feature frequency tuning parameters
thresh_nobs_consol <- 15
thresh_nobs_indic_other <- 25

thresh_frac_obs <- .001
  
#stronger priors first
# par_prior_beta <- list("a" = 10, "b" = 90)
# par_prior_beta <- list("a" = 8, "b" = 72)
# par_prior_beta <- list("a" = 6, "b" = 54)
# par_prior_beta <- list("a" = 5, "b" = 45)
# par_prior_beta <- list("a" = 4, "b" = 36)
# par_prior_beta <- list("a" = 3, "b" = 27)
par_prior_beta <- list("a" = 2, "b" = 18)


varnames_factor_encode_numeric <- c(
  "Make_consol"
  , "Make_Model_consol"
  # , "Make_Model_SubModel_consol"
  , "VehYear_consol"
  
  , "Size_consol"
  , "WheelType"
  , "Color_consol"
  
  , "Auction"
  , "AUCGUART"
  , "PRIMEUNIT"
  
  , "VNST_consol"
  , "VNZIP1_consol"
  , "BYRNO_consol"
  
  , "engine_type_consol"
  , "engine_vol_consol"
  , "engine_type_engine_vol_consol"
  
  , "PurchDate_month"
  , "PurchDate_day"
  , "PurchDate_wday"
)
  

#if factors should be spread, then 'form_wide'
# form_data <- "form1"
form_data <- "form_wide"

#only a subset of provided variables relevant for modeling
varnames_discrete_save <- c( 
  # "Make_Model_SubModel_consol", "Make_Model_SubModel_indic_other",
  "Make_consol", "Make_indic_other"
  , "Make_Model_consol", "Make_Model_indic_other",
  "VehYear_consol", "VehYear_indic_other" 
  
  , "Size_consol"
  , "WheelType"
  , "Color_consol"
  
  , "Auction"
  
  , "VNST_consol", "VNST_indic_other"
  , "VNZIP1_consol", "VNZIP1_indic_other"
  , "BYRNO_consol", "BYRNO_indic_other"
  
  , "engine_type_consol", "engine_vol_consol"
  , "engine_type_engine_vol_consol"
  
  , "PurchDate_month"
  , "PurchDate_day"
  , "PurchDate_wday"
)
# varnames_discrete <- c("Make_consol", "Make_indic_other", "VehYear_consol", "VehYear_indic_other" 
#                        # "Size_consol", "Size_indic_other", 
#                        , "WheelType"
#                        
#                        , "Auction" #"AUCGUART", 
#                        
#                        , "VNST_consol", "VNST_indic_other", 
#                        "VNZIP1_consol", "VNZIP1_indic_other", 
#                        "BYRNO_consol", "BYRNO_indic_other",
#                        
#                        # "engine_type_engine_vol_consol",
#                        "Make_engine_type_engine_vol_consol",
#                        
#                        "Make_Model_consol", "Make_Model_indic_other", 
#                        "Make_VehYear_consol", "Make_VehYear_indic_other",
#                        
#                        "Make_Model_SubModel_consol", "Make_Model_SubModel_indic_other",
#                        # "Make_Model_Trim_consol", "Make_Model_TrimOTHER",
#                        "Make_Model_VehYear_consol", "Make_Model_VehYear_indic_other",
#                        
#                        # "Make_Model_SubModel_Trim_consol", "Make_Model_SubModel_TrimOTHER",
#                        "Make_Model_SubModel_VehYear_consol", "Make_Model_SubModel_VehYear_indic_other"
#                        
#                        # "Make_Model_SubModel_Trim_VehYear_consol", "Make_Model_SubModel_Trim_VehYearOTHER"
#                        
#                        , "PurchDate_day"
#                        #, "PurchDate_month_PurchDate_wday_consol" 
# )
varnames_conti_save_pattern <- 
  paste(c("RatioWarrantyVehBCost", "RatioCostAuctionAverage", "RatioCostAuctionClean", "SpreadRatioCostAuctionAC", 
  "VehOdo", "hhinc_p50_VNZIP1", "l_odds_p_summ", "PurchDate_day_int"), sep = "", collapse = "|")

#explicit factor-conti interactions likely most helpful for linear models
make_factor_interacts <- FALSE
  #if applicable, see 'main' script for varnames to receive B-spline transforms, etc

source("src/prep_main.R")
# ----------------------------



#model execution --------------
set.seed(1)
source("src/models/calc_scores.R")

directory_data_mdl_in <- "data/processed/enter_model/full/"
directory_mdl_save <- "models/full/"

filename_X_train <- paste(directory_data_mdl_in, "X_train_wide.rds", sep = "")
filename_Y_train <- paste(directory_data_mdl_in, "Y_train.rds", sep = "")
  
filename_X_test <- paste(directory_data_mdl_in, "X_test_wide.rds", sep = "")
filename_Y_test <- paste(directory_data_mdl_in, "Y_test.rds", sep = "")

run_type_train <- FALSE
  do_validate <- FALSE
run_type_test <- TRUE
if (run_type_train && do_validate) run_type_test <- TRUE

model_type <- "xgboost"
#more basis functions means, 
#lower chance sparse saves space (post-standardization)
to_sparse <- FALSE

filename_identifiers_test <- "data/raw/test.csv"
  varnames_identifiers_test <- c("RefId")
  if (!is.null(filename_identifiers_test)) {
    
    identifiers_test <- if (grepl("csv", filename_identifiers_test)) {
      read_csv(filename_identifiers_test) 
    } else if (grepl("rds", filename_identifiers_test)) {
      readRDS(filename_identifiers_test)
    }
    
    identifiers_test <- identifiers_test %>% dplyr::select(!!!rlang::syms(varnames_identifiers_test))
    
  }
  
filename_pred <- "data/processed/Yhat.csv"

source("src/model_main.R")
# ---------------------------























path_data_raw <- "data_raw/"
path_data_processed <- "data_processed/"
path_data_ref <- "data_ref/"
path_plots <- "plots/"
path_code <- "code/"

filename1_raw <- "training.csv"
filename2_raw <- "test.csv"
ref1 <- "plot_type_x_by_y.csv"
ref2 <- "make_model_tab_agg.csv"
ref3 <- "varname_description_lookup.csv"





#data inread and modifications ------------------------------------

#read filenames
train1 <- read_csv(paste(path_data_raw, filename1_raw, sep=""))
test1 <- read_csv(paste(path_data_raw, filename2_raw, sep=""))

#correct variable spelling
train1 <- train1 %>% 
  rename(MMRAcquisitionRetailCleanPrice = MMRAcquisitonRetailCleanPrice)
test1 <- test1 %>% 
  rename(MMRAcquisitionRetailCleanPrice = MMRAcquisitonRetailCleanPrice)
  


#read reference data
plot_types_x_by_y <- read_csv(paste(path_data_ref, ref1, sep=""))
#aggregated make-model data -- increase sample sizes with less-specific car models.  
  #where harmonized Model is NA, impute Model
make_model_agg <- read_csv(paste(path_data_ref, ref2, sep="")) %>% 
  mutate(Model_agg = ifelse(is.na(Model_agg), Model, Model_agg))
#variable name and description
varname_desc_tbl <- read_csv(paste(path_data_ref, ref3, sep=""))
#presentation-ready variable names
tbl_pres_pred <- read_csv(paste(path_data_ref, "varname_description_present_predictors.csv", sep = ""))


#merge harmonized make-model reference data into primary datasets
train1 <- left_join(train1,
                    make_model_agg %>% dplyr::select(-n))
test1 <- left_join(test1,
                    make_model_agg %>% dplyr::select(-n))

#end data inread and mods ------------------------------------







#function definitions ----------------


#simple multi-var tabulate function
tab_vars <- function(df, varnames) {
  
  varnames.syms <- rlang::syms(varnames)
  
  out <- df %>% group_by(!!!varnames.syms) %>% 
    summarize(n = n()) %>% 
    arrange(desc(n))
  
  return(out)
  
}

#end function definitions ----------- 




# prep ---------

source("code/prep/edit_data_types.R")
source("code/prep/edit_feature_values.R")
source("code/prep/impute_features.R")
source("code/prep/construct_features.R")

# source("code/explore/tab_features.R")

# source("code/explore/plot_features_distr_univar.R")
# source("code/present/present_plot_features_distr_univar.R")

source("code/prep/enforce_holdout.R")
source("code/prep/prep_matrix_model.R")

#end prep ------



#explore (enforcing keep/holdout paradigm) -----

# source("code/explore/unsuper_conti_features.R")

# source("code/explore/plot_features_distr_by_outcome.R")
# source("code/present/present_plot_features_distr_by_outcome.R")

#end explore ----




#model ----------

# source("code/model/summarize_pred_binary.R")
# source("code/model/baseline_fixed_effects_logistic.R")
source("code/model/penalized_linear.R")
# source("code/model/sandbox.R")


#end model -----

