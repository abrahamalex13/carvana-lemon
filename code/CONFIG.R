#CONFIG.R

rm(list=ls())
library(tidyverse)
library(data.table)
library(mgcv)
library(lubridate)
library(VIM)
library(caret)
# library(rlist)
library(doPrepExplore)
library(scales)
library(doParallel)
library(parallel)

library(glmnet)

set.seed(2349)

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

varname_y <- "IsBadBuy"



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

source("code/model/summarize_pred_binary.R")
# source("code/model/baseline_fixed_effects_logistic.R")
source("code/model/penalized_linear.R")
# source("code/model/sandbox.R")


#end model -----

