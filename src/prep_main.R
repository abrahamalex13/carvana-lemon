#prep_main.R


if (data_source_is_static_augmented) {

  
  #load primary --------
  
  df <- read_csv(paste("data/raw/", filename_raw_in, sep=""))
  
  #processing pipeline applicable _strictly_ for X, not Y
  if (varname_y %in% colnames(df)) {
    df <- doPrepExplore::separate_store_Y(
      df, varname_y = varname_y, directory = directory_data_mdl_in, run_type = run_type
    )
  }
  
  #unexpected columns/column order unsafe
  df <- doPrepExplore::check_columns_order(
    df = df, 
    filename_operator = paste(directory_operators_prep, "initial_check_columns_order_operator.rds", sep = ""),
    run_type = run_type
  )
  
  #ensure no train/test leakage
  #note external seed dependence!
  if (do_split_data) df <- doPrepExplore::split_df(df, fraction_train = fraction_train_split, run_type = run_type)
  
  #end load primary ---------
  
  
  
  #edit metadata -------
  
  #mis-spelled field names, for example.
  source("src/features/correct_typos_inread.R")
  df <- correct_typos_inread(df)
  
  source("src/features/edit_data_types.R")
  df <- edit_data_types(df)
  #data types must match expectation from train
  data_types <- doPrepExplore::examine_data_types(
    df = df, 
    filename_operator = paste(directory_operators_prep, "data_types_expected.rds", sep = ""), 
    run_type = run_type
  )
  
  source("src/features/edit_feature_values.R")
  df <- edit_feature_values(df)
  
  #end edit metadata -----
  
  
  
  #preliminary feature prep ------
  
  #zero/near-zero variance predictors carry little information for model-building
  # df <- doPrepExplore::drop_nzv(
  #   df = df, filename_operator = "data/processed/tabulate_nzv_train.rds", run_type = run_type
  #   )
  
  
  #tabulate summary stats, primarily for imputation
  ptile_probs <- c(.1, .25, .5, .75, .9)
  summ_fun_ptile <- 
    purrr::map(ptile_probs, 
               .f = function(.x) partial(quantile, probs = .x, na.rm = TRUE))
  names(summ_fun_ptile) <- paste("p", 100 * ptile_probs, sep = "")
  summ_fun <- c(summ_fun_ptile, "mean" = partial(mean, na.rm = TRUE))
  varnames_group <- c("VehYear")
  
  filename_imputation_reference <- 
    paste(directory_operators_prep, "summary_stats_train.rds", sep = "")
  
  if (run_type == "train") {
    summ <- doPrepExplore::stack_summary_stats(
      df = df, varnames_group = varnames_group, varnames_summarize = "all", 
      funs_summary = summ_fun, 
      filename_operator = filename_imputation_reference
      )
  }
  
  #imputation
  stat_impute <- "p50"
  varnames_join_by <- varnames_group
  imputation_reference <- readRDS(filename_imputation_reference) 
  imputation_reference <- imputation_reference %>% 
    dplyr::filter(summary_statistic == stat_impute) %>% 
    dplyr::select(-summary_statistic)
  
  df <- doPrepExplore::impute_via_join(
    x = df, imputation_reference = imputation_reference, by = varnames_join_by
  )
  
  #end prelim feature prep -------
  
  
}

#informative predictors outside of primary data
source("src/data/joins_external.R")
df <- joins_external(df)







# feature engineering --------------------------

if (data_source_is_static_augmented) {

  varnames_x_to_log <- c(
    
    "VehOdo", "MMRAcquisitionAuctionAveragePrice",
    "MMRAcquisitionAuctionCleanPrice", "MMRAcquisitionRetailAveragePrice",
    "MMRAcquisitionRetailCleanPrice", "MMRCurrentAuctionAveragePrice",
    "MMRCurrentAuctionCleanPrice", "MMRCurrentRetailAveragePrice", 
    "MMRCurrentRetailCleanPrice", "VehBCost", "WarrantyCost"
    
  )
  source("src/features/transform_conti_direct.R")
  df <- transform_conti_direct(df, varnames_x_to_log)
  
}


#(natural entry point for reduced-column user input)


source("src/features/transform_categorical_direct.R")
df <- transform_categorical_direct(df)

source("src/features/transform_categorical_derived.R")
df <- transform_categorical_derived(
  df = df, run_type = run_type, 
  thresh_nobs_consol = thresh_nobs_consol, thresh_nobs_indic_other = thresh_nobs_indic_other, 
  directory_operators = paste(directory_operators_prep, "consolidate_categories_other/", sep = "")
)

source("src/features/declare_factors.R")
df <- declare_factors_custom(df)

#encode factor levels with implied-y numeric
if (run_type == "train") {
  Y <- readRDS(paste(directory_data_mdl_in, "Y_train.rds", sep = ""))
} else Y <- NULL
for (v in varnames_factor_encode_numeric) {

  df <- doPrepExplore::transform_posteriors_p_summaries_supervised_factor(
    df = df, varname_factor = v, run_type = run_type,
    par_prior_beta = as.data.frame(par_prior_beta), Y = Y,
    directory_operators = paste(directory_operators_prep, "posteriors_p_supervised/", sep = "")
  )

}
rm(Y)



#(natural opening for EDA)



#save (narrow) form1 --------

  #form1 does not spread factor types
  if (form_data == "form1") {
    
    if (run_type == "train") {
      
      saveRDS(df[, c(grep(varnames_conti_save_pattern, colnames(df), value = TRUE), varnames_discrete_save)], 
              paste(directory_data_mdl_in, "X_train_form1.rds", sep = ""))
      
    } else saveRDS(df[, c(grep(varnames_conti_save_pattern, colnames(df), value = TRUE), varnames_discrete_save)], 
                   paste(directory_data_mdl_in, "X_test_form1.rds", sep = ""))
    
  }

#end save form1 ------------



#spread to wider representation ----------
#primarily, factor types are expanded

if (form_data == "form_wide") {
  
  #spread factor features: to one feature per factor level, from single factor-type feature
  #must observe a factor level sufficiently often to justify selection
  source("src/features/derive_indic_varnames_rare_operator.R")
  if (run_type == "train") invisible(
    derive_indic_varnames_rare_operator(
      df = df, varnames_cat = varnames_factor_encode_numeric, thresh_frac_obs = thresh_frac_obs, 
      directory_operator = directory_operators_prep)
  )
  varnames_levels_drop <- readRDS(paste(directory_operators_prep, "varnames_rare_drop.rds", sep = ""))
  
  X_wide <- doPrepExplore::spread_factor_features(
    df[, c(grep(varnames_conti_save_pattern, colnames(df), value = TRUE), varnames_discrete_save)]
    )
  
  varnames_levels_drop <- intersect(varnames_levels_drop, colnames(X_wide))
  X_wide <- X_wide[, -which(colnames(X_wide) %in% varnames_levels_drop)]
  


  if (make_factor_interacts) {
  
    varnames_pairs_interact <- list(
      
      # c("Make_Model_consol", "VehOdo")
      c("Make_Model_consol", "RatioWarrantyVehBCost")
      
      # , c("Make_VehYear_consol", "VehOdo")
      
      # , c("Make_Model_VehYear_consol", "VehOdo")
      # , c("Make_Model_VehYear_consol", "RatioWarrantyVehBCost")
      
      , c("VNZIP1_consol", "VehOdo")
      
      , c("Make_engine_type_engine_vol_consol", "VehOdo")
      
    )
    source("src/features/factor_interactions.R")
    X_wide <- bind_cols(X_wide, vars_interact)
    rm(vars_interact)
    
    
    
    varnames_bs <- c("VehOdo", "RatioWarrantyVehBCost", 
                     colnames(X_wide)[grepl(":VehOdo", colnames(X_wide))], 
                     colnames(X_wide)[grepl(":RatioWarrantyVehBCost", colnames(X_wide))])
    bs_df <- 5
    source("src/features/transforms_conti_derived.R")
    X_wide <- bind_cols(X_wide, values_bs)
    rm(bs.l, values_bs)
    
  }


#end feature engineering --------------------------



  #pre-standardize, ensure train and test columns match _exactly_
  source("src/features/enforce_train_columns_structure.R")  
  filename_save_X <- if (run_type == "train") {
    paste(directory_data_mdl_in, "X_train_wide.rds", sep = "") } else { 
    paste(directory_data_mdl_in, "X_test_wide.rds", sep = "") }
  
  X_wide <- enforce_train_columns_structure(
    X = X_wide, filename_save_X = filename_save_X,   
    filename_operator = paste(directory_operators_prep, "final_check_columns_order_operator.rds", sep = ""), 
    run_type = run_type)
  
  
  varnames_conti <- grep(varnames_conti_save_pattern, colnames(X_wide), value = TRUE)
  source("src/features/standardize.R")
  X_wide_std <- do_standardize(
    X = X_wide, varnames_standardize = varnames_conti,
    filename_operator = paste(directory_operators_prep, "standardize_operator.rds", sep = ""), 
    run_type = run_type)
  
  filename_save <- if (run_type == "train") {
    paste(directory_data_mdl_in, "X_train_wide_std.rds", sep = "") } else {
    paste(directory_data_mdl_in, "X_test_wide_std.rds", sep = "") }
  saveRDS(X_wide_std, file = filename_save)


}