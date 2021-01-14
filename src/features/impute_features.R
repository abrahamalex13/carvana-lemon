#impute_features

imputation_reference <- readRDS(filename_imputation_reference) 




#for one-off VehYear-Make-Model combinations with averages still missing, sub near-year.
varnames_mmr_impute <- c(colnames(train1)[grepl("MMR", colnames(train1))])
df_means <- MMR_impute_results.l[["df_means"]]

for (varname_i in varnames_mmr_impute) {
  
  varname_mean_sub <- paste(varname_i, "_mean", sep = "")
  
  train1[train1$VehYear == 2002 & train1$Make == "SUZUKI" & train1$Model_agg == "VITARA", varname_i] <- 
    df_means[df_means$VehYear == 2004 & df_means$Make == "SUZUKI" & df_means$Model_agg == "VITARA", varname_mean_sub]
  
  train1[train1$VehYear == 2006 & train1$Make == "CHEVROLET" & train1$Model_agg == "SUBURBAN", varname_i] <- 
    df_means[df_means$VehYear == 2005 & df_means$Make == "CHEVROLET" & df_means$Model_agg == "SUBURBAN", varname_mean_sub]
  
  train1[train1$VehYear == 2006 & train1$Make == "SUBARU" & train1$Model_agg == "OUTBACK", varname_i] <- 
    df_means[df_means$VehYear == 2007 & df_means$Make == "SUBARU" & df_means$Model_agg == "OUTBACK", varname_mean_sub]
  
  train1[train1$VehYear == 2007 & train1$Make == "HONDA" & train1$Model_agg == "ELEMENT", varname_i] <- 
    df_means[df_means$VehYear == 2008 & df_means$Make == "HONDA" & df_means$Model_agg == "ELEMENT", varname_mean_sub]  
  
}
  