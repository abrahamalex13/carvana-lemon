#truncate_aggregate_zip_stats.R

vals <- read_csv("data/external/hhincome_median_zip.csv")
vals <- vals %>% 
  dplyr::select(-pop) %>% 
  mutate_at("VNZIP1", as.character) %>% 
  mutate(VNZIP1 = case_when(
    
    nchar(VNZIP1) == 4 ~ paste("0", VNZIP1, sep = ""),
    TRUE ~ VNZIP1
    
  )) %>% 
  rename(hhinc_p50_VNZIP1 = hhinc_p50)

saveRDS(vals, "data/external/hhincome_median_zip5.rds")


truncate_aggregate_zip <- function(df, varname_zip, nchar_keep, varname_value) {
  
  varname_zip_trunc <- paste(varname_zip, "_trunc", sep = "")
  
  varname_zip <- rlang::sym(varname_zip)
  varname_zip_trunc <- rlang::sym(varname_zip_trunc)
  
  varname_value_trunc <- paste(varname_value, "_trunc", sep = "")
  varname_value_trunc <- rlang::sym(varname_value_trunc)
  varname_value <- rlang::sym(varname_value)
  
  df <- df %>% 
    mutate(!!varname_zip_trunc := substr(!!varname_zip, 1, nchar_keep))
  
  df %>% 
    group_by(!!varname_zip_trunc) %>% 
    summarize(!!varname_value_trunc := median(!!varname_value, na.rm = TRUE)) 
  
}

agg4 <- truncate_aggregate_zip(vals, varname_zip = "VNZIP1", 4, "hhinc_p50_VNZIP1")
saveRDS(agg4, "data/external/hhincome_median_zip4.rds")

agg3 <- truncate_aggregate_zip(vals, varname_zip = "VNZIP1", 3, "hhinc_p50_VNZIP1")
saveRDS(agg3, "data/external/hhincome_median_zip3.rds")