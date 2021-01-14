#joins_external.R

joins_external <- function(df) {
  
  ext <- readRDS("data/external/hhincome_median_zip5.rds")
  df <- left_join(df, ext)
  
  #regional data lacks coverage for _all_ 5-digit Zips.
  ext <- readRDS("data/external/hhincome_median_zip4.rds")
  df <- df %>% 
    mutate(VNZIP1_trunc = substr(VNZIP1, 1, 4)) %>% 
    left_join(., ext) %>% 
      
    mutate(hhinc_p50_VNZIP1 = case_when(
      
      is.na(hhinc_p50_VNZIP1) ~ hhinc_p50_VNZIP1_trunc, 
      TRUE ~ hhinc_p50_VNZIP1
      
    )) %>% 
    dplyr::select(-c(VNZIP1_trunc, hhinc_p50_VNZIP1_trunc))
    
  
  ext <- readRDS("data/external/hhincome_median_zip3.rds")
  df <- df %>% 
    mutate(VNZIP1_trunc = substr(VNZIP1, 1, 3)) %>% 
    left_join(., ext) %>% 
    
    mutate(hhinc_p50_VNZIP1 = case_when(
      
      is.na(hhinc_p50_VNZIP1) ~ hhinc_p50_VNZIP1_trunc, 
      TRUE ~ hhinc_p50_VNZIP1
      
    )) %>% 
    dplyr::select(-c(VNZIP1_trunc, hhinc_p50_VNZIP1_trunc))
    
  df
  
}