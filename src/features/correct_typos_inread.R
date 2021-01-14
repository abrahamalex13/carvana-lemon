correct_typos_inread <- function(df) {
  
  if ("MMRAcquisitonRetailCleanPrice" %in% colnames(df)) {
    
    df <- df %>% 
      rename(MMRAcquisitionRetailCleanPrice = MMRAcquisitonRetailCleanPrice)
    
  }
  
  df
}