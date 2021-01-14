#edit_data_types.R

edit_data_types <- function(df) {

  #numeric variables with some values stored NULL inread as character, 
  #but should be converted to numeric
  varnames_to_num <- c("MMRAcquisitionAuctionAveragePrice",
                       "MMRAcquisitionAuctionCleanPrice",
                       "MMRAcquisitionRetailAveragePrice",
                       "MMRAcquisitionRetailCleanPrice",
                       "MMRCurrentAuctionAveragePrice",
                       "MMRCurrentAuctionCleanPrice",
                       "MMRCurrentRetailAveragePrice",
                       "MMRCurrentRetailCleanPrice")
  df <- df %>% 
    mutate_at(varnames_to_num, as.numeric)
  
  varnames_to_char <- c("RefId", "BYRNO", "VNZIP1")
  df <- df %>% 
    mutate_at(varnames_to_char, as.character)
  
    df <- df %>% 
      mutate(VNZIP1 = case_when(
        
        nchar(VNZIP1) == 4 ~ paste("0", VNZIP1, sep = ""),
        TRUE ~ VNZIP1
        
      ))
  
  varnames_to_date <- c("PurchDate")
  df <- df %>% 
    mutate_at(varnames_to_date, format = "%m/%d/%Y", as.Date)
  
  df
  
}