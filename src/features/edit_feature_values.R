#edit_feature_values.R

#there exist rare 'Make' which naturally re-map
edit_make <- function(data) {
  
  data <- data %>% 
    mutate(Make = case_when( 
      
      Make == "PLYMOUTH" ~ "CHRYSLER",
      Make == "HUMMER" ~ "GMC",
      Make == "TOYOTA SCION" ~ "TOYOTA",
      TRUE ~ Make
      
    ))  
  
  return(data)
}

#price features equal 0 or 1 appear to indicate missing
edit_missing_price <- function(varname_price, data) {
  
  varname_price.sym <- rlang::sym(varname_price)
  
  data <- data %>%
    mutate(!!varname_price.sym := ifelse(!!varname_price.sym == 0 | !!varname_price.sym == 1,
                                         NA, !!varname_price.sym))
  
  return(data)
  
}

edit_feature_values <- function(df) {

  df <- edit_make(df)
  
  #avoid case sensitivity issues
  df[["Transmission"]] <- toupper(df[["Transmission"]])
  
  #there exist easily mapped 'NULL' Size values
  df <- df %>% 
    mutate(Size = case_when(
      Size == "NULL" & Make == "JEEP" & grepl("PATRIOT", Model) ~ "SMALL SUV",
      Size == "NULL" & Make == "DODGE" & grepl("NITRO", Model) ~ "MEDIUM SUV",
      Size == "NULL" & Make == "GMC" & grepl("1500 SIERRA PICKUP", Model) ~ "LARGE TRUCK",
      Size == "NULL" & Make == "HYUNDAI" & grepl("ELANTRA", Model) ~ "MEDIUM",
      TRUE ~ Size
      
    ))

  df <- edit_missing_price("MMRAcquisitionAuctionAveragePrice", df)
  df <- edit_missing_price("MMRAcquisitionAuctionCleanPrice", df)
  df <- edit_missing_price("MMRAcquisitionRetailAveragePrice", df)
  df <- edit_missing_price("MMRAcquisitionRetailCleanPrice", df)
  df <- edit_missing_price("VehBCost", df)
  
  df
  
}