#edit_feature_values.R


#edit 'Make' where very few obs exist in raw data.
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

train1 <- edit_make(train1)
test1 <- edit_make(test1)




#ensure all 'Transmission' values are uppercase
train1[["Transmission"]] <- toupper(train1[["Transmission"]])
test1[["Transmission"]] <- toupper(test1[["Transmission"]])




#re-assign NULL 'Size' values
train1 <- train1 %>% 
  mutate(Size = case_when(
    Size == "NULL" & Make == "JEEP" & Model_agg == "PATRIOT" ~ "SMALL SUV",
    Size == "NULL" & Make == "DODGE" & Model_agg == "NITRO" ~ "MEDIUM SUV",
    Size == "NULL" & Make == "GMC" & Model_agg == "1500 SIERRA PICKUP" ~ "LARGE TRUCK",
    Size == "NULL" & Make == "HYUNDAI" & Model_agg == "ELANTRA" ~ "MEDIUM",
    TRUE ~ Size
    
  ))






#if price variable reads 0 or 1, declare it missing
edit_missing_price <- function(varname_price, data) {

  varname_price.sym <- rlang::sym(varname_price)

  data <- data %>%
    mutate(!!varname_price.sym := ifelse(!!varname_price.sym == 0 | !!varname_price.sym == 1,
                                         NA, !!varname_price.sym))

  return(data)

}

train1 <- edit_missing_price("MMRAcquisitionAuctionAveragePrice", train1)
train1 <- edit_missing_price("MMRAcquisitionAuctionCleanPrice", train1)
train1 <- edit_missing_price("MMRAcquisitionRetailAveragePrice", train1)
train1 <- edit_missing_price("MMRAcquisitionRetailCleanPrice", train1)
train1 <- edit_missing_price("VehBCost", train1)

test1 <- edit_missing_price("MMRAcquisitionAuctionAveragePrice", test1)
test1 <- edit_missing_price("MMRAcquisitionAuctionCleanPrice", test1)
test1 <- edit_missing_price("MMRAcquisitionRetailAveragePrice", test1)
test1 <- edit_missing_price("MMRAcquisitionRetailCleanPrice", test1)
test1 <- edit_missing_price("VehBCost", test1)
