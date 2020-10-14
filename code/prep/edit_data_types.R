#edit_data_types.R

#numeric variables with some values stored NULL 
#inread as character, but should be converted to numeric
varnames_to_num <- c("MMRAcquisitionAuctionAveragePrice",
                     "MMRAcquisitionAuctionCleanPrice",
                     "MMRAcquisitionRetailAveragePrice",
                     "MMRAcquisitionRetailCleanPrice",
                     "MMRCurrentAuctionAveragePrice",
                     "MMRCurrentAuctionCleanPrice",
                     "MMRCurrentRetailAveragePrice",
                     "MMRCurrentRetailCleanPrice")
varnames_to_num.syms <- rlang::syms(varnames_to_num)
train1 <- train1 %>% 
  mutate_at(varnames_to_num, as.numeric)
test1 <- test1 %>% 
  mutate_at(varnames_to_num, as.numeric)




varname_to_char <- c("BYRNO")
train1 <- train1 %>% 
  mutate_at(varname_to_char, as.character)
test1 <- test1 %>% 
  mutate_at(varname_to_char, as.character)




varname_to_date <- c("PurchDate")
train1 <- train1 %>% 
  mutate_at(varname_to_date, format = "%m/%d/%Y", as.Date)
test1 <- test1 %>% 
  mutate_at(varname_to_date, format = "%m/%d/%Y", as.Date)