#explore_tabs.R

#this script explores frequency tabulations



#univariate -----------------------------

#frequency tabulations for discrete explanatory variables
varnames_tab <- c("PurchDate", "PurchDate_year", "Auction", 
                  
                  "VehYear", "VehicleAge", 
                  "Make", "Model", "Trim", "SubModel", "Color", "Transmission",
                  "WheelTypeID", "WheelType", 
                  "Nationality", "Size", 
                  
                  "PRIMEUNIT", "AUCGUART", "BYRNO", "VNZIP1", "VNST", "IsOnlineSale")

freq_tab.l <- lapply(varnames_tab, df = train1, FUN = tab_var)
names(freq_tab.l) <- varnames_tab
freq_tab_discrete_x.l <- freq_tab.l
freq_tab_missing_discrete_x.l <- 
  lapply(freq_tab_discrete_x.l, function(x) {
    
    varname_x <- colnames(x)[1]
    varname_x.sym <- rlang::sym(varname_x)
    
    out <- x %>% 
      mutate(!!varname_x.sym := as.character(!!varname_x.sym)) %>% 
      dplyr::filter(is.na(!!varname_x.sym))
    
    return(out)
    
})


#frequency tabulations for conti explanatory vars -- 
#simply to know, how many missing?
varnames_tab <- c("VehOdo", "MMRAcquisitionAuctionAveragePrice",
                  "MMRAcquisitionAuctionCleanPrice", "MMRAcquisitionRetailAveragePrice",
                  "MMRAcquisitionRetailCleanPrice", varnames_price_ratios)
freq_tab.l <- lapply(varnames_tab, df = train1, function(x, df) {
                       varname_x.sym <- rlang::sym(x)
                       df_m <- df %>% dplyr::filter(is.na(!!varname_x.sym))
                       out <- tab_var(df_m, x)
})
names(freq_tab.l) <- varnames_tab
freq_tab_missing_conti_x.l <- freq_tab.l


#y overall
y_tab <- tab_var(train1, varname_y)
y_tab <- y_tab %>% 
  mutate(denom = nrow(train1), share = n / denom)




#structure report table tabulating missingness
tidy_missing_tab <- function(df_tab) {
  
  series <- colnames(df_tab)[1]
  if (nrow(df_tab) == 0) out <- data.frame("series" = series, "n" = 0, stringsAsFactors = FALSE)
  else out <- df_tab[, -1] %>% mutate(series = series)
  
  return(out)  
  
}

freq_tab_missing_discrete_x <- bind_rows(lapply(freq_tab_missing_discrete_x.l, FUN = tidy_missing_tab) )
freq_tab_missing_conti_x <- bind_rows(lapply(freq_tab_missing_conti_x.l, FUN = tidy_missing_tab) )
freq_tab_missing_x <- bind_rows(freq_tab_missing_discrete_x, freq_tab_missing_conti_x)
freq_tab_missing_x <- left_join(freq_tab_missing_x, varname_desc_tbl, by = c("series" = "Field Name"))
n_obs_train1 <- nrow(train1)

save(freq_tab_missing_x, n_obs_train1, y_tab,
     file = paste(path_data_processed, "freq_tab_missing_x.RData", sep = ""))



#end univariate -----------------------------







#multivariate --------------------------

#Size - Make - Model_hmnz
freq_tab_size_make_model_hmnz <- 
  tab_vars(df = train1, varnames = c("Size", "Make", "Model_hmnz"))

#for report -- for each size, rank make-model_hmnz by sample size, take top 3
freq_tab_size_make_model_hmnz_rank <- freq_tab_size_make_model_hmnz %>% 
  arrange(Size, desc(n)) %>% 
  group_by(Size) %>% 
  mutate(rank = row_number())

freq_tab_size_make_model_hmnz_top3 <- freq_tab_size_make_model_hmnz_rank %>% 
  dplyr::filter(rank <= 3)


#BYRNO-PurchDate-Make-Model_hmnz-VehYear-Mileage bin
#GOAL: test assumption of independence between vehicles
freq_tab_byrno_date_make_model_hmnz_year <- 
  tab_vars(df = train1, varnames = c("BYRNO", "PurchDate", "Make", "Model_hmnz"))

freq_tab_byrno_date_make_model_hmnz_year_milesbin_isBad <- 
  tab_vars(df = train1, varnames = c("BYRNO", "PurchDate", "Make", "Model_hmnz", "VehYear", "VehOdo_10tile", varname_y))
sum(freq_tab_byrno_date_make_model_hmnz_year_milesbin_isBad %>% dplyr::filter(n > 1 & IsBadBuy == 1) %>% pull(n) )
table(freq_tab_byrno_date_make_model_hmnz_year_milesbin_isBad %>% dplyr::filter(IsBadBuy == 1) %>% pull(n) )
table(freq_tab_byrno_date_make_model_hmnz_year_milesbin_isBad %>% dplyr::filter(IsBadBuy == 0) %>% pull(n) )

# freq_tab_byrno_date_make_model_hmnz_year_milesbin_isBad <- 
#   left_join(freq_tab_byrno_date_make_model_hmnz_year_milesbin_isBad, 
#             freq_tab_byrno_date_make_model_hmnz_year %>% rename(denom = n)) %>% 
#   mutate(share = n / denom)


save(freq_tab_size_make_model_hmnz_top3,
     file = paste(path_data_processed, "freq_tab_multivar.RData", sep = ""))


#does VehicleAge == PurchDate_year - VehYear?
table(train1[["VehicleAge"]] - ( train1[["PurchDate_year"]] - train1[["VehYear"]] ) )







stop()

#end for doc ----------------















#year, make, model
year_age_make_model_isBad_tab <- tab_vars(df = train1, varnames = c("VehYear", "VehicleAge", "Make", "Model", varname_y))
year_age_make_model_tab <- tab_vars(df = train1, varnames = c("VehYear", "VehicleAge", "Make", "Model"))
year_age_make_model_isBad_tab <- left_join(year_age_make_model_isBad_tab, 
                                           year_age_make_model_tab %>% rename(denom = n) ) %>%  
  mutate(share = n / denom)

year_make_model_isBad_tab <- tab_vars(df = train1, varnames = c("VehYear", "Make", "Model", varname_y))
year_make_model_tab <- tab_vars(df = train1, varnames = c("VehYear", "Make", "Model"))
year_make_model_isBad_tab <- left_join(year_make_model_isBad_tab, 
                                       year_make_model_tab %>% rename(denom = n) ) %>%  
                                mutate(share = n / denom)
year_make_model_isBad_tab <- left_join(year_make_model_isBad_tab, 
                                  y_tab %>% 
                                    dplyr::select(-denom, -n) %>% 
                                    rename(share_overall = share) ) %>%  
  mutate(diff_share_v_benchmark = share - share_overall)

#by-year-make-model variability around overall freq tabulation?
dev.new()
ggplot(year_make_model_isBad_tab %>% dplyr::filter(!!varname_y.sym == 1)) + 
  geom_density(aes(share)) + geom_rug(aes(share)) + 
  geom_vline(aes(xintercept = share_overall)) + 
  labs(title = "By-year-make-model variation around overall prb of y == 1")




#make model
make_model_isBad_tab <- tab_vars(df = train1, varnames = c("Make", "Model", varname_y))
make_model_tab <- tab_vars(df = train1, varnames = c("Make", "Model"))
make_model_isBad_tab <- left_join(make_model_isBad_tab, 
                                  make_model_tab %>% rename(denom = n) ) %>%  
  mutate(share = n / denom)
make_model_isBad_tab <- left_join(make_model_isBad_tab, 
                                   y_tab %>% 
                                     dplyr::select(-denom, -n) %>% 
                                     rename(share_overall = share) ) %>%  
  mutate(diff_share_v_benchmark = share - share_overall)

#by-make-model variability around overall freq tabulation?
dev.new()
ggplot(make_model_isBad_tab %>% dplyr::filter(!!varname_y.sym == 1)) + 
  geom_density(aes(share)) + geom_rug(aes(share)) + 
  geom_vline(aes(xintercept = share_overall)) + 
  labs(title = "By-make-model variation around overall prb of y == 1")



#make model_hmnz
make_model_hmnz_isBad_tab <- tab_vars(df = left_join(train1, make_model_hmnz %>% dplyr::select(-n)), 
                                      varnames = c("Make", "Model_hmnz", varname_y))





#by-mfg year variability around make-model freq tabulation?
year_make_model_isBad_tab <- left_join(year_make_model_isBad_tab, 
                                       make_model_isBad_tab %>% 
                                         dplyr::select(Make, Model, !!varname_y.sym, share) %>% 
                                         rename(share_make_model = share)) %>% 
  mutate(diff_share_v_benchmark = share - share_make_model)
dev.new()
ggplot(year_make_model_isBad_tab %>% dplyr::filter(!!varname_y.sym == 1)) + 
  geom_density(aes(diff_share_v_benchmark)) + geom_rug(aes(diff_share_v_benchmark)) + 
  labs(title = "By-year variation around make-model prb of y == 1")











year_age_make_model_isBad_tab <- left_join(year_age_make_model_isBad_tab, 
                                           make_model_isBad_tab %>% 
                                         dplyr::select(-c(n, denom)) %>% 
                                         rename(share_make_model = share)) %>% 
  mutate(diff_share_v_benchmark = share - share_make_model)






#is there by-mfg-year variability around make-model tabulation?
ggplot(year_age_make_model_isBad_tab %>% dplyr::filter(!!varname_y.sym == 1)) + 
  geom_density(aes(diff_share_v_benchmark)) + 
  geom_rug(aes(diff_share_v_benchmark)) + 
  facet_wrap(~VehicleAge)





#year, make, harmonized model
year_make_model_hmnz_isBad_tab <- tab_vars(df = left_join(train1, make_model_hmnz %>% dplyr::select(-n) ), 
                                           varnames = c("VehYear", "Make", "Model_hmnz", varname_y))
year_make_model_hmnz_tab <- tab_vars(df = left_join(train1, make_model_hmnz %>% dplyr::select(-n) ), 
                                     varnames = c("VehYear", "Make", "Model_hmnz"))
year_make_model_hmnz_isBad_tab <- left_join(year_make_model_hmnz_isBad_tab, 
                                            year_make_model_hmnz_tab %>% rename(denom = n) ) %>%  
  mutate(share = n / denom)

year_make_model_hmnz_isBad_tab <- left_join(year_make_model_hmnz_isBad_tab, 
                                       y_tab %>% 
                                         dplyr::select(-denom, -n) %>% 
                                         rename(share_overall = share) ) %>%  
  mutate(diff_share_v_benchmark = share - share_overall)

#by-year-make-model_hmnz variability around overall freq tabulation?
dev.new()
ggplot(year_make_model_hmnz_isBad_tab %>% dplyr::filter(!!varname_y.sym == 1)) + 
  geom_density(aes(share)) + geom_rug(aes(share)) + 
  geom_vline(aes(xintercept = share_overall)) + 
  labs(title = "By-year-make-model_hmnz variation around overall prb of y == 1")




#make, harmonized model
make_model_hmnz_isBad_tab <- tab_vars(df = left_join(train1, make_model_hmnz %>% dplyr::select(-n) ), 
                                           varnames = c("Make", "Model_hmnz", varname_y))
make_model_hmnz_tab <- tab_vars(df = left_join(train1, make_model_hmnz %>% dplyr::select(-n) ), 
                                     varnames = c("Make", "Model_hmnz"))
make_model_hmnz_isBad_tab <- left_join(make_model_hmnz_isBad_tab, 
                                       make_model_hmnz_tab %>% rename(denom = n) ) %>%  
  mutate(share = n / denom)

make_model_hmnz_isBad_tab <- left_join(make_model_hmnz_isBad_tab, 
                                            y_tab %>% 
                                              dplyr::select(-denom, -n) %>% 
                                              rename(share_overall = share) ) %>%  
  mutate(diff_share_v_benchmark = share - share_overall)

#by-year-make-model_hmnz variability around overall freq tabulation?
dev.new()
ggplot(make_model_hmnz_isBad_tab %>% dplyr::filter(!!varname_y.sym == 1)) + 
  geom_density(aes(share)) + geom_rug(aes(share)) + 
  geom_vline(aes(xintercept = share_overall)) + 
  labs(title = "By-make-model_hmnz variation around overall prb of y == 1")




is_base_model_tab <- tab_vars(df = left_join(train1, make_model_hmnz %>% dplyr::select(-n)),
                             varnames = c("Model_is_base", varname_y) )




#end multivariate --------------------------
