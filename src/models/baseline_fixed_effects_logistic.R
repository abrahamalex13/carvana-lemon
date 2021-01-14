#baseline_fixed_effects_logistic.R

cl <- makeCluster(8)
registerDoParallel(cl)

mdl <- mgcv::bam(IsBadBuy ~ Make_consol + VehYear_consol + Size_consol + WheelType 
                 
                 # + s(VehOdo_log, bs="cr", by = factor(Make_consol)) +  
                 + RatioWarrantyVehBCost 
             
                 + Auction
                 + AUCGUART 
                 + VNST_consol
                 + factor(VNZIP1_consol)
                 + factor(BYRNO_consol)
                 
                 + factor(Make_Model_consol) + factor(Make_VehYear_consol)
                 # + s(VehOdo_log, bs="cr", by = factor(Make_Model_consol))
                 
                 + factor(Make_Model_SubModel_consol)
                 + factor(Make_Model_Trim_consol)
                 + factor(Make_Model_VehYear_consol)
                 
                 + factor(Make_Model_SubModel_Trim_consol)
                 + factor(Make_Model_SubModel_VehYear_consol)
                 
                 + factor(Make_Model_SubModel_Trim_VehYear_consol)
                 ,
               
               data = train1_keep, family = binomial(), cluster = cl)

stopCluster(cl)
registerDoSEQ()

summary(mdl)

pred <- predict.bam(mdl, newdata = train1_excl, type = "response")
chk <- data.frame("actual" = train1_excl[["IsBadBuy"]], 
                  "predicted" = round(pred, 0))
chk_summ <- chk %>% 
  group_by(actual, predicted) %>% 
  summarize(n = n())
chk_summ
