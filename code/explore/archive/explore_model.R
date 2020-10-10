#explore_model.R


#broad models --------------------------------


mdl_expr <- as.formula(paste(varname_y, " ~ factor(Make)", sep=""))
mdl <- mgcv::gam(mdl_expr, data = train1, family = "binomial")
mdl1 <- mdl

mdl_expr <- as.formula(paste(varname_y, " ~ factor(Make) + s(VehicleAge, bs = 'cr')", sep=""))
mdl <- mgcv::gam(mdl_expr, data = train1, family = "binomial")
mdl2 <- mdl

anova(mdl1, mdl2, test = "Chisq")

#end broad models --------------------------------







mdl_expr <- 
  as.formula(paste(varname_y, " ~ ",
                              " factor(Make)", 
                              " + factor(VehYear)",
                              " + factor(VehicleAge)",
                              " + factor(WheelType)",
                              # " + factor(Transmission)",
                              # " + factor(VNST)", 
                              # " + factor(Auction)",
                              # " + s(VehicleAge, by = factor(VehYear), bs = 'cr', k = 9)",
                              # " + s(VehicleAge, by = factor(VehYear), bs = 'cr', k = 9)",
                              " + s(VehOdo, by = factor(VehicleAge), bs = 'cr')",
                              " + s(VehOdo, by = factor(Make), bs = 'cr')",
                              " + s(VehicleAge, by = factor(Make), bs = 'cr')",
                              # " + s(VehOdo, by = factor(Transmission), bs = 'cr')",
                   sep=""))
data_sub <- left_join(train1, make_model_hmnz %>% dplyr::select(-n)) #%>% 
                # dplyr::filter(Make == "CHRYSLER" & Model_hmnz == "PT CRUISER")
# mdl <- mgcv::gam(mdl_expr, data=data_sub, family = "binomial")

library(parallel)
nc <- detectCores() - 2
cl <- makeCluster(nc)
mdl <- mgcv::bam(mdl_expr, data=data_sub, family = "binomial", cluster=cl)
stopCluster(cl)

summary(mdl)
# gam.check(mdl)
# 
# predict_mdl <- cbind(data_sub,
#                      "yhat" = predict.gam(mdl, newdata = data_sub, type = "response"))
# predict_mdl <- predict_mdl %>% mutate(yhat_class = round(yhat))
# View(predict_mdl %>% group_by(IsBadBuy, yhat_class) %>% summarize(n = n()))
# 
# ggplot(predict_mdl %>% dplyr::filter(VehYear == 2001 & VehicleAge == 8)) + 
#   geom_point(aes(VehOdo, yhat)) + 
#   geom_line(aes(VehOdo, yhat)) + 
#   facet_wrap(~Auction)
