#enforce_holdout.R
#before proceeding with any supervised-type work, 
#mitigate overfitting risk.

set.seed(1)
index_row_keep <- createDataPartition(train1[[varname_y]], p = .75, list = F)
train1_keep <- train1[index_row_keep[, 1], ]
train1_excl <- train1[-index_row_keep[, 1], ]