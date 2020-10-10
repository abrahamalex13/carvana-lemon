#unsuper_conti_features.R
#do standardized continuous features measure similar variation?

varnames_conti <- c("VehOdo_log",
                    "RatioCostAuctionAverage", "RatioCostRetailAverage",
                    "RatioCostAuctionClean", "RatioCostRetailClean")
varnames_conti.syms <- rlang::syms(varnames_conti)

df <- train1
#as n increases, may want to avoid hierarchical clustering computation times.
do_hierarchical <- FALSE



X_conti_std <- apply(df %>% dplyr::select(!!!varnames_conti.syms), 
                     MARGIN = 2, FUN = function(x) (x - mean(x)) / sd(x) )


#pca ----------------

X_svd <- svd(X_conti_std)
X_pca_z <- X_svd$u %*% diag(X_svd$d)
var_X_pca_z <- apply(X_pca_z, 2, function(x) var(x))
cumsum(var_X_pca_z) / ncol(X_pca_z)

#ex: col1 conveys - to move 1 unit in x1 direction, 
#move a units in PC1 direction; b units in PC2 direction; and on down the col.
t(X_svd$v^2)


#end pca -----------




# clustering ------

#k-means
#k determined by lemon outcomes
#rationale for 4 - clear lemon; borderline lemon; borderline clean; high clean
k <- 4
km <- kmeans(X_conti_std[, c("VehOdo_log", "RatioCostAuctionAverage")], centers = k)
df_plot <- data.frame(X_conti_std, "cluster" = km$cluster)
ggplot() + 
  geom_point(data = df_plot, aes(VehOdo_log, RatioCostAuctionAverage, color = factor(cluster)))



#hierarchical
if (do_hierarchical) {
  
  Dist <- dist(X_conti_std[, c("VehOdo_log", "RatioCostAuctionAverage")])
  hclust_single <- hclust(Dist, method = "single")
  hclust_complete <- hclust(Dist, method = "complete")
  hclust_avg <- hclust(Dist, method = "average")
  
  df_plot <- data.frame(X_conti_std, 
                        "cluster" = cutree(hclust_avg, k = k))
  ggplot(df_plot) + 
    geom_point(aes(VehOdo_log, RatioCostAuctionAverage, color = factor(cluster)))
  
}

# ------------