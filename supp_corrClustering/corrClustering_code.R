##Import packages for clusteranalysis and set options
require(dplyr)
require(broom)
require(ggplot2)
require(ggdendro)
require(cluster)
library(fpc) #bootstrapping
options(max.print=100000000)

##Choose data for clusteranalysis
###data Gyrification
data.Clustering <- select( 
  Patients_NoOutliers_June2023,
  Gyrification_lbankssts,
  Gyrification_rbankssts,
  Gyrification_lcaudalanteriorcingulate,
  Gyrification_rcaudalanteriorcingulate,
  Gyrification_lcaudalmiddlefrontal,
  Gyrification_rcaudalmiddlefrontal,
  Gyrification_lcuneus,
  Gyrification_rcuneus,
  Gyrification_lentorhinal,
  Gyrification_rentorhinal,
  Gyrification_lfusiform,
  Gyrification_rfusiform,
  Gyrification_linferiorparietal,
  Gyrification_rinferiorparietal,
  Gyrification_linferiortemporal,
  Gyrification_rinferiortemporal,
  Gyrification_listhmuscingulate,
  Gyrification_risthmuscingulate,
  Gyrification_llateraloccipital,
  Gyrification_rlateraloccipital,
  Gyrification_llateralorbitofrontal,
  Gyrification_rlateralorbitofrontal,
  Gyrification_llingual,
  Gyrification_rlingual,
  Gyrification_lmedialorbitofrontal,
  Gyrification_rmedialorbitofrontal,
  Gyrification_lmiddletemporal,
  Gyrification_rmiddletemporal,
  Gyrification_lparahippocampal,
  Gyrification_rparahippocampal,
  Gyrification_lparacentral,
  Gyrification_rparacentral,
  Gyrification_lparsopercularis,
  Gyrification_rparsopercularis,
  Gyrification_lparsorbitalis,
  Gyrification_rparsorbitalis,
  Gyrification_lparstriangularis,
  Gyrification_rparstriangularis,
  Gyrification_lpericalcarine,
  Gyrification_rpericalcarine,
  Gyrification_lpostcentral,
  Gyrification_rpostcentral,
  Gyrification_lposteriorcingulate,
  Gyrification_rposteriorcingulate,
  Gyrification_lprecentral,
  Gyrification_rprecentral,
  Gyrification_lprecuneus,
  Gyrification_rprecuneus,
  Gyrification_lrostralanteriorcingulate,
  Gyrification_rrostralanteriorcingulate,
  Gyrification_lrostralmiddlefrontal,
  Gyrification_rrostralmiddlefrontal,
  Gyrification_lsuperiorfrontal,
  Gyrification_rsuperiorfrontal,
  Gyrification_lsuperiorparietal,
  Gyrification_rsuperiorparietal,
  Gyrification_lsuperiortemporal,
  Gyrification_rsuperiortemporal,
  Gyrification_lsupramarginal,
  Gyrification_rsupramarginal,
  Gyrification_lfrontalpole,
  Gyrification_rfrontalpole,
  Gyrification_ltemporalpole,
  Gyrification_rtemporalpole,
  Gyrification_ltransversetemporal,
  Gyrification_rtransversetemporal,
  Gyrification_linsula,
  Gyrification_rinsula
)

#____________________Correlation Clustering________________________________________________#
library(pheatmap)
pheatmap(data.Clustering, scale = "row")

# Pairwise correlation between subjects (rows)
rows.cor <- cor(t(data.Clustering), use = "pairwise.complete.obs", method = "pearson")

# Plot the heatmap
pheatmap(
  data.Clustering, scale = "row", 
  clustering_distance_rows = as.dist(1 - rows.cor)
)

dis_corr <- as.dist(1-cor(t(data.Clustering))) # correlation distance
hclust.row <- hclust(as.dist(1-rows.cor))

# Export Output corr clustering
write.csv(hclust.row[["order"]], "Output_hclusterCorrelationDistance_order.csv")
write.csv(hclust.row[["height"]], "Output_hclusterCorrelationDistance_Distanzen.csv")
write.csv(hclust.row[["merge"]], "Output_hclusterCorrelationDistance_Fusionierung.csv")

##Screeplot hierarchical clustering for best clustering solution 
multi.clust <- data.frame(k = 1:10) %>% group_by(k) %>% do(clust = kmeans(as.dist(1-rows.cor), .$k))
sumsq.clust <- multi.clust %>% group_by(k) %>% do(glance(.$clust[[1]]))
ggplot(sumsq.clust, aes(k, tot.withinss)) + geom_line() + geom_point() + scale_x_continuous(breaks=1:10)

##Choose k, Cut Tree, plot dendrogram and save cluster classification for chosen k
Cluster <- cutree(as.hclust(hclust.row), k=2) 
write.csv(Cluster, "clusterGroups_Corr.csv")
plot(hclust.row, which.plots=3, main="Tree") ##Plot Cut Tree
rect.hclust(as.hclust(hclust.row), k=2, border="red") 
table(Cluster)