##  Import packages for clusteranalysis and set options
require(dplyr)
require(broom)
require(ggplot2)
require(ggdendro)
require(cluster)
library(fpc) #bootstrapping
options(max.print=100000000)

# Import Dataset
library(readxl)
data.Clustering <- read_excel("data_gyr.xlsx")
View(data.Clustering)

# Sample for validation, 80% of subjects from original data matrix; exchange data.Clustering with data_validation in following analyses code 
# for getting results of validation sample:

# data_validaton<- sample_n(data.Clustering, 822)
# write.csv(data_validaton, "Data_Validation2")



# Choose method for cluster analysis
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(data.Clustering, method = x)$ac
} 

# get agglomerative coefficient for each linkage method
purrr::map_dbl(m, ac) 


#----------Hierarchical Clusteranalysis with agnes language----------#

##  compute dissimilarity matrix
data_dis <- daisy(data.Clustering, metric="euclidean", stand=FALSE)

##  main clusteranalysis !!using the computed dissimilarity matrix!!
h.clusterEx <-  agnes(data_dis, method = "ward")
summary(h.clusterEx)
ggdendrogram(h.clusterEx) #plot dendrogramm


##  Export output of h.cluster individually 
write.csv(h.clusterEx[["order"]], "Output_order.csv")
write.csv(h.clusterEx[["height"]], "Output_distances.csv")
write.csv(h.clusterEx[["merge"]], "Output_fusion.csv")

##  Screeplot hierarchical clustering to inspect for best clustering solution 
multi.clust <- data.frame(k = 1:10) %>% group_by(k) %>% do(clust = kmeans(data_dis, .$k))
sumsq.clust <- multi.clust %>% group_by(k) %>% do(glance(.$clust[[1]]))
ggplot(sumsq.clust, aes(k, tot.withinss)) + geom_line() + geom_point() + scale_x_continuous(breaks=1:10)

##  Choose k, Cut Tree, plot dendrogram and save cluster classification for chosen k
cluster_groups <- cutree(as.hclust(h.clusterEx), k=3) 
write.csv(cluster_groups, "cluster_groups.csv")
plot(h.clusterEx, which.plots=3, main="Tree") ##Plot Cut Tree
rect.hclust(as.hclust(h.clusterEx), k=3, border="red") 
table(cluster_groups)

##  Bootstrapping for chosen k 
k = 3 
stab <- clusterboot(dis_corr, B=100, distances=TRUE, bootmethod="boot", clustermethod=hclustCBI, k=k, cut="number", method="ward.D2", showplots=FALSE)
stab
