########################################################################################
########################################################################################
### ======= ------- MÓDULO 1 - Agrupamento ------- ======= ###
###
###                     - Questão 1
### 
#Execute o algoritmo K-Means tendo como posição inicial dos centroides as k primeiras linhas do arquivo.
#
.libPaths("C:/Program Files/R/R-3.5.1/library")
library(data.table)
library(tidyverse)
library(parallel)
library(doParallel)
library(ggplot2)
library(cluster)
library(factoextra)


d_kMeans <- data.table::fread("E:\\base1.csv", na.strings = 'NA') %>% data.frame()
d_centroides <- data.table::fread("E:\\base2.csv", na.strings = 'NA') %>% data.frame()

## === factoextra contains many functions for cluster analysis and visualization, including: === ##
## = Functions                     Description
#=) dist(fviz_dist, get_dist)      Distance Matrix Computation and Visualization
#=) get_clust_tendency             Assessing Clustering Tendency
#=) fviz_nbclust(fviz_gap_stat)    Determining the Optimal Number of Clusters
#=) fviz_dend                      Enhanced Visualization of Dendrogram
#=) fviz_cluster                   Visualize Clustering Results
#=) fviz_mclust                    Visualize Model-based Clustering Results
#=) fviz_silhouette                Visualize Silhouette Information from Clustering
#=) hcut                           Computes Hierarchical Clustering and Cut the Tree
#=) hkmeans                        Hierarchical k-means clustering
#=) eclust                         Visual enhancement of clustering analysis

# == Analyse the distribution of the variable == #
d_kMeans %>% ggplot() + geom_histogram(mapping = aes(x = V3),binwidth = 0.008)

# == Descriptive analysis of all variable to see if there are normalized ....
# u(0,1)
summary(d_kMeans)

# == To remove any missing value that might be present in the data == #
d_kMeans_complete <- d_kMeans[complete.cases(d_kMeans),] 

# == The data must be standardized (i.e., scaled) to make variables comparable. Recall
# that, standardization consists of transforming the variables such that they have
# mean zero and standard deviation one. The value of distance measures is intimately 
# related to the scale on which measurements are made.
d_kMeans <- d_kMeans_complete %>% scale() %>% data.frame()


# == Clustering Distance Measures == #
##"euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski".
dist_kMeans_complete_1o <- d_kMeans %>% dist(method = "euclidean")

#"euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", 
#"spearman" or "kendall".
dist_kMeans_complete_2o <- d_kMeans %>% get_dist(method = "euclidean") 

#"euclidean" (the default), "manhattan" and "gower".
dist_kMeans_complete_3o <- d_kMeans %>% daisy(metric = "euclidean") 

# Subset the first 3 columns and rows and Round the values
round(as.matrix(dist_kMeans_complete_1o)[1:5, 1:5], 5)


# === Visualizing distance matrices === #
# fviz_dist(dist_kMeans_complete_1o) # Very slowly


# === -----------------  Partitioning clustering  ------------------------------- === #

km.res <- kmeans(d_kMeans_complete, centers = d_centroides[1:8,],iter.max = 10, 
                 nstart = 1, algorithm = c("Hartigan-Wong"), trace = TRUE)
print(km.res)


# = Estimating the optimal number of clusters = #
# OBS: I used three method "silhouette", "wss", "gap_stat". The 'gap_stat' is very slowly
fviz_nbclust(d_kMeans, kmeans, method = "wss") +
  geom_vline(xintercept = 8, linetype = 2)



##                   Plus analysis from Questão 1                                 ##
# It's possible to compute the mean of each variables by clusters using the original data:
aggregate(d_kMeans_complete, by=list(cluster=km.res$cluster), mean)

#If you want to add the point classifications to the original data, use this:
d_kMeans_complete_new <- cbind(d_kMeans_complete, cluster = km.res$cluster)
head(d_kMeans_complete_new)


#Visualizing k-means clusters. OBS: Very Slowly
fviz_cluster(km.res, data = d_kMeans_complete,
             palette = rainbow(8),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)
