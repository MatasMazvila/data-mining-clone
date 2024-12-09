#------------------------------------------------------------------------------
############## Hierarchical Agglomerative Method ##############
#------------------------------------------------------------------------------
# Libraries
library(readr)
library(cluster)
library(ggplot2)
library(dbscan)
library(kernlab)
library(dplyr)
library(igraph)
library(mclust)
library(data.table)
library(factoextra)
library(plotly)
library(hopkins)
library(Rtsne)
library(uwot)
library(pheatmap)
library(dendextend)
library(gridExtra)
#------------------------------------------------------------------------------
############## DATA ##############
#------------------------------------------------------------------------------
channel_info <- read_csv("data/channel_info.csv")
commenters_jaccard <- read_csv("data/comment_jaccard_matrix.csv")
commenters_overlap <- read_csv("data/comment_overlap_matrix.csv")
subs_jaccard <- read_csv("data/subs_jaccard_matrix.csv")
subs_overlap <- read_csv("data/subs_overlap_matrix.csv")
#------------------------------------------------------------------------------
############## Commenters Jaccard Dataset (HAC) ##############
#------------------------------------------------------------------------------

# Ensure that information and distance matrix line up
stopifnot(all(channel_info$title == commenters_jaccard$title))

# Define similarity as matrix
similarity_matrix_hac_cj <- as.matrix(commenters_jaccard[, -1])

# Remove dimnames to avoid warnings
rownames(similarity_matrix_hac_cj) <- NULL
colnames(similarity_matrix_hac_cj) <- NULL

# Convert similarity matrix to a distance matrix
distance_matrix_hac_cj <- as.dist(1 - similarity_matrix_hac_cj)

# Multidimensional Scaling (MDS) for Visualization
mds_coords_hac_cj <- cmdscale(distance_matrix_hac_cj, k = 2)

#------------------------------------------------------------------------------
############## Hierarchical Agglomerative Clustering ##############
#------------------------------------------------------------------------------

# Perform hierarchical clustering using Ward's method
hc_result_hac_cj <- hclust(distance_matrix_hac_cj, method = "ward.D2")

# Plot the dendrogram
plot(hc_result_hac_cj, main = "Agglomerative Hierarchical Clustering (Commenters Jaccard)", 
     sub = "", xlab = "", cex = 0.8)

# Set range of clusters to evaluate
k_values_hac_cj <- 2:15
set.seed(123)
# Compute silhouette scores for each number of clusters
silhouette_scores_hac_cj <- sapply(k_values_hac_cj, function(k) {
  # Cut the dendrogram into k clusters
  clusters_hac_cj <- cutree(hc_result_hac_cj, k)
  # Compute silhouette scores directly
  silhouette_result_hac_cj <- silhouette(clusters_hac_cj, distance_matrix_hac_cj)
  # Return the mean silhouette score
  mean(silhouette_result_hac_cj[, 3], na.rm = TRUE) # Avoid NA issues
})

# Plot Silhouette Scores
plot(k_values_hac_cj, silhouette_scores_hac_cj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for HAC (Commenters Jaccard)")

set.seed(123)
# Compute total within-cluster sum of squares (WSS) for each k
wss_hac_cj <- sapply(k_values_hac_cj, function(k) {
  # Cut the dendrogram into k clusters
  clusters_hac_cj <- cutree(hc_result_hac_cj, k)
  # Calculate WSS for each cluster and sum them up
  cluster_wss_hac_cj <- sapply(unique(clusters_hac_cj), function(cluster) {
    cluster_points_hac_cj <- which(clusters_hac_cj == cluster)
    cluster_distances_hac_cj <- as.matrix(distance_matrix_hac_cj)[cluster_points_hac_cj, cluster_points_hac_cj]
    mean(as.matrix(cluster_distances_hac_cj)) * length(cluster_points_hac_cj)
  })
  sum(cluster_wss_hac_cj)
})
# Plot the Elbow Curve
plot(k_values_hac_cj, wss_hac_cj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for HAC (Commenters Jaccard)")

# Cut the dendrogram into clusters
k_hac_cj <- 5 # visual insoection
hc_clusters_hac_cj <- cutree(hc_result_hac_cj, k = k_hac_cj)

# Add cluster labels to the dataset
channel_info$hac_cluster_cj <- hc_clusters_hac_cj

# Visualize clusters using MDS
visualization_data_hac_cj <- data.frame(
  X1 = mds_coords_hac_cj[, 1],
  X2 = mds_coords_hac_cj[, 2],
  cluster = as.factor(hc_clusters_hac_cj)
)

# Plot with channel names
ggplot(visualization_data_hac_cj, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "HAC Clustering (Commenters Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#------------------------------------------------------------------------------
############## Commenters Overlap Dataset (HAC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == commenters_overlap$title))

similarity_matrix_hac_co <- as.matrix(commenters_overlap[, -1])

rownames(similarity_matrix_hac_co) <- NULL
colnames(similarity_matrix_hac_co) <- NULL

distance_matrix_hac_co <- as.dist(1 - similarity_matrix_hac_co)

mds_coords_hac_co <- cmdscale(distance_matrix_hac_co, k = 2)

hc_result_hac_co <- hclust(distance_matrix_hac_co, method = "ward.D2")

plot(hc_result_hac_co, main = "Agglomerative Hierarchical Clustering (Commenters Overlap)", 
     sub = "", xlab = "", cex = 0.8)

k_values_hac_co <- 2:15
set.seed(123)
silhouette_scores_hac_co <- sapply(k_values_hac_co, function(k) {
  clusters_hac_co <- cutree(hc_result_hac_co, k)
  silhouette_result_hac_co <- silhouette(clusters_hac_co, distance_matrix_hac_co)
  mean(silhouette_result_hac_co[, 3], na.rm = TRUE)
})

plot(k_values_hac_co, silhouette_scores_hac_co, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for HAC (Commenters Overlap)")

set.seed(123)
wss_hac_co <- sapply(k_values_hac_co, function(k) {
  clusters_hac_co <- cutree(hc_result_hac_co, k)
  cluster_wss_hac_co <- sapply(unique(clusters_hac_co), function(cluster) {
    cluster_points_hac_co <- which(clusters_hac_co == cluster)
    cluster_distances_hac_co <- as.matrix(distance_matrix_hac_co)[cluster_points_hac_co, cluster_points_hac_co]
    mean(as.matrix(cluster_distances_hac_co)) * length(cluster_points_hac_co)
  })
  sum(cluster_wss_hac_co)
})

plot(k_values_hac_co, wss_hac_co, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for HAC (Commenters Overlap)")

k_hac_co <- 4 # visual insoection
hc_clusters_hac_co <- cutree(hc_result_hac_co, k = k_hac_co)

channel_info$hac_cluster_co <- hc_clusters_hac_co

visualization_data_hac_co <- data.frame(
  X1 = mds_coords_hac_co[, 1],
  X2 = mds_coords_hac_co[, 2],
  cluster = as.factor(hc_clusters_hac_co)
)

ggplot(visualization_data_hac_co, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "HAC Clustering (Commenters Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#------------------------------------------------------------------------------
############## Subs Jaccard Dataset (HAC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_jaccard$title))

similarity_matrix_hac_sj <- as.matrix(subs_jaccard[, -1])

rownames(similarity_matrix_hac_sj) <- NULL
colnames(similarity_matrix_hac_sj) <- NULL

distance_matrix_hac_sj <- as.dist(1 - similarity_matrix_hac_sj)

mds_coords_hac_sj <- cmdscale(distance_matrix_hac_sj, k = 2)

hc_result_hac_sj <- hclust(distance_matrix_hac_sj, method = "ward.D2")

plot(hc_result_hac_sj, main = "Agglomerative Hierarchical Clustering (Subs Jaccard)", 
     sub = "", xlab = "", cex = 0.8)

k_values_hac_sj <- 2:15
set.seed(123)
silhouette_scores_hac_sj <- sapply(k_values_hac_sj, function(k) {
  clusters_hac_sj <- cutree(hc_result_hac_sj, k)
  silhouette_result_hac_sj <- silhouette(clusters_hac_sj, distance_matrix_hac_sj)
  mean(silhouette_result_hac_sj[, 3], na.rm = TRUE)
})

plot(k_values_hac_sj, silhouette_scores_hac_sj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for HAC (Subs Jaccard)")

set.seed(123)
wss_hac_sj <- sapply(k_values_hac_sj, function(k) {
  clusters_hac_sj <- cutree(hc_result_hac_sj, k)
  cluster_wss_hac_sj <- sapply(unique(clusters_hac_sj), function(cluster) {
    cluster_points_hac_sj <- which(clusters_hac_sj == cluster)
    cluster_distances_hac_sj <- as.matrix(distance_matrix_hac_sj)[cluster_points_hac_sj, cluster_points_hac_sj]
    mean(as.matrix(cluster_distances_hac_sj)) * length(cluster_points_hac_sj)
  })
  sum(cluster_wss_hac_sj)
})

plot(k_values_hac_sj, wss_hac_sj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for HAC (Subs Jaccard)")

k_hac_sj <- 4 # visual insoection
hc_clusters_hac_sj <- cutree(hc_result_hac_sj, k = k_hac_sj)

channel_info$hac_cluster_sj <- hc_clusters_hac_sj

visualization_data_hac_sj <- data.frame(
  X1 = mds_coords_hac_sj[, 1],
  X2 = mds_coords_hac_sj[, 2],
  cluster = as.factor(hc_clusters_hac_sj)
)

ggplot(visualization_data_hac_sj, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "HAC Clustering (Subs Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#------------------------------------------------------------------------------
############## Subs Overlap Dataset (HAC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_overlap$title))

similarity_matrix_hac_so <- as.matrix(subs_overlap[, -1])

rownames(similarity_matrix_hac_so) <- NULL
colnames(similarity_matrix_hac_so) <- NULL

distance_matrix_hac_so <- as.dist(1 - similarity_matrix_hac_so)

mds_coords_hac_so <- cmdscale(distance_matrix_hac_so, k = 2)

hc_result_hac_so <- hclust(distance_matrix_hac_so, method = "ward.D2")

plot(hc_result_hac_so, main = "Agglomerative Hierarchical Clustering (Subs Overlap)", 
     sub = "", xlab = "", cex = 0.8)

k_values_hac_so <- 2:15
set.seed(123)
silhouette_scores_hac_so <- sapply(k_values_hac_so, function(k) {
  clusters_hac_so <- cutree(hc_result_hac_so, k)
  silhouette_result_hac_so <- silhouette(clusters_hac_so, distance_matrix_hac_so)
  mean(silhouette_result_hac_so[, 3], na.rm = TRUE)
})

plot(k_values_hac_so, silhouette_scores_hac_so, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for HAC (Subs Overlap)")

set.seed(123)
wss_hac_so <- sapply(k_values_hac_so, function(k) {
  clusters_hac_so <- cutree(hc_result_hac_so, k)
  cluster_wss_hac_so <- sapply(unique(clusters_hac_so), function(cluster) {
    cluster_points_hac_so <- which(clusters_hac_so == cluster)
    cluster_distances_hac_so <- as.matrix(distance_matrix_hac_so)[cluster_points_hac_so, cluster_points_hac_so]
    mean(as.matrix(cluster_distances_hac_so)) * length(cluster_points_hac_so)
  })
  sum(cluster_wss_hac_so)
})

plot(k_values_hac_so, wss_hac_so, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for HAC (Subs Overlap)")

k_hac_so <- 5 # visual inspection
hc_clusters_hac_so <- cutree(hc_result_hac_so, k = k_hac_so)

channel_info$hac_cluster_so <- hc_clusters_hac_so

visualization_data_hac_so <- data.frame(
  X1 = mds_coords_hac_so[, 1],
  X2 = mds_coords_hac_so[, 2],
  cluster = as.factor(hc_clusters_hac_so)
)

ggplot(visualization_data_hac_so, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "HAC Clustering (Subs Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

