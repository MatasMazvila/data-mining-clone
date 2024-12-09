#------------------------------------------------------------------------------
############## Hierarchical Divisive Clustering (HDC) ##############
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
############## Commenters Jaccard Dataset (HDC) ##############
#------------------------------------------------------------------------------

# Ensure that information and distance matrix line up
stopifnot(all(channel_info$title == commenters_jaccard$title))

# Define similarity as matrix
similarity_matrix_hdc_cj <- as.matrix(commenters_jaccard[, -1])

# Remove dimnames to avoid warnings
rownames(similarity_matrix_hdc_cj) <- NULL
colnames(similarity_matrix_hdc_cj) <- NULL

# Convert similarity matrix to a distance matrix
distance_matrix_hdc_cj <- as.dist(1 - similarity_matrix_hdc_cj)

# Multidimensional Scaling (MDS) for Visualization
mds_coords_hdc_cj <- cmdscale(distance_matrix_hdc_cj, k = 2)

#------------------------------------------------------------------------------
############## Hierarchical Divisive Clustering ##############
#------------------------------------------------------------------------------

# Perform divisive clustering
divisive_result_hdc_cj <- diana(distance_matrix_hdc_cj)

# Plot the dendrogram
plot(as.hclust(divisive_result_hdc_cj), 
     main = "Divisive Hierarchical Clustering (Commenters Jaccard)", 
     sub = "", 
     xlab = "", 
     cex = 0.8)

k_values_hdc_cj <- 2:15
set.seed(123)
# Compute silhouette scores for divisive clustering
silhouette_scores_hdc_cj <- sapply(k_values_hdc_cj, function(k) {
  clusters_hdc_cj <- cutree(as.hclust(divisive_result_hdc_cj), k)
  silhouette_result_hdc_cj <- silhouette(clusters_hdc_cj, distance_matrix_hdc_cj)
  mean(silhouette_result_hdc_cj[, 3], na.rm = TRUE)
})

# Plot silhouette scores
plot(k_values_hdc_cj, silhouette_scores_hdc_cj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for Divisive Clustering (Commenters Jaccard)")

set.seed(123)
# Compute WSS for divisive clustering
wss_hdc_cj <- sapply(k_values_hdc_cj, function(k) {
  clusters_hdc_cj <- cutree(as.hclust(divisive_result_hdc_cj), k)
  cluster_wss_hdc_cj <- sapply(unique(clusters_hdc_cj), function(cluster) {
    cluster_points_hdc_cj <- which(clusters_hdc_cj == cluster)
    cluster_distances_hdc_cj <- as.matrix(distance_matrix_hdc_cj)[cluster_points_hdc_cj, cluster_points_hdc_cj]
    mean(as.matrix(cluster_distances_hdc_cj)) * length(cluster_points_hdc_cj)
  })
  sum(cluster_wss_hdc_cj)
})

# Plot the Elbow Curve
plot(k_values_hdc_cj, wss_hdc_cj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Divisive Clustering (Commenters Jaccard)")

# Cut the dendrogram into 6 clusters
k_hdc_cj <- 6 # silhouette
divisive_clusters_hdc_cj <- cutree(as.hclust(divisive_result_hdc_cj), k = k_hdc_cj)

# Add cluster labels to the dataset
channel_info$hdc_cluster_cj <- divisive_clusters_hdc_cj

# Visualize the clusters using MDS
visualization_data_hdc_cj <- data.frame(
  X1 = mds_coords_hdc_cj[, 1],
  X2 = mds_coords_hdc_cj[, 2],
  cluster = as.factor(divisive_clusters_hdc_cj)
)

# Plot with channel names
ggplot(visualization_data_hdc_cj, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +  # Plot the points
  geom_text(aes(label = channel_info$title),  # Add channel names
            size = 3,  # Adjust text size
            hjust = 0.5, vjust = -0.5,  # Position the labels slightly above the points
            check_overlap = TRUE) +  # Avoid label overlap
  labs(
    title = "Divisive Hierarchical Clustering (Commenters Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend to bottom
    plot.title = element_text(hjust = 0.5)  # Center-align title
  )

#------------------------------------------------------------------------------
############## Commenters Overlap Dataset (HDC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == commenters_overlap$title))

similarity_matrix_hdc_co <- as.matrix(commenters_overlap[, -1])

rownames(similarity_matrix_hdc_co) <- NULL
colnames(similarity_matrix_hdc_co) <- NULL

distance_matrix_hdc_co <- as.dist(1 - similarity_matrix_hdc_co)

mds_coords_hdc_co <- cmdscale(distance_matrix_hdc_co, k = 2)

#------------------------------------------------------------------------------
############## Hierarchical Divisive Clustering ##############
#------------------------------------------------------------------------------

divisive_result_hdc_co <- diana(distance_matrix_hdc_co)

plot(as.hclust(divisive_result_hdc_co), 
     main = "Divisive Hierarchical Clustering (Commenters Overlap)", 
     sub = "", 
     xlab = "", 
     cex = 0.8)

k_values_hdc_co <- 2:30
set.seed(123)
silhouette_scores_hdc_co <- sapply(k_values_hdc_co, function(k) {
  clusters_hdc_co <- cutree(as.hclust(divisive_result_hdc_co), k)
  silhouette_result_hdc_co <- silhouette(clusters_hdc_co, distance_matrix_hdc_co)
  mean(silhouette_result_hdc_co[, 3], na.rm = TRUE)
})

plot(k_values_hdc_co, silhouette_scores_hdc_co, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for Divisive Clustering (Commenters Overlap)")

set.seed(123)
wss_hdc_co <- sapply(k_values_hdc_co, function(k) {
  clusters_hdc_co <- cutree(as.hclust(divisive_result_hdc_co), k)
  cluster_wss_hdc_co <- sapply(unique(clusters_hdc_co), function(cluster) {
    cluster_points_hdc_co <- which(clusters_hdc_co == cluster)
    cluster_distances_hdc_co <- as.matrix(distance_matrix_hdc_co)[cluster_points_hdc_co, cluster_points_hdc_co]
    mean(as.matrix(cluster_distances_hdc_co)) * length(cluster_points_hdc_co)
  })
  sum(cluster_wss_hdc_co)
})

plot(k_values_hdc_co, wss_hdc_co, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Divisive Clustering (Commenters Overlap)")

k_hdc_co <- 6 # prediction
divisive_clusters_hdc_co <- cutree(as.hclust(divisive_result_hdc_co), k = k_hdc_co)

channel_info$hdc_cluster_co <- divisive_clusters_hdc_co

visualization_data_hdc_co <- data.frame(
  X1 = mds_coords_hdc_co[, 1],
  X2 = mds_coords_hdc_co[, 2],
  cluster = as.factor(divisive_clusters_hdc_co)
)

ggplot(visualization_data_hdc_co, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +  
  geom_text(aes(label = channel_info$title),  
            size = 3,  
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) + 
  labs(
    title = "Divisive Hierarchical Clustering (Commenters Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5)  
  )

#------------------------------------------------------------------------------
############## Subs Jaccard Dataset (HDC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_jaccard$title))

similarity_matrix_hdc_sj <- as.matrix(subs_jaccard[, -1])

rownames(similarity_matrix_hdc_sj) <- NULL
colnames(similarity_matrix_hdc_sj) <- NULL

distance_matrix_hdc_sj <- as.dist(1 - similarity_matrix_hdc_sj)


mds_coords_hdc_sj <- cmdscale(distance_matrix_hdc_sj, k = 2)

#------------------------------------------------------------------------------
############## Hierarchical Divisive Clustering ##############
#------------------------------------------------------------------------------

divisive_result_hdc_sj <- diana(distance_matrix_hdc_sj)

plot(as.hclust(divisive_result_hdc_sj), 
     main = "Divisive Hierarchical Clustering (Subs Jaccard)", 
     sub = "", 
     xlab = "", 
     cex = 0.8)

k_values_hdc_sj <- 2:15
set.seed(123)
silhouette_scores_hdc_sj <- sapply(k_values_hdc_sj, function(k) {
  clusters_hdc_sj <- cutree(as.hclust(divisive_result_hdc_sj), k)
  silhouette_result_hdc_sj <- silhouette(clusters_hdc_sj, distance_matrix_hdc_sj)
  mean(silhouette_result_hdc_sj[, 3], na.rm = TRUE)
})

plot(k_values_hdc_sj, silhouette_scores_hdc_sj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for Divisive Clustering (Subs Jaccard)")

set.seed(123)
wss_hdc_sj <- sapply(k_values_hdc_sj, function(k) {
  clusters_hdc_sj <- cutree(as.hclust(divisive_result_hdc_sj), k)
  cluster_wss_hdc_sj <- sapply(unique(clusters_hdc_sj), function(cluster) {
    cluster_points_hdc_sj <- which(clusters_hdc_sj == cluster)
    cluster_distances_hdc_sj <- as.matrix(distance_matrix_hdc_sj)[cluster_points_hdc_sj, cluster_points_hdc_sj]
    mean(as.matrix(cluster_distances_hdc_sj)) * length(cluster_points_hdc_sj)
  })
  sum(cluster_wss_hdc_sj)
})

plot(k_values_hdc_sj, wss_hdc_sj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Divisive Clustering (Subs Jaccard)")

k_hdc_sj <- 3 # prediction
divisive_clusters_hdc_sj <- cutree(as.hclust(divisive_result_hdc_sj), k = k_hdc_sj)

channel_info$hdc_cluster_sj <- divisive_clusters_hdc_sj

visualization_data_hdc_sj <- data.frame(
  X1 = mds_coords_hdc_sj[, 1],
  X2 = mds_coords_hdc_sj[, 2],
  cluster = as.factor(divisive_clusters_hdc_sj)
)

ggplot(visualization_data_hdc_sj, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +  
  geom_text(aes(label = channel_info$title),  
            size = 3,  
            hjust = 0.5, vjust = -0.5,  
            check_overlap = TRUE) + 
  labs(
    title = "Divisive Hierarchical Clustering (Subs Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5)  
  )

#------------------------------------------------------------------------------
############## Subs Overlap Dataset (HDC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_overlap$title))

similarity_matrix_hdc_so <- as.matrix(subs_overlap[, -1])

rownames(similarity_matrix_hdc_so) <- NULL
colnames(similarity_matrix_hdc_so) <- NULL

distance_matrix_hdc_so <- as.dist(1 - similarity_matrix_hdc_so)

mds_coords_hdc_so <- cmdscale(distance_matrix_hdc_so, k = 2)

divisive_result_hdc_so <- diana(distance_matrix_hdc_so)

plot(as.hclust(divisive_result_hdc_so), 
     main = "Divisive Hierarchical Clustering (Subs Overlap)", 
     sub = "", 
     xlab = "", 
     cex = 0.8)

k_values_hdc_so <- 2:15
set.seed(123)

silhouette_scores_hdc_so <- sapply(k_values_hdc_so, function(k) {
  clusters_hdc_so <- cutree(as.hclust(divisive_result_hdc_so), k)
  silhouette_result_hdc_so <- silhouette(clusters_hdc_so, distance_matrix_hdc_so)
  mean(silhouette_result_hdc_so[, 3], na.rm = TRUE)
})

plot(k_values_hdc_so, silhouette_scores_hdc_so, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for Divisive Clustering (Subs Overlap)")

set.seed(123)
wss_hdc_so <- sapply(k_values_hdc_so, function(k) {
  clusters_hdc_so <- cutree(as.hclust(divisive_result_hdc_so), k)
  cluster_wss_hdc_so <- sapply(unique(clusters_hdc_so), function(cluster) {
    cluster_points_hdc_so <- which(clusters_hdc_so == cluster)
    cluster_distances_hdc_so <- as.matrix(distance_matrix_hdc_so)[cluster_points_hdc_so, cluster_points_hdc_so]
    mean(as.matrix(cluster_distances_hdc_so)) * length(cluster_points_hdc_so)
  })
  sum(cluster_wss_hdc_so)
})

plot(k_values_hdc_so, wss_hdc_so, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Divisive Clustering (Subs Overlap)")

k_hdc_so <- 3 # prediction
divisive_clusters_hdc_so <- cutree(as.hclust(divisive_result_hdc_so), k = k_hdc_so)

channel_info$hdc_cluster_so <- divisive_clusters_hdc_so

visualization_data_hdc_so <- data.frame(
  X1 = mds_coords_hdc_so[, 1],
  X2 = mds_coords_hdc_so[, 2],
  cluster = as.factor(divisive_clusters_hdc_so)
)

ggplot(visualization_data_hdc_so, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "Divisive Hierarchical Clustering (Subs Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

