#------------------------------------------------------------------------------
############## K-Medoids Clustering ##############
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
############## Commenters Jaccard Dataset (K-Medoids) ##############
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
############## Similarity and Distance Matrix Preparation ##############
#------------------------------------------------------------------------------

# Ensure that information and distance matrix line up
stopifnot(all(channel_info$title == commenters_jaccard$title))

# Define similarity as matrix
similarity_matrix_cj_medoids <- as.matrix(commenters_jaccard[, -1])

# Remove dimnames to avoid warnings
rownames(similarity_matrix_cj_medoids) <- NULL
colnames(similarity_matrix_cj_medoids) <- NULL

# Convert similarity matrix to a distance matrix
distance_matrix_cj_medoids <- as.dist(1 - similarity_matrix_cj_medoids)

# Multidimensional Scaling (MDS) for Visualization
mds_coords_cj_medoids <- cmdscale(distance_matrix_cj_medoids, k = 2)

# Compute Hopkins Statistic
set.seed(123)
hopkins_stat_cj_medoids <- hopkins(mds_coords_cj_medoids, m = nrow(mds_coords_cj_medoids) - 1)
print(paste("Hopkins Statistic:", round(hopkins_stat_cj_medoids, 4)))

#------------------------------------------------------------------------------
############## Elbow and Silhouette Methods ##############
#------------------------------------------------------------------------------

# Elbow Method to Determine Optimal k
k_values_medoids <- 2:15
set.seed(123)
total_wsd_medoids_cj <- sapply(k_values_medoids, function(k) {
  kmedoids_result_cj <- pam(distance_matrix_cj_medoids, k)
  sum(sapply(1:k, function(cluster) {
    cluster_members_cj <- which(kmedoids_result_cj$clustering == cluster)
    cluster_distances_cj <- as.matrix(distance_matrix_cj_medoids)[cluster_members_cj, cluster_members_cj]
    sum(cluster_distances_cj)
  }))
})
# Plot the Elbow Curve
plot(k_values_medoids, total_wsd_medoids_cj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Distances",
     main = "Elbow Method for K-Medoids")

# Store average silhouette width for each k
set.seed(123)
silhouette_scores_cj_medoids <- sapply(k_values_medoids, function(k) {
  kmedoids_result_cj <- pam(distance_matrix_cj_medoids, k)
  silhouette_result_cj <- silhouette(kmedoids_result_cj$clustering, distance_matrix_cj_medoids)
  mean(silhouette_result_cj[, 3])  # Average silhouette width
})
# Plot Silhouette Scores
plot(k_values_medoids, silhouette_scores_cj_medoids, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Medoids")

k_cj_medoids <- 6 # elbow - 6, silhouette - 6
# Perform K-Medoids clustering with the chosen number of clusters
kmedoids_result_cj <- pam(distance_matrix_cj_medoids, k = k_cj_medoids)

# Extract cluster labels
kmedoids_clusters_cj_medoids <- kmedoids_result_cj$clustering

# Add cluster labels to the original data
channel_info$kmedoids_cluster_cj <- kmedoids_clusters_cj_medoids

# Prepare data for visualization
kmedoids_data_cj_medoids <- data.frame(
  X1 = mds_coords_cj_medoids[, 1],
  X2 = mds_coords_cj_medoids[, 2],
  cluster = as.factor(kmedoids_clusters_cj_medoids),
  title = channel_info$title
)

# Plot the K-Medoids clustering result
ggplot(kmedoids_data_cj_medoids, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +  # Points with transparency
  geom_text(aes(label = title),        # Add channel names
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Medoids Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#------------------------------------------------------------------------------
############## Commenters Overlap Dataset (K-Medoids) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == commenters_overlap$title))

similarity_matrix_co_medoids <- as.matrix(commenters_overlap[, -1])

rownames(similarity_matrix_co_medoids) <- NULL
colnames(similarity_matrix_co_medoids) <- NULL

distance_matrix_co_medoids <- as.dist(1 - similarity_matrix_co_medoids)

mds_coords_co_medoids <- cmdscale(distance_matrix_co_medoids, k = 2)

set.seed(123)
hopkins_stat_co_medoids <- hopkins(mds_coords_co_medoids, m = nrow(mds_coords_co_medoids) - 1)
print(paste("Hopkins Statistic:", round(hopkins_stat_co_medoids, 4)))

k_values_medoids <- 2:15
set.seed(123)
total_wsd_medoids_co <- sapply(k_values_medoids, function(k) {
  kmedoids_result_co <- pam(distance_matrix_co_medoids, k)
  sum(sapply(1:k, function(cluster) {
    cluster_members_co <- which(kmedoids_result_co$clustering == cluster)
    cluster_distances_co <- as.matrix(distance_matrix_co_medoids)[cluster_members_co, cluster_members_co]
    sum(cluster_distances_co)
  }))
})
plot(k_values_medoids, total_wsd_medoids_co, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Distances",
     main = "Elbow Method for K-Medoids")

set.seed(123)
silhouette_scores_co_medoids <- sapply(k_values_medoids, function(k) {
  kmedoids_result_co <- pam(distance_matrix_co_medoids, k)
  silhouette_result_co <- silhouette(kmedoids_result_co$clustering, distance_matrix_co_medoids)
  mean(silhouette_result_co[, 3]) 
})
plot(k_values_medoids, silhouette_scores_co_medoids, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Medoids")

k_co_medoids <- 7 # elbow - 7, silhouette - 13

kmedoids_result_co <- pam(distance_matrix_co_medoids, k = k_co_medoids)

kmedoids_clusters_co_medoids <- kmedoids_result_co$clustering

channel_info$kmedoids_cluster_co <- kmedoids_clusters_co_medoids

kmedoids_data_co_medoids <- data.frame(
  X1 = mds_coords_co_medoids[, 1],
  X2 = mds_coords_co_medoids[, 2],
  cluster = as.factor(kmedoids_clusters_co_medoids),
  title = channel_info$title
)

ggplot(kmedoids_data_co_medoids, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) + 
  geom_text(aes(label = title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Medoids Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#------------------------------------------------------------------------------
############## Subs Jaccard Dataset (K-Medoids) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_jaccard$title))

similarity_matrix_sj_medoids <- as.matrix(subs_jaccard[, -1])

rownames(similarity_matrix_sj_medoids) <- NULL
colnames(similarity_matrix_sj_medoids) <- NULL

distance_matrix_sj_medoids <- as.dist(1 - similarity_matrix_sj_medoids)

mds_coords_sj_medoids <- cmdscale(distance_matrix_sj_medoids, k = 2)

set.seed(123)
hopkins_stat_sj_medoids <- hopkins(mds_coords_sj_medoids, m = nrow(mds_coords_sj_medoids) - 1)
print(paste("Hopkins Statistic:", round(hopkins_stat_sj_medoids, 4)))

k_values_medoids <- 2:15
set.seed(123)
total_wsd_medoids_sj <- sapply(k_values_medoids, function(k) {
  kmedoids_result_sj <- pam(distance_matrix_sj_medoids, k)
  sum(sapply(1:k, function(cluster) {
    cluster_members_sj <- which(kmedoids_result_sj$clustering == cluster)
    cluster_distances_sj <- as.matrix(distance_matrix_sj_medoids)[cluster_members_sj, cluster_members_sj]
    sum(cluster_distances_sj)
  }))
})
plot(k_values_medoids, total_wsd_medoids_sj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Distances",
     main = "Elbow Method for K-Medoids")

set.seed(123)
silhouette_scores_sj_medoids <- sapply(k_values_medoids, function(k) {
  kmedoids_result_sj <- pam(distance_matrix_sj_medoids, k)
  silhouette_result_sj <- silhouette(kmedoids_result_sj$clustering, distance_matrix_sj_medoids)
  mean(silhouette_result_sj[, 3]) 
})
plot(k_values_medoids, silhouette_scores_sj_medoids, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Medoids")

k_sj_medoids <- 9 # elbow - 7, silhouette - 9

kmedoids_result_sj <- pam(distance_matrix_sj_medoids, k = k_sj_medoids)

kmedoids_clusters_sj_medoids <- kmedoids_result_sj$clustering

channel_info$kmedoids_cluster_sj <- kmedoids_clusters_sj_medoids

kmedoids_data_sj_medoids <- data.frame(
  X1 = mds_coords_sj_medoids[, 1],
  X2 = mds_coords_sj_medoids[, 2],
  cluster = as.factor(kmedoids_clusters_sj_medoids),
  title = channel_info$title
)

ggplot(kmedoids_data_sj_medoids, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) + 
  geom_text(aes(label = title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Medoids Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

#------------------------------------------------------------------------------
############## Subs Overlap Dataset (K-Medoids) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_overlap$title))

similarity_matrix_so_medoids <- as.matrix(subs_overlap[, -1])

rownames(similarity_matrix_so_medoids) <- NULL
colnames(similarity_matrix_so_medoids) <- NULL

distance_matrix_so_medoids <- as.dist(1 - similarity_matrix_so_medoids)

mds_coords_so_medoids <- cmdscale(distance_matrix_so_medoids, k = 2)

set.seed(123)
hopkins_stat_so_medoids <- hopkins(mds_coords_so_medoids, m = nrow(mds_coords_so_medoids) - 1)
print(paste("Hopkins Statistic:", round(hopkins_stat_so_medoids, 4)))

k_values_medoids <- 2:15
set.seed(123)
total_wsd_medoids_so <- sapply(k_values_medoids, function(k) {
  kmedoids_result_so <- pam(distance_matrix_so_medoids, k)
  sum(sapply(1:k, function(cluster) {
    cluster_members_so <- which(kmedoids_result_so$clustering == cluster)
    cluster_distances_so <- as.matrix(distance_matrix_so_medoids)[cluster_members_so, cluster_members_so]
    sum(cluster_distances_so)
  }))
})
plot(k_values_medoids, total_wsd_medoids_so, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Distances",
     main = "Elbow Method for K-Medoids")

set.seed(123)
silhouette_scores_so_medoids <- sapply(k_values_medoids, function(k) {
  kmedoids_result_so <- pam(distance_matrix_so_medoids, k)
  silhouette_result_so <- silhouette(kmedoids_result_so$clustering, distance_matrix_so_medoids)
  mean(silhouette_result_so[, 3]) 
})
plot(k_values_medoids, silhouette_scores_so_medoids, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Medoids")

k_so_medoids <- 7 # elbow - 6, silhouette - 7

kmedoids_result_so <- pam(distance_matrix_so_medoids, k = k_so_medoids)

kmedoids_clusters_so_medoids <- kmedoids_result_so$clustering

channel_info$kmedoids_cluster_so <- kmedoids_clusters_so_medoids

kmedoids_data_so_medoids <- data.frame(
  X1 = mds_coords_so_medoids[, 1],
  X2 = mds_coords_so_medoids[, 2],
  cluster = as.factor(kmedoids_clusters_so_medoids),
  title = channel_info$title
)

ggplot(kmedoids_data_so_medoids, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) + 
  geom_text(aes(label = title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Medoids Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
