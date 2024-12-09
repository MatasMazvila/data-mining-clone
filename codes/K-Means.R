#------------------------------------------------------------------------------
############## K-Means Clustering ##############
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
############## Commenters Jaccard Dataset (K-Means) ##############
#------------------------------------------------------------------------------
# Ensure that information and distance matrix line up
stopifnot(all(channel_info$title == commenters_jaccard$title))

# Define similarity as matrix
similarity_matrix_cj_means <- as.matrix(commenters_jaccard[, -1])

# Remove dimnames to avoid warnings
rownames(similarity_matrix_cj_means) <- NULL
colnames(similarity_matrix_cj_means) <- NULL

# Convert similarity matrix to a distance matrix
distance_matrix_cj_means <- as.dist(1 - similarity_matrix_cj_means)

# Multidimensional Scaling (MDS) for Visualization
mds_coords_cj_means <- cmdscale(distance_matrix_cj_means, k = 2)

# Compute Hopkins Statistic
set.seed(123)
hopkins_stat_cj_means <- hopkins(mds_coords_cj_means, m = nrow(mds_coords_cj_means) - 1)
print(paste("Hopkins Statistic:", round(hopkins_stat_cj_means, 4)))

# Use MDS for K-Means clustering
kmeans_data_cj_means <- data.frame(mds_coords_cj_means)

# Elbow Method for K-Means
set.seed(123)
k_values_means <- 2:15
wss_means_cj <- sapply(k_values_means, function(k) {
  kmeans_result_cj <- kmeans(kmeans_data_cj_means, centers = k, nstart = 25)
  kmeans_result_cj$tot.withinss
})
# Plot the Elbow Curve
plot(k_values_means, wss_means_cj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for K-Means (Commenters Jaccard)")

# Silhouette for K-Means
set.seed(123)
silhouette_scores_means_cj <- sapply(k_values_means, function(k) {
  kmeans_result_cj <- kmeans(kmeans_data_cj_means, centers = k, nstart = 25)
  silhouette_result_cj <- silhouette(kmeans_result_cj$cluster, distance_matrix_cj_means)
  mean(silhouette_result_cj[, 3])
})
# Plot Silhouette Scores
plot(k_values_means, silhouette_scores_means_cj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Means (Commenters Jaccard)")

# Run K-Means
k_cj_means <- 4 # elbow - 4, silhouette - 4
kmeans_result_cj <- kmeans(kmeans_data_cj_means, centers = k_cj_means, nstart = 25)

# Add cluster labels to the dataset
kmeans_data_cj_means$cluster <- as.factor(kmeans_result_cj$cluster)

# Visualize the clusters
ggplot(kmeans_data_cj_means, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Means Clustering (Commenters Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
############## K-Means Clustering for Commenters Overlap ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == commenters_overlap$title))

similarity_matrix_co_means <- as.matrix(commenters_overlap[, -1])

rownames(similarity_matrix_co_means) <- NULL
colnames(similarity_matrix_co_means) <- NULL

distance_matrix_co_means <- as.dist(1 - similarity_matrix_co_means)

mds_coords_co_means <- cmdscale(distance_matrix_co_means, k = 2)

set.seed(123)
hopkins_stat_co_means <- hopkins(mds_coords_co_means, m = nrow(mds_coords_co_means) - 1)
print(paste("Hopkins Statistic:", round(hopkins_stat_co_means, 4)))

kmeans_data_co_means <- data.frame(mds_coords_co_means)

set.seed(123)
k_values_means <- 2:15
wss_means_co <- sapply(k_values_means, function(k) {
  kmeans_result_co <- kmeans(kmeans_data_co_means, centers = k, nstart = 25)
  kmeans_result_co$tot.withinss
})
plot(k_values_means, wss_means_co, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for K-Means (Commenters Overlap)")

set.seed(123)
silhouette_scores_means_co <- sapply(k_values_means, function(k) {
  kmeans_result_co <- kmeans(kmeans_data_co_means, centers = k, nstart = 25)
  silhouette_result_co <- silhouette(kmeans_result_co$cluster, distance_matrix_co_means)
  mean(silhouette_result_co[, 3])
})
plot(k_values_means, silhouette_scores_means_co, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Means (Commenters Overlap)")

k_co_means <- 5 # elbow - 5, silhouette - 5
kmeans_result_co <- kmeans(kmeans_data_co_means, centers = k_co_means, nstart = 25)

kmeans_data_co_means$cluster <- as.factor(kmeans_result_co$cluster)

ggplot(kmeans_data_co_means, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Means Clustering (Commenters Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
############## K-Means Clustering for Subs Jaccard ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_jaccard$title))

similarity_matrix_sj_means <- as.matrix(subs_jaccard[, -1])

rownames(similarity_matrix_sj_means) <- NULL
colnames(similarity_matrix_sj_means) <- NULL

distance_matrix_sj_means <- as.dist(1 - similarity_matrix_sj_means)

mds_coords_sj_means <- cmdscale(distance_matrix_sj_means, k = 2)

set.seed(123)
hopkins_stat_sj_means <- hopkins(mds_coords_sj_means, m = nrow(mds_coords_sj_means) - 1)
print(paste("Hopkins Statistic:", round(hopkins_stat_sj_means, 4)))

kmeans_data_sj_means <- data.frame(mds_coords_sj_means)

set.seed(123)
k_values_means <- 2:15
wss_means_sj <- sapply(k_values_means, function(k) {
  kmeans_result_sj <- kmeans(kmeans_data_sj_means, centers = k, nstart = 25)
  kmeans_result_sj$tot.withinss
})
plot(k_values_means, wss_means_sj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for K-Means (Subs Jaccard)")

set.seed(123)
silhouette_scores_means_sj <- sapply(k_values_means, function(k) {
  kmeans_result_sj <- kmeans(kmeans_data_sj_means, centers = k, nstart = 25)
  silhouette_result_sj <- silhouette(kmeans_result_sj$cluster, distance_matrix_sj_means)
  mean(silhouette_result_sj[, 3])
})
plot(k_values_means, silhouette_scores_means_sj, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Means (Subs Jaccard)")

k_sj_means <- 5 # elbow - 5, silhouette - 4
kmeans_result_sj <- kmeans(kmeans_data_sj_means, centers = k_sj_means, nstart = 25)

kmeans_data_sj_means$cluster <- as.factor(kmeans_result_sj$cluster)

ggplot(kmeans_data_sj_means, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Means Clustering (Subs Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
############## K-Means Clustering for Subs Overlap ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_overlap$title))

similarity_matrix_so_means <- as.matrix(subs_overlap[, -1])

rownames(similarity_matrix_so_means) <- NULL
colnames(similarity_matrix_so_means) <- NULL

distance_matrix_so_means <- as.dist(1 - similarity_matrix_so_means)

mds_coords_so_means <- cmdscale(distance_matrix_so_means, k = 2)

kmeans_data_so_means <- data.frame(mds_coords_so_means)

set.seed(123)
k_values_means <- 2:15
wss_means_so <- sapply(k_values_means, function(k) {
  kmeans_result_so <- kmeans(kmeans_data_so_means, centers = k, nstart = 25)
  kmeans_result_so$tot.withinss
})
plot(k_values_means, wss_means_so, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for K-Means (Subs Overlap)")

set.seed(123)
silhouette_scores_means_so <- sapply(k_values_means, function(k) {
  kmeans_result_so <- kmeans(kmeans_data_so_means, centers = k, nstart = 25)
  silhouette_result_so <- silhouette(kmeans_result_so$cluster, distance_matrix_so_means)
  mean(silhouette_result_so[, 3])
})
plot(k_values_means, silhouette_scores_means_so, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Means (Subs Overlap)")

k_so_means <- 4 # elbow - 6, silhouette - 4
kmeans_result_so <- kmeans(kmeans_data_so_means, centers = k_so_means, nstart = 25)

kmeans_data_so_means$cluster <- as.factor(kmeans_result_so$cluster)

ggplot(kmeans_data_so_means, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Means Clustering (Subs Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()
