#------------------------------------------------------------------------------
############## DBSCAN ##############
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
############## Commenters Jaccard Dataset (DBSCAN) ##############
#------------------------------------------------------------------------------
# Ensure that information and distance matrix line up
stopifnot(all(channel_info$title == commenters_jaccard$title))

# Define similarity as matrix
similarity_matrix_dbscan_cj <- as.matrix(commenters_jaccard[, -1])

# Remove dimnames to avoid warnings
rownames(similarity_matrix_dbscan_cj) <- NULL
colnames(similarity_matrix_dbscan_cj) <- NULL

# Scale the similarity matrix (galima ir nedaryti sito, bet pakelia silhouette lol)
similarity_matrix_dbscan_cj_scaled <- scale(similarity_matrix_dbscan_cj, center = TRUE, scale = TRUE)
similarity_matrix_dbscan_cj_scaled <- similarity_matrix_dbscan_cj_scaled - min(similarity_matrix_dbscan_cj_scaled) 

# Convert similarity matrix to a distance matrix
distance_matrix_dbscan_cj <- as.dist(1 - similarity_matrix_dbscan_cj_scaled)

#------------------------------------------------------------------------------
############## Determine eps Using k-Distance Plot ##############
#------------------------------------------------------------------------------

# Compute the k-nearest neighbor distances (k = minPts - 1)
k_dbscan_cj <- 4  # Adjust based on dataset characteristics
kNNdist_dbscan_cj <- kNNdist(similarity_matrix_dbscan_cj_scaled, k = k_dbscan_cj)

# Plot the k-distance graph
plot(sort(kNNdist_dbscan_cj), type = "l", main = "k-Distance Plot (Scaled Data)",
     xlab = "Points sorted by distance", ylab = "k-distance")

# eps based on the elbow in the k-distance graph
eps_dbscan_cj <- 19.5 

#------------------------------------------------------------------------------
############## DBSCAN Clustering ##############
#------------------------------------------------------------------------------

# Run DBSCAN
dbscan_result_dbscan_cj <- dbscan(similarity_matrix_dbscan_cj_scaled, eps = eps_dbscan_cj, minPts = k_dbscan_cj)

# Check the number of clusters (excluding noise)
num_clusters_dbscan_cj <- length(unique(dbscan_result_dbscan_cj$cluster[dbscan_result_dbscan_cj$cluster > 0]))
cat("Number of clusters (excluding noise):", num_clusters_dbscan_cj, "\n")

# Compute silhouette score only if there are at least 2 clusters
if (num_clusters_dbscan_cj > 1) {
  silhouette_result_dbscan_cj <- silhouette(dbscan_result_dbscan_cj$cluster, distance_matrix_dbscan_cj)
  avg_silhouette_dbscan_cj <- mean(silhouette_result_dbscan_cj[, 3])
  cat("Average silhouette score for eps =", eps_dbscan_cj, ":", round(avg_silhouette_dbscan_cj, 4), "\n")
} else {
  cat("Silhouette score cannot be calculated as there is only one cluster or all points are noise.\n")
}

# Add cluster labels to the original data
channel_info$cluster_dbscan_cj <- as.factor(dbscan_result_dbscan_cj$cluster)

# Summary of clusters
print(table(channel_info$cluster_dbscan_cj))

# Visualize the clustering result (using mds coords)
dbscan_data_dbscan_cj <- data.frame(
  X1 = cmdscale(distance_matrix_dbscan_cj, k = 2)[, 1],
  X2 = cmdscale(distance_matrix_dbscan_cj, k = 2)[, 2],
  cluster = as.factor(dbscan_result_dbscan_cj$cluster),
  title = channel_info$title
)

ggplot(dbscan_data_dbscan_cj, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "DBSCAN Clustering (Commenters Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
############## Commenters Overlap Dataset (DBSCAN) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == commenters_overlap$title))

similarity_matrix_dbscan_co <- as.matrix(commenters_overlap[, -1])
rownames(similarity_matrix_dbscan_co) <- NULL
colnames(similarity_matrix_dbscan_co) <- NULL

similarity_matrix_dbscan_co_scaled <- scale(similarity_matrix_dbscan_co, center = TRUE, scale = TRUE)
similarity_matrix_dbscan_co_scaled <- similarity_matrix_dbscan_co_scaled - min(similarity_matrix_dbscan_co_scaled)

distance_matrix_dbscan_co <- as.dist(1 - similarity_matrix_dbscan_co_scaled)

k_dbscan_co <- 4
kNNdist_dbscan_co <- kNNdist(similarity_matrix_dbscan_co_scaled, k = k_dbscan_co)

plot(sort(kNNdist_dbscan_co), type = "l", main = "k-Distance Plot (Scaled Data)",
     xlab = "Points sorted by distance", ylab = "k-distance")

eps_dbscan_co <- 16

dbscan_result_dbscan_co <- dbscan(similarity_matrix_dbscan_co_scaled, eps = eps_dbscan_co, minPts = k_dbscan_co)

num_clusters_dbscan_co <- length(unique(dbscan_result_dbscan_co$cluster[dbscan_result_dbscan_co$cluster > 0]))
cat("Number of clusters (excluding noise):", num_clusters_dbscan_co, "\n")

if (num_clusters_dbscan_co > 1) {
  silhouette_result_dbscan_co <- silhouette(dbscan_result_dbscan_co$cluster, distance_matrix_dbscan_co)
  avg_silhouette_dbscan_co <- mean(silhouette_result_dbscan_co[, 3])
  cat("Average silhouette score for eps =", eps_dbscan_co, ":", round(avg_silhouette_dbscan_co, 4), "\n")
} else {
  cat("Silhouette score cannot be calculated as there is only one cluster or all points are noise.\n")
}

channel_info$cluster_dbscan_co <- as.factor(dbscan_result_dbscan_co$cluster)

print(table(channel_info$cluster_dbscan_co))

dbscan_data_dbscan_co <- data.frame(
  X1 = cmdscale(distance_matrix_dbscan_co, k = 2)[, 1],
  X2 = cmdscale(distance_matrix_dbscan_co, k = 2)[, 2],
  cluster = as.factor(dbscan_result_dbscan_co$cluster),
  title = channel_info$title
)

ggplot(dbscan_data_dbscan_co, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "DBSCAN Clustering (Commenters Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
############## Subs Jaccard Dataset (DBSCAN) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_jaccard$title))

similarity_matrix_dbscan_sj <- as.matrix(subs_jaccard[, -1])
rownames(similarity_matrix_dbscan_sj) <- NULL
colnames(similarity_matrix_dbscan_sj) <- NULL

similarity_matrix_dbscan_sj_scaled <- scale(similarity_matrix_dbscan_sj, center = TRUE, scale = TRUE)
similarity_matrix_dbscan_sj_scaled <- similarity_matrix_dbscan_sj_scaled - min(similarity_matrix_dbscan_sj_scaled)

distance_matrix_dbscan_sj <- as.dist(1 - similarity_matrix_dbscan_sj_scaled)

k_dbscan_sj <- 4
kNNdist_dbscan_sj <- kNNdist(similarity_matrix_dbscan_sj_scaled, k = k_dbscan_sj)

plot(sort(kNNdist_dbscan_sj), type = "l", main = "k-Distance Plot (Scaled Data)",
     xlab = "Points sorted by distance", ylab = "k-distance")

eps_dbscan_sj <- 16

dbscan_result_dbscan_sj <- dbscan(similarity_matrix_dbscan_sj_scaled, eps = eps_dbscan_sj, minPts = k_dbscan_sj)

num_clusters_dbscan_sj <- length(unique(dbscan_result_dbscan_sj$cluster[dbscan_result_dbscan_sj$cluster > 0]))
cat("Number of clusters (excluding noise):", num_clusters_dbscan_sj, "\n")

if (num_clusters_dbscan_sj > 1) {
  silhouette_result_dbscan_sj <- silhouette(dbscan_result_dbscan_sj$cluster, distance_matrix_dbscan_sj)
  avg_silhouette_dbscan_sj <- mean(silhouette_result_dbscan_sj[, 3])
  cat("Average silhouette score for eps =", eps_dbscan_sj, ":", round(avg_silhouette_dbscan_sj, 4), "\n")
} else {
  cat("Silhouette score cannot be calculated as there is only one cluster or all points are noise.\n")
}

channel_info$cluster_dbscan_sj <- as.factor(dbscan_result_dbscan_sj$cluster)

print(table(channel_info$cluster_dbscan_sj))

dbscan_data_dbscan_sj <- data.frame(
  X1 = cmdscale(distance_matrix_dbscan_sj, k = 2)[, 1],
  X2 = cmdscale(distance_matrix_dbscan_sj, k = 2)[, 2],
  cluster = as.factor(dbscan_result_dbscan_sj$cluster),
  title = channel_info$title
)

ggplot(dbscan_data_dbscan_sj, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "DBSCAN Clustering (Subs Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
############## Subs Overlap Dataset (DBSCAN) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_overlap$title))

similarity_matrix_dbscan_so <- as.matrix(subs_overlap[, -1])
rownames(similarity_matrix_dbscan_so) <- NULL
colnames(similarity_matrix_dbscan_so) <- NULL

similarity_matrix_dbscan_so_scaled <- scale(similarity_matrix_dbscan_so, center = TRUE, scale = TRUE)
similarity_matrix_dbscan_so_scaled <- similarity_matrix_dbscan_so_scaled - min(similarity_matrix_dbscan_so_scaled)

distance_matrix_dbscan_so <- as.dist(1 - similarity_matrix_dbscan_so_scaled)

k_dbscan_so <- 4
kNNdist_dbscan_so <- kNNdist(similarity_matrix_dbscan_so_scaled, k = k_dbscan_so)

plot(sort(kNNdist_dbscan_so), type = "l", main = "k-Distance Plot (Scaled Data)",
     xlab = "Points sorted by distance", ylab = "k-distance")

eps_dbscan_so <- 8.5

dbscan_result_dbscan_so <- dbscan(similarity_matrix_dbscan_so_scaled, eps = eps_dbscan_so, minPts = k_dbscan_so)

num_clusters_dbscan_so <- length(unique(dbscan_result_dbscan_so$cluster[dbscan_result_dbscan_so$cluster > 0]))
cat("Number of clusters (excluding noise):", num_clusters_dbscan_so, "\n")

if (num_clusters_dbscan_so > 1) {
  silhouette_result_dbscan_so <- silhouette(dbscan_result_dbscan_so$cluster, distance_matrix_dbscan_so)
  avg_silhouette_dbscan_so <- mean(silhouette_result_dbscan_so[, 3])
  cat("Average silhouette score for eps =", eps_dbscan_so, ":", round(avg_silhouette_dbscan_so, 4), "\n")
} else {
  cat("Silhouette score cannot be calculated as there is only one cluster or all points are noise.\n")
}

channel_info$cluster_dbscan_so <- as.factor(dbscan_result_dbscan_so$cluster)

print(table(channel_info$cluster_dbscan_so))

dbscan_data_dbscan_so <- data.frame(
  X1 = cmdscale(distance_matrix_dbscan_so, k = 2)[, 1],
  X2 = cmdscale(distance_matrix_dbscan_so, k = 2)[, 2],
  cluster = as.factor(dbscan_result_dbscan_so$cluster),
  title = channel_info$title
)

ggplot(dbscan_data_dbscan_so, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "DBSCAN Clustering (Subs Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()
