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
############## Commenters Jaccard Dataset ##############
#------------------------------------------------------------------------------
# Ensure that information and distance matrix line up
stopifnot(all(channel_info$title == commenters_jaccard$title))

# Define similarity as matrix
similarity_matrix_cj <- as.matrix(commenters_jaccard[, -1])

# Remove dimnames to avoid warnings
rownames(similarity_matrix_cj) <- NULL
colnames(similarity_matrix_cj) <- NULL

# Scale the similarity matrix (galima ir nedaryti sito, bet pakelia silhouette lol)
similarity_matrix_cj_scaled <- scale(similarity_matrix_cj, center = TRUE, scale = TRUE)
similarity_matrix_cj_scaled <- similarity_matrix_cj_scaled - min(similarity_matrix_cj_scaled) 

# Convert similarity matrix to a distance matrix
distance_matrix_cj <- as.dist(1 - similarity_matrix_cj_scaled)

#------------------------------------------------------------------------------
############## Determine eps Using k-Distance Plot ##############
#------------------------------------------------------------------------------

# Compute the k-nearest neighbor distances (k = minPts - 1)
k <- 4  # Adjust based on dataset characteristics
kNNdist_cj <- kNNdist(similarity_matrix_cj_scaled, k = k)

# Plot the k-distance graph
plot(sort(kNNdist_cj), type = "l", main = "k-Distance Plot (Scaled Data)",
     xlab = "Points sorted by distance", ylab = "k-distance")

# eps based on the elbow in the k-distance graph
eps_cj <- 19.5  # Adjust based on k-distance plot

#------------------------------------------------------------------------------
############## DBSCAN Clustering ##############
#------------------------------------------------------------------------------

# Run DBSCAN
dbscan_result_cj <- dbscan(similarity_matrix_cj_scaled, eps = eps_cj, minPts = k)

# Check the number of clusters (excluding noise)
num_clusters <- length(unique(dbscan_result_cj$cluster[dbscan_result_cj$cluster > 0]))
cat("Number of clusters (excluding noise):", num_clusters, "\n")

# Compute silhouette score only if there are at least 2 clusters
if (num_clusters > 1) {
  silhouette_result_cj <- silhouette(dbscan_result_cj$cluster, distance_matrix_cj)
  avg_silhouette_cj <- mean(silhouette_result_cj[, 3])
  cat("Average silhouette score for eps =", eps_cj, ":", round(avg_silhouette_cj, 4), "\n")
} else {
  cat("Silhouette score cannot be calculated as there is only one cluster or all points are noise.\n")
}

# Add cluster labels to the original data
channel_info$cluster <- as.factor(dbscan_result_cj$cluster)

# Summary of clusters
print(table(channel_info$cluster))

# Visualize the clustering result (using mds coords)
dbscan_data_cj <- data.frame(
  X1 = cmdscale(distance_matrix_cj, k = 2)[, 1],
  X2 = cmdscale(distance_matrix_cj, k = 2)[, 2],
  cluster = as.factor(dbscan_result_cj$cluster),
  title = channel_info$title
)

ggplot(dbscan_data_cj, aes(x = X1, y = X2, color = cluster)) +
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
############## Commenters Overlap Dataset ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == commenters_overlap$title))

similarity_matrix_co <- as.matrix(commenters_overlap[, -1])
rownames(similarity_matrix_co) <- NULL
colnames(similarity_matrix_co) <- NULL

similarity_matrix_co_scaled <- scale(similarity_matrix_co, center = TRUE, scale = TRUE)
similarity_matrix_co_scaled <- similarity_matrix_co_scaled - min(similarity_matrix_co_scaled)

distance_matrix_co <- as.dist(1 - similarity_matrix_co_scaled)

k <- 4
kNNdist_co <- kNNdist(similarity_matrix_co_scaled, k = k)

plot(sort(kNNdist_co), type = "l", main = "k-Distance Plot (Scaled Data)",
     xlab = "Points sorted by distance", ylab = "k-distance")

eps_co <- 16

dbscan_result_co <- dbscan(similarity_matrix_co_scaled, eps = eps_co, minPts = k)

num_clusters <- length(unique(dbscan_result_co$cluster[dbscan_result_co$cluster > 0]))
cat("Number of clusters (excluding noise):", num_clusters, "\n")

if (num_clusters > 1) {
  silhouette_result_co <- silhouette(dbscan_result_co$cluster, distance_matrix_co)
  avg_silhouette_co <- mean(silhouette_result_co[, 3])
  cat("Average silhouette score for eps =", eps_co, ":", round(avg_silhouette_co, 4), "\n")
} else {
  cat("Silhouette score cannot be calculated as there is only one cluster or all points are noise.\n")
}

channel_info$cluster <- as.factor(dbscan_result_co$cluster)

print(table(channel_info$cluster))

dbscan_data_co <- data.frame(
  X1 = cmdscale(distance_matrix_co, k = 2)[, 1],
  X2 = cmdscale(distance_matrix_co, k = 2)[, 2],
  cluster = as.factor(dbscan_result_co$cluster),
  title = channel_info$title
)

ggplot(dbscan_data_co, aes(x = X1, y = X2, color = cluster)) +
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
############## Subs Jaccard Dataset ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_jaccard$title))

similarity_matrix_sj <- as.matrix(subs_jaccard[, -1])
rownames(similarity_matrix_sj) <- NULL
colnames(similarity_matrix_sj) <- NULL

similarity_matrix_sj_scaled <- scale(similarity_matrix_sj, center = TRUE, scale = TRUE)
similarity_matrix_sj_scaled <- similarity_matrix_sj_scaled - min(similarity_matrix_sj_scaled)

distance_matrix_sj <- as.dist(1 - similarity_matrix_sj_scaled)

k <- 4
kNNdist_sj <- kNNdist(similarity_matrix_sj_scaled, k = k)

plot(sort(kNNdist_sj), type = "l", main = "k-Distance Plot (Scaled Data)",
     xlab = "Points sorted by distance", ylab = "k-distance")

eps_sj <- 16

dbscan_result_sj <- dbscan(similarity_matrix_sj_scaled, eps = eps_sj, minPts = k)

num_clusters <- length(unique(dbscan_result_sj$cluster[dbscan_result_sj$cluster > 0]))
cat("Number of clusters (excluding noise):", num_clusters, "\n")

if (num_clusters > 1) {
  silhouette_result_sj <- silhouette(dbscan_result_sj$cluster, distance_matrix_sj)
  avg_silhouette_sj <- mean(silhouette_result_sj[, 3])
  cat("Average silhouette score for eps =", eps_sj, ":", round(avg_silhouette_sj, 4), "\n")
} else {
  cat("Silhouette score cannot be calculated as there is only one cluster or all points are noise.\n")
}

channel_info$cluster <- as.factor(dbscan_result_sj$cluster)

print(table(channel_info$cluster))

dbscan_data_sj <- data.frame(
  X1 = cmdscale(distance_matrix_sj, k = 2)[, 1],
  X2 = cmdscale(distance_matrix_sj, k = 2)[, 2],
  cluster = as.factor(dbscan_result_sj$cluster),
  title = channel_info$title
)

ggplot(dbscan_data_sj, aes(x = X1, y = X2, color = cluster)) +
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
############## Subs Overlap Dataset ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_overlap$title))

similarity_matrix_so <- as.matrix(subs_overlap[, -1])
rownames(similarity_matrix_so) <- NULL
colnames(similarity_matrix_so) <- NULL

similarity_matrix_so_scaled <- scale(similarity_matrix_so, center = TRUE, scale = TRUE)
similarity_matrix_so_scaled <- similarity_matrix_so_scaled - min(similarity_matrix_so_scaled)

distance_matrix_so <- as.dist(1 - similarity_matrix_so_scaled)

k <- 4
kNNdist_so <- kNNdist(similarity_matrix_so_scaled, k = k)

plot(sort(kNNdist_so), type = "l", main = "k-Distance Plot (Scaled Data)",
     xlab = "Points sorted by distance", ylab = "k-distance")

eps_so <- 8.5

dbscan_result_so <- dbscan(similarity_matrix_so_scaled, eps = eps_so, minPts = k)

num_clusters <- length(unique(dbscan_result_so$cluster[dbscan_result_so$cluster > 0]))
cat("Number of clusters (excluding noise):", num_clusters, "\n")

if (num_clusters > 1) {
  silhouette_result_so <- silhouette(dbscan_result_so$cluster, distance_matrix_so)
  avg_silhouette_so <- mean(silhouette_result_so[, 3])
  cat("Average silhouette score for eps =", eps_so, ":", round(avg_silhouette_so, 4), "\n")
} else {
  cat("Silhouette score cannot be calculated as there is only one cluster or all points are noise.\n")
}

channel_info$cluster <- as.factor(dbscan_result_so$cluster)

print(table(channel_info$cluster))

dbscan_data_so <- data.frame(
  X1 = cmdscale(distance_matrix_so, k = 2)[, 1],
  X2 = cmdscale(distance_matrix_so, k = 2)[, 2],
  cluster = as.factor(dbscan_result_so$cluster),
  title = channel_info$title
)

ggplot(dbscan_data_so, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "DBSCAN Clustering (Subs Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()





