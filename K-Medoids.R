# K-Medoids Clustering

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


############## DATA ##############

# Load the data (Justas)
channel_info <- read_csv("youtube/clustering/channel_info.csv")
# View(channel_info)

commenter_jaccard <- read_csv("youtube/clustering/commenter_jaccard.csv")
# View(commenter_jaccard)

# Load the data (Matas)
#setwd("C:/Users/DELL i5/OneDrive/Desktop/Data Mining/data-mining")
channel_info <- read_csv("data/channel_info.csv")
commenter_jaccard <- read_csv("data/comment_jaccard_matrix.csv")

############## K-Medoids Clustering ############## 

# Define similarity matrix
similarity_matrix <- as.matrix(commenter_jaccard[, -1])

# Ensure the similarity matrix is symmetric
similarity_matrix <- (similarity_matrix + t(similarity_matrix)) / 2

# Remove dimnames to avoid warnings
rownames(similarity_matrix) <- NULL
colnames(similarity_matrix) <- NULL

# Convert similarity matrix to a distance matrix
distance_matrix <- as.dist(1 - similarity_matrix) %>% 
  as.matrix()

# Convert the dist object to a full matrix for Elbow Method
#distance_matrix_full <- as.matrix(distance_matrix)

# Elbow Method to Determine Optimal k
k_values <- 2:15
total_wsd_medoids <- sapply(k_values, function(k) {
  kmedoids_result <- pam(distance_matrix, k)
  sum(sapply(1:k, function(cluster) {
    cluster_members <- which(kmedoids_result$clustering == cluster)
    cluster_distances <- distance_matrix[cluster_members, cluster_members]
    sum(cluster_distances)
  }))
})

# Plot the Elbow Curve
plot(k_values, total_wsd_medoids, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Distances",
     main = "Elbow Method for K-Medoids")

# Store average silhouette width for each k
silhouette_scores <- sapply(k_values, function(k) {
  kmedoids_result <- pam(distance_matrix, k)
  silhouette_result <- silhouette(kmedoids_result$clustering, dist(distance_matrix))
  mean(silhouette_result[, 3])  # Average silhouette width
})

# Plot Silhouette Scores
plot(k_values, silhouette_scores, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Medoids")

# Print scores for inspection
print(data.frame(k = k_values, silhouette_score = silhouette_scores))

# Gap statistic
gap_stat <- clusGap(distance_matrix_full, FUN = pam, K.max = 15, B = 50)

# Plot the Gap Statistic
fviz_gap_stat(gap_stat)

# Inspect Gap Statistic
print(gap_stat)

# Choose the optimal k (based on the Elbow Method, or use silhouette width)
optimal_k <- 7  # probably 6 or 8 according to Elbow
# 7 according to Silhoutte
# 4 according to Gap Statistic

# Perform K-Medoids clustering with the chosen number of clusters
kmedoids_result <- pam(distance_matrix, k = optimal_k)

# Extract cluster labels
kmedoids_clusters <- kmedoids_result$clustering

# Add cluster labels to the original data
commenter_jaccard$kmedoids_cluster <- kmedoids_clusters

# Multidimensional Scaling (MDS) for Visualization
mds_coords <- cmdscale(distance_matrix, k = 2)

# Prepare data for visualization
kmedoids_data <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(kmedoids_clusters),
  title = commenter_jaccard$title
)

# Plot the K-Medoids clustering result
ggplot(kmedoids_data, aes(x = X1, y = X2, color = cluster)) +
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