#############################################
# # # # # # # # # # # # # # # # # # # # # # #
# Data Mining
# Mapping Lithuanian YouTube: Clustering Channels by Audience Overlap
# Matas Mazvila, Justas Tamulis
# 2024-11-30
# # # # # # # # # # # # # # # # # # # # # # #
#############################################

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

#------------------------------------------------------------------------------
############## DATA ##############
#------------------------------------------------------------------------------

channel_info <- read_csv("data/channel_info.csv")
similarity <- read_csv("data/comment_jaccard_matrix.csv")

#------------------------------------------------------------------------------
############## Similarity and Distance Matrix Preparation ##############
#------------------------------------------------------------------------------

# Ensure that information and distance matrix line up
stopifnot(all(channel_info$title == similarity$title))

# Define similarity as matrix
similarity_matrix <- as.matrix(similarity[, -1])

# Remove dimnames to avoid warnings
rownames(similarity_matrix) <- NULL
colnames(similarity_matrix) <- NULL

# Convert similarity matrix to a distance matrix
distance_matrix <- as.dist(1 - similarity_matrix)
distance_matrix_full <- as.matrix(distance_matrix)

# Multidimensional Scaling (MDS) for Visualization
mds_coords <- cmdscale(distance_matrix_full, k = 2)

# Compute Hopkins Statistic
set.seed(123)
hopkins_stat <- hopkins(mds_coords, m=nrow(mds_coords)-1)
print(paste("Hopkins Statistic:", round(hopkins_stat, 4)))

#------------------------------------------------------------------------------
############## K-Medoids Clustering ##############
#------------------------------------------------------------------------------

# Elbow Method to Determine Optimal k
k_values <- 2:15
total_wsd_medoids <- sapply(k_values, function(k) {
  kmedoids_result <- pam(distance_matrix_full, k)
  sum(sapply(1:k, function(cluster) {
    cluster_members <- which(kmedoids_result$clustering == cluster)
    cluster_distances <- distance_matrix_full[cluster_members, cluster_members]
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
  kmedoids_result <- pam(distance_matrix_full, k)
  silhouette_result <- silhouette(kmedoids_result$clustering, distance_matrix_full)
  mean(silhouette_result[, 3])  # Average silhouette width
})
# Plot Silhouette Scores
plot(k_values, silhouette_scores, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Medoids")


k <- 4  
# Perform K-Medoids clustering with the chosen number of clusters
kmedoids_result <- pam(distance_matrix_full, k = k)

# Extract cluster labels
kmedoids_clusters <- kmedoids_result$clustering

# Add cluster labels to the original data
channel_info$kmedoids_cluster <- kmedoids_clusters

# Prepare data for visualization
kmedoids_data <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(kmedoids_clusters),
  title = channel_info$title
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

#------------------------------------------------------------------------------
############## K-Means Clustering ############## 
#------------------------------------------------------------------------------

# Use MDS for k means clustering
kmeans_data <- data.frame(mds_coords)

# Elbow Method for K-Means
set.seed(123)
k_values <- 2:15
wss_means <- sapply(k_values, function(k) {
  kmeans_result <- kmeans(kmeans_data, centers = k, nstart = 25)
  kmeans_result$tot.withinss
})
# Plot the Elbow Curve
plot(k_values, wss_means, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for K-Means")

# Silhouette for K-Means
silhouette_scores_means <- sapply(k_values, function(k) {
  kmeans_result <- kmeans(kmeans_data [, 1:2], centers = k, nstart = 25)
  silhouette_result <- silhouette(kmeans_result$cluster, distance_matrix)
  mean(silhouette_result[, 3])
})
# Plot Silhouette Scores
plot(k_values, silhouette_scores_means, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for K-Means")

# Run K-means
k <- 6
kmeans_result <- kmeans(kmeans_data, centers = k, nstart = 25)

# Add cluster labels to the dataset
kmeans_data$cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters
ggplot(kmeans_data, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Means Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
############## DBSCAN (Density-Based Spatial Clustering of Applications with Noise) ############## 
#------------------------------------------------------------------------------

# Method 1: k-Distance Plot to Find Optimal `eps`
k <- 6  # Typically, k = minPts - 1
knn_distances <- kNNdist(similarity_matrix, k = k)

# Plot k-distance to find the "elbow"
plot(
  sort(knn_distances), type = "l", col = "blue", lwd = 2,
  xlab = "Points sorted by distance",
  ylab = "k-NN Distance",
  main = "k-Distance Plot for DBSCAN"
)

# Based on the elbow in the plot, choose a value for `eps`
eps_value <- 1.36  # Adjust based on the elbow in the k-distance plot
min_points <- 7    # Typical values are between 3 and 10

# Perform DBSCAN
dbscan_result <- dbscan(distance_matrix, eps = eps_value, minPts = min_points)

# Add cluster labels to the dataset
channel_info$dbscan_cluster <- dbscan_result$cluster

# Prepare data for visualization
visualization_data_dbscan <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(dbscan_result$cluster)
)

# Plot the clustering result
ggplot(visualization_data_dbscan, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  labs(
    title = "DBSCAN Clustering Visualization",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

# Testing min points and eps values (we probably need a bigged data frame)
for (minPts_test in 1:4) {
  for (eps_test in seq(0.5, 2, by = 0.05)) {
    test_result <- dbscan(similarity_matrix, eps = eps_test, minPts = minPts_test)
    cat("\nDBSCAN with eps =", eps_test, "and minPts =", minPts_test, "\n")
    print(table(test_result$cluster))
  }
}

#------------------------------------------------------------------------------
############## Hierarchical Agglomerative Clustering ############## 
#------------------------------------------------------------------------------

# Perform hierarchical clustering using Ward's method
hc_result <- hclust(distance_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc_result, main = "Agglomerative Hierarchical Clustering", sub = "", xlab = "", cex = 0.8)

# Set range of clusters to evaluate
k_values <- 2:15
# Compute silhouette scores for each number of clusters
silhouette_scores_hc <- sapply(k_values, function(k) {
  # Cut the dendrogram into k clusters
  clusters <- cutree(hc_result, k)
  # Compute silhouette scores directly
  silhouette_result <- silhouette(clusters, distance_matrix_full)
  # Return the mean silhouette score
  mean(silhouette_result[, 3], na.rm = TRUE) # Avoid NA issues
})
# Plot Silhouette Scores
plot(k_values, silhouette_scores_hc, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Simplified Silhouette Method for Hierarchical Clustering")


# Compute total within-cluster sum of squares (WSS) for each k
wss_hc <- sapply(k_values, function(k) {
  # Cut the dendrogram into k clusters
  clusters <- cutree(hc_result, k)
  # Calculate WSS for each cluster and sum them up
  cluster_wss <- sapply(unique(clusters), function(cluster) {
    cluster_points <- which(clusters == cluster)
    cluster_distances <- distance_matrix_full[cluster_points, cluster_points]
    mean(as.matrix(cluster_distances)) * length(cluster_points)
  })
  sum(cluster_wss)
})
# Plot the Elbow Curve
plot(k_values, wss_hc, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Hierarchical Clustering")

# Cut the dendrogram into clusters
k <- 6  
hc_clusters <- cutree(hc_result, k = k)

# Add cluster labels to the dataset
channel_info$hc_cluster <- hc_clusters

# Visualize clusters using MDS
visualization_data_aggl <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(hc_clusters)
)

# Plot with channel names
ggplot(visualization_data_aggl, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title),  # Add channel names
            size = 3,  # Adjust text size
            hjust = 0.5, vjust = -0.5,  # Position the labels slightly above the points
            check_overlap = TRUE) +  # Prevent overlapping labels where possible
  labs(
    title = "Agglomerative Hierarchical Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#------------------------------------------------------------------------------
############## Hierarchical Divisive Clustering ############## 
#------------------------------------------------------------------------------

# Perform divisive clustering
divisive_result <- diana(distance_matrix)

# Plot the dendrogram
plot(as.hclust(divisive_result), 
     main = "Divisive Hierarchical Clustering", 
     sub = "", 
     xlab = "", 
     cex = 0.8)

k_values <- 2:15  
# Compute silhouette scores for divisive clustering
silhouette_scores_div <- sapply(k_values, function(k) {
  clusters <- cutree(as.hclust(divisive_result), k)
  silhouette_result <- silhouette(clusters, distance_matrix)
  mean(silhouette_result[, 3], na.rm = TRUE)
})

# Plot silhouette scores
plot(k_values, silhouette_scores_div, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for Divisive Clustering")

# Compute WSS for divisive clustering
wss_div <- sapply(k_values, function(k) {
  clusters <- cutree(as.hclust(divisive_result), k)
  cluster_wss <- sapply(unique(clusters), function(cluster) {
    cluster_points <- which(clusters == cluster)
    cluster_distances <- distance_matrix_full[cluster_points, cluster_points]
    mean(as.matrix(cluster_distances)) * length(cluster_points)
  })
  sum(cluster_wss)
})

# Plot the Elbow Curve
plot(k_values, wss_div, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Divisive Clustering")

# Cut the dendrogram into 6 clusters
k <- 6
divisive_clusters <- cutree(as.hclust(divisive_result), k = k)

# Add cluster labels to the dataset
channel_info$divisive_cluster <- divisive_clusters

# Visualize the clusters using MDS
visualization_data_div <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(divisive_clusters)
)

# Plot with channel names
ggplot(visualization_data_div, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +  # Plot the points
  geom_text(aes(label = channel_info$title),  # Add channel names
            size = 3,  # Adjust text size
            hjust = 0.5, vjust = -0.5,  # Position the labels slightly above the points
            check_overlap = TRUE) +  # Avoid label overlap
  labs(
    title = "Divisive Hierarchical Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend to bottom
    plot.title = element_text(hjust = 0.5)  # Center-align title
  )

#------------------------------------------------------------------------------
############## Graph-Based Local Clustering ############## 
#------------------------------------------------------------------------------

# Create a graph from the Jaccard similarity matrix
graph <- similarity_matrix %>%
  graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE)

# Perform community detection using a local clustering algorithm
# Here, we use the Louvain method (fast and widely used)
community_result <- cluster_louvain(graph)

# Extract cluster memberships
cluster_memberships <- membership(community_result)

# Add cluster labels to the dataset
channel_info$graph_cluster <- cluster_memberships

# Visualize the graph with clusters
layout <- layout_with_fr(graph)  # Fruchterman-Reingold layout for visualization

# Plot
plot(graph, layout = layout, 
     vertex.color = cluster_memberships, 
     vertex.label = channel_info$title,  # Add channel names as labels
     vertex.label.cex = 0.7,  # Adjust label size
     vertex.label.color = "black",  # Set label color
     vertex.size = 20,  # Increase node size for better visibility
     edge.color = "gray",  # Make edges less visually dominant
     edge.width = 0.5,  # Thin out edges for less clutter
     main = "Graph-Based Clustering")

# INTERACTIVE GRAPH!!!!!!!!!!!!!!! OMG OMG!!!!!!!! CRAZY
visIgraph(graph) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

# MDS-based visualization
visualization_data_grph <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(cluster_memberships)
)

ggplot(visualization_data_grph, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "Graph-Based Clustering with Channel Names",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Another interactive example
library(plotly)
plot_ly(
  x = mds_coords[, 1],
  y = mds_coords[, 2],
  text = channel_info$title,  # Hover text (channel names)
  color = as.factor(cluster_memberships),  # Cluster colors
  type = 'scatter',
  mode = 'markers+text'
) %>%
  layout(
    title = "MDS Visualization of Clusters",
    xaxis = list(title = "MDS Dimension 1"),
    yaxis = list(title = "MDS Dimension 2")
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

k_hac_sj <- 5
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

