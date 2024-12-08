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
