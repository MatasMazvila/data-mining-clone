##############################
#
# Data Mining
# Mapping Lithuanian YouTube: Clustering Channels by Audience Overlap
# Matas Mazvila, Justas Tamulis
# 2024-11-30
#
##############################

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

############## DATA ##############

# Load the data (Justas)
example_channel_info <- read_csv("youtube/clustering/example_channel_info.csv")
# View(example_channel_info)

example_commenter_jaccard <- read_csv("youtube/clustering/example_commenter_jaccard.csv")
# View(example_commenter_jaccard)

# Load the data (Matas)
example_channel_info <- read_csv("example_channel_info.csv")
example_commenter_jaccard <- read_csv("example_commenter_jaccard.csv")

############## K-Medoids Clustering ############## 

# Define similarity matrix
similarity_matrix <- as.matrix(example_commenter_jaccard[, -1]) %>%
  .[1:31, 1:31] # sita dalis biski keisis

# Convert similarity to distance
distance_matrix <- as.dist(1 - similarity_matrix)

# Perform k-medoids clustering
k <- 6 # number of clusters (k)
kmedoids_result <- pam(distance_matrix, k)
kmedoids_result

# View the clustering result
kmedoids_clusters <- kmedoids_result$clustering

# Add the cluster labels to your original data
example_commenter_jaccard$kmedoids_cluster <- kmedoids_clusters

# Set Multidimensional Scaling coordinate
mds_coords <- cmdscale(as.dist(1 - similarity_matrix), k = 2)

# Prepare data for visualization
kmedoids_data <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(kmedoids_clusters),
  title = example_commenter_jaccard$title
)

# Plot the K-Medoids clustering result
ggplot(kmedoids_data, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +  # Plot points with transparency
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

############## K-Means Clustering ############## 

# Reduction to 2D
kmeans_data <- data.frame(X1 = mds_coords[, 1], X2 = mds_coords[, 2])

# Apply K-means clustering
set.seed(123)  # Set seed for reproducibility
num_clusters <- 6  # Set the number of clusters
kmeans_result <- kmeans(kmeans_data, centers = num_clusters, nstart = 25)

# Add cluster labels to the dataset
kmeans_data$cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters
ggplot(kmeans_data, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = example_commenter_jaccard$title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "K-Means Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

############## DBSCAN (Density-Based Spatial Clustering of Applications with Noise) ############## 

# Scale the similarity matrix
scaled_matrix <- scale(similarity_matrix)

# DBSCAN parameters
eps_value <- 0.05
min_points <- 4

# Perform DBSCAN
dbscan_result <- dbscan(similarity_matrix, eps = eps_value, minPts = min_points)

# Add cluster labels to the dataset
example_commenter_jaccard$dbscan_cluster <- dbscan_result$cluster

# Reduce dimensionality using MDS
mds_coords <- cmdscale(as.dist(1 - similarity_matrix), k = 2)

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
  for (eps_test in seq(0.05, 0.5, by = 0.05)) {
    test_result <- dbscan(similarity_matrix, eps = eps_test, minPts = minPts_test)
    cat("\nDBSCAN with eps =", eps_test, "and minPts =", minPts_test, "\n")
    print(table(test_result$cluster))
  }
}

############## Hierarchical Agglomerative Clustering ############## 

# Perform hierarchical clustering using Ward's method
distance_matrix <- as.dist(1 - similarity_matrix)
hc_result <- hclust(distance_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc_result, main = "Agglomerative Hierarchical Clustering", sub = "", xlab = "", cex = 0.8)

# Cut the dendrogram into clusters
num_clusters <- 6  # Adjustable (naudoju 6, nes tu nauji 6 :))
hc_clusters <- cutree(hc_result, k = num_clusters)

# Add cluster labels to the dataset
example_commenter_jaccard$hc_cluster <- hc_clusters

# Visualize clusters using MDS
mds_coords <- cmdscale(distance_matrix, k = 2)
visualization_data_aggl <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(hc_clusters)
)

# Plot with channel names
ggplot(visualization_data_aggl, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = example_commenter_jaccard$title),  # Add channel names
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

############## Hierarchical Divisive Clustering ############## 

# Perform divisive clustering
divisive_result <- diana(distance_matrix)

# Plot the dendrogram
plot(as.hclust(divisive_result), 
     main = "Divisive Hierarchical Clustering", 
     sub = "", 
     xlab = "", 
     cex = 0.8)

# Cut the dendrogram into 6 clusters
num_clusters <- 6
divisive_clusters <- cutree(as.hclust(divisive_result), k = num_clusters)

# Add cluster labels to the dataset
example_commenter_jaccard$divisive_cluster <- divisive_clusters

# Visualize the clusters using MDS
mds_coords <- cmdscale(distance_matrix, k = 2)
visualization_data_div <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(divisive_clusters)
)

# Plot with channel names
ggplot(visualization_data_div, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +  # Plot the points
  geom_text(aes(label = example_commenter_jaccard$title),  # Add channel names
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

############## Probabilistic Hierarchical Clustering ############## 

# Use the commenter Jaccard similarity data
data_matrix <- as.matrix(distance_matrix)

# Perform model-based clustering
#phc_result <- Mclust(data_matrix)

# Set the number of clusters manually (otherwise it's equal to 1)
phc_result <- Mclust(data_matrix, G = 6)
summary(phc_result)

# Add cluster labels to the dataset
example_commenter_jaccard$phc_cluster <- phc_result$classification

# Visualize the clustering results using MDS
mds_coords <- cmdscale(distance_matrix, k = 2)
visualization_data_prob <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(phc_result$classification)
)

ggplot(visualization_data_prob, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = example_commenter_jaccard$title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "Probabilistic Hierarchical Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()

############## Spectral Clustering ############## 

# Number of clusters
num_clusters <- 6

# Symmetrize the similarity matrix
symmetric_similarity_matrix <- (similarity_matrix + t(similarity_matrix)) / 2

# Fix dimnames to ensure consistency
dimnames(symmetric_similarity_matrix) <- NULL

# Normalize the matrix to a [0, 1] range
symmetric_similarity_matrix <- symmetric_similarity_matrix / max(symmetric_similarity_matrix)

# Perform spectral clustering (first time - error, second time it works) as nezinau, kas cia per xujne naxui
spectral_result <- specc(symmetric_similarity_matrix, centers = num_clusters)
spectral_result <- specc(symmetric_similarity_matrix, centers = num_clusters)

# Add cluster labels to the dataset
example_commenter_jaccard$spectral_cluster <- as.factor(spectral_result@.Data)

# Visualize the clusters using MDS
mds_coords <- cmdscale(distance_matrix, k = 2)
visualization_data_spec <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = example_commenter_jaccard$spectral_cluster
)

# Plot the clustering result with channel names
ggplot(visualization_data_spec, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +  # Plot the points
  geom_text(aes(label = example_commenter_jaccard$title),  # Add channel names
            size = 3,  # Adjust text size
            hjust = 0.5, vjust = -0.5,  # Position labels slightly above the points
            check_overlap = TRUE) +  # Avoid label overlap
  labs(
    title = "Spectral Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    plot.title = element_text(hjust = 0.5)  # Center-align title
  )

############## Graph-Based Local Clustering ############## 

# Create a graph from the Jaccard similarity matrix
graph <- similarity_matrix %>%
  graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE)

# Perform community detection using a local clustering algorithm
# Here, we use the Louvain method (fast and widely used)
community_result <- cluster_louvain(graph)

# Extract cluster memberships
cluster_memberships <- membership(community_result)

# Add cluster labels to the dataset
example_commenter_jaccard$graph_cluster <- cluster_memberships

# Visualize the graph with clusters
layout <- layout_with_fr(graph)  # Fruchterman-Reingold layout for visualization

# Another layout option
#layout <- layout_with_kk(graph)  # Kamada-Kawai layout (NOT KAWAII AT ALL)

plot(graph, layout = layout, 
     vertex.color = cluster_memberships, 
     vertex.label = example_commenter_jaccard$title,  # Add channel names as labels
     vertex.label.cex = 0.7,  # Adjust label size
     vertex.label.color = "black",  # Set label color
     vertex.size = 20,  # Increase node size for better visibility
     edge.color = "gray",  # Make edges less visually dominant
     edge.width = 0.5,  # Thin out edges for less clutter
     main = "Graph-Based Clustering")

# INTERACTIVE GRAPH!!!!!!!!!!!!!!! OMG OMG!!!!!!!! CRAZY
visIgraph(graph) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)


# MDS-based visualization
visualization_data_grph <- data.frame(
  X1 = mds_coords[, 1],
  X2 = mds_coords[, 2],
  cluster = as.factor(cluster_memberships)
)

ggplot(visualization_data_grph, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = example_commenter_jaccard$title), 
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

# ----------
# Prašau parunink šitą kodą :)
# Aš, kai Justas sako, kad mums reikia patobulinti vizualizaciją:
vis_nodes <- data.frame(
  id = V(graph)$name,
  label = V(graph)$name,
  group = as.vector(cluster_memberships),
  title = V(graph)$name 
)

vis_edges <- as.data.frame(get.edgelist(graph)) 
colnames(vis_edges) <- c("from", "to")

visNetwork(vis_nodes, vis_edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 123) 
# ----------

# Another interactive example
library(plotly)
plot_ly(
  x = mds_coords[, 1],
  y = mds_coords[, 2],
  text = example_commenter_jaccard$title,  # Hover text (channel names)
  color = as.factor(cluster_memberships),  # Cluster colors
  type = 'scatter',
  mode = 'markers+text'
) %>%
  layout(
    title = "MDS Visualization of Clusters",
    xaxis = list(title = "MDS Dimension 1"),
    yaxis = list(title = "MDS Dimension 2")
  )

############## Grid-Based Clustering ############## 

# Use MDS for reducing dimensionality to 2D (if needed for visualization)
grid_data <- data.frame(X1 = mds_coords[, 1], X2 = mds_coords[, 2])

# Define the grid
grid_size <- 6  # Adjust the grid resolution (higher = finer grid)
grid_data$grid_x <- cut(grid_data$X1, breaks = grid_size, labels = FALSE)
grid_data$grid_y <- cut(grid_data$X2, breaks = grid_size, labels = FALSE)

# Assign cluster IDs based on grid cells
grid_data$grid_cluster <- as.factor(paste(grid_data$grid_x, grid_data$grid_y, sep = "-"))

# Visualize the grid-based clustering
ggplot(grid_data, aes(x = X1, y = X2, color = grid_cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = example_commenter_jaccard$title), 
            size = 3, hjust = 0.5, vjust = -0.5, check_overlap = TRUE) +
  labs(
    title = "Grid-Based Clustering",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal()













