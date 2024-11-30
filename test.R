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

############## DATA ##############

# Load the data (Justas)
example_channel_info <- read_csv("youtube/clustering/example_channel_info.csv")
# View(example_channel_info)

example_commenter_jaccard <- read_csv("youtube/clustering/example_commenter_jaccard.csv")
# View(example_commenter_jaccard)

# Load the data (Matas)
example_channel_info <- read_csv("example_channel_info.csv")
example_commenter_jaccard <- read_csv("example_commenter_jaccard.csv")

############## ANALYSIS ############## 

# Set the row names to the titles
similarity_matrix <- as.matrix(example_commenter_jaccard[, -1])
rownames(similarity_matrix) <- example_commenter_jaccard$title

# Convert similarity to distance
distance_matrix <- 1 - similarity_matrix

# Convert to 'dist' object
distance_object <- as.dist(distance_matrix)


# Perform k-medoids clustering
k <- 6 # number of clusters (k)
kmedoids_result <- pam(distance_object, k)
kmedoids_result


# View the clustering result
kmedoids_clusters <- kmedoids_result$clustering

# Add the cluster labels to your original data
example_commenter_jaccard$kmedoids_cluster <- kmedoids_clusters

# Perform MDS to reduce dimensionality for visualization
mds_coords <- cmdscale(distance_object, k = 2)

# Plot k-medoids clustering result
plot(mds_coords, col = kmedoids_clusters, pch = 19, main = "K-Medoids Clustering")
text(mds_coords, labels = example_commenter_jaccard$title, pos = 3, cex = 0.7)