#------------------------------------------------------------------------------
############## Graph-Based Clustering (GBC) ##############
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
library(plotly)
#------------------------------------------------------------------------------
############## DATA ##############
#------------------------------------------------------------------------------
channel_info <- read_csv("data/channel_info.csv")
commenters_jaccard <- read_csv("data/comment_jaccard_matrix.csv")
commenters_overlap <- read_csv("data/comment_overlap_matrix.csv")
subs_jaccard <- read_csv("data/subs_jaccard_matrix.csv")
subs_overlap <- read_csv("data/subs_overlap_matrix.csv")
#------------------------------------------------------------------------------
############## Commenters Jaccard Dataset (Graph-Based Clustering) ##############
#------------------------------------------------------------------------------

# Ensure that information and distance matrix line up
stopifnot(all(channel_info$title == commenters_jaccard$title))

# Define similarity as matrix
similarity_matrix_gbc_cj <- as.matrix(commenters_jaccard[, -1])

# Remove dimnames to avoid warnings
rownames(similarity_matrix_gbc_cj) <- NULL
colnames(similarity_matrix_gbc_cj) <- NULL

# Create a graph from the Jaccard similarity matrix
graph_gbc_cj <- similarity_matrix_gbc_cj %>%
  graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE)

# Perform community detection using the Louvain method
community_result_gbc_cj <- cluster_louvain(graph_gbc_cj)

# Compute the modularity score to check clustering quality
modularity_score <- modularity(community_result_gbc_cj)
cat("Modularity Score:", modularity_score, "\n")

# Extract cluster memberships
cluster_memberships_gbc_cj <- membership(community_result_gbc_cj)

# Add cluster labels to the dataset
channel_info$gbc_cluster_cj <- cluster_memberships_gbc_cj

# MDS-based visualization
mds_coords_gbc_cj <- cmdscale(as.dist(1 - similarity_matrix_gbc_cj), k = 2)

visualization_data_gbc_cj <- data.frame(
  X1 = mds_coords_gbc_cj[, 1],
  X2 = mds_coords_gbc_cj[, 2],
  cluster = as.factor(cluster_memberships_gbc_cj)
)

ggplot(visualization_data_gbc_cj, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "Graph-Based Clustering with Channel Names (Commenters Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#------------------------------------------------------------------------------
############## Commenters Overlap Dataset (GBC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == commenters_overlap$title))

similarity_matrix_gbc_co <- as.matrix(commenters_overlap[, -1])
rownames(similarity_matrix_gbc_co) <- NULL
colnames(similarity_matrix_gbc_co) <- NULL

graph_gbc_co <- similarity_matrix_gbc_co %>%
  graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE)

community_result_gbc_co <- cluster_louvain(graph_gbc_co)

modularity_score <- modularity(community_result_gbc_co)
cat("Modularity Score:", modularity_score, "\n")

cluster_memberships_gbc_co <- membership(community_result_gbc_co)

channel_info$gbc_cluster_co <- cluster_memberships_gbc_co

mds_coords_gbc_co <- cmdscale(as.dist(1 - similarity_matrix_gbc_co), k = 2)

visualization_data_gbc_co <- data.frame(
  X1 = mds_coords_gbc_co[, 1],
  X2 = mds_coords_gbc_co[, 2],
  cluster = as.factor(cluster_memberships_gbc_co)
)

ggplot(visualization_data_gbc_co, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "Graph-Based Clustering with Channel Names (Commenters Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#------------------------------------------------------------------------------
############## Subsriber Jaccard Dataset (GBC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_jaccard$title))

similarity_matrix_gbc_sj <- as.matrix(subs_jaccard[, -1])
rownames(similarity_matrix_gbc_sj) <- NULL
colnames(similarity_matrix_gbc_sj) <- NULL

graph_gbc_sj <- similarity_matrix_gbc_sj %>%
  graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE)

community_result_gbc_sj <- cluster_louvain(graph_gbc_sj)

modularity_score <- modularity(community_result_gbc_sj)
cat("Modularity Score:", modularity_score, "\n")

cluster_memberships_gbc_sj <- membership(community_result_gbc_sj)

channel_info$gbc_cluster_sj <- cluster_memberships_gbc_sj

mds_coords_gbc_sj <- cmdscale(as.dist(1 - similarity_matrix_gbc_sj), k = 2)

visualization_data_gbc_sj <- data.frame(
  X1 = mds_coords_gbc_sj[, 1],
  X2 = mds_coords_gbc_sj[, 2],
  cluster = as.factor(cluster_memberships_gbc_sj)
)

ggplot(visualization_data_gbc_sj, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "Graph-Based Clustering with Channel Names (Subs Jaccard)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#------------------------------------------------------------------------------
############## Subsriber Overlap Dataset (GBC) ##############
#------------------------------------------------------------------------------

stopifnot(all(channel_info$title == subs_overlap$title))

similarity_matrix_gbc_so <- as.matrix(subs_overlap[, -1])
rownames(similarity_matrix_gbc_so) <- NULL
colnames(similarity_matrix_gbc_so) <- NULL

graph_gbc_so <- similarity_matrix_gbc_so %>%
  graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE, diag = FALSE)

community_result_gbc_so <- cluster_louvain(graph_gbc_so)

modularity_score <- modularity(community_result_gbc_so)
cat("Modularity Score:", modularity_score, "\n")

cluster_memberships_gbc_so <- membership(community_result_gbc_so)

channel_info$gbc_cluster_so <- cluster_memberships_gbc_so

mds_coords_gbc_so <- cmdscale(as.dist(1 - similarity_matrix_gbc_so), k = 2)

visualization_data_gbc_so <- data.frame(
  X1 = mds_coords_gbc_so[, 1],
  X2 = mds_coords_gbc_so[, 2],
  cluster = as.factor(cluster_memberships_gbc_so)
)

ggplot(visualization_data_gbc_so, aes(x = X1, y = X2, color = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = channel_info$title), 
            size = 3, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE) +
  labs(
    title = "Graph-Based Clustering with Channel Names (Subs Overlap)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )
