# K-Means 

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