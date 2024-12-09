#------------------------------------------------------------------------------
############## Collect Hopkins Statistics and Silhouette Scores ##############
#------------------------------------------------------------------------------

# Collect Hopkins statistics for K-Medoids
hopkins_stat_cj <- round(hopkins_stat_cj_medoids, 4)
hopkins_stat_co <- round(hopkins_stat_co_medoids, 4)
hopkins_stat_sj <- round(hopkins_stat_sj_medoids, 4)
hopkins_stat_so <- round(hopkins_stat_so_medoids, 4)

# Collect silhouette scores for K-Medoids
silhouette_cj_medoids <- silhouette(kmedoids_result_cj$clustering, distance_matrix_cj_medoids)
avg_silhouette_cj_medoids <- round(mean(silhouette_cj_medoids[, 3]), 4)

silhouette_co_medoids <- silhouette(kmedoids_result_co$clustering, distance_matrix_co_medoids)
avg_silhouette_co_medoids <- round(mean(silhouette_co_medoids[, 3]), 4)

silhouette_sj_medoids <- silhouette(kmedoids_result_sj$clustering, distance_matrix_sj_medoids)
avg_silhouette_sj_medoids <- round(mean(silhouette_sj_medoids[, 3]), 4)

silhouette_so_medoids <- silhouette(kmedoids_result_so$clustering, distance_matrix_so_medoids)
avg_silhouette_so_medoids <- round(mean(silhouette_so_medoids[, 3]), 4)

# Collect silhouette scores for K-Means
silhouette_cj_means <- silhouette(kmeans_result_cj$cluster, distance_matrix_cj_means)
avg_silhouette_cj_means <- round(mean(silhouette_cj_means[, 3]), 4)

silhouette_co_means <- silhouette(kmeans_result_co$cluster, distance_matrix_co_means)
avg_silhouette_co_means <- round(mean(silhouette_co_means[, 3]), 4)

silhouette_sj_means <- silhouette(kmeans_result_sj$cluster, distance_matrix_sj_means)
avg_silhouette_sj_means <- round(mean(silhouette_sj_means[, 3]), 4)

silhouette_so_means <- silhouette(kmeans_result_so$cluster, distance_matrix_so_means)
avg_silhouette_so_means <- round(mean(silhouette_so_means[, 3]), 4)

# Collect silhouette scores for DBSCAN
silhouette_cj_dbscan <- silhouette(dbscan_result_dbscan_cj$cluster, distance_matrix_dbscan_cj)
avg_silhouette_cj_dbscan <- round(mean(silhouette_cj_dbscan[, 3]), 4)

silhouette_co_dbscan <- silhouette(dbscan_result_dbscan_co$cluster, distance_matrix_dbscan_co)
avg_silhouette_co_dbscan <- round(mean(silhouette_co_dbscan[, 3]), 4)

silhouette_sj_dbscan <- silhouette(dbscan_result_dbscan_sj$cluster, distance_matrix_dbscan_sj)
avg_silhouette_sj_dbscan <- round(mean(silhouette_sj_dbscan[, 3]), 4)

silhouette_so_dbscan <- silhouette(dbscan_result_dbscan_so$cluster, distance_matrix_dbscan_so)
avg_silhouette_so_dbscan <- round(mean(silhouette_so_dbscan[, 3]), 4)

# Collect silhouette scores for HAC
silhouette_cj_hac <- silhouette(hc_clusters_hac_cj, distance_matrix_hac_cj)
avg_silhouette_cj_hac <- round(mean(silhouette_cj_hac[, 3]), 4)

silhouette_co_hac <- silhouette(hc_clusters_hac_co, distance_matrix_hac_co)
avg_silhouette_co_hac <- round(mean(silhouette_co_hac[, 3]), 4)

silhouette_sj_hac <- silhouette(hc_clusters_hac_sj, distance_matrix_hac_sj)
avg_silhouette_sj_hac <- round(mean(silhouette_sj_hac[, 3]), 4)

silhouette_so_hac <- silhouette(hc_clusters_hac_so, distance_matrix_hac_so)
avg_silhouette_so_hac <- round(mean(silhouette_so_hac[, 3]), 4)

# Create the data frame
clustering_scores_df <- data.frame(
  dataset = c("commenter jaccard", "commenter overlap", "subscriber jaccard", "subscriber overlap"),
  hopkins_statistic = c(
    hopkins_stat_cj,
    hopkins_stat_co,
    hopkins_stat_sj,
    hopkins_stat_so
  ),
  silhouette_k_medoids = c(
    avg_silhouette_cj_medoids,
    avg_silhouette_co_medoids,
    avg_silhouette_sj_medoids,
    avg_silhouette_so_medoids
  ),
  silhouette_k_means = c(
    avg_silhouette_cj_means,
    avg_silhouette_co_means,
    avg_silhouette_sj_means,
    avg_silhouette_so_means
  ),
  silhouette_dbscan = c(
    avg_silhouette_cj_dbscan,
    avg_silhouette_co_dbscan,
    avg_silhouette_sj_dbscan,
    avg_silhouette_so_dbscan
  ),
  silhouette_hac = c(
    avg_silhouette_cj_hac,
    avg_silhouette_co_hac,
    avg_silhouette_sj_hac,
    avg_silhouette_so_hac
  )
)

# Print the resulting data frame
print(clustering_scores_df)

# Table
library(knitr)
clustering_scores_table <- kable(
  clustering_scores_df,
  format = "simple",
  col.names = c("Dataset", "Hopkins Statistic", "Silhouette (K-Medoids)", "Silhouette (K-Means)", "Silhouette (DBSCAN)", "Silhouette (HAC)"),
  align = c("l", "c", "c", "c", "c") 
)
print(clustering_scores_table)
