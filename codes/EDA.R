library(readr)
library(xtable)

# Set to TRUE to save plots
saveplots <- TRUE

# Directory for saving plots
plot_dir <- "plotseda"

if(!dir.exists(plot_dir)) dir.create(plot_dir)

# save plot function
plotsave <- function(file, width = 16, height = 10)
{
  if(saveplots){
    fname <- file.path(plot_dir, paste0(file, ".pdf"))
    pdf(
      fname,
      width = width / 2.54,
      height = height / 2.54,
      pointsize = 10
    )
  } 
  par(
    mgp = c(2.5, 0.45, 0),
    tcl = -0.4,
    mar = c(5, 3.6, 1.1, 1.1)
  )
}


###############################################################################
# Channel Information
###############################################################################

channel_info <- read_csv("data/channel_info.csv")
channel_info$publishedAt <- as.Date(channel_info$publishedAt)
channel_info$year <- format(channel_info$publishedAt, "%Y")

channel_info$topicCategories_list <- lapply(channel_info$topicCategories, function(x) {
  cleaned <- gsub("\\[|\\]|'", "", x)
  sort(strsplit(cleaned, ",\\s*")[[1]])
})

# Convert to desired units
channel_info$viewCount_m <- channel_info$viewCount / 1e6
channel_info$subscriberCount_k <- channel_info$subscriberCount / 1e3
channel_info$period_views_m <- channel_info$period_views / 1e6
channel_info$sub_count_k <- channel_info$sub_count / 1e3
channel_info$commenter_count_k <- channel_info$commenter_count / 1e3

###############################################################################
# Figures: Creation Year
###############################################################################
plotsave("time", height=5)
barplot(table(channel_info$year), 
        xlab = "Creation Year", 
        ylab = "Channel Count", 
        las = 2,
        lwd = 2)
dev.off()


###############################################################################
# Histograms: Total views (M) and Total subscribers (K)
###############################################################################
plotsave("hist_general")
par(mfrow = c(1, 2), height=3)
hist(channel_info$subscriberCount_k, 
     breaks = 50,
     ylab = "Channel Count", 
     xlab = "Subscribers (K)",
     main = "Histogram of Total Subscribers"
)

hist(channel_info$viewCount_m, 
     breaks = 50,
     xlab = "Total Views (M)",
     ylab = "Channel Count",
     main = "Histogram of Total Views"
)
par(mfrow = c(1, 1))
dev.off()


# Channel summaries
summary(channel_info[, c("subscriberCount_k", "viewCount_m")])

###############################################################################
# Histograms: Period stats (subs (K) and commenters (K))
###############################################################################

channel_info$sub_ratio = channel_info$sub_count_k / channel_info$subscriberCount_k

plotsave("hist_subs_commenters", height=6)
par(mfrow = c(1, 3))

hist(channel_info$commenter_count_k, 
     breaks = 50,
     xlab = "Unique Commenters (K)",
     ylab = "Channel Count",
     main = "Commenters (Period)"
)

hist(channel_info$sub_count_k, 
     breaks = 50,
     ylab = "Channel Count", 
     xlab = "Approx. Subscribers (K)",
     main = "Approx. Subscribers (Period)"
)

hist(channel_info$sub_ratio,
     breaks = 50,
     xlab = "Approx. sub count / total sub count",
     ylab = "Channel Count",
     main = "Ratio of Subscribers found"
)
par(mfrow = c(1, 1))
dev.off()

summary(channel_info[, c("commenter_count_k", "sub_count_k", "subscriberCount_k", "sub_ratio")])

channel_info[order(channel_info$sub_ratio, decreasing = FALSE), c("title", "sub_ratio")][1:10, ]


###############################################################################
# Tables: Most Common Video Categories and Topic Categories
###############################################################################
cat_counts <- sort(table(channel_info$most_common_video_category_name), decreasing=TRUE)
all_topics <- unlist(channel_info$topicCategories_list)
topic_counts <- sort(table(all_topics), decreasing=TRUE)

# Print top categories to screen
cat_df <- data.frame(Category=names(cat_counts), Count=as.vector(cat_counts))
topic_df <- data.frame(Topic=names(topic_counts), Count=as.vector(topic_counts))

cat_xt <- xtable(head(cat_df, 10), caption="Top Video Categories")
topic_xt <- xtable(head(topic_df, 10), caption="Top Topic Categories")
print(cat_xt, type="latex")
print(topic_xt, type="latex")


###############################################################################
# Similarity Matrices
###############################################################################
process_matrix <- function(file) {
  sim_matrix <- read_csv(file)
  similarity_matrix <- as.matrix(sim_matrix[, -1])
  rownames(similarity_matrix) <- sim_matrix$title
  as.dist(similarity_matrix)
}

comment_jaccard_vals <- process_matrix("data/comment_jaccard_matrix.csv")
comment_overlap_vals <- process_matrix("data/comment_overlap_matrix.csv")
subs_jaccard_vals <- process_matrix("data/subs_jaccard_matrix.csv")
subs_overlap_vals <- process_matrix("data/subs_overlap_matrix.csv")

# Print stats for distance matrices
cat("Comment Jaccard Summary:\n")
print(summary(comment_jaccard_vals))

cat("Comment Overlap Summary:\n")
print(summary(comment_overlap_vals))

cat("Subscriber Jaccard Summary:\n")
print(summary(subs_jaccard_vals))

cat("Subscriber Overlap Summary:\n")
print(summary(subs_overlap_vals))

plotsave("matrix_hist")
par(mfrow = c(2, 2))
hist(comment_jaccard_vals,
     breaks = 50,
     xlab = "Jaccard index",
     ylab = "Frequency",
     main = "Comment Jaccard"
)

hist(as.vector(comment_overlap_vals),
     breaks = 50,
     xlab = "Overlap coefficient",
     ylab = "Frequency",
     main = "Comment Overlap"
)

hist(subs_jaccard_vals,
     breaks = 50,
     xlab = "Jaccard index",
     ylab = "Frequency",
     main = "Subscriber Jaccard"
)

hist(subs_overlap_vals,
     breaks = 50,
     xlab = "Overlap coefficient",
     ylab = "Frequency",
     main = "Subscriber Overlap"
)
par(mfrow = c(1, 1))
dev.off()


###############################################################################
# Correlations
###############################################################################
cor_cols <- c("commenter_count", "period_views", "sub_count", "viewCount", "subscriberCount")
cor_matrix <- cor(channel_info[, cor_cols], use="pairwise.complete.obs")
cat("\nCorrelation Matrix:\n")
print(cor_matrix)

###############################################################################
# Top Channels by Various Metrics
###############################################################################
# Replace period_comments with viewCount now
metrics <- c("commenter_count", "period_views", "sub_count")

top_n <- 3
top_channels_list <- lapply(metrics, function(m) {
  vals <- channel_info[[m]]
  
  # Scale units
  if(m == "commenter_count") {
    scaled_vals <- vals / 1e3
    val_label <- "(K)"
  } else if(m == "sub_count") {
    scaled_vals <- vals / 1e3
    val_label <- "(K)"
  } else if(m == "period_views") {
    scaled_vals <- vals / 1e6
    val_label <- "(M)"
  }
  
  idx <- order(vals, decreasing=TRUE)[1:top_n]
  data.frame(
    Metric = m,
    Channel = channel_info$title[idx],
    Value = paste0(round(scaled_vals[idx], 2), " ", val_label)
  )
})
names(top_channels_list) <- metrics

for (m in metrics) {
  cat("\nTop 3 by", m, ":\n")
  print(top_channels_list[[m]])
}
