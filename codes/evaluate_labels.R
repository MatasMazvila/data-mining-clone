
#------------------------------------------------------------------------------
# Topic analysis, need to run EDA data loading first and select cluster below
#------------------------------------------------------------------------------

library(dplyr)
library(xtable)
library(clipr)

channel_info$main_cluster <- channel_info$gbc_cluster_cj

table(channel_info$main_cluster)

channel_info$topicCategories_list <- lapply(channel_info$topicCategories, function(x) {
  cleaned <- gsub("\\[|\\]|'", "", x)
  sort(strsplit(cleaned, ",\\s*")[[1]])
})



x <- channel_info %>%
  group_by(main_cluster) %>%
  summarise(
    total_comments = sum(commenter_count, na.rm = TRUE)/1e3,    # in thousands
    total_subs = sum(sub_count, na.rm = TRUE)/1e3,              # in thousands
    total_period_views = sum(period_views, na.rm = TRUE)/1e6   # in millions
)
xtable(x)

remove_wiki_prefix <- function(url) {
  sub("https://en.wikipedia.org/wiki/", "", url)
}

top_3_plus_others <- function(df, name_col="Category", count_col="Count") {
  df <- df[order(df[[count_col]], decreasing=TRUE), ]
  if(nrow(df) <= 3) {
    return(df)
  } else {
    top3 <- df[1:3, ]
    others_count <- sum(df[-(1:3), count_col])
    others_row <- data.frame(
      stringsAsFactors = FALSE,
      setNames(list("Others", others_count), c(name_col, count_col))
    )
    return(rbind(top3, others_row))
  }
}

clusters <- sort(unique(channel_info$main_cluster))

# Store all cluster tables in a variable
all_tables <- ""

for (cnum in clusters) {
  cluster_data <- channel_info %>% filter(main_cluster == cnum)
  
  # Video categories
  cat_tbl <- sort(table(cluster_data$most_common_video_category_name), decreasing=TRUE)
  cat_df <- data.frame(Category=names(cat_tbl), Count=as.vector(cat_tbl), stringsAsFactors=FALSE)
  cat_df <- top_3_plus_others(cat_df, name_col="Category", count_col="Count")
  
  # Topic categories
  all_topics <- unlist(cluster_data$topicCategories_list)
  all_topics <- sapply(all_topics, remove_wiki_prefix, USE.NAMES=FALSE)
  topic_tbl <- sort(table(all_topics), decreasing=TRUE)
  topic_df <- data.frame(Topic=names(topic_tbl), Count=as.vector(topic_tbl), stringsAsFactors=FALSE)
  topic_df <- top_3_plus_others(topic_df, name_col="Topic", count_col="Count")
  
  cat_xt <- print(
    xtable(cat_df),
    include.rownames = FALSE, floating = FALSE, comment = FALSE, print.results = FALSE
  )
  
  topic_xt <- print(
    xtable(topic_df),
    include.rownames = FALSE, floating = FALSE, comment = FALSE, print.results = FALSE
  )
  
  cat_xt_no_caption <- sub("\\\\caption\\{.*\\}\n", "", cat_xt)
  topic_xt_no_caption <- sub("\\\\caption\\{.*\\}\n", "", topic_xt)
  
  # Create the code for one cluster table
  cluster_table <- paste0(
    "\n% Combined table for cluster ", cnum, "\n",
    "\\vspace{1em}\n",  # add some vertical space before each cluster if needed
    "\\begin{minipage}{\\textwidth}\n",
    "\\centering\n",
    "\\begin{minipage}[c]{0.05\\linewidth}\n",
    "\\raggedright\\rotatebox{90}{\\vspace{0.5em}\\textbf{Cluster ", cnum, "}}\n",  # add vertical space before text
    "\\end{minipage}\n",
    "\\hfill\n",
    "\\begin{minipage}[c]{0.90\\linewidth}\n",
    "\\centering\n",
    "\\begin{minipage}[t]{0.45\\linewidth}\n\\centering\n",
    cat_xt_no_caption,
    "\\end{minipage}\n",
    "\\hfill\n",
    "\\begin{minipage}[t]{0.45\\linewidth}\n\\centering\n",
    topic_xt_no_caption,
    "\\end{minipage}\n",
    "\\end{minipage}\n",
    "\\end{minipage}\n"
  )
  
  # Append to all_tables
  all_tables <- paste0(all_tables, cluster_table, "\n")
}

# Print all tables wrapped in a single table environment
cat("\\begin{table}[H]\n",
    "\\centering\n",
    all_tables,
    "\\caption{All Clusters: Video and Topic Categories}\n",
    "\\label{tab:all_clusters}\n",
    "\\end{table}\n", sep="")
