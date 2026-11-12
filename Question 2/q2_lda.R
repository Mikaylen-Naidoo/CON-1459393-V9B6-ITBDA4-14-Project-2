
# q2_lda.R  -  ITBDA4-14 Project 2, Question 2
# Topic modelling of news articles with Latent Dirichlet Allocation
#   2.1 Build data frame (file_name, content) and pre-process the text
#   2.2 LDA: top n topics, top k words per topic, word distributions
#   2.3 Visualise the top words per topic (3 methods)
#

# Settings
input_dir <- "data/news"
n_topics  <- 3      # n : number of topics
k_words   <- 10     # k : top words shown per topic
seed      <- 1234   # makes the LDA result reproducible

# Packages (installed automatically if missing)
pkgs <- c("tm", "topicmodels", "ggplot2", "wordcloud", "RColorBrewer")
missing_pkgs <- pkgs[!(pkgs %in% rownames(installed.packages()))]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

cat("Working directory (outputs are saved here):\n", getwd(), "\n\n")


# 2.1  DATA FRAME + PRE-PROCESSING

if (!dir.exists(input_dir)) stop("Input folder not found: ", input_dir)

files <- list.files(input_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
files <- files[!grepl("__MACOSX", files) & !grepl("^\\._", basename(files))]
if (length(files) == 0) stop("No .txt files found in: ", input_dir)
files <- files[order(as.numeric(gsub("\\D", "", basename(files))))]   # news_1, news_2, ... news_10

read_doc <- function(f) {
  txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
  Encoding(txt) <- "UTF-8"
  paste(txt, collapse = " ")
}

# Data frame: col_1 = file_name, col_2 = content (raw text of each file)
news_df <- data.frame(
  file_name = basename(files),
  content   = vapply(files, read_doc, character(1), USE.NAMES = FALSE),
  stringsAsFactors = FALSE
)
headlines <- vapply(files, function(f) trimws(readLines(f, n = 1, warn = FALSE, encoding = "UTF-8")),
                    character(1), USE.NAMES = FALSE)

cat("---- 2.1  DATA FRAME (raw) ----\n")
cat("Rows:", nrow(news_df), " Columns:", paste(names(news_df), collapse = ", "), "\n\n")
print(data.frame(file_name = news_df$file_name,
                 content   = paste0(substr(news_df$content, 1, 60), "..."),
                 words     = vapply(strsplit(news_df$content, "\\s+"), length, integer(1))),
      row.names = FALSE)

# Data denoising 
custom_stop <- c("said", "says", "say", "will", "also", "can", "may", "must", "would",
                 "could", "should", "one", "per", "cent", "added", "according",
                 "however", "including", "among", "since", "new", "year", "years",
                 "sanews", "gov", "monday", "tuesday", "wednesday", "thursday", "friday",
                 "saturday", "sunday", "january", "february", "march", "april", "june",
                 "july", "august", "september", "october", "november", "december")
stop_all <- unique(c(tm::stopwords("english"), custom_stop))

weekdays_rx <- "(monday|tuesday|wednesday|thursday|friday|saturday|sunday)"
months_rx   <- "(january|february|march|april|may|june|july|august|september|october|november|december)"

clean_text <- function(x) {
  x <- gsub("[\r\n\t]", " ", x)                                     # line breaks, tabs
  x <- gsub("[\u2018\u2019\u201B\u00B4`]", "'", x, perl = TRUE)     # curly apostrophes -> '
  x <- gsub("[\u201C\u201D]", " ", x, perl = TRUE)                  # curly quotes
  x <- tolower(x)                                                   # lower case
  x <- gsub(paste0(weekdays_rx, ",? +", months_rx, " +[0-9]{1,2},? +[0-9]{4}"), " ", x)  # date lines
  x <- gsub("https?://[^ ]+|www\\.[^ ]+|[a-z0-9.]+\\.(gov|co|org|com)\\.za", " ", x)  # URLs / site names
  x <- gsub("[^ ]+@[^ ]+", " ", x)                                  # e-mail addresses
  x <- gsub("'s\\b", "", x, perl = TRUE)                            # possessives (Eskom's -> eskom)
  x <- gsub("[^a-z ]", " ", x)                                      # punctuation, numbers, symbols
  words <- strsplit(x, " +")[[1]]
  words <- words[nchar(words) >= 3 & !(words %in% stop_all)]        # short words + stop words
  paste(words, collapse = " ")
}

news_df$clean_content <- vapply(news_df$content, clean_text, character(1), USE.NAMES = FALSE)

cat("\n---- Pre-processing summary ----\n")
summary_df <- data.frame(
  file_name    = news_df$file_name,
  words_before = vapply(strsplit(news_df$content, "\\s+"), length, integer(1)),
  words_after  = vapply(strsplit(news_df$clean_content, " "), length, integer(1))
)
print(summary_df, row.names = FALSE)
cat("\nExample - before (", news_df$file_name[1], "):\n", substr(news_df$content[1], 1, 250), "...\n", sep = "")
cat("\nExample - after  (", news_df$file_name[1], "):\n", substr(news_df$clean_content[1], 1, 250), "...\n", sep = "")

write.csv(news_df, "q2_clean_dataframe.csv", row.names = FALSE)


# 2.2  LATENT DIRICHLET ALLOCATION

cat("\n\n---- 2.2  LDA (n =", n_topics, "topics, k =", k_words, "words) ----\n")

corpus <- VCorpus(VectorSource(news_df$clean_content))
dtm    <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))
dtm    <- dtm[, slam::col_sums(dtm > 0) >= 2]          # keep terms found in at least 2 documents
keep   <- slam::row_sums(dtm) > 0                      # drop any document left empty
dtm    <- dtm[keep, ]
doc_names <- news_df$file_name[keep]
doc_heads <- headlines[keep]
cat("Document-term matrix:", nrow(dtm), "documents x", ncol(dtm), "terms\n\n")

lda_model <- LDA(dtm, k = n_topics, method = "Gibbs",
                 control = list(seed = seed, burnin = 1000, iter = 2000, thin = 100))

post  <- posterior(lda_model)
beta  <- post$terms      # topic x term  : word distribution per topic
gamma <- post$topics     # document x topic : topic mixture per document

# Top k words per topic, with their probability (word distribution)
top_terms <- do.call(rbind, lapply(seq_len(n_topics), function(t) {
  ord <- order(beta[t, ], decreasing = TRUE)[seq_len(k_words)]
  data.frame(topic = paste("Topic", t), rank = seq_len(k_words),
             term = colnames(beta)[ord], probability = round(beta[t, ord], 4),
             stringsAsFactors = FALSE)
}))

cat("---- Top", k_words, "words per topic ----\n")
for (t in seq_len(n_topics)) {
  sub <- top_terms[top_terms$topic == paste("Topic", t), ]
  cat("\nTopic ", t, ":  ", paste(sub$term, collapse = ", "), "\n", sep = "")
}

cat("\n---- Word distribution per topic (probability of each word within the topic) ----\n")
for (t in seq_len(n_topics)) {
  sub <- top_terms[top_terms$topic == paste("Topic", t), c("rank", "term", "probability")]
  cat("\nTopic ", t, "  (top ", k_words, " words cover ",
      round(100 * sum(sub$probability), 1), "% of the topic)\n", sep = "")
  print(sub, row.names = FALSE)
}

cat("\n---- Topic mix per document (document-topic probabilities) ----\n")
doc_topics <- data.frame(file_name = doc_names,
                         round(gamma, 3), check.names = FALSE, stringsAsFactors = FALSE)
colnames(doc_topics)[2:(n_topics + 1)] <- paste("Topic", seq_len(n_topics))
doc_topics$dominant_topic <- paste("Topic", apply(gamma, 1, which.max))
doc_topics$headline <- doc_heads
print(doc_topics[, c("file_name", paste("Topic", seq_len(n_topics)), "dominant_topic")], row.names = FALSE)
cat("\nDocuments per dominant topic:\n"); print(table(doc_topics$dominant_topic))

write.csv(top_terms,  "q2_top_terms.csv",  row.names = FALSE)
write.csv(doc_topics, "q2_doc_topics.csv", row.names = FALSE)

# 2.3  VISUALISATION  (use the arrows in the Plots pane to move between plots)

cat("\n\n---- 2.3  VISUALISATIONS ----\n")

# (1) Bar charts: top k words per topic, bar length = word probability
bar_df <- top_terms
bar_df$id <- factor(seq_len(nrow(bar_df)), levels = rev(seq_len(nrow(bar_df))))
p_bar <- ggplot(bar_df, aes(x = id, y = probability, fill = topic)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ topic, scales = "free") +
  scale_x_discrete(labels = setNames(bar_df$term, bar_df$id)) +
  labs(title = paste("Top", k_words, "words per topic (LDA)"),
       x = NULL, y = "Word probability within topic") +
  theme_minimal(base_size = 12)
print(p_bar)
ggsave("q2_barplot_top_words.png", p_bar, width = 11, height = 5, dpi = 150)

# (2) Word clouds: one per topic, word size = word probability
draw_clouds <- function() {
  op <- par(mfrow = c(1, n_topics), mar = c(0, 0, 2, 0))
  on.exit(par(op))
  for (t in seq_len(n_topics)) {
    top <- sort(beta[t, ], decreasing = TRUE)[1:20]
    set.seed(seed)
    wordcloud(names(top), top, scale = c(3, 0.6), min.freq = 0, random.order = FALSE,
              colors = brewer.pal(8, "Dark2")[t])
    title(main = paste("Topic", t), cex.main = 1.4)
  }
}
draw_clouds()
png("q2_wordclouds.png", width = 1800, height = 600, res = 150); draw_clouds(); invisible(dev.off())

# (3) Heatmap: how strongly each document belongs to each topic
heat_df <- data.frame(
  file_name   = rep(doc_names, times = n_topics),
  topic       = rep(paste("Topic", seq_len(n_topics)), each = length(doc_names)),
  probability = as.vector(gamma)
)
heat_df$file_name <- factor(heat_df$file_name, levels = rev(doc_names))
p_heat <- ggplot(heat_df, aes(x = topic, y = file_name, fill = probability)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", probability)), size = 3.5) +
  scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
  labs(title = "Topic mix per document", x = NULL, y = NULL, fill = "Probability") +
  theme_minimal(base_size = 12)
print(p_heat)
ggsave("q2_heatmap_doc_topics.png", p_heat, width = 7, height = 5, dpi = 150)

cat("\nSaved to", getwd(), ":\n",
    " q2_clean_dataframe.csv, q2_top_terms.csv, q2_doc_topics.csv\n",
    " q2_barplot_top_words.png, q2_wordclouds.png, q2_heatmap_doc_topics.png\n")