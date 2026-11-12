
#q3_webtext.R  -  ITBDA4-14 Project 2, Question 3
# WebText mining proof of concept: Wikipedia article "Natural language processing"
#   3.1 Compliant web scraping -> R data structure
#   3.2 Text pre-processing (denoising)
#   3.3 Text summarisation (3 techniques) + 4 visualisations


# Settings
page_url <- "https://en.wikipedia.org/wiki/Natural_language_processing"
robots_url <- "https://en.wikipedia.org/robots.txt"
snapshot <- "q3_raw_page.html"      # local copy of the downloaded page
top_n_terms <- 15                   # words shown in frequency chart
n_summary_sentences <- 5            # length of extractive summaries
seed <- 1234

# Packages (installed automatically if missing)
pkgs <- c("httr", "rvest", "xml2", "tm", "SnowballC", "ggplot2", "wordcloud", "RColorBrewer")
missing_pkgs <- pkgs[!(pkgs %in% rownames(installed.packages()))]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
cat("Working directory (outputs are saved here):\n", getwd(), "\n\n")


# 3.1  COMPLIANT WEB SCRAPING

cat("---- 3.1  WEB SCRAPING ----\n")

# (a) Identify ourselves: Wikimedia requires a descriptive User-Agent with contact details
if (interactive()) {
  contact <- trimws(readline("Enter your e-mail address (used in the User-Agent so Wikimedia can contact you): "))
} else {
  contact <- "student@example.com"
}
if (!nzchar(contact)) stop("An e-mail address is required for the User-Agent. Source the script again and type one.")
ua <- paste0("ITBDA4-14-Project2-Coursework/1.0 (Eduvos student research; single page request; contact: ", contact, ")")
cat("User-Agent:", ua, "\n")

# (b) robots.txt check: is this page allowed for automated access?
robots_allows <- function(txt, path) {
  lines <- trimws(sub("#.*$", "", strsplit(txt, "\r?\n")[[1]]))
  in_star <- FALSE; prev_ua <- FALSE; rules <- list()
  for (ln in lines) {
    if (!nzchar(ln)) next
    kv <- regmatches(ln, regexec("^([A-Za-z-]+)\\s*:\\s*(.*)$", ln))[[1]]
    if (length(kv) != 3) next
    key <- tolower(kv[2]); val <- kv[3]
    if (key == "user-agent") {
      if (!prev_ua) in_star <- FALSE
      if (val == "*") in_star <- TRUE
      prev_ua <- TRUE
    } else {
      prev_ua <- FALSE
      if (in_star && key %in% c("allow", "disallow") && nzchar(val)) rules[[length(rules) + 1]] <- c(key, val)
    }
  }
  allowed <- TRUE; best <- -1; best_rule <- "none"
  for (r in rules) {
    if (startsWith(path, r[2]) && nchar(r[2]) > best) {
      best <- nchar(r[2]); allowed <- (r[1] == "allow"); best_rule <- paste(r[1], r[2])
    }
  }
  list(allowed = allowed, n_rules = length(rules), rule = best_rule)
}

page_path <- sub("^https?://[^/]+", "", page_url)
online <- TRUE
robots_resp <- tryCatch(GET(robots_url, user_agent(ua), timeout(30)), error = function(e) NULL)
if (!is.null(robots_resp) && status_code(robots_resp) == 200) {
  robots_txt <- content(robots_resp, as = "text", encoding = "UTF-8")
  writeLines(robots_txt, "q3_robots.txt")
  chk <- robots_allows(robots_txt, page_path)
  cat("robots.txt read (", chk$n_rules, " rules for all bots). Path tested: ", page_path, "\n", sep = "")
  cat("Matching rule:", chk$rule, "\n")
  cat("Automated access to this page allowed:", chk$allowed, "\n")
  if (!chk$allowed) stop("robots.txt does not allow access to this page. Scraping stopped.")
} else {
  online <- FALSE
  cat("Could not read robots.txt (no connection or access refused).\n")
}

# (c) Download ONE page, politely, and keep a local copy
html_text <- NULL
if (online) {
  Sys.sleep(1)
  resp <- tryCatch(GET(page_url, user_agent(ua), timeout(60)), error = function(e) NULL)
  if (!is.null(resp) && status_code(resp) == 200) {
    writeBin(content(resp, as = "raw"), snapshot)
    html_text <- content(resp, as = "text", encoding = "UTF-8")
    cat("Page downloaded (HTTP 200) and saved as", snapshot, "\n")
  } else {
    cat("Download failed (status:", if (is.null(resp)) "no response" else status_code(resp), ").\n")
  }
}
if (is.null(html_text)) {
  if (file.exists(snapshot)) {
    cat("Using the saved local copy:", snapshot, "\n")
    html_text <- paste(readLines(snapshot, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    stop("Could not download the page and no saved copy exists. Check your internet connection and try again.")
  }
}
accessed <- format(Sys.time(), "%d %B %Y")

# (d) Parse the HTML into an R data structure
page <- read_html(html_text)
title_node <- html_element(page, "h1")
page_title <- if (inherits(title_node, "xml_missing")) "Natural language processing" else html_text2(title_node)
lastmod_node <- html_element(page, "#footer-info-lastmod")
last_edited <- if (inherits(lastmod_node, "xml_missing")) "not stated" else trimws(html_text2(lastmod_node))

body <- html_element(page, "div.mw-parser-output")
if (inherits(body, "xml_missing")) stop("Could not find the article text in the page (the page layout may have changed).")

# remove non-prose elements: citation markers, edit links, tables/infoboxes, figures, maths, navigation boxes
remove_sel <- c("sup", "style", "script", "table", "figure", ".mw-editsection", ".reference", ".noprint",
                ".navbox", ".hatnote", ".mwe-math-element", ".thumb", ".ambox", ".mw-empty-elt",
                ".metadata", ".sidebar", ".shortdescription")
for (sel in remove_sel) {
  nodes <- html_elements(body, sel)
  if (length(nodes) > 0) xml_remove(nodes)
}

# the article is split into nested <section> blocks, so collect headings, paragraphs and lists in document order
nodes <- html_elements(body, xpath = paste0(
  ".//*[self::h2 or self::h3 or self::h4 or ",
  "(self::p and not(ancestor::li)) or ",
  "((self::ul or self::ol) and not(ancestor::li) and not(ancestor::ul) and not(ancestor::ol))]"))
sec <- "Introduction"; subsec <- NA_character_
rows <- list()
add_row <- function(type, txt) {
  txt <- trimws(txt)
  if (nchar(txt) >= 20) rows[[length(rows) + 1]] <<- data.frame(section = sec, subsection = subsec,
                                                                type = type, text = txt, stringsAsFactors = FALSE)
}
for (i in seq_along(nodes)) {
  node <- nodes[[i]]
  nm   <- xml_name(node)
  if (nm %in% c("h2", "h3", "h4")) {
    htxt <- trimws(html_text2(node))
    if (nm == "h2") { sec <- htxt; subsec <- NA_character_ } else subsec <- htxt
  } else if (nm == "p") {
    add_row("paragraph", html_text2(node))
  } else {
    for (it in html_elements(node, xpath = "./li")) add_row("list item", html_text2(it))
  }
}
if (length(rows) == 0) stop("No article text could be extracted.")
web_df <- do.call(rbind, rows)
web_df <- data.frame(id = seq_len(nrow(web_df)), web_df, stringsAsFactors = FALSE)

# R data structure: a list holding the metadata and the content data frame
wiki_data <- list(
  meta = list(source = "Wikipedia (English)", title = page_title, url = page_url,
              accessed = accessed, last_edited = last_edited,
              licence = "CC BY-SA 4.0", user_agent = ua),
  content = web_df
)

cat("\nArticle        :", wiki_data$meta$title, "\n")
cat("URL            :", wiki_data$meta$url, "\n")
cat("Accessed       :", wiki_data$meta$accessed, "\n")
cat("Page last edit :", wiki_data$meta$last_edited, "\n")
cat("Extracted      :", nrow(web_df), "text blocks in", length(unique(web_df$section)), "sections\n\n")
cat("Structure of the R data structure:\n"); str(wiki_data, max.level = 1, give.attr = FALSE)
cat("\nFirst rows of the content data frame:\n")
print(data.frame(web_df[1:min(6, nrow(web_df)), c("id", "section", "type")],
                 text = paste0(substr(web_df$text[1:min(6, nrow(web_df))], 1, 70), "...")), row.names = FALSE)
cat("\nText blocks per section:\n"); print(table(factor(web_df$section, levels = unique(web_df$section))))

write.csv(web_df, "q3_web_data.csv", row.names = FALSE)


# 3.2  TEXT PRE-PROCESSING (DENOISING)

cat("\n\n---- 3.2  PRE-PROCESSING ----\n")

count_words <- function(x) sum(vapply(strsplit(trimws(x), "\\s+"), function(w) sum(nzchar(w)), integer(1)))

custom_stop <- c("also", "may", "can", "one", "used", "use", "using", "many", "well", "often", "called",
                 "known", "two", "new", "first", "however", "based", "within", "including", "example",
                 "examples", "such", "like", "since", "among", "per", "will", "would", "could", "much",
                 "often", "various", "several", "became", "become", "due", "thus", "eg", "etc")
stop_all <- unique(c(tm::stopwords("english"), custom_stop))

# Step 1: remove non-pertinent sections (references, links, navigation)
nonpert_rx <- "^(references|external links|see also|further reading|notes|sources|bibliography|citations|footnotes)$"
keep_rows  <- !grepl(nonpert_rx, trimws(web_df$section), ignore.case = TRUE)
clean_df   <- web_df[keep_rows, ]

# Step 2: remove citation markers, URLs, HTML entities, curly quotes, possessives, extra spaces
strip_markup <- function(x) {
  x <- gsub("\\[[^]]*\\]", " ", x)                          # [1], [citation needed], [edit]
  x <- gsub("https?://[^ ]+|www\\.[^ ]+", " ", x)           # URLs
  x <- gsub("&[a-z]+;|&#[0-9]+;", " ", x)                   # HTML entities
  x <- gsub("[\u2018\u2019\u201B`\u00B4]", "'", x, perl = TRUE)
  x <- gsub("[\u201C\u201D]", "\"", x, perl = TRUE)
  x <- gsub("[\u2013\u2014]", " ", x, perl = TRUE)          # dashes
  x <- gsub("[\r\n\t]", " ", x)
  gsub("\\s+", " ", trimws(x))
}
clean_df$text_clean <- strip_markup(clean_df$text)

# Step 3-6: tokenise -> lower case -> remove punctuation/numbers/symbols -> stop words and short words -> stem
tokenise <- function(x) {
  x <- tolower(x)
  x <- gsub("'s\\b", "", x, perl = TRUE)
  x <- gsub("[^a-z ]", " ", x)
  w <- unlist(strsplit(x, " +"))
  w[nchar(w) >= 3 & !(w %in% stop_all)]
}
clean_df$words <- lapply(clean_df$text_clean, tokenise)

raw_words      <- count_words(web_df$text)
sec_words      <- count_words(clean_df$text)
markup_words   <- count_words(clean_df$text_clean)
tokens_before  <- unlist(lapply(clean_df$text_clean, function(x) {
  w <- unlist(strsplit(gsub("[^a-z ]", " ", tolower(x)), " +")); w[nzchar(w)] }))
token_words    <- length(unlist(clean_df$words))

tok <- data.frame(section = rep(clean_df$section, lengths(clean_df$words)),
                  word = unlist(clean_df$words), stringsAsFactors = FALSE)
tok$stem <- SnowballC::wordStem(tok$word, language = "english")

steps <- data.frame(
  step = c("0. Raw extracted text (all sections)",
           "1. Remove non-pertinent sections (references, see also, ...)",
           "2. Remove citation markers, URLs, entities, odd symbols",
           "3. Lower case, remove punctuation and numbers",
           "4. Remove stop words and words shorter than 3 letters"),
  words = c(raw_words, sec_words, markup_words, length(tokens_before), token_words)
)
steps$change <- c(NA, diff(steps$words))   # negative = words removed; step 3 can rise because hyphenated words split in two
print(steps, row.names = FALSE)
cat("\nUnique words before stemming:", length(unique(tok$word)), "\n")
cat("Unique terms after stemming :", length(unique(tok$stem)), "\n")

ex_i <- which(nchar(clean_df$text) > 200)[1]; if (is.na(ex_i)) ex_i <- 1
cat("\nExample - raw text:\n", substr(clean_df$text[ex_i], 1, 300), "...\n", sep = "")
cat("\nExample - after cleaning:\n", paste(head(clean_df$words[[ex_i]], 35), collapse = " "), " ...\n", sep = "")

# Stem completion: show the most common real word for each stem so charts stay readable
tb <- table(tok$stem)
freq <- data.frame(stem = names(tb), freq = as.integer(tb), stringsAsFactors = FALSE)
lab  <- tapply(tok$word, tok$stem, function(w) names(sort(table(w), decreasing = TRUE))[1])
freq$term <- as.character(lab[freq$stem])
freq <- freq[order(-freq$freq, freq$term), ]
freq$share <- round(100 * freq$freq / sum(freq$freq), 2)
rownames(freq) <- NULL

write.csv(clean_df[, c("id", "section", "type", "text_clean")], "q3_clean_text.csv", row.names = FALSE)


# 3.3  TEXT SUMMARISATION

cat("\n\n---- 3.3  TEXT SUMMARISATION ----\n")

caption_txt <- paste0("Source: Wikipedia, '", page_title, "' (CC BY-SA 4.0), accessed ", accessed)

# Technique 1: term frequency and TF-IDF keywords
cat("\n[Technique 1] Keyword summary using term frequency\n")
cat("Total terms:", nrow(freq), " | Total tokens:", sum(freq$freq), "\n\n")
print(head(freq[, c("term", "freq", "share")], top_n_terms), row.names = FALSE)

# TF-IDF with each section treated as one document
sec_sizes <- table(tok$section)
min_tokens <- 50
big_secs <- names(sec_sizes)[sec_sizes >= min_tokens]
if (length(big_secs) < 3) big_secs <- names(sort(sec_sizes, decreasing = TRUE))[seq_len(min(3, length(sec_sizes)))]
big_secs <- names(sort(sec_sizes[big_secs], decreasing = TRUE))[seq_len(min(6, length(big_secs)))]
all_secs <- names(sec_sizes)
N <- length(all_secs)
doc_freq <- table(unlist(lapply(all_secs, function(s) unique(tok$stem[tok$section == s]))))
tfidf_list <- lapply(big_secs, function(s) {
  st <- tok$stem[tok$section == s]
  cnt <- table(st)
  tf  <- as.numeric(cnt) / length(st)
  idf <- log(N / as.numeric(doc_freq[names(cnt)]))
  d <- data.frame(section = s, stem = names(cnt), tf_idf = tf * idf, stringsAsFactors = FALSE)
  d <- d[order(-d$tf_idf), ]
  d <- head(d, 5)
  d$term <- as.character(lab[d$stem])
  d
})
tfidf_df <- do.call(rbind, tfidf_list)
cat("\nTop TF-IDF words per section (words that characterise each section):\n")
for (s in big_secs) cat("  ", s, ": ", paste(tfidf_df$term[tfidf_df$section == s], collapse = ", "), "\n", sep = "")

# Sentences (paragraphs only) for the extractive techniques
split_sentences <- function(x) {
  x <- gsub("\\b(e\\.g|i\\.e|etc|vs|Dr|Mr|Mrs|Prof|Fig|approx|cf|al)\\.", "\\1<dot>", x)
  s <- unlist(strsplit(x, "(?<=[.!?])\\s+(?=[A-Z\"'])", perl = TRUE))
  trimws(gsub("<dot>", ".", s, fixed = TRUE))
}
para <- clean_df[clean_df$type == "paragraph", ]
sent_list <- lapply(seq_len(nrow(para)), function(i) {
  s <- split_sentences(para$text_clean[i])
  if (length(s) == 0) return(NULL)
  data.frame(para_id = para$id[i], section = para$section[i], sentence = s, stringsAsFactors = FALSE)
})
sent_df <- do.call(rbind, sent_list)
sent_df$tokens <- lapply(sent_df$sentence, function(s) SnowballC::wordStem(tokenise(s), language = "english"))
sent_df$n_tokens <- lengths(sent_df$tokens)
sent_df <- sent_df[sent_df$n_tokens >= 6, ]
rownames(sent_df) <- NULL
sent_df$sent_id <- seq_len(nrow(sent_df))
cat("\nSentences available for extractive summaries:", nrow(sent_df), "\n")
article_words <- count_words(sent_df$sentence)

wrap_print <- function(sents) for (i in seq_along(sents)) cat(strwrap(paste0(i, ". ", sents[i]), width = 90, exdent = 3), sep = "\n")

# Technique 2: frequency-based sentence scoring
cat("\n[Technique 2] Extractive summary: frequency-based sentence scoring\n")
tf_norm <- setNames(freq$freq / max(freq$freq), freq$stem)
sent_df$freq_score <- vapply(sent_df$tokens, function(tk) sum(tf_norm[tk], na.rm = TRUE) / sqrt(length(tk)), numeric(1))
pick_freq <- head(order(-sent_df$freq_score), n_summary_sentences)
pick_freq <- sort(pick_freq)
sum_freq <- sent_df$sentence[pick_freq]
wrap_print(sum_freq)

# Technique 3: TextRank (graph-based ranking, PageRank on a sentence similarity graph)
cat("\n[Technique 3] Extractive summary: TextRank\n")
vocab <- unique(unlist(sent_df$tokens))
M <- matrix(0, nrow = nrow(sent_df), ncol = length(vocab), dimnames = list(NULL, vocab))
for (i in seq_len(nrow(sent_df))) {
  ct <- table(sent_df$tokens[[i]]); M[i, names(ct)] <- as.numeric(ct)
}
idf_v <- log(nrow(M) / pmax(colSums(M > 0), 1))
W <- sweep(M, 2, idf_v, `*`)
norms <- sqrt(rowSums(W^2)); norms[norms == 0] <- 1
Wn <- W / norms
S <- Wn %*% t(Wn); diag(S) <- 0                        # cosine similarity between sentences
rs <- rowSums(S); rs[rs == 0] <- 1
P <- S / rs                                              # transition matrix
d <- 0.85; n <- nrow(S); r <- rep(1 / n, n)
for (it in 1:100) {
  r_new <- (1 - d) / n + d * as.numeric(t(P) %*% r)
  if (sum(abs(r_new - r)) < 1e-8) { r <- r_new; break }
  r <- r_new
}
sent_df$textrank <- r
pick_tr <- sort(head(order(-sent_df$textrank), n_summary_sentences))
sum_tr <- sent_df$sentence[pick_tr]
wrap_print(sum_tr)

# Comparison of the two summaries
overlap <- length(intersect(pick_freq, pick_tr))
cat("\nSummary comparison\n")
cat("  Article length (words in sentences):", article_words, "\n")
cat("  Frequency summary:", count_words(sum_freq), "words (", round(100 * count_words(sum_freq) / article_words, 1), "% of article )\n")
cat("  TextRank summary :", count_words(sum_tr), "words (", round(100 * count_words(sum_tr) / article_words, 1), "% of article )\n")
cat("  Sentences chosen by both methods:", overlap, "of", n_summary_sentences, "\n")

writeLines(c(paste("Summaries of:", page_title), caption_txt, "",
             "FREQUENCY-BASED EXTRACTIVE SUMMARY", sum_freq, "",
             "TEXTRANK EXTRACTIVE SUMMARY", sum_tr), "q3_summaries.txt")
write.csv(freq, "q3_term_frequency.csv", row.names = FALSE)
write.csv(tfidf_df[, c("section", "term", "tf_idf")], "q3_tfidf_by_section.csv", row.names = FALSE)
write.csv(sent_df[, c("sent_id", "section", "sentence", "freq_score", "textrank")], "q3_sentence_scores.csv", row.names = FALSE)


# VISUALISATIONS (use the arrows in the Plots pane to move between plots)

cat("\n\n---- VISUALISATIONS ----\n")

# Plot 1: most frequent terms
top_df <- head(freq, top_n_terms)
top_df$term <- factor(top_df$term, levels = rev(top_df$term))
p1 <- ggplot(top_df, aes(x = term, y = freq)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = freq), hjust = -0.2, size = 3.5) +
  coord_flip() +
  expand_limits(y = max(top_df$freq) * 1.1) +
  labs(title = paste("Top", top_n_terms, "most frequent terms"),
       subtitle = paste0("Pre-processed text of the Wikipedia article '", page_title, "'"),
       x = "Term (stem completed to the most common word form)", y = "Frequency (number of occurrences)",
       caption = caption_txt) +
  theme_minimal(base_size = 12)
print(p1); ggsave("q3_plot1_term_frequency.png", p1, width = 10, height = 6, dpi = 150)

# Plot 2: word cloud
draw_cloud <- function(sc = c(2.4, 0.45)) {
  op <- par(mar = c(0, 0, 2.5, 0)); on.exit(par(op))
  wc <- head(freq, 50)
  set.seed(seed)
  wordcloud(wc$term, wc$freq, scale = sc, min.freq = 1, random.order = FALSE,
            colors = brewer.pal(8, "Dark2"))
  title(main = paste0("Word cloud of the 50 most frequent terms - ", page_title), cex.main = 1.1)
  mtext("Word size is proportional to term frequency", side = 1, line = -1, cex = 0.8)
}
draw_cloud()
png("q3_plot2_wordcloud.png", width = 1600, height = 1000, res = 150); draw_cloud(c(3.6, 0.7)); invisible(dev.off())

# Plot 3: TF-IDF keywords by section
tfidf_df$term_id <- factor(seq_len(nrow(tfidf_df)), levels = rev(seq_len(nrow(tfidf_df))))
tfidf_df$section <- factor(tfidf_df$section, levels = big_secs)
p3 <- ggplot(tfidf_df, aes(x = term_id, y = tf_idf, fill = section)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ section, scales = "free") +
  scale_x_discrete(labels = setNames(tfidf_df$term, tfidf_df$term_id)) +
  labs(title = "Characteristic words per section (TF-IDF)",
       subtitle = "Higher TF-IDF = word is frequent in this section but rare in the other sections",
       x = "Term", y = "TF-IDF score", caption = caption_txt) +
  theme_minimal(base_size = 11)
print(p3); ggsave("q3_plot3_tfidf_by_section.png", p3, width = 11, height = 7, dpi = 150)

# Plot 4: TextRank sentence scores
top_s <- head(sent_df[order(-sent_df$textrank), ], 10)
top_s$label <- paste0("S", top_s$sent_id, ": ", substr(top_s$sentence, 1, 60), "...")
top_s$in_summary <- ifelse(top_s$sent_id %in% sent_df$sent_id[pick_tr], "Selected for summary", "Not selected")
top_s$label <- factor(top_s$label, levels = rev(top_s$label))
p4 <- ggplot(top_s, aes(x = label, y = textrank, fill = in_summary)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Selected for summary" = "darkorange", "Not selected" = "grey70")) +
  labs(title = "TextRank: ten highest-ranked sentences",
       subtitle = "Sentences are ranked by how similar they are to many other sentences in the article",
       x = "Sentence (number and opening words)", y = "TextRank score", fill = NULL, caption = caption_txt) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
print(p4); ggsave("q3_plot4_textrank_sentences.png", p4, width = 11, height = 6, dpi = 150)

cat("\nSaved to", getwd(), ":\n",
    " q3_raw_page.html, q3_robots.txt, q3_web_data.csv, q3_clean_text.csv, q3_summaries.txt\n",
    " q3_term_frequency.csv, q3_tfidf_by_section.csv, q3_sentence_scores.csv\n",
    " q3_plot1_term_frequency.png, q3_plot2_wordcloud.png, q3_plot3_tfidf_by_section.png, q3_plot4_textrank_sentences.png\n")