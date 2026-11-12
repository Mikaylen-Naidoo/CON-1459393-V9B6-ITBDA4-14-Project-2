#!/usr/bin/env Rscript
# mapper.R - Hadoop Streaming mapper (keyword search with row/column)
# Usage : Rscript mapper.R <keyword>
# Input : text lines on stdin (one input split = one document)
# Output: filename <TAB> row <TAB> column      (one record per match)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || !nzchar(args[1])) {
  write("ERROR: keyword argument missing. Usage: Rscript mapper.R <keyword>", stderr())
  quit(status = 1)
}
keyword <- args[1]

# Escape regex metacharacters so the keyword is matched literally,
# then add word boundaries so "africa" does not match "African".
escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
pattern <- paste0("\\b", escape_regex(keyword), "\\b")

# Hadoop exposes the current input file as an environment variable
input_file <- Sys.getenv("mapreduce_map_input_file",
                         unset = Sys.getenv("map_input_file", unset = "unknown"))
doc_name <- basename(input_file)

con <- file("stdin", open = "r")
row <- 0L

repeat {
  lines <- readLines(con, n = 1000L, warn = FALSE)
  if (length(lines) == 0L) break
  Encoding(lines) <- "UTF-8"   # mark (do not re-encode) so columns count characters, not bytes
  
  for (line in lines) {
    row <- row + 1L            # blank lines still count as rows
    if (!nzchar(line)) next
    
    # case-insensitive search; (?i) works with perl = TRUE
    m <- gregexpr(paste0("(?i)", pattern), line, perl = TRUE)[[1]]
    if (m[1] != -1L) {
      for (col in as.integer(m)) {   # one record per occurrence
        cat(doc_name, "\t", row, "\t", col, "\n", sep = "")
      }
    }
  }
}
close(con)