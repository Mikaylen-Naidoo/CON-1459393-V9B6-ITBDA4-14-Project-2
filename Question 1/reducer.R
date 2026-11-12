#!/usr/bin/env Rscript
# reducer.R - Hadoop Streaming reducer
# Input : filename <TAB> row <TAB> column   (sorted by filename by the shuffle)
# Output: filename <TAB> total matches <TAB> (row,col) (row,col) ...

con <- file("stdin", open = "r")

current <- NULL
rows <- integer(0); cols <- integer(0)

emit <- function(key, rows, cols) {
  if (is.null(key)) return(invisible())
  o <- order(rows, cols)                        # numeric order: row, then column
  locs <- paste0("(", rows[o], ",", cols[o], ")", collapse = " ")
  cat(key, "\t", length(rows), "\t", locs, "\n", sep = "")
}

repeat {
  lines <- readLines(con, n = 1000L, warn = FALSE)
  if (length(lines) == 0L) break
  for (line in lines) {
    f <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (length(f) < 3L) next                    # skip malformed records
    key <- f[1]
    if (is.null(current) || key != current) {   # new key -> flush previous group
      emit(current, rows, cols)
      current <- key
      rows <- integer(0); cols <- integer(0)
    }
    rows <- c(rows, as.integer(f[2]))
    cols <- c(cols, as.integer(f[3]))
  }
}
emit(current, rows, cols)                       # flush the last group
close(con)