
# run_local.R  -  Local demonstration of the Hadoop Streaming job
# Simulates: MAP (one mapper per document) -> SHUFFLE & SORT -> REDUCE
# using the SAME mapper.R and reducer.R that Hadoop would run.
#
# HOW TO RUN: open this file in RStudio and click "Source".
#             Type a keyword in the Console when asked, then press Enter.

# ---- Settings (no changes needed) --------------------------------
input_dir <- "data/map_reduce_test_docs"
rscript   <- file.path(R.home("bin"), "Rscript")

# ---- Dynamic keyword input (not hardcoded) ------------------------
if (interactive()) {
  keyword <- trimws(readline("Enter keyword to search: "))
} else {
  keyword <- "africa"        # only used if run outside RStudio
}
if (!nzchar(keyword)) stop("No keyword entered. Source the script again and type a keyword.")

# ---- Safety checks ------------------------------------------------
if (!file.exists("mapper.R") || !file.exists("reducer.R")) {
  stop("mapper.R / reducer.R not found in the working directory: ", getwd(),
       "\nSave both files in your RStudio project folder and try again.")
}
if (!dir.exists(input_dir)) {
  stop("Input folder not found: ", input_dir)
}

# ---- Collect the documents (ignore macOS junk files) --------------
files <- list.files(input_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
files <- files[!grepl("__MACOSX", files) & !grepl("^\\._", basename(files))]
files <- sort(files)
if (length(files) == 0) stop("No .txt documents found in: ", input_dir)

cat("Keyword  :", keyword, "\n")
cat("Documents:", length(files), "\n\n")

# ---- MAP: one mapper process per document (= one input split) -----
map_out <- character(0)
for (f in files) {
  Sys.setenv(mapreduce_map_input_file = f)       # what Hadoop sets for each split
  out <- system2(rscript, c("mapper.R", shQuote(keyword)),
                 stdin = f, stdout = TRUE)
  map_out <- c(map_out, out)
}
cat("---- MAP output (filename <TAB> row <TAB> column) ----\n")
if (length(map_out) == 0) {
  cat("(no matches found)\n")
  stop_quietly <- TRUE
} else {
  writeLines(map_out)
}

# ---- SHUFFLE & SORT: group records by key (filename) ---------------
if (length(map_out) > 0) {
  key      <- sub("\t.*", "", map_out)
  shuffled <- map_out[order(key, method = "radix")]
  tmp      <- tempfile()
  writeLines(shuffled, tmp)
  
  cat("\n---- SHUFFLE & SORT output (grouped by filename) ----\n")
  writeLines(shuffled)
  
  # ---- REDUCE -------------------------------------------------------
  red_out <- system2(rscript, "reducer.R", stdin = tmp, stdout = TRUE)
  
  cat("\n---- REDUCE output (filename <TAB> total <TAB> (row,col) list) ----\n")
  writeLines(red_out)
  
  writeLines(red_out, "part-00000")             # mimics Hadoop's output file
  cat("\nResult saved to:", file.path(getwd(), "part-00000"), "\n")
}