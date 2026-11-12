# q4_scrape_clean.R  -  ITBDA4-14 Project 2, Question 4 (parts 4.1 and 4.2)
# Use case: profile dataset of South Africa's public universities, scraped from Wikipedia
#   4.1 Web scraping + regex: 26 article records -> data frame (text + numeric attributes)
#   4.2 Cleaning, denoising, missing-value imputation -> cleaned CSV for loading into Postgres

# Settings
page_titles <- c("University of Cape Town", "University of the Witwatersrand", "Stellenbosch University",
                 "University of Pretoria", "University of Johannesburg", "University of KwaZulu-Natal",
                 "University of the Free State", "Nelson Mandela University", "Rhodes University",
                 "North-West University", "University of Limpopo", "University of Fort Hare",
                 "University of the Western Cape", "University of Venda", "University of Zululand",
                 "Walter Sisulu University", "Durban University of Technology",
                 "Cape Peninsula University of Technology", "Tshwane University of Technology",
                 "Vaal University of Technology", "Central University of Technology",
                 "Mangosuthu University of Technology", "University of South Africa",
                 "Sol Plaatje University", "University of Mpumalanga",
                 "Sefako Makgatho Health Sciences University")
base_url    <- "https://en.wikipedia.org/wiki/"
robots_url  <- "https://en.wikipedia.org/robots.txt"
cache_dir   <- "q4_raw_pages"     # downloaded pages are kept here, so re-runs make no new requests
refresh     <- FALSE              # TRUE = download everything again
delay_sec   <- 1.5                # pause between requests (polite scraping)
min_records <- 20                 # brief: at least 20 records

# Packages (installed automatically if missing)
pkgs <- c("httr", "rvest", "xml2")
missing_pkgs <- pkgs[!(pkgs %in% rownames(installed.packages()))]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
cat("Working directory (outputs are saved here):\n", getwd(), "\n\n")
if (!dir.exists(cache_dir)) dir.create(cache_dir)


# 4.1  WEB SCRAPING AND REGEX ANALYSIS

cat("---- 4.1  WEB SCRAPING ----\n")
cat("Use case: profile of South Africa's public universities (source: Wikipedia, CC BY-SA 4.0)\n\n")

# (a) Identify ourselves: Wikimedia requires a descriptive User-Agent with contact details
if (interactive()) {
  contact <- trimws(readline("Enter your e-mail address (used in the User-Agent so Wikimedia can contact you): "))
} else {
  contact <- "student@example.com"
}
if (!nzchar(contact)) stop("An e-mail address is required for the User-Agent. Source the script again and type one.")
ua <- paste0("ITBDA4-14-Project2-Coursework/1.0 (Eduvos student research; ", length(page_titles),
             " article requests; contact: ", contact, ")")
cat("User-Agent:", ua, "\n")

# (b) robots.txt check
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
  allowed <- TRUE; best <- -1
  for (r in rules) {
    if (startsWith(path, r[2]) && nchar(r[2]) > best) { best <- nchar(r[2]); allowed <- (r[1] == "allow") }
  }
  list(allowed = allowed, n_rules = length(rules))
}

title_to_path <- function(t) paste0("/wiki/", gsub(" ", "_", t))
online <- TRUE
robots_resp <- tryCatch(GET(robots_url, user_agent(ua), timeout(30)), error = function(e) NULL)
if (!is.null(robots_resp) && status_code(robots_resp) == 200) {
  robots_txt <- content(robots_resp, as = "text", encoding = "UTF-8")
  writeLines(robots_txt, "q4_robots.txt")
  ok <- vapply(page_titles, function(t) robots_allows(robots_txt, title_to_path(t))$allowed, logical(1))
  cat("robots.txt read (", robots_allows(robots_txt, "/")$n_rules, " rules for all bots).\n", sep = "")
  cat("Article pages allowed for automated access:", sum(ok), "of", length(ok), "\n")
  if (!all(ok)) {
    cat("Skipping pages disallowed by robots.txt:", paste(page_titles[!ok], collapse = "; "), "\n")
    page_titles <- page_titles[ok]
  }
} else {
  online <- FALSE
  cat("Could not read robots.txt (no connection or access refused). Only saved pages will be used.\n")
}

# (c) Download each article once, politely, and keep a local copy
fetch_page <- function(title) {
  fn  <- file.path(cache_dir, paste0(gsub("[^A-Za-z0-9]+", "_", title), ".html"))
  url <- paste0(base_url, gsub(" ", "_", title))
  if (!refresh && file.exists(fn)) {
    return(list(html = paste(readLines(fn, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                url = url, source = "saved copy", status = 200L,
                fetched = format(as.Date(file.info(fn)$mtime), "%Y-%m-%d")))
  }
  if (!online) return(list(html = NULL, url = url, source = "none", status = NA_integer_, fetched = NA_character_))
  Sys.sleep(delay_sec)
  resp <- tryCatch(GET(url, user_agent(ua), timeout(60)), error = function(e) NULL)
  if (!is.null(resp) && status_code(resp) == 200) {
    writeBin(content(resp, as = "raw"), fn)
    return(list(html = content(resp, as = "text", encoding = "UTF-8"), url = url, source = "downloaded", status = 200L,
                fetched = format(Sys.Date(), "%Y-%m-%d")))
  }
  list(html = NULL, url = url, source = "failed", status = if (is.null(resp)) NA_integer_ else status_code(resp), fetched = NA_character_)
}

# (d) Parse one article: infobox rows (label/value) + lead paragraphs + page-level counts
get_label_value <- function(tbl) {
  rows <- html_elements(tbl, xpath = ".//tr[th and td]")
  if (length(rows) == 0) return(data.frame(label = character(0), value = character(0), stringsAsFactors = FALSE))
  lab <- vapply(rows, function(r) trimws(html_text2(html_element(r, "th"))), character(1))
  val <- vapply(rows, function(r) trimws(html_text2(html_element(r, "td"))), character(1))
  data.frame(label = lab, value = val, stringsAsFactors = FALSE)
}
pick <- function(kv, rx) {
  i <- which(grepl(rx, kv$label, ignore.case = TRUE) & nzchar(kv$value))
  if (length(i) == 0) NA_character_ else kv$value[i[1]]
}
count_words <- function(x) { w <- strsplit(trimws(x), "\\s+")[[1]]; sum(nzchar(w)) }

parse_article <- function(html_text, title, url) {
  page <- read_html(html_text)
  bodies <- html_elements(page, "div.mw-parser-output")          # some pages also have a tiny coordinates box
  if (length(bodies) == 0) return(NULL)
  body <- bodies[[which.max(vapply(bodies, function(b) nchar(html_text(b)), integer(1)))]]
  for (sel in c("style", "script")) {                              # hidden CSS/JS code must not leak into the text
    nodes <- html_elements(body, sel)
    if (length(nodes) > 0) xml_remove(nodes)
  }
  h1   <- html_element(page, "h1")
  name <- if (inherits(h1, "xml_missing")) title else trimws(html_text2(h1))
  
  ib <- html_element(body, "table.infobox")
  kv <- if (inherits(ib, "xml_missing")) data.frame(label = character(0), value = character(0)) else get_label_value(ib)
  
  paras <- html_elements(body, xpath = ".//p[not(ancestor::table)]")
  ptxt  <- vapply(paras, function(p) trimws(html_text2(p)), character(1))
  lead  <- paste(head(ptxt[nchar(ptxt) >= 60], 2), collapse = " ")
  if (!nzchar(lead)) lead <- NA_character_
  
  # first-pass numbers: deliberately simple regex (the first number-like token), cleaned properly in 4.2
  first_number <- function(x) {
    if (is.na(x)) return(NA_real_)
    m <- regmatches(x, regexpr("[0-9][0-9,. \u00A0]*", x, perl = TRUE))
    if (length(m) == 0) NA_real_ else suppressWarnings(as.numeric(gsub("[^0-9.]", "", m)))
  }
  first_year <- function(x) {
    if (is.na(x)) return(NA_real_)
    m <- regmatches(x, regexpr("[0-9]{4}", x))
    if (length(m) == 0) NA_real_ else as.numeric(m)
  }
  est_raw <- pick(kv, "^(established|founded|founding|opened)")
  stu_raw <- pick(kv, "^(students|total students|total enrol|enrol|student (body|population|enrol))")
  stf_raw <- pick(kv, "^(academic staff|faculty|staff|total staff|employees)")
  
  data.frame(
    university          = name,
    source_url          = url,
    lead_text           = lead,
    established_raw     = est_raw,
    type_raw            = pick(kv, "^type"),
    students_raw        = stu_raw,
    undergraduates_raw  = pick(kv, "^undergrad"),
    postgraduates_raw   = pick(kv, "^(postgrad|post-grad)"),
    staff_raw           = stf_raw,
    location_raw        = pick(kv, "^(location|campus|campuses|address|city)"),
    founded_first_pass  = first_year(est_raw),
    students_first_pass = first_number(stu_raw),
    staff_first_pass    = first_number(stf_raw),
    article_words       = sum(vapply(ptxt, count_words, integer(1))),
    n_citations         = length(html_elements(body, "sup.reference")),
    n_sections          = length(html_elements(body, "h2")),
    stringsAsFactors = FALSE
  )
}

cat("\nDownloading and parsing ", length(page_titles), " article pages (one request each, ", delay_sec, " s pause) ...\n", sep = "")
rows <- list(); log_df <- data.frame()
for (i in seq_along(page_titles)) {
  pg <- fetch_page(page_titles[i])
  rec <- if (!is.null(pg$html)) tryCatch(parse_article(pg$html, page_titles[i], pg$url), error = function(e) NULL) else NULL
  if (!is.null(rec)) { rec$scraped_on <- pg$fetched; rows[[length(rows) + 1]] <- rec }
  log_df <- rbind(log_df, data.frame(page = page_titles[i], source = pg$source,
                                     parsed = !is.null(rec), stringsAsFactors = FALSE))
  cat(sprintf("  %2d/%d  %-45s %-12s %s\n", i, length(page_titles), page_titles[i], pg$source,
              if (!is.null(rec)) "parsed" else "NOT parsed"))
}
if (length(rows) < min_records) {
  stop("Only ", length(rows), " pages could be parsed (need at least ", min_records,
       "). Check your internet connection and the messages above, then Source the script again.")
}
raw_df <- do.call(rbind, rows)
raw_df <- data.frame(id = seq_len(nrow(raw_df)), raw_df, stringsAsFactors = FALSE)

cat("\nRecords scraped:", nrow(raw_df), " | Columns:", ncol(raw_df), "\n")
cat("Request summary: ", sum(log_df$source == "downloaded"), " downloaded, ",
    sum(log_df$source == "saved copy"), " from saved copies, ",
    sum(log_df$source %in% c("failed", "none")), " failed\n", sep = "")

cat("\nStructure of the raw data frame:\n"); str(raw_df, vec.len = 2, give.attr = FALSE)
cat("\nFirst rows (selected columns):\n")
print(data.frame(id = raw_df$id, university = raw_df$university,
                 established_raw = substr(raw_df$established_raw, 1, 28),
                 students_raw = substr(raw_df$students_raw, 1, 24),
                 staff_raw = substr(raw_df$staff_raw, 1, 18),
                 article_words = raw_df$article_words)[1:min(8, nrow(raw_df)), ], row.names = FALSE)

# Regex analysis of the raw data: how messy is it?
has <- function(x, rx) sum(!is.na(x) & grepl(rx, x, perl = TRUE))
n_years <- function(x) {
  if (is.na(x)) return(0L)
  length(unique(regmatches(x, gregexpr("\\b(1[6-9][0-9]{2}|20[0-2][0-9])\\b", x))[[1]]))
}
mess <- data.frame(
  issue = c("Missing founding date text", "Missing student count text", "Missing academic staff count text",
            "Missing location text",
            "Lead text with citation markers like [1]",
            "Lead text with pronunciation / foreign-language brackets",
            "Infobox values with citation markers",
            "Founding date text with several different years",
            "Student counts with a year or note in brackets, e.g. (2023)",
            "Student counts with a trailing + (lower bound) or approx./c.",
            "Student counts using spaces or thin spaces as thousands separator",
            "Undergraduate/postgraduate fields containing no number (e.g. 'Yes')",
            "Location text containing GPS coordinates"),
  records = c(sum(is.na(raw_df$established_raw)), sum(is.na(raw_df$students_raw)), sum(is.na(raw_df$staff_raw)),
              sum(is.na(raw_df$location_raw)),
              has(raw_df$lead_text, "\\[[0-9a-z ]+\\]"),
              has(raw_df$lead_text, "\\((?:[^()]*?(?:Afrikaans|isiZulu|isiXhosa|Xhosa|Zulu|Sesotho|Setswana|pronounced|IPA|/)[^()]*)\\)"),
              has(paste(raw_df$established_raw, raw_df$students_raw, raw_df$staff_raw), "\\[[0-9a-z ]+\\]"),
              sum(vapply(raw_df$established_raw, n_years, integer(1)) >= 2),
              has(raw_df$students_raw, "\\([^)]*[0-9]{4}[^)]*\\)"),
              has(raw_df$students_raw, "(?i)([0-9]\\s*\\+|\\bc\\.|~|\u2248|approx|about|circa)"),
              has(raw_df$students_raw, "[0-9][ \u00A0\u2009\u202F][0-9]{3}\\b"),
              sum(!is.na(c(raw_df$undergraduates_raw, raw_df$postgraduates_raw)) &
                    !grepl("[0-9]", c(raw_df$undergraduates_raw, raw_df$postgraduates_raw))),
              has(raw_df$location_raw, "\u00B0"))
)
cat("\nRegex analysis of the raw data (real-world inconsistencies found):\n")
print(mess, row.names = FALSE)

write.csv(raw_df, "q4_universities_raw.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")


# 4.2  DATA CLEANING, DENOISING AND MISSING-VALUE IMPUTATION

cat("\n\n---- 4.2  DATA CLEANING ----\n")
clean_df <- raw_df

# Step 1: remove duplicate records
n0 <- nrow(clean_df)
clean_df <- clean_df[!duplicated(tolower(trimws(clean_df$university))), ]
cat("Step 1  Duplicate records removed      :", n0 - nrow(clean_df), "\n")

# Step 2: denoise text (citation markers, pronunciation brackets, special spaces, entities)
clean_txt <- function(x) {
  x <- gsub("\\[[^]]*\\]", " ", x)                                                     # [1], [a], [citation needed]
  x <- gsub("\\((?:[^()]*?(?:Afrikaans|isiZulu|isiXhosa|Xhosa|Zulu|Sesotho|Setswana|pronounced|IPA|/)[^()]*)\\)", " ", x, perl = TRUE)
  x <- gsub("[\u00A0\u2009\u200B\u202F]", " ", x, perl = TRUE)                         # non-breaking / thin spaces
  x <- gsub("&[a-z]+;|&#[0-9]+;", " ", x)                                              # HTML entities
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+([,.;:])", "\\1", x)                                                  # space before punctuation
  trimws(gsub("\\s+", " ", x))
}
clean_df$university <- clean_txt(clean_df$university)
clean_df$lead_text  <- clean_txt(clean_df$lead_text)
for (col in c("established_raw", "students_raw", "undergraduates_raw", "postgraduates_raw", "staff_raw", "location_raw"))
  clean_df[[col]] <- clean_txt(clean_df[[col]])
cat("Step 2  Text denoised                  : citation markers, pronunciations, special spaces removed\n")

# Step 3: regex extraction of numbers from inconsistent strings
parse_count <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  s <- tolower(x)
  s <- gsub("\\((?:[^)]*)\\b(19|20)[0-9]{2}\\b(?:[^)]*)\\)", " ", s, perl = TRUE)          # "(2022)" etc.
  s <- gsub("\\b(as of|in|est\\.?|estimate|census)\\s*(19|20)[0-9]{2}\\b", " ", s, perl = TRUE)
  s <- gsub("(?<=\\d)[ ,](?=\\d{3}\\b)", "", s, perl = TRUE)                            # 53,000 / 53 000 -> 53000
  tk <- regmatches(s, gregexpr("[0-9]+(?:\\.[0-9]+)?\\s*(?:million|thousand|m\\b|k\\b)?", s, perl = TRUE))[[1]]
  if (length(tk) == 0) return(NA_real_)
  val <- vapply(tk, function(t) {
    n <- as.numeric(sub("^([0-9]+(?:\\.[0-9]+)?).*$", "\\1", t, perl = TRUE))
    if (grepl("million|m$", trimws(t))) n <- n * 1e6 else if (grepl("thousand|k$", trimws(t))) n <- n * 1e3
    n
  }, numeric(1))
  val <- val[!(val >= 1900 & val <= 2030 & val == round(val))]                         # drop stray years
  val <- val[val >= 100]
  if (length(val) == 0) return(NA_real_)
  if (length(val) == 2 && grepl("[\u2013-]|\\bto\\b", s, perl = TRUE)) return(mean(val))             # range -> midpoint
  val[1]
}
parse_year <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  y <- as.numeric(regmatches(x, gregexpr("\\b(1[6-9][0-9]{2}|20[0-2][0-9])\\b", x))[[1]])
  y <- y[y >= 1800 & y <= 2026]
  if (length(y) == 0) NA_real_ else min(y)                                             # earliest = original founding
}

clean_df$founded_year <- vapply(clean_df$established_raw, parse_year, numeric(1), USE.NAMES = FALSE)
clean_df$students     <- vapply(clean_df$students_raw,    parse_count, numeric(1), USE.NAMES = FALSE)
clean_df$staff        <- vapply(clean_df$staff_raw,        parse_count, numeric(1), USE.NAMES = FALSE)
ug <- vapply(clean_df$undergraduates_raw, parse_count, numeric(1), USE.NAMES = FALSE)
pg <- vapply(clean_df$postgraduates_raw,  parse_count, numeric(1), USE.NAMES = FALSE)
from_parts <- is.na(clean_df$students) & (!is.na(ug) | !is.na(pg))
clean_df$students[from_parts] <- rowSums(cbind(ug, pg), na.rm = TRUE)[from_parts]
cat("Step 3  Numbers extracted with regex   : founding year, students, staff (", sum(from_parts),
    " student totals rebuilt from undergraduate + postgraduate)\n", sep = "")

# Step 4: recover missing values from the lead paragraph with regex
n_before <- c(year = sum(is.na(clean_df$founded_year)), students = sum(is.na(clean_df$students)), staff = sum(is.na(clean_df$staff)))
founded_rx  <- "(?i)(?:founded|established|created|opened|formed)\\s+(?:in|on)?\\s*(?:[0-9]{1,2}\\s+[A-Za-z]+\\s+)?(1[6-9][0-9]{2}|20[0-2][0-9])"
students_rx <- "(?i)([0-9]{1,3}(?:[, ][0-9]{3})+|[0-9]{3,6})\\s+(?:full-time\\s+|registered\\s+|enrolled\\s+)?students"
staff_rx    <- "(?i)([0-9]{1,3}(?:[, ][0-9]{3})+|[0-9]{3,5})\\s+(?:academic\\s+|permanent\\s+)?(?:staff|employees)"
grab <- function(txt, rx) {
  if (is.na(txt)) return(NA_real_)
  m <- regmatches(txt, regexec(rx, txt, perl = TRUE))[[1]]
  if (length(m) < 2) NA_real_ else as.numeric(gsub("[, ]", "", m[2]))
}
for (i in seq_len(nrow(clean_df))) {
  lt <- clean_df$lead_text[i]
  if (is.na(clean_df$founded_year[i])) clean_df$founded_year[i] <- grab(lt, founded_rx)
  if (is.na(clean_df$students[i]))     clean_df$students[i]     <- grab(lt, students_rx)
  if (is.na(clean_df$staff[i]))        clean_df$staff[i]        <- grab(lt, staff_rx)
}
n_after <- c(year = sum(is.na(clean_df$founded_year)), students = sum(is.na(clean_df$students)), staff = sum(is.na(clean_df$staff)))
cat("Step 4  Values recovered from lead text: founding year ", n_before["year"] - n_after["year"],
    ", students ", n_before["students"] - n_after["students"], ", staff ", n_before["staff"] - n_after["staff"], "\n", sep = "")

# Step 5: derive categorical attributes with regex (institution type, province)
clean_df$institution_type <- ifelse(grepl("(?i)university of technology|technikon", clean_df$university, perl = TRUE), "University of Technology",
                                    ifelse(grepl("(?i)comprehensive", clean_df$lead_text, perl = TRUE), "Comprehensive University",
                                           "Traditional University"))
province_map <- c("Western Cape" = "Western Cape|Cape Town|Stellenbosch|Bellville|Mowbray",
                  "Gauteng" = "Gauteng|Johannesburg|Pretoria|Tshwane|Soweto|Vanderbijlpark|Braamfontein|Auckland Park|Ga-Rankuwa|Medunsa",
                  "KwaZulu-Natal" = "KwaZulu|Durban|Pietermaritzburg|Richards Bay|Empangeni|Westville|Umlazi",
                  "Eastern Cape" = "Eastern Cape|Gqeberha|Port Elizabeth|Makhanda|Grahamstown|Mthatha|Alice|East London",
                  "Free State" = "Free State|Bloemfontein|Welkom|Qwaqwa",
                  "Limpopo" = "Limpopo|Polokwane|Thohoyandou|Mankweng|Sovenga",
                  "North West" = "North West|Potchefstroom|Mahikeng|Mafikeng|Mmabatho",
                  "Mpumalanga" = "Mpumalanga|Mbombela|Nelspruit|Siyabuswa",
                  "Northern Cape" = "Northern Cape|Kimberley")
find_province <- function(txt) {                       # the province mentioned FIRST in the text wins
  if (is.na(txt)) return(NA_character_)
  pos <- vapply(province_map, function(rx) {
    m <- regexpr(rx, txt, perl = TRUE)
    if (m[1] == -1) Inf else as.numeric(m[1])
  }, numeric(1))
  if (all(is.infinite(pos))) NA_character_ else names(province_map)[which.min(pos)]
}
prov <- vapply(clean_df$location_raw, find_province, character(1), USE.NAMES = FALSE)
miss <- is.na(prov)
prov[miss] <- vapply(clean_df$lead_text[miss], find_province, character(1), USE.NAMES = FALSE)
clean_df$province <- ifelse(is.na(prov), "Unknown", prov)
cat("Step 5  Institution type and province derived with regex (province unknown for ",
    sum(clean_df$province == "Unknown"), " records)\n", sep = "")

# Step 6: remove spurious / impossible numeric values (set to missing, then imputed)
bad_year <- !is.na(clean_df$founded_year) & (clean_df$founded_year < 1800 | clean_df$founded_year > 2026)
bad_stu  <- !is.na(clean_df$students) & (clean_df$students < 500 | clean_df$students > 500000)
bad_stf  <- !is.na(clean_df$staff) & (clean_df$staff < 20 | clean_df$staff > 20000)
ratio_now <- clean_df$students / clean_df$staff
bad_ratio <- !is.na(ratio_now) & !bad_stu & !bad_stf & (ratio_now < 5 | ratio_now > 60)     # implausible students per academic staff
clean_df$founded_year[bad_year] <- NA; clean_df$students[bad_stu] <- NA
clean_df$staff[bad_stf | bad_ratio] <- NA
cat("Step 6  Spurious values set to missing : founding year ", sum(bad_year), ", students ", sum(bad_stu),
    ", staff ", sum(bad_stf | bad_ratio), " (", sum(bad_ratio), " because of an implausible student-to-staff ratio)\n", sep = "")

# Step 7: missing-value imputation (flagged so the original gaps stay visible)
clean_df$founded_year_imputed <- is.na(clean_df$founded_year)
clean_df$students_imputed     <- is.na(clean_df$students)
clean_df$staff_imputed        <- is.na(clean_df$staff)
grp_median <- function(v, g) {
  m <- tapply(v, g, median, na.rm = TRUE)
  out <- m[g]; out[is.na(out)] <- median(v, na.rm = TRUE); as.numeric(out)
}
clean_df$founded_year <- ifelse(is.na(clean_df$founded_year), round(grp_median(clean_df$founded_year, clean_df$institution_type)), clean_df$founded_year)
clean_df$students     <- ifelse(is.na(clean_df$students), round(grp_median(clean_df$students, clean_df$institution_type)), clean_df$students)
ratio <- clean_df$staff / clean_df$students
ratio_fill <- grp_median(ifelse(clean_df$staff_imputed, NA, ratio), clean_df$institution_type)
clean_df$staff <- ifelse(is.na(clean_df$staff), round(clean_df$students * ratio_fill), clean_df$staff)
cat("Step 7  Missing values imputed         : founding year ", sum(clean_df$founded_year_imputed),
    " (median by institution type), students ", sum(clean_df$students_imputed),
    " (median by type), staff ", sum(clean_df$staff_imputed), " (staff-to-student ratio by type)\n", sep = "")

# Step 8: derived numeric attributes and tidy column set for the database
this_year <- as.integer(format(Sys.Date(), "%Y"))
clean_df$years_since_founding <- this_year - clean_df$founded_year
clean_df$students_per_staff   <- round(clean_df$students / clean_df$staff, 1)
clean_df$lead_words           <- vapply(clean_df$lead_text, function(t) if (is.na(t)) 0L else count_words(t), integer(1), USE.NAMES = FALSE)
clean_df$lead_text[is.na(clean_df$lead_text)] <- ""

final_df <- data.frame(
  university = clean_df$university, institution_type = clean_df$institution_type, province = clean_df$province,
  founded_year = as.integer(clean_df$founded_year), years_since_founding = as.integer(clean_df$years_since_founding),
  students = as.integer(clean_df$students), staff = as.integer(clean_df$staff),
  students_per_staff = clean_df$students_per_staff,
  article_words = as.integer(clean_df$article_words), n_citations = as.integer(clean_df$n_citations),
  lead_words = as.integer(clean_df$lead_words), lead_text = clean_df$lead_text,
  founded_year_imputed = clean_df$founded_year_imputed, students_imputed = clean_df$students_imputed,
  staff_imputed = clean_df$staff_imputed, source_url = clean_df$source_url, scraped_on = clean_df$scraped_on,
  stringsAsFactors = FALSE
)
final_df <- data.frame(university_id = seq_len(nrow(final_df)), final_df, stringsAsFactors = FALSE)
cat("Step 8  Derived attributes added       : years_since_founding, students_per_staff, lead_words\n")

# Before / after quality report
before_after <- data.frame(
  check = c("Records", "Missing founding year", "Missing student count", "Missing staff count",
            "Lead texts with citation markers",
            "Student counts outside 500 - 500 000", "Province known"),
  before = c(nrow(raw_df), sum(is.na(raw_df$founded_first_pass)), sum(is.na(raw_df$students_first_pass)), sum(is.na(raw_df$staff_first_pass)),
             has(raw_df$lead_text, "\\[[0-9a-z ]+\\]"),
             sum(!is.na(raw_df$students_first_pass) & (raw_df$students_first_pass < 500 | raw_df$students_first_pass > 500000)),
             sum(!is.na(raw_df$location_raw))),
  after = c(nrow(final_df), sum(is.na(final_df$founded_year)), sum(is.na(final_df$students)), sum(is.na(final_df$staff)),
            has(final_df$lead_text, "\\[[0-9a-z ]+\\]"),
            sum(final_df$students < 500 | final_df$students > 500000),
            sum(final_df$province != "Unknown"))
)
cat("\nBefore and after cleaning:\n"); print(before_after, row.names = FALSE)

cat("\nExamples of regex extraction - student counts (raw text -> cleaned value):\n")
idx <- match(raw_df$id, clean_df$id)
ex_stu <- data.frame(university = raw_df$university,
                     students_raw = gsub("\\s+", " ", substr(raw_df$students_raw, 1, 34)),
                     cleaned = clean_df$students[idx], imputed = clean_df$students_imputed[idx], stringsAsFactors = FALSE)
ex_stu <- ex_stu[!is.na(ex_stu$students_raw) & grepl("[^0-9, ]", ex_stu$students_raw), ]
print(head(ex_stu, 10), row.names = FALSE)

cat("\nExamples of regex extraction - founding year (first 4-digit number vs rule 'earliest year stated'):\n")
ex_yr <- data.frame(university = raw_df$university,
                    established_raw = gsub("\\s+", " ", substr(raw_df$established_raw, 1, 52)),
                    first_pass = raw_df$founded_first_pass, cleaned = clean_df$founded_year[idx], stringsAsFactors = FALSE)
print(head(ex_yr[!is.na(ex_yr$first_pass) & ex_yr$first_pass != ex_yr$cleaned, ], 8), row.names = FALSE)

cat("\nRecords where a missing value was imputed:\n")
imp <- data.frame(university = clean_df$university, students = clean_df$students, students_imputed = clean_df$students_imputed,
                  staff = clean_df$staff, staff_imputed = clean_df$staff_imputed, stringsAsFactors = FALSE)
cat("  Student counts imputed:", sum(imp$students_imputed), "of", nrow(imp), " | Staff counts imputed:", sum(imp$staff_imputed), "of", nrow(imp), "\n")
print(imp[imp$students_imputed, ], row.names = FALSE)

cat("\nSummary of the cleaned numeric columns:\n")
print(summary(final_df[, c("founded_year", "students", "staff", "students_per_staff", "article_words", "n_citations")]))
cat("\nStructure of the cleaned data frame:\n"); str(final_df, vec.len = 2, give.attr = FALSE)
cat("\nCleaned data (first rows):\n")
print(head(final_df[, c("university_id", "university", "institution_type", "province", "founded_year", "students", "staff", "students_per_staff")], 10), row.names = FALSE)

# Export for further analysis / loading into Postgres (UTF-8, empty string = NULL)
write.csv(final_df, "q4_universities_clean.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv(before_after, "q4_quality_report.csv", row.names = FALSE)
cat("\nSaved to", getwd(), ":\n",
    " q4_universities_raw.csv (4.1), q4_universities_clean.csv (4.2), q4_quality_report.csv, q4_robots.txt, folder q4_raw_pages/\n")