Question 4 - ETL with R and PostgreSQL (South African public
universities)
================

- [4.1 Extract - web scraping and regex
  analysis](#41-extract---web-scraping-and-regex-analysis)
  - [Parse each article into a
    record](#parse-each-article-into-a-record)
  - [Regex analysis of the raw data](#regex-analysis-of-the-raw-data)
- [4.2 Transform - cleaning, denoising and
  imputation](#42-transform---cleaning-denoising-and-imputation)
- [4.3 Load - PostgreSQL and SQL
  analysis](#43-load---postgresql-and-sql-analysis)
  - [Investigation 1 - institution types
    compared](#investigation-1---institution-types-compared)
  - [Investigation 2 - enrolment by
    province](#investigation-2---enrolment-by-province)
- [4.4 Repository](#44-repository)

**Use case.** A profile dataset of South Africa’s 26 public universities
(type, province, founding year, enrolment, staff) for analytical
reporting. The workflow is a complete **Extract - Transform - Load**
pipeline: scrape unstructured web articles (Extract), clean and impute
them with regular expressions (Transform) and load them into PostgreSQL
for SQL analysis (Load).

## 4.1 Extract - web scraping and regex analysis

**Source and compliance.** English Wikipedia (CC BY-SA 4.0). One article
per university (26 requests, 1.5 s apart), after reading `robots.txt`,
with a descriptive `User-Agent` containing contact details. Every page
is saved in `q4_raw_pages/`, so the notebook below re-uses those saved
copies and makes no new requests. The download step used by
`q4_scrape_clean.R` is:

``` r
for (title in page_titles) {
  Sys.sleep(1.5)                                                   # polite pause
  resp <- httr::GET(paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", title)), httr::user_agent(ua))
  if (httr::status_code(resp) == 200)
    writeBin(httr::content(resp, as = "raw"), file.path(cache_dir, paste0(gsub("[^A-Za-z0-9]+", "_", title), ".html")))
}
```

### Parse each article into a record

Each record has **text** attributes (the lead paragraphs and the infobox
strings) and **numerical** attributes (first-pass numbers pulled out by
a deliberately simple regex, plus word, citation and section counts).

``` r
get_label_value <- function(tbl) {
  rows <- html_elements(tbl, xpath = ".//tr[th and td]")
  if (length(rows) == 0) return(data.frame(label = character(0), value = character(0)))
  data.frame(label = vapply(rows, function(r) trimws(html_text2(html_element(r, "th"))), character(1)),
             value = vapply(rows, function(r) trimws(html_text2(html_element(r, "td"))), character(1)))
}
pick <- function(kv, rx) { i <- which(grepl(rx, kv$label, ignore.case = TRUE) & nzchar(kv$value)); if (length(i) == 0) NA_character_ else kv$value[i[1]] }
count_words <- function(x) { w <- strsplit(trimws(x), "\\s+")[[1]]; sum(nzchar(w)) }

parse_article <- function(html_text, title, url) {
  page   <- read_html(html_text)
  bodies <- html_elements(page, "div.mw-parser-output")          # some pages also contain a tiny coordinates box
  body   <- bodies[[which.max(vapply(bodies, function(b) nchar(html_text(b)), integer(1)))]]
  for (sel in c("style", "script")) { nodes <- html_elements(body, sel); if (length(nodes) > 0) xml_remove(nodes) }
  name <- trimws(html_text2(html_element(page, "h1")))
  ib   <- html_element(body, "table.infobox")
  kv   <- if (inherits(ib, "xml_missing")) data.frame(label = character(0), value = character(0)) else get_label_value(ib)
  ptxt <- vapply(html_elements(body, xpath = ".//p[not(ancestor::table)]"), function(p) trimws(html_text2(p)), character(1))
  lead <- paste(head(ptxt[nchar(ptxt) >= 60], 2), collapse = " ")
  first_number <- function(x) { if (is.na(x)) return(NA_real_); m <- regmatches(x, regexpr("[0-9][0-9,. \u00A0]*", x, perl = TRUE))
    if (length(m) == 0) NA_real_ else suppressWarnings(as.numeric(gsub("[^0-9.]", "", m))) }
  first_year <- function(x) { if (is.na(x)) return(NA_real_); m <- regmatches(x, regexpr("[0-9]{4}", x)); if (length(m) == 0) NA_real_ else as.numeric(m) }
  est <- pick(kv, "^(established|founded|founding|opened)")
  stu <- pick(kv, "^(students|total students|total enrol|enrol|student (body|population|enrol))")
  stf <- pick(kv, "^(academic staff|faculty|staff|total staff|employees)")
  data.frame(university = name, source_url = url, lead_text = if (nzchar(lead)) lead else NA_character_,
             established_raw = est, type_raw = pick(kv, "^type"), students_raw = stu,
             undergraduates_raw = pick(kv, "^undergrad"), postgraduates_raw = pick(kv, "^(postgrad|post-grad)"),
             staff_raw = stf, location_raw = pick(kv, "^(location|campus|campuses|address|city)"),
             founded_first_pass = first_year(est), students_first_pass = first_number(stu), staff_first_pass = first_number(stf),
             article_words = sum(vapply(ptxt, count_words, integer(1))), n_citations = length(html_elements(body, "sup.reference")),
             n_sections = length(html_elements(body, "h2")), stringsAsFactors = FALSE)
}

rows <- lapply(page_titles, function(t) {
  fn <- file.path(cache_dir, paste0(gsub("[^A-Za-z0-9]+", "_", t), ".html"))
  rec <- parse_article(paste(readLines(fn, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), t,
                       paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", t)))
  rec$scraped_on <- format(as.Date(file.info(fn)$mtime), "%Y-%m-%d"); rec
})
raw_df <- do.call(rbind, rows)
raw_df <- data.frame(id = seq_len(nrow(raw_df)), raw_df)
cat("Records scraped:", nrow(raw_df), "| columns:", ncol(raw_df), "\n")
```

    #> Records scraped: 26 | columns: 18

``` r
knitr::kable(head(data.frame(id = raw_df$id, university = raw_df$university,
                             established_raw = gsub("\\s+", " ", substr(raw_df$established_raw, 1, 36)),
                             students_raw = gsub("\\s+", " ", substr(raw_df$students_raw, 1, 30)),
                             staff_raw = substr(raw_df$staff_raw, 1, 22), article_words = raw_df$article_words), 10),
             caption = "Raw scraped data (first 10 records) - note the inconsistent formats and missing values")
```

| id | university | established_raw | students_raw | staff_raw | article_words |
|---:|:---|:---|:---|:---|---:|
| 1 | University of Cape Town | 1 October 1829; 197 years ago (1829- | 28,233\[4\] | 1,176\[3\] | 4364 |
| 2 | University of the Witwatersrand | 1 March 1922; 104 years ago (1922-03 | 40,259\[3\] | 1,112 (Full-time, Perm | 3929 |
| 3 | Stellenbosch University | 2 April 1918; 108 years ago (1918-04 | NA | 1,028\[2\] | 2011 |
| 4 | University of Pretoria | 4 March 1908; 118 years ago (1908-03 | 56,409 (total)\[8\] 53,131 (cont | NA | 5743 |
| 5 | University of Johannesburg | 1 January 2005; 21 years ago (2005-0 | 50,274 (2024)\[6\] | 1,384 permanent and 3, | 1980 |
| 6 | University of KwaZulu-Natal | 1 January 2004; 22 years ago (1 Janu | 46,539\[4\] (2016) | 1,328\[4\] (2016) | 1352 |
| 7 | University of the Free State | 1904; 122 years ago (1904) | 41,169 (2023)\[2\] | NA | 1505 |
| 8 | Nelson Mandela University | 1882 (Port Elizabeth Art School) 200 | 28,000 | NA | 1144 |
| 9 | Rhodes University | 31 May 1904; 122 years ago (1904-05- | 7,005\[2\] | 357\[2\] | 1332 |
| 10 | North-West University | 2004 (2004) (by merger of existing i | 56,000 | NA | 179 |

Raw scraped data (first 10 records) - note the inconsistent formats and
missing values

### Regex analysis of the raw data

``` r
has <- function(x, rx) sum(!is.na(x) & grepl(rx, x, perl = TRUE))
n_years <- function(x) if (is.na(x)) 0L else length(unique(regmatches(x, gregexpr("\\b(1[6-9][0-9]{2}|20[0-2][0-9])\\b", x))[[1]]))
mess <- data.frame(
  issue = c("Missing founding date text", "Missing student count text", "Missing academic staff count text", "Missing location text",
            "Lead text with citation markers like [1]", "Lead text with pronunciation / foreign-language brackets",
            "Infobox values with citation markers", "Founding date text with several different years",
            "Student counts with a year or note in brackets, e.g. (2023)", "Student counts with a trailing + (lower bound) or approx./c.",
            "Student counts using spaces or thin spaces as thousands separator",
            "Undergraduate/postgraduate fields containing no number (e.g. 'Yes')", "Location text containing GPS coordinates"),
  records = c(sum(is.na(raw_df$established_raw)), sum(is.na(raw_df$students_raw)), sum(is.na(raw_df$staff_raw)), sum(is.na(raw_df$location_raw)),
              has(raw_df$lead_text, "\\[[0-9a-z ]+\\]"),
              has(raw_df$lead_text, "\\((?:[^()]*?(?:Afrikaans|isiZulu|isiXhosa|Xhosa|Zulu|Sesotho|Setswana|pronounced|IPA|/)[^()]*)\\)"),
              has(paste(raw_df$established_raw, raw_df$students_raw, raw_df$staff_raw), "\\[[0-9a-z ]+\\]"),
              sum(vapply(raw_df$established_raw, n_years, integer(1)) >= 2),
              has(raw_df$students_raw, "\\([^)]*[0-9]{4}[^)]*\\)"),
              has(raw_df$students_raw, "(?i)([0-9]\\s*\\+|\\bc\\.|~|\u2248|approx|about|circa)"),
              has(raw_df$students_raw, "[0-9][ \u00A0\u2009\u202F][0-9]{3}\\b"),
              sum(!is.na(c(raw_df$undergraduates_raw, raw_df$postgraduates_raw)) & !grepl("[0-9]", c(raw_df$undergraduates_raw, raw_df$postgraduates_raw))),
              has(raw_df$location_raw, "\u00B0")))
knitr::kable(mess, caption = "Real-world inconsistencies found in the scraped data (number of records affected)")
```

| issue | records |
|:---|---:|
| Missing founding date text | 0 |
| Missing student count text | 4 |
| Missing academic staff count text | 18 |
| Missing location text | 0 |
| Lead text with citation markers like \[1\] | 22 |
| Lead text with pronunciation / foreign-language brackets | 14 |
| Infobox values with citation markers | 14 |
| Founding date text with several different years | 3 |
| Student counts with a year or note in brackets, e.g. (2023) | 9 |
| Student counts with a trailing + (lower bound) or approx./c. | 5 |
| Student counts using spaces or thin spaces as thousands separator | 2 |
| Undergraduate/postgraduate fields containing no number (e.g. ‘Yes’) | 2 |
| Location text containing GPS coordinates | 16 |

Real-world inconsistencies found in the scraped data (number of records
affected)

``` r
write.csv(raw_df, "q4_universities_raw.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
```

## 4.2 Transform - cleaning, denoising and imputation

``` r
clean_txt <- function(x) {
  x <- gsub("\\[[^]]*\\]", " ", x)
  x <- gsub("\\((?:[^()]*?(?:Afrikaans|isiZulu|isiXhosa|Xhosa|Zulu|Sesotho|Setswana|pronounced|IPA|/)[^()]*)\\)", " ", x, perl = TRUE)
  x <- gsub("[\u00A0\u2009\u200B\u202F]", " ", x, perl = TRUE)
  x <- gsub("&[a-z]+;|&#[0-9]+;", " ", x); x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+([,.;:])", "\\1", x)
  trimws(gsub("\\s+", " ", x))
}
clean_df <- raw_df[!duplicated(tolower(trimws(raw_df$university))), ]               # step 1: duplicates
clean_df$university <- clean_txt(clean_df$university)                                # step 2: denoise text
clean_df$lead_text  <- clean_txt(clean_df$lead_text)
for (col in c("established_raw", "students_raw", "undergraduates_raw", "postgraduates_raw", "staff_raw", "location_raw"))
  clean_df[[col]] <- clean_txt(clean_df[[col]])
cat("Duplicates removed:", nrow(raw_df) - nrow(clean_df), "\n")
```

    #> Duplicates removed: 0

``` r
cat("BEFORE:", substr(raw_df$lead_text[1], 1, 200), "...\n")
```

    #> BEFORE: The University of Cape Town (UCT) (Afrikaans: Universiteit van Kaapstad, Xhosa: iYunivesithi yaseKapa) is a public research university in Cape Town, South Africa. Established in 1829 as the South Afri ...

``` r
cat("AFTER :", substr(clean_df$lead_text[1], 1, 200), "...\n")
```

    #> AFTER : The University of Cape Town (UCT) is a public research university in Cape Town, South Africa. Established in 1829 as the South African College, it was granted full university status in 1918, making it ...

**Regex extraction of numbers** from inconsistent strings such as
`35,000+ (2026)[2]`, `21 993` or `420,000 (as of 2019)`: strip years in
brackets, join digit groups separated by spaces or commas, understand
“million” and “k”, take the midpoint of a range, ignore stray years. The
founding year is the earliest year stated.

``` r
parse_count <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  s <- tolower(x)
  s <- gsub("\\((?:[^)]*)\\b(19|20)[0-9]{2}\\b(?:[^)]*)\\)", " ", s, perl = TRUE)
  s <- gsub("\\b(as of|in|est\\.?|estimate|census)\\s*(19|20)[0-9]{2}\\b", " ", s, perl = TRUE)
  s <- gsub("(?<=\\d)[ ,](?=\\d{3}\\b)", "", s, perl = TRUE)
  tk <- regmatches(s, gregexpr("[0-9]+(?:\\.[0-9]+)?\\s*(?:million|thousand|m\\b|k\\b)?", s, perl = TRUE))[[1]]
  if (length(tk) == 0) return(NA_real_)
  val <- vapply(tk, function(t) {
    n <- as.numeric(sub("^([0-9]+(?:\\.[0-9]+)?).*$", "\\1", t, perl = TRUE))
    if (grepl("million|m$", trimws(t))) n <- n * 1e6 else if (grepl("thousand|k$", trimws(t))) n <- n * 1e3
    n }, numeric(1))
  val <- val[!(val >= 1900 & val <= 2030 & val == round(val))]; val <- val[val >= 100]
  if (length(val) == 0) return(NA_real_)
  if (length(val) == 2 && grepl("[\u2013-]|\\bto\\b", s, perl = TRUE)) return(mean(val))
  val[1]
}
parse_year <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  y <- as.numeric(regmatches(x, gregexpr("\\b(1[6-9][0-9]{2}|20[0-2][0-9])\\b", x))[[1]]); y <- y[y >= 1800 & y <= 2026]
  if (length(y) == 0) NA_real_ else min(y)
}
clean_df$founded_year <- vapply(clean_df$established_raw, parse_year, numeric(1), USE.NAMES = FALSE)
clean_df$students     <- vapply(clean_df$students_raw, parse_count, numeric(1), USE.NAMES = FALSE)
clean_df$staff        <- vapply(clean_df$staff_raw, parse_count, numeric(1), USE.NAMES = FALSE)
ug <- vapply(clean_df$undergraduates_raw, parse_count, numeric(1), USE.NAMES = FALSE)
pg <- vapply(clean_df$postgraduates_raw,  parse_count, numeric(1), USE.NAMES = FALSE)
from_parts <- is.na(clean_df$students) & (!is.na(ug) | !is.na(pg))
clean_df$students[from_parts] <- rowSums(cbind(ug, pg), na.rm = TRUE)[from_parts]
cat("Student totals rebuilt from undergraduate + postgraduate:", sum(from_parts), "\n")
```

    #> Student totals rebuilt from undergraduate + postgraduate: 1

``` r
idx <- match(raw_df$id, clean_df$id)
ex <- data.frame(university = raw_df$university, students_raw = gsub("\\s+", " ", substr(raw_df$students_raw, 1, 34)), cleaned = clean_df$students[idx])
knitr::kable(head(ex[!is.na(ex$students_raw) & grepl("[^0-9, ]", ex$students_raw), ], 10), row.names = FALSE,
             caption = "Regex extraction of student counts: raw text and cleaned value")
```

| university | students_raw | cleaned |
|:---|:---|---:|
| University of Cape Town | 28,233\[4\] | 28233 |
| University of the Witwatersrand | 40,259\[3\] | 40259 |
| University of Pretoria | 56,409 (total)\[8\] 53,131 (contact) | 56409 |
| University of Johannesburg | 50,274 (2024)\[6\] | 50274 |
| University of KwaZulu-Natal | 46,539\[4\] (2016) | 46539 |
| University of the Free State | 41,169 (2023)\[2\] | 41169 |
| Rhodes University | 7,005\[2\] | 7005 |
| University of Fort Hare | 13,331 (2015) | 13331 |
| University of the Western Cape | 23,000+ (2023)\[1\] | 23000 |
| University of Venda | 15,248 (2023)\[1\] | 15248 |

Regex extraction of student counts: raw text and cleaned value

**Recover missing values from the lead paragraph, derive categories,
remove spurious values.**

``` r
n_miss0 <- c(year = sum(is.na(clean_df$founded_year)), students = sum(is.na(clean_df$students)), staff = sum(is.na(clean_df$staff)))
grab <- function(txt, rx) { if (is.na(txt)) return(NA_real_); m <- regmatches(txt, regexec(rx, txt, perl = TRUE))[[1]]; if (length(m) < 2) NA_real_ else as.numeric(gsub("[, ]", "", m[2])) }
students_rx <- "(?i)([0-9]{1,3}(?:[, ][0-9]{3})+|[0-9]{3,6})\\s+(?:full-time\\s+|registered\\s+|enrolled\\s+)?students"
staff_rx    <- "(?i)([0-9]{1,3}(?:[, ][0-9]{3})+|[0-9]{3,5})\\s+(?:academic\\s+|permanent\\s+)?(?:staff|employees)"
for (i in seq_len(nrow(clean_df))) {
  if (is.na(clean_df$students[i])) clean_df$students[i] <- grab(clean_df$lead_text[i], students_rx)
  if (is.na(clean_df$staff[i]))    clean_df$staff[i]    <- grab(clean_df$lead_text[i], staff_rx)
}

clean_df$institution_type <- ifelse(grepl("(?i)university of technology|technikon", clean_df$university, perl = TRUE), "University of Technology",
                             ifelse(grepl("(?i)comprehensive", clean_df$lead_text, perl = TRUE), "Comprehensive University", "Traditional University"))
province_map <- c("Western Cape" = "Western Cape|Cape Town|Stellenbosch|Bellville|Mowbray",
  "Gauteng" = "Gauteng|Johannesburg|Pretoria|Tshwane|Soweto|Vanderbijlpark|Braamfontein|Auckland Park|Ga-Rankuwa|Medunsa",
  "KwaZulu-Natal" = "KwaZulu|Durban|Pietermaritzburg|Richards Bay|Empangeni|Westville|Umlazi",
  "Eastern Cape" = "Eastern Cape|Gqeberha|Port Elizabeth|Makhanda|Grahamstown|Mthatha|Alice|East London",
  "Free State" = "Free State|Bloemfontein|Welkom|Qwaqwa", "Limpopo" = "Limpopo|Polokwane|Thohoyandou|Mankweng|Sovenga",
  "North West" = "North West|Potchefstroom|Mahikeng|Mafikeng|Mmabatho", "Mpumalanga" = "Mpumalanga|Mbombela|Nelspruit|Siyabuswa",
  "Northern Cape" = "Northern Cape|Kimberley")
find_province <- function(txt) {                       # the province mentioned first wins
  if (is.na(txt)) return(NA_character_)
  pos <- vapply(province_map, function(rx) { m <- regexpr(rx, txt, perl = TRUE); if (m[1] == -1) Inf else as.numeric(m[1]) }, numeric(1))
  if (all(is.infinite(pos))) NA_character_ else names(province_map)[which.min(pos)]
}
prov <- vapply(clean_df$location_raw, find_province, character(1), USE.NAMES = FALSE)
miss <- is.na(prov); prov[miss] <- vapply(clean_df$lead_text[miss], find_province, character(1), USE.NAMES = FALSE)
clean_df$province <- ifelse(is.na(prov), "Unknown", prov)

bad_year <- !is.na(clean_df$founded_year) & (clean_df$founded_year < 1800 | clean_df$founded_year > 2026)
bad_stu  <- !is.na(clean_df$students) & (clean_df$students < 500 | clean_df$students > 500000)
bad_stf  <- !is.na(clean_df$staff) & (clean_df$staff < 20 | clean_df$staff > 20000)
ratio_now <- clean_df$students / clean_df$staff
bad_ratio <- !is.na(ratio_now) & !bad_stu & !bad_stf & (ratio_now < 5 | ratio_now > 60)
clean_df$founded_year[bad_year] <- NA; clean_df$students[bad_stu] <- NA; clean_df$staff[bad_stf | bad_ratio] <- NA
cat("Spurious values set to missing - students:", sum(bad_stu), "| staff:", sum(bad_stf | bad_ratio),
    "(", sum(bad_ratio), "because of an implausible students-per-staff ratio )\n")
```

    #> Spurious values set to missing - students: 0 | staff: 1 ( 1 because of an implausible students-per-staff ratio )

``` r
cat("Missing after extraction - founding year:", sum(is.na(clean_df$founded_year)), "| students:", sum(is.na(clean_df$students)),
    "| academic staff:", sum(is.na(clean_df$staff)), "of", nrow(clean_df), "\n")
```

    #> Missing after extraction - founding year: 0 | students: 3 | academic staff: 19 of 26

**Imputation.** Gaps are filled but flagged, so the original missingness
stays visible: students and founding year with the **median of the
institution type**, academic staff with the **staff-to-student ratio of
the institution type**.

``` r
clean_df$founded_year_imputed <- is.na(clean_df$founded_year)
clean_df$students_imputed     <- is.na(clean_df$students)
clean_df$staff_imputed        <- is.na(clean_df$staff)
grp_median <- function(v, g) { m <- tapply(v, g, median, na.rm = TRUE); out <- m[g]; out[is.na(out)] <- median(v, na.rm = TRUE); as.numeric(out) }
clean_df$founded_year <- ifelse(is.na(clean_df$founded_year), round(grp_median(clean_df$founded_year, clean_df$institution_type)), clean_df$founded_year)
clean_df$students     <- ifelse(is.na(clean_df$students), round(grp_median(clean_df$students, clean_df$institution_type)), clean_df$students)
ratio      <- clean_df$staff / clean_df$students
ratio_fill <- grp_median(ifelse(clean_df$staff_imputed, NA, ratio), clean_df$institution_type)
clean_df$staff <- ifelse(is.na(clean_df$staff), round(clean_df$students * ratio_fill), clean_df$staff)
cat("Imputed - founding year:", sum(clean_df$founded_year_imputed), "| students:", sum(clean_df$students_imputed), "| staff:", sum(clean_df$staff_imputed), "\n")
```

    #> Imputed - founding year: 0 | students: 3 | staff: 19

``` r
clean_df$years_since_founding <- as.integer(format(Sys.Date(), "%Y")) - clean_df$founded_year
clean_df$students_per_staff   <- round(clean_df$students / clean_df$staff, 1)
clean_df$lead_words           <- vapply(clean_df$lead_text, function(t) if (is.na(t)) 0L else count_words(t), integer(1), USE.NAMES = FALSE)
clean_df$lead_text[is.na(clean_df$lead_text)] <- ""
final_df <- data.frame(
  university_id = seq_len(nrow(clean_df)), university = clean_df$university, institution_type = clean_df$institution_type,
  province = clean_df$province, founded_year = as.integer(clean_df$founded_year), years_since_founding = as.integer(clean_df$years_since_founding),
  students = as.integer(clean_df$students), staff = as.integer(clean_df$staff), students_per_staff = clean_df$students_per_staff,
  article_words = as.integer(clean_df$article_words), n_citations = as.integer(clean_df$n_citations), lead_words = as.integer(clean_df$lead_words),
  lead_text = clean_df$lead_text, founded_year_imputed = clean_df$founded_year_imputed, students_imputed = clean_df$students_imputed,
  staff_imputed = clean_df$staff_imputed, source_url = clean_df$source_url, scraped_on = clean_df$scraped_on)

before_after <- data.frame(
  check = c("Records", "Missing founding year", "Missing student count", "Missing academic staff count", "Lead texts with citation markers"),
  before = c(nrow(raw_df), sum(is.na(raw_df$founded_first_pass)), sum(is.na(raw_df$students_first_pass)), sum(is.na(raw_df$staff_first_pass)),
             has(raw_df$lead_text, "\\[[0-9a-z ]+\\]")),
  after  = c(nrow(final_df), sum(is.na(final_df$founded_year)), sum(is.na(final_df$students)), sum(is.na(final_df$staff)),
             has(final_df$lead_text, "\\[[0-9a-z ]+\\]")))
knitr::kable(before_after, caption = "Data quality before and after cleaning")
```

| check                            | before | after |
|:---------------------------------|-------:|------:|
| Records                          |     26 |    26 |
| Missing founding year            |      0 |     0 |
| Missing student count            |      4 |     0 |
| Missing academic staff count     |     18 |     0 |
| Lead texts with citation markers |     22 |     0 |

Data quality before and after cleaning

``` r
write.csv(final_df, "q4_universities_clean.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
knitr::kable(final_df[, c("university_id", "university", "institution_type", "province", "founded_year", "students", "staff", "students_imputed", "staff_imputed")],
             caption = "Cleaned dataset exported to q4_universities_clean.csv")
```

| university_id | university | institution_type | province | founded_year | students | staff | students_imputed | staff_imputed |
|---:|:---|:---|:---|---:|---:|---:|:---|:---|
| 1 | University of Cape Town | Traditional University | Western Cape | 1829 | 28233 | 1176 | FALSE | FALSE |
| 2 | University of the Witwatersrand | Traditional University | Gauteng | 1922 | 40259 | 1112 | FALSE | FALSE |
| 3 | Stellenbosch University | Traditional University | Western Cape | 1918 | 35093 | 1028 | FALSE | FALSE |
| 4 | University of Pretoria | Traditional University | Gauteng | 1908 | 56409 | 1631 | FALSE | TRUE |
| 5 | University of Johannesburg | Traditional University | Gauteng | 2005 | 50274 | 1384 | FALSE | FALSE |
| 6 | University of KwaZulu-Natal | Traditional University | KwaZulu-Natal | 1910 | 46539 | 1328 | FALSE | FALSE |
| 7 | University of the Free State | Traditional University | Free State | 1904 | 41169 | 1190 | FALSE | TRUE |
| 8 | Nelson Mandela University | Comprehensive University | Eastern Cape | 1882 | 28000 | 591 | FALSE | TRUE |
| 9 | Rhodes University | Traditional University | Eastern Cape | 1904 | 7005 | 357 | FALSE | FALSE |
| 10 | North-West University | Traditional University | North West | 2004 | 56000 | 1619 | FALSE | TRUE |
| 11 | University of Limpopo | Traditional University | Limpopo | 2005 | 35093 | 1015 | TRUE | TRUE |
| 12 | University of Fort Hare | Traditional University | Eastern Cape | 1916 | 13331 | 385 | FALSE | TRUE |
| 13 | University of the Western Cape | Traditional University | Western Cape | 1959 | 23000 | 665 | FALSE | TRUE |
| 14 | University of Venda | Comprehensive University | Limpopo | 1981 | 15248 | 322 | FALSE | TRUE |
| 15 | University of Zululand | Comprehensive University | KwaZulu-Natal | 1960 | 16337 | 345 | FALSE | FALSE |
| 16 | Walter Sisulu University | Traditional University | Eastern Cape | 2005 | 31500 | 911 | FALSE | TRUE |
| 17 | Durban University of Technology | University of Technology | KwaZulu-Natal | 1907 | 31991 | 913 | FALSE | TRUE |
| 18 | Cape Peninsula University of Technology | University of Technology | Western Cape | 2005 | 35000 | 999 | FALSE | TRUE |
| 19 | Tshwane University of Technology | University of Technology | Gauteng | 2004 | 65000 | 1855 | FALSE | TRUE |
| 20 | Vaal University of Technology | University of Technology | Gauteng | 1966 | 19000 | 542 | FALSE | TRUE |
| 21 | Central University of Technology, Free State | University of Technology | Free State | 1981 | 21993 | 628 | FALSE | TRUE |
| 22 | Mangosuthu University of Technology | University of Technology | KwaZulu-Natal | 1979 | 13000 | 371 | FALSE | TRUE |
| 23 | University of South Africa | Comprehensive University | Gauteng | 1873 | 420000 | 8869 | FALSE | TRUE |
| 24 | Sol Plaatje University | Traditional University | Northern Cape | 2014 | 35093 | 1015 | TRUE | TRUE |
| 25 | University of Mpumalanga | Traditional University | Mpumalanga | 2014 | 35093 | 1015 | TRUE | TRUE |
| 26 | Sefako Makgatho Health Sciences University | Traditional University | Gauteng | 2015 | 7000 | 202 | FALSE | TRUE |

Cleaned dataset exported to q4_universities_clean.csv

## 4.3 Load - PostgreSQL and SQL analysis

The cleaned CSV is imported into a relational table with a primary key,
a unique name and `CHECK` constraints. The chunks below run against a
live PostgreSQL server when the environment variable `PGPASSWORD` is set
before knitting (for example `Sys.setenv(PGPASSWORD = "...")` in the R
console - the password is never stored in the notebook). Otherwise the
notebook shows the results saved by `q4_load_postgres.R`.

``` r
pw <- Sys.getenv("PGPASSWORD")
con <- NULL
if (nzchar(pw)) {
  con <- tryCatch(dbConnect(RPostgres::Postgres(), host = "localhost", port = 5432, dbname = "universities_db",
                            user = "postgres", password = pw, bigint = "numeric"), error = function(e) NULL)
}
pg_ok <- !is.null(con)
cat(if (pg_ok) "Connected to PostgreSQL: live queries will run.\n" else "No PostgreSQL connection: saved query results are shown instead.\n")
```

    #> No PostgreSQL connection: saved query results are shown instead.

``` r
create_sql <- "
DROP TABLE IF EXISTS universities;
CREATE TABLE universities (
    university_id         INTEGER       PRIMARY KEY,
    university            VARCHAR(150)  NOT NULL UNIQUE,
    institution_type      VARCHAR(40)   NOT NULL
                          CHECK (institution_type IN ('Traditional University', 'Comprehensive University', 'University of Technology')),
    province              VARCHAR(30)   NOT NULL,
    founded_year          INTEGER       CHECK (founded_year BETWEEN 1800 AND 2026),
    years_since_founding  INTEGER,
    students              INTEGER       CHECK (students > 0),
    staff                 INTEGER       CHECK (staff > 0),
    students_per_staff    NUMERIC(6,1),
    article_words         INTEGER,
    n_citations           INTEGER,
    lead_words            INTEGER,
    lead_text             TEXT,
    founded_year_imputed  BOOLEAN       NOT NULL,
    students_imputed      BOOLEAN       NOT NULL,
    staff_imputed         BOOLEAN       NOT NULL,
    source_url            TEXT          NOT NULL,
    scraped_on            DATE          NOT NULL
);"
for (stmt in strsplit(create_sql, ";")[[1]]) if (nzchar(trimws(stmt))) suppressMessages(dbExecute(con, stmt))
df <- read.csv("q4_universities_clean.csv", stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
df$scraped_on <- as.Date(df$scraped_on)
dbWriteTable(con, "universities", df, append = TRUE, row.names = FALSE)
cat("Rows in the PostgreSQL table:", dbGetQuery(con, "SELECT COUNT(*) AS n FROM universities")$n, "\n")
```

### Investigation 1 - institution types compared

``` r
query1 <- "
SELECT institution_type,
       COUNT(*)                                                              AS universities,
       SUM(students)                                                         AS total_students,
       ROUND(AVG(students))                                                  AS avg_students,
       ROUND((PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY students))::numeric) AS median_students,
       ROUND(AVG(students_per_staff)
             FILTER (WHERE NOT staff_imputed AND NOT students_imputed), 1)   AS avg_students_per_staff_reported,
       COUNT(*) FILTER (WHERE NOT staff_imputed AND NOT students_imputed)    AS unis_with_reported_ratio,
       ROUND(AVG(years_since_founding))                                      AS avg_age_years,
       COUNT(*) FILTER (WHERE students_imputed)                              AS imputed_enrolments
FROM universities
GROUP BY institution_type
ORDER BY total_students DESC;"
cat(query1)
```

    #> 
    #> SELECT institution_type,
    #>        COUNT(*)                                                              AS universities,
    #>        SUM(students)                                                         AS total_students,
    #>        ROUND(AVG(students))                                                  AS avg_students,
    #>        ROUND((PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY students))::numeric) AS median_students,
    #>        ROUND(AVG(students_per_staff)
    #>              FILTER (WHERE NOT staff_imputed AND NOT students_imputed), 1)   AS avg_students_per_staff_reported,
    #>        COUNT(*) FILTER (WHERE NOT staff_imputed AND NOT students_imputed)    AS unis_with_reported_ratio,
    #>        ROUND(AVG(years_since_founding))                                      AS avg_age_years,
    #>        COUNT(*) FILTER (WHERE students_imputed)                              AS imputed_enrolments
    #> FROM universities
    #> GROUP BY institution_type
    #> ORDER BY total_students DESC;

``` r
res1 <- if (pg_ok) dbGetQuery(con, query1) else read.csv("q4_sql_result1_institution_types.csv")
knitr::kable(res1, caption = "Investigation 1 result")
```

| institution_type | universities | total_students | avg_students | median_students | avg_students_per_staff_reported | unis_with_reported_ratio | avg_age_years | imputed_enrolments |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| Traditional University | 16 | 541091 | 33818 | 35093 | 30.9 | 6 | 74 | 3 |
| Comprehensive University | 4 | 479585 | 119896 | 22169 | 47.4 | 1 | 102 | 0 |
| University of Technology | 6 | 185984 | 30997 | 26992 | NA | 0 | 52 | 0 |

Investigation 1 result

**Interpretation.** Traditional universities are the largest group (16
of 26 institutions) and enrol the most students overall, with an average
and median close together. Comprehensive universities look biggest per
institution, but this is driven by one case: UNISA (about 420,000
students) lifts the average far above the median. Universities of
Technology are the youngest group (average age about 52 years). Staffing
comparisons are weak evidence, because academic staff counts are
reported for only 8 of 26 universities; once imputed values are
excluded, the ratio exists for 6 Traditional universities, 1
Comprehensive university and no Universities of Technology.

### Investigation 2 - enrolment by province

``` r
query2 <- "
WITH province_totals AS (
    SELECT province, COUNT(*) AS universities, SUM(students) AS total_students,
           COUNT(*) FILTER (WHERE students_imputed) AS imputed_enrolments
    FROM universities WHERE province <> 'Unknown' GROUP BY province
),
largest AS (
    SELECT DISTINCT ON (province) province, university AS largest_university, students AS largest_enrolment
    FROM universities WHERE province <> 'Unknown' ORDER BY province, students DESC
)
SELECT p.province, p.universities, p.total_students,
       ROUND(100.0 * p.total_students / SUM(p.total_students) OVER (), 1) AS pct_of_national_enrolment,
       RANK() OVER (ORDER BY p.total_students DESC)                       AS enrolment_rank,
       p.imputed_enrolments, l.largest_university, l.largest_enrolment
FROM province_totals p JOIN largest l USING (province)
ORDER BY enrolment_rank;"
cat(query2)
```

    #> 
    #> WITH province_totals AS (
    #>     SELECT province, COUNT(*) AS universities, SUM(students) AS total_students,
    #>            COUNT(*) FILTER (WHERE students_imputed) AS imputed_enrolments
    #>     FROM universities WHERE province <> 'Unknown' GROUP BY province
    #> ),
    #> largest AS (
    #>     SELECT DISTINCT ON (province) province, university AS largest_university, students AS largest_enrolment
    #>     FROM universities WHERE province <> 'Unknown' ORDER BY province, students DESC
    #> )
    #> SELECT p.province, p.universities, p.total_students,
    #>        ROUND(100.0 * p.total_students / SUM(p.total_students) OVER (), 1) AS pct_of_national_enrolment,
    #>        RANK() OVER (ORDER BY p.total_students DESC)                       AS enrolment_rank,
    #>        p.imputed_enrolments, l.largest_university, l.largest_enrolment
    #> FROM province_totals p JOIN largest l USING (province)
    #> ORDER BY enrolment_rank;

``` r
res2 <- if (pg_ok) dbGetQuery(con, query2) else read.csv("q4_sql_result2_provinces.csv")
knitr::kable(res2, caption = "Investigation 2 result")
```

| province | universities | total_students | pct_of_national_enrolment | enrolment_rank | imputed_enrolments | largest_university | largest_enrolment |
|:---|---:|---:|---:|---:|---:|:---|---:|
| Gauteng | 7 | 657942 | 54.5 | 1 | 0 | University of South Africa | 420000 |
| Western Cape | 4 | 121326 | 10.1 | 2 | 0 | Stellenbosch University | 35093 |
| KwaZulu-Natal | 4 | 107867 | 8.9 | 3 | 0 | University of KwaZulu-Natal | 46539 |
| Eastern Cape | 4 | 79836 | 6.6 | 4 | 0 | Walter Sisulu University | 31500 |
| Free State | 2 | 63162 | 5.2 | 5 | 0 | University of the Free State | 41169 |
| North West | 1 | 56000 | 4.6 | 6 | 0 | North-West University | 56000 |
| Limpopo | 2 | 50341 | 4.2 | 7 | 1 | University of Limpopo | 35093 |
| Mpumalanga | 1 | 35093 | 2.9 | 8 | 1 | University of Mpumalanga | 35093 |
| Northern Cape | 1 | 35093 | 2.9 | 8 | 1 | Sol Plaatje University | 35093 |

Investigation 2 result

**Interpretation.** Enrolment is highly concentrated: Gauteng holds
about 54% of students and the top three provinces about 73%. Gauteng’s
share is inflated by UNISA, which is headquartered there but teaches
nationwide by distance learning. The bottom of the ranking is not
reliable evidence: Mpumalanga and the Northern Cape tie only because
each has one university whose enrolment was imputed with the same
median, which is why the query reports the number of imputed enrolments.

**Limitations.** The data come from Wikipedia infoboxes citing different
years, so totals are approximate; 3 of 26 enrolment figures and 19 of 26
academic staff figures were imputed (and are flagged in the database);
and institution type is derived by regex from names and lead text.

## 4.4 Repository

This notebook, the scripts, the saved source pages and the datasets are
version-controlled in the project’s GitHub repository (see the
repository README for the layout).
