# q4_load_postgres.R  -  ITBDA4-14 Project 2, Question 4.3
# Loads the cleaned CSV into a PostgreSQL relational table and runs two SQL investigations.

# Settings
db_host <- "localhost"
db_port <- 5432
db_name <- "universities_db"
db_user <- "postgres"
csv_file <- "q4_universities_clean.csv"

# Packages (installed automatically if missing)
pkgs <- c("DBI", "RPostgres")
missing_pkgs <- pkgs[!(pkgs %in% rownames(installed.packages()))]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
cat("Working directory:\n", getwd(), "\n\n")
if (!file.exists(csv_file)) stop("Cannot find ", csv_file, " in the working directory. Run q4_scrape_clean.R first.")


# 1. CONNECT TO POSTGRESQL (the password is typed in, never saved in the code)

cat("---- 1. CONNECTING TO POSTGRESQL ----\n")
pw <- Sys.getenv("PGPASSWORD", unset = "")
if (!nzchar(pw)) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    pw <- rstudioapi::askForPassword(paste0("PostgreSQL password for user '", db_user, "'"))
  } else {
    pw <- readline(paste0("PostgreSQL password for user '", db_user, "': "))
  }
}
if (is.null(pw) || !nzchar(pw)) stop("No password entered. Source the script again and type the password.")

con <- tryCatch(
  dbConnect(RPostgres::Postgres(), host = db_host, port = db_port, dbname = db_name, user = db_user, password = pw,
            bigint = "numeric"),
  error = function(e) stop("Could not connect to PostgreSQL: ", conditionMessage(e),
                           "\nCheck that PostgreSQL is running, the password is right and the database '", db_name, "' exists."))
cat("Connected to database '", db_name, "' on ", db_host, ":", db_port, "\n", sep = "")
cat(dbGetQuery(con, "SELECT version() AS server_version")$server_version, "\n\n")


# 2. CREATE THE RELATIONAL TABLE (data types, primary key, constraints)

cat("---- 2. CREATING THE TABLE ----\n")
create_sql <- "
DROP TABLE IF EXISTS universities;

CREATE TABLE universities (
    university_id         INTEGER       PRIMARY KEY,
    university            VARCHAR(150)  NOT NULL UNIQUE,
    institution_type      VARCHAR(40)   NOT NULL
                          CHECK (institution_type IN ('Traditional University',
                                                      'Comprehensive University',
                                                      'University of Technology')),
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
);
"
writeLines(create_sql, "q4_create_table.sql")
for (stmt in strsplit(create_sql, ";")[[1]]) if (nzchar(trimws(stmt))) suppressMessages(dbExecute(con, stmt))
cat("Table 'universities' created (SQL saved in q4_create_table.sql)\n\n")


# 3. IMPORT THE CLEANED CSV INTO THE TABLE

cat("---- 3. IMPORTING THE CSV ----\n")
df <- read.csv(csv_file, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
df$scraped_on <- as.Date(df$scraped_on)
cat("Rows read from CSV:", nrow(df), " | Columns:", ncol(df), "\n")
dbWriteTable(con, "universities", df, append = TRUE, row.names = FALSE)
n_loaded <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM universities")$n
cat("Rows now in the PostgreSQL table:", n_loaded, "\n\n")

cat("Table structure as stored in PostgreSQL:\n")
print(dbGetQuery(con, "
  SELECT column_name, data_type, is_nullable
  FROM information_schema.columns
  WHERE table_name = 'universities'
  ORDER BY ordinal_position"), row.names = FALSE)
cat("\nFirst rows of the table:\n")
print(dbGetQuery(con, "SELECT university_id, university, institution_type, province, founded_year, students, staff
                       FROM universities ORDER BY university_id LIMIT 6"), row.names = FALSE)


# 4. TWO SQL ANALYTICAL INVESTIGATIONS

cat("\n\n---- 4. SQL INVESTIGATIONS ----\n")

# Investigation 1: how do the three institution types compare in size, staffing and age?
# (staffing figures use only universities whose staff AND student counts were actually reported)
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
ORDER BY total_students DESC;
"

# Investigation 2: how is enrolment spread across provinces, and which university leads each one?
query2 <- "
WITH province_totals AS (
    SELECT province,
           COUNT(*)                                AS universities,
           SUM(students)                           AS total_students,
           COUNT(*) FILTER (WHERE students_imputed) AS imputed_enrolments
    FROM universities
    WHERE province <> 'Unknown'
    GROUP BY province
),
largest AS (
    SELECT DISTINCT ON (province)
           province, university AS largest_university, students AS largest_enrolment
    FROM universities
    WHERE province <> 'Unknown'
    ORDER BY province, students DESC
)
SELECT p.province,
       p.universities,
       p.total_students,
       ROUND(100.0 * p.total_students / SUM(p.total_students) OVER (), 1) AS pct_of_national_enrolment,
       RANK() OVER (ORDER BY p.total_students DESC)                       AS enrolment_rank,
       p.imputed_enrolments,
       l.largest_university,
       l.largest_enrolment
FROM province_totals p
JOIN largest l USING (province)
ORDER BY enrolment_rank;
"
writeLines(c("-- Investigation 1: institution types compared", trimws(query1), "",
             "-- Investigation 2: enrolment by province", trimws(query2)), "q4_analysis.sql")

cat("\nInvestigation 1 - institution types compared (size, staffing, age):\n")
res1 <- dbGetQuery(con, query1); print(res1, row.names = FALSE)

cat("\nInvestigation 2 - enrolment by province, share of national total and largest university:\n")
res2 <- dbGetQuery(con, query2); print(res2, row.names = FALSE)

write.csv(res1, "q4_sql_result1_institution_types.csv", row.names = FALSE)
write.csv(res2, "q4_sql_result2_provinces.csv", row.names = FALSE)

dbDisconnect(con)    # close the connection once all the work is done

cat("\nSaved to", getwd(), ":\n",
    " q4_create_table.sql, q4_analysis.sql, q4_sql_result1_institution_types.csv, q4_sql_result2_provinces.csv\n",
    "Open q4_analysis.sql in pgAdmin (Query Tool on universities_db) to run the queries there too.\n")