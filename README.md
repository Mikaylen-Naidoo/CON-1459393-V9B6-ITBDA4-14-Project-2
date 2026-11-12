# ITBDA4-14 Project 2 – Big data analytics, text mining and ETL in R

**Author:** Mikaylen Naidoo · **Student number:** CON-1459393-V9B6
**Module:** ITBDA4-14 (Eduvos) · **Tools:** R / RStudio, Hadoop Streaming (simulated locally), PostgreSQL 18, Git

This repository contains the documentation, datasets, scripts and analytics notebooks for all four questions of Project 2.

## Contents

| Question | Topic | Notebook (rendered) | Scripts | Data |
|---|---|---|---|---|
| 1 | Keyword search with MapReduce (Hadoop Streaming, R) | [Q1 notebook](Question%201/Q1_MapReduce_Notebook.md) | `mapper.R`, `reducer.R`, `run_local.R`, `run_job.sh` | `Question 1/data/map_reduce_test_docs/` (7 news articles supplied with the brief) |
| 2 | Topic modelling of news articles with LDA | [Q2 notebook](Question%202/Q2_LDA_Notebook.md) | `q2_lda.R` | `Question 2/data/news/` (10 news articles supplied with the brief) |
| 3 | Web text mining proof of concept (Wikipedia: *Natural language processing*) | [Q3 notebook](Question%203/Q3_WebText_Notebook.md) | `q3_webtext.R` | `Question 3/q3_raw_page.html` (saved page) |
| 4 | ETL with R and PostgreSQL (South African public universities) | [Q4 notebook](Question%204/Q4_ETL_Notebook.md) | `q4_scrape_clean.R`, `q4_load_postgres.R` | `Question 4/q4_raw_pages/` (26 saved Wikipedia articles), `q4_universities_raw.csv`, `q4_universities_clean.csv` |

The full project report for Questions 1–4 is in the `Final Report (Project 2)` folder.

## Repository layout

```
Question 1/                 mapper.R, reducer.R, run_local.R, run_job.sh, outputs, data/, notebook
Question 2/                 q2_lda.R, outputs, data/, notebook
Question 3/                 q3_webtext.R, q3_raw_page.html, outputs, notebook
Question 4/                 q4_scrape_clean.R, q4_load_postgres.R, q4_raw_pages/, CSV and SQL outputs, notebook
Final Report (Project 2)/   project report
```

## How to reproduce

1. Install R (4.4 or later) and RStudio, then download or clone this repository.
2. Required packages (installed automatically by the scripts if missing): `tm`, `topicmodels`, `ggplot2`, `wordcloud`, `RColorBrewer`, `httr`, `rvest`, `xml2`, `SnowballC`, `DBI`, `RPostgres`; to knit the notebooks also `rmarkdown` and `knitr`.
3. **Scripts:** set the working directory to a question folder (Session > Set Working Directory) and `Source` the script. Q3 and Q4 ask for an e-mail address (used in the Wikipedia User-Agent) and Q4.3 asks for the PostgreSQL password.
4. **Notebooks:** open a `.Rmd` file in a question folder and click *Knit*. They use the saved datasets, so they run offline.
5. **PostgreSQL (Q4.3):** create a database called `universities_db`, then run `q4_load_postgres.R`. To run the live SQL queries inside the Q4 notebook, set `Sys.setenv(PGPASSWORD = "...")` in the console and, from the repository folder, call `rmarkdown::render("Question 4/Q4_ETL_Notebook.Rmd")`; otherwise the notebook shows the saved results.
6. Hadoop is not required: the Q1 job is simulated locally with the same mapper and reducer scripts; `run_job.sh` is the configuration for a real cluster.

## Key results

- **Q1:** for the keyword "africa" the MapReduce job reports 16 whole-word matches (doc1: 1, doc2: 3, doc3: 8, doc5: 4) with exact row and column positions, matching an independent search.
- **Q2:** LDA with 3 topics on 10 news articles separates governance and administration language, public-service commission reporting, and departmental, policing and power-utility news.
- **Q3:** the article's vocabulary is dominated by *language*, *word*, *NLP*, *natural* and *processing*; two extractive summaries (frequency-based and TextRank) agree on 3 of 5 sentences.
- **Q4:** 26 university articles were scraped, cleaned and loaded into PostgreSQL. Gauteng holds about 54% of enrolment, inflated by UNISA; 3 of 26 enrolment and 19 of 26 academic staff figures were imputed and are flagged in the database.

## Data sources, licensing and compliance

- Q3 and Q4 data come from English Wikipedia, licensed under CC BY-SA 4.0 (attribution: Wikipedia contributors, with the page titles and access dates recorded in the data). Pages were requested politely (robots.txt checked, descriptive User-Agent, delays between requests) and saved locally so that analysis never needs to re-request them.
- The Q1 and Q2 datasets were supplied with the module brief for coursework and are included for assessment purposes only.
- No passwords or credentials are stored in this repository; the scripts ask for them at run time.

## Limitations

Wikipedia content changes over time and infobox figures cite different years, so the Q4 results are approximate. Imputed values are flagged (`*_imputed` columns). Extractive