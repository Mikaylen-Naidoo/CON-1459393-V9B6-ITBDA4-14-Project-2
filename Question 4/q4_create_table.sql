
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

