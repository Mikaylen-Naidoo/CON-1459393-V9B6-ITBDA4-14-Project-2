-- Investigation 1: institution types compared
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

-- Investigation 2: enrolment by province
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
