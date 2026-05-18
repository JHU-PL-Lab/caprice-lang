.headers on
.mode markdown

.print "#### Average runtimes"
SELECT 
    ROUND(AVG(time_us_blue3)) || 'μs' AS blue3,
    ROUND(AVG(time_us_z3)) || 'μs' AS z3
FROM benchmarks;

.print "\n#### Blue3-only count"
SELECT COUNT(DISTINCT formula_id) AS count
FROM benchmarks
WHERE was_backend_used = 'false';

.print "\n#### Z3-deferred count"
SELECT COUNT(DISTINCT formula_id) AS count
FROM benchmarks
WHERE was_backend_used = 'true';

.print "\n#### Deferred overhead"
SELECT
  COUNT(*) AS cases,
  ROUND(AVG(time_us_blue3 - time_us_z3), 2) || 'μs' AS diff,
  ROUND(AVG(((time_us_blue3 - time_us_z3) * 100.0) / time_us_z3), 2) || '%' AS pct
FROM benchmarks
WHERE was_backend_used = 'true';

.print "\n#### Fast cases"
SELECT
  COUNT(*) AS cases,
  ROUND(AVG(time_us_z3 - time_us_blue3), 2) || 'μs' AS diff,
  ROUND(AVG(((time_us_z3 - time_us_blue3) * 100.0) / time_us_z3), 2) || '%' AS pct
FROM benchmarks
WHERE time_us_blue3 < time_us_z3;

.print "\n#### Slow cases"
SELECT
  COUNT(*) AS cases,
  ROUND(AVG(time_us_blue3 - time_us_z3), 2) || 'μs' AS diff,
  ROUND(AVG(((time_us_blue3 - time_us_z3) * 100.0) / time_us_blue3), 2) || '%' AS pct
FROM benchmarks
WHERE time_us_blue3 > time_us_z3;

.print "\n#### Top 5 fastest Blue3-only cases"
SELECT
  formula_id AS id,
  formula,
  ROUND(AVG(time_us_blue3), 2) || 'μs' AS blue3,
  ROUND(AVG(time_us_z3), 2) || 'μs' AS z3,
  ROUND(AVG(time_us_blue3) - AVG(time_us_z3), 2) || 'μs' AS diff
FROM benchmarks
WHERE was_backend_used = 'false'
GROUP BY formula_id, formula
ORDER BY AVG(time_us_blue3) ASC
LIMIT 5;

.print "\n#### Top 5 slowest Blue3-only cases"
SELECT
  formula_id AS id,
  formula,
  ROUND(AVG(time_us_blue3), 2) || 'μs' AS blue3,
  ROUND(AVG(time_us_z3), 2) || 'μs' AS z3,
  ROUND(AVG(time_us_blue3) - AVG(time_us_z3), 2) || 'μs' AS diff
FROM benchmarks
WHERE was_backend_used = 'false'
GROUP BY formula_id, formula
ORDER BY AVG(time_us_blue3) DESC
LIMIT 5;

.print "\n#### Biggest Blue3 win"
WITH diffs AS (
    SELECT time_us_z3 - time_us_blue3 AS diff
    FROM benchmarks
)
SELECT 
    ROUND(MAX(diff)) || 'μs' AS diff
FROM diffs;

.print "\n#### Biggest Z3 win"
WITH diffs AS (
    SELECT time_us_blue3 - time_us_z3 AS diff
    FROM benchmarks
)
SELECT 
    ROUND(MAX(diff)) || 'μs' AS diff
FROM diffs;

.print "\n#### Cases Z3 beat Blue3"
SELECT
  b.formula_id AS id,
  b.formula,
  ROUND(b.time_us_blue3, 2) AS blue3,
  ROUND(b.time_us_z3, 2) AS z3
FROM benchmarks b
WHERE time_us_z3 < time_us_blue3
ORDER BY time_us_blue3 DESC
LIMIT 5;
