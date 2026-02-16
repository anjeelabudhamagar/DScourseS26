-- Set CSV mode
.mode csv

-- Import the CSV into a table called 'insurance'
.import FL_insurance_sample.csv insurance

-- Optional checks
SELECT * FROM insurance LIMIT 10;
SELECT DISTINCT county FROM insurance;
SELECT AVG(tiv_2012 - tiv_2011) FROM insurance;
SELECT construction,
       COUNT(*) AS count,
       COUNT(*)*1.0/(SELECT COUNT(*) FROM insurance) AS fraction
FROM insurance
GROUP BY construction;

