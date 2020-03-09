SELECT
      schemaname
    , tablename
    , tableowner
FROM
    pg_tables
WHERE
    schemaname = ?
