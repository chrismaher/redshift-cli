SELECT
      nspname AS name
    , nspowner AS owner
FROM
    pg_namespace
WHERE 1=1
    AND nspowner <> 1
    AND nspname LIKE ?
ORDER BY
    nspname
