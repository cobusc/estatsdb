--
-- Run this script as the 'postgres' user!
-- Create the estatsdb database and user.
-- 

DROP DATABASE IF EXISTS estatsdb;
DROP USER IF EXISTS estatsdb;

CREATE DATABASE estatsdb
       ENCODING 'UTF8'
       TEMPLATE template0;

CREATE USER estatsdb PASSWORD 'password';
GRANT ALL ON DATABASE estatsdb TO estatsdb;

--
-- Create template tables using the 'estatsdb' user
--
\connect estatsdb;
SET ROLE estatsdb;

CREATE TABLE hourly_example_stats
(
    hour TIMESTAMP(0),
    host TEXT,
    metric1 INTEGER,
    metric2 FLOAT,
    metric3 BIGINT,
    PRIMARY KEY(hour, host)
);


SELECT pg_attribute.attname AS column_name,
       FORMAT_TYPE(pg_attribute.atttypid, pg_attribute.atttypmod) AS format_type,
       pg_attribute.attnum = ANY(pg_index.indkey) AS is_pk
  FROM pg_attribute,
       pg_index
 WHERE pg_attribute.attrelid = 'hourly_example_stats'::regclass                 
   AND pg_index.indrelid = pg_attribute.attrelid
   AND pg_index.indisprimary
   AND FORMAT_TYPE(pg_attribute.atttypid, pg_attribute.atttypmod) NOT IN ('oid', 'cid', 'xid', 'tid');
