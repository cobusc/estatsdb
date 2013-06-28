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


