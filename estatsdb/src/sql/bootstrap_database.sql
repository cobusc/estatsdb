--
-- Run this script as the 'postgres' user!
-- Create the estatsdb database and user.
-- 

DROP DATABASE IF EXISTS estatsdb;
DROP USER IF EXISTS estatsdb;

CREATE USER estatsdb PASSWORD 'password';

CREATE DATABASE estatsdb
       OWNER estatsdb
       ENCODING 'UTF8'
       TEMPLATE template0;

