[![Build Status](https://travis-ci.org/cobusc/estatsdb.png?branch=master)](https://www.travis-ci.org/cobusc/estatsdb)

estatsdb
========

Estatsdb is an Erlang server which collects metrics in a PostgreSQL DB. While there are many tools for publishing metrics to backends like graphite, I had the need to publish metrics to a database which
* can be used by reporting tools like JasperReports,
* allows for the easy management of data (via partitions, for instance)
* allows for the creation of views, and
* provides a flexible querying interface.

The application reads the database table definitions of tables in the specified schemas (default "public") at startup or when refreshed. The definitions are used to 
* check the validity of requests and
* construct SQL queries using explicit type casts from strings (the values passed in the URL) to the known column types in the database.

Access control is currently provided using IP-address checking, but it is trivial to add password-based authentication.

Integration with existing databases is easy, since _estatsdb_ only requires two helper functions to be created. Refer to https://github.com/cobusc/estatsdb/blob/master/estatsdb/src/sql/create_estatsdb_functions.sql for details.

Requirements
------------
PostgreSQL 8.4 or higher. Whilst I initially used functions only available since 9.1, I subsequently removed it for compatibility's sake. The code contains the 9.1 conventions/functions as comments.

Usage
-----

Create a table for you application statistics. It can exist in its own schema.
```sql
CREATE SCHEMA myapp;
CREATE TABLE myapp.daily_stats
(
    day DATE,
    host TEXT,
    metric1 INTEGER,
    metric2 FLOAT
    metric3 BIGINT,
    PRIMARY KEY (day, host)
);
```

Refresh the `schema_server` by calling a URL (access control based on the caller's IP-address):
```
curl "http://localhost:8000/refresh"
```

or alternatively from the Erlang shell:
```erlang
schema_server:refresh().
```

To *set* values, call:
```
http://localhost:8000/set?tablename=myapp.daily_stats&day=2013-01-01&host=testhost&metric1=100
```

This will return something like:
```json
{
    "tablename": "myapp.daily_stats",
    "day": "2013-01-01",
    "host": "testhost",
    "metric1": 100,
    "metric2": null,
    "metric3": null
}
```

Note that the table name and primary key values *must* always be specified. At least *one* metric value is required.
If a value already existed, it is replaced.

To *update* values, call:

```
http://localhost:8000/update?tablename=myapp.daily_stats&day=2013-01-01&host=testhost&metric1=50
```

This will return something like:
```json
{
    "tablename": "myapp.daily_stats",
    "day": "2013-01-01",
    "host": "testhost",
    "metric1": 150,
    "metric2": null,
    "metric3": null
}
```

As for the *set* function, the table name and primary key values *must* always be specified. At least *one* metric value is required.
If the value to update was not set before, the new value is set. Otherwise, the existing value is incremented with the specified value.

To *get* values, call:
```
http://localhost:8000/get?tablename=myapp.daily_stats&day=2013-01-01&host=testhost&metric1
```

This will return something like:
```json
{
    "tablename": "myapp.daily_stats",
    "day": "2013-01-01",
    "host": "testhost",
    "metric1": 100
}
```
As for the other functionality, the tablename and primary key columns and values are expected. Note that only the specified metrics will be returned. If there is no data, an HTTP 404 response is returned.

Notes on concurrency
--------------------
In the event that two (or more) requests simultaneously try to create the same primary key in a table, some of the requests may fail with an HTTP 500 error. It is up to the caller to resubmit the request.

