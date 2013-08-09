-- Since PostgrSQL 9.1 CREATE EXTENSION is prefered over CREATE LANGUAGE
-- CREATE EXTENSION IF NOT EXISTS plpgsql;
CREATE LANGUAGE plpgsql;

--
-- Helper function to set metric value(s).
-- First an attempt is made to INSERT the value. If an entry already exists, it is UPDATEd.
--

CREATE OR REPLACE
FUNCTION set_helper(_InsertStatement TEXT, 
                    _UpdateStatement TEXT,
                    _ReturningStatement TEXT)
  RETURNS RECORD
AS $$
DECLARE
    _Result RECORD;
BEGIN
    BEGIN
        EXECUTE format('%s %s', _InsertStatement, _ReturningStatement)
           INTO STRICT _Result;
    EXCEPTION
        WHEN unique_violation THEN
            EXECUTE format('%s %s',_UpdateStatement, _ReturningStatement)
               INTO STRICT _Result;
    END;
    RETURN _Result;
END
$$
LANGUAGE plpgsql;

--
-- Helper function to update a metric value(s).
-- First an attempt is made to UPDATE the value(s). If no entries exist, it is INSERTed.
--

CREATE OR REPLACE
FUNCTION update_helper(_UpdateStatement TEXT, 
                       _InsertStatement TEXT,
                       _ReturningStatement TEXT)
  RETURNS RECORD
AS $$
DECLARE
    _Result RECORD;
BEGIN
    BEGIN
--        EXECUTE format('%s %s', _UpdateStatement, _ReturningStatement) -- PG 9.1+
          EXECUTE _UpdateStatement || ' ' || _ReturningStatement
          INTO STRICT _Result;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
--            EXECUTE format('%s %s', _InsertStatement, _ReturningStatement) -- PG 9.1+
              EXECUTE _InsertStatement || ' ' || _ReturningStatement
               INTO STRICT _Result;
    END;
    RETURN _Result;
END
$$
LANGUAGE plpgsql;
