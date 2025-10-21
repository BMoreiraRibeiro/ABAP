# Joins

```abap
SELECT a~carrid, a~connid, b~carrname
  FROM spfli AS a
  INNER JOIN scarr AS b ON a~carrid = b~carrid
  INTO TABLE @DATA(lt_join).
```
