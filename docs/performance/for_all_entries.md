# FOR ALL ENTRIES

```abap
SELECT carrid, connid FROM sflight INTO TABLE @DATA(lt_s).
SELECT * FROM spfli INTO TABLE @DATA(lt_p)
  FOR ALL ENTRIES IN @lt_s
  WHERE carrid = @lt_s-carrid.
```
