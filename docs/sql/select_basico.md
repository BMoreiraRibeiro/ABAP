# SELECT básico

```abap
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).
LOOP AT lt_scarr INTO DATA(ls_scarr).
  WRITE: / ls_scarr-carrid, ls_scarr-carrname.
ENDLOOP.
```
