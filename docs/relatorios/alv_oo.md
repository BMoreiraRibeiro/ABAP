# ALV OO (CL_SALV_TABLE)

```abap
SELECT * FROM sflight INTO TABLE @DATA(lt_data).
cl_salv_table=>factory( IMPORTING r_salv_table = DATA(o_alv) CHANGING t_table = lt_data ).
o_alv->display( ).
```
