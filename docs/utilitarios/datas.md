# Datas e Horas

```abap
DATA lv_hoje TYPE d VALUE sy-datum.
DATA(lv_amanha) = lv_hoje + 1.
WRITE: / lv_hoje, lv_amanha.
```
