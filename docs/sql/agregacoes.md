# Agregações

```abap
SELECT carrid, COUNT( * ) AS total_voos, AVG( price ) AS preco_medio
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_stats).
```
