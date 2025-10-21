# BAPI Flight GetList

```abap
CALL FUNCTION 'BAPI_FLIGHT_GETLIST'
  EXPORTING airline = 'LH'
  TABLES flight_list = DATA(lt_flights).
```
