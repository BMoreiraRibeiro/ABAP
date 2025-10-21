# CDS View

```sql
@AbapCatalog.sqlViewName: 'ZV_FLIGHTS'
@EndUserText.label: 'Voos e Companhias'
define view ZCDS_Flights as
  select from sflight
  association [0..1] to scarr as _Carrier on $projection.carrid = _Carrier.carrid
{
  key sflight.carrid,
  key sflight.connid,
  sflight.price,
  _Carrier.carrname
}
```
