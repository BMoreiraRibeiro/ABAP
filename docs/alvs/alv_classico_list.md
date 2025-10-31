# ALV Clássico - List Display

Apresentação em formato lista (menos funcionalidades que Grid).

---

## 🔹 Exemplo

```abap
REPORT z_alv_list_classico.

DATA: lt_scarr TYPE TABLE OF scarr,
      lt_fieldcat TYPE slis_t_fieldcat_alv.

START-OF-SELECTION.
  SELECT * FROM scarr INTO TABLE lt_scarr.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'SCARR'
    CHANGING
      ct_fieldcat      = lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      it_fieldcat = lt_fieldcat
    TABLES
      t_outtab    = lt_scarr.
```

---

💡 Menos usado que Grid, mas útil para reports simples.
