# ALV com Eventos

Capturar ações do utilizador (clique duplo, botões, hotspots).

---

## 🔹 Evento Hotspot Click (CL_SALV_TABLE)

```abap
DATA: lo_events TYPE REF TO cl_salv_events_table.

lo_events = lo_alv->get_event( ).
SET HANDLER handle_link_click FOR lo_events.

" ... na classe event handler ...
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD handle_link_click.
    READ TABLE lt_sflight INDEX row INTO DATA(ls_flight).
    MESSAGE |Clicaste em: { ls_flight-carrid }| TYPE 'I'.
  ENDMETHOD.
ENDCLASS.
```

---

## 🔹 Evento User Command (Clássico)

```abap
FORM user_command USING r_ucomm TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'.  " Duplo clique
      READ TABLE lt_sflight INDEX rs_selfield-tabindex INTO DATA(ls_line).
      MESSAGE |Duplo clique: { ls_line-connid }| TYPE 'I'.
  ENDCASE.
ENDFORM.
```
