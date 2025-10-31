# ALV Popup

Exibir ALV numa janela modal (popup).

---

## 🔹 Exemplo com CL_SALV_TABLE

```abap
REPORT z_alv_popup.

DATA: lt_scarr TYPE TABLE OF scarr,
      lo_alv   TYPE REF TO cl_salv_table.

START-OF-SELECTION.
  SELECT * FROM scarr INTO TABLE lt_scarr.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false   " Não fullscreen
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = lt_scarr ).

      " Configurar como popup
      lo_alv->set_screen_popup(
        start_column = 10
        end_column   = 100
        start_line   = 5
        end_line     = 20 ).

      lo_alv->display( ).

    CATCH cx_salv_msg.
  ENDTRY.
```

---

## 🔹 Popup com REUSE_ALV_POPUP_TO_SELECT

```abap
DATA: lt_return TYPE TABLE OF scarr.

CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
  EXPORTING
    i_title               = 'Selecionar Companhia'
    i_tabname             = 'SCARR'
    i_structure_name      = 'SCARR'
  IMPORTING
    es_selfield           = DATA(ls_selected)
  TABLES
    t_outtab              = lt_scarr
  EXCEPTIONS
    program_error         = 1
    OTHERS                = 2.

IF sy-subrc = 0.
  WRITE: / 'Selecionado:', ls_selected-value.
ENDIF.
```
