# ALV Editável

Permitir edição direta de dados no ALV.

---

## 🔹 Com CL_GUI_ALV_GRID

```abap
DATA: lt_sflight TYPE TABLE OF sflight,
      lo_alv     TYPE REF TO cl_gui_alv_grid,
      lt_fieldcat TYPE lvc_t_fcat,
      ls_layout  TYPE lvc_s_layo.

" ... criar container e dados ...

" Marcar campo como editável no fieldcat
LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
  CASE <fs_fcat>-fieldname.
    WHEN 'PRICE' OR 'CURRENCY'.
      <fs_fcat>-edit = 'X'.  " Permitir edição
  ENDCASE.
ENDLOOP.

" Layout para edição
ls_layout-edit = 'X'.

CALL METHOD lo_alv->set_table_for_first_display
  EXPORTING
    is_layout       = ls_layout
  CHANGING
    it_outtab       = lt_sflight
    it_fieldcatalog = lt_fieldcat.
```

---

## 🔹 Validar Dados Editados

```abap
" Evento DATA_CHANGED
SET HANDLER handle_data_changed FOR lo_alv.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_data_changed.
    " er_data_changed contém as alterações
    LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_cell).
      " Validar valor
      IF ls_cell-value < 0.
        MESSAGE 'Preço não pode ser negativo' TYPE 'E'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```
