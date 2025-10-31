# ALV OO - CL_GUI_ALV_GRID

ALV com container para embedding em Dynpros ou integração avançada.

---

## 🔹 Exemplo Completo

```abap
REPORT z_alv_gui_grid.

DATA: lo_container TYPE REF TO cl_gui_custom_container,
      lo_alv       TYPE REF TO cl_gui_alv_grid,
      lt_sflight   TYPE TABLE OF sflight,
      lt_fieldcat  TYPE lvc_t_fcat,
      ls_layout    TYPE lvc_s_layo.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE lt_sflight UP TO 100 ROWS.

  " Montar fieldcat
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'SFLIGHT'
    CHANGING
      ct_fieldcat      = lt_fieldcat.

  " Layout
  ls_layout-zebra = 'X'.
  ls_layout-cwidth_opt = 'X'.

  " Criar container (em Dynpro seria ligado a um controlo)
  CREATE OBJECT lo_container
    EXPORTING
      container_name = 'CONTAINER'.

  " Criar ALV Grid
  CREATE OBJECT lo_alv
    EXPORTING
      i_parent = lo_container.

  " Exibir dados
  CALL METHOD lo_alv->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_outtab       = lt_sflight
      it_fieldcatalog = lt_fieldcat.

  CALL SCREEN 100.
```

---

## 🔹 Screen 100 (Dynpro)

- Criar screen 100 na SE51
- Adicionar Custom Container chamado `CONTAINER`
- PBO:
```abap
PROCESS BEFORE OUTPUT.
  MODULE status_0100.
```

---

💡 Usado quando precisas de controlo total sobre o ALV e integração com Dynpros.
