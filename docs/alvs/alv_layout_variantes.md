# ALV com Layout e Variantes

Permitir que utilizadores guardem layouts personalizados.

---

## 🔹 Com CL_SALV_TABLE

```abap
DATA: lo_layout TYPE REF TO cl_salv_layout,
      ls_key    TYPE salv_s_layout_key.

" Definir chave para guardar variantes
ls_key-report = sy-repid.

lo_layout = lo_alv->get_layout( ).
lo_layout->set_key( ls_key ).
lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).  " Permitir guardar

lo_alv->display( ).
```

---

## 🔹 Layout Clássico

```abap
DATA: ls_variant TYPE disvariant.

ls_variant-report = sy-repid.
ls_variant-username = sy-uname.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_save          = 'A'  " Permitir guardar variantes
    is_variant      = ls_variant
    it_fieldcat     = lt_fieldcat
  TABLES
    t_outtab        = lt_data.
```

---

💡 Variantes permitem que cada utilizador configure o seu próprio layout (ordem de colunas, filtros, etc.).
