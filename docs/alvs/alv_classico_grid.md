# ALV Clássico - Grid Display

O ALV Grid clássico usa o function module `REUSE_ALV_GRID_DISPLAY`.

---

## 🔹 Exemplo Básico

```abap
REPORT z_alv_grid_classico.

DATA: lt_sflight TYPE TABLE OF sflight,
      lt_fieldcat TYPE slis_t_fieldcat_alv.

START-OF-SELECTION.
  " Buscar dados
  SELECT * FROM sflight INTO TABLE lt_sflight UP TO 100 ROWS.

  " Montar catálogo de campos (automático via estrutura)
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'SFLIGHT'
    CHANGING
      ct_fieldcat      = lt_fieldcat.

  " Exibir ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat = lt_fieldcat
    TABLES
      t_outtab    = lt_sflight.
```

---

## 🔹 Com Layout Customizado

```abap
DATA: ls_layout TYPE slis_layout_alv.

" Configurar layout
ls_layout-zebra = 'X'.              " Linhas zebradas
ls_layout-colwidth_optimize = 'X'.  " Otimizar largura das colunas
ls_layout-box_fieldname = 'SEL'.    " Checkbox de seleção

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    is_layout   = ls_layout
    it_fieldcat = lt_fieldcat
  TABLES
    t_outtab    = lt_sflight.
```

---

## 💡 Parâmetros Úteis
- `i_callback_program` → Nome do programa para eventos
- `i_callback_user_command` → Rotina para comandos do utilizador
- `i_callback_pf_status_set` → GUI Status customizado
