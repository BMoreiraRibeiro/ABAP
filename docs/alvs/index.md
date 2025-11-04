# Tipos de ALV em ABAP

Esta secção aborda os diferentes tipos de ALV (ABAP List Viewer) disponíveis em ABAP para apresentação de dados.

---

## 📊 O que é ALV?

ALV (ABAP List Viewer) é uma ferramenta SAP para exibir dados de forma estruturada com funcionalidades automáticas como:
- Ordenação
- Filtros
- Exportação (Excel, PDF)
- Totalizações
- Layout personalizável

---

## 🎯 Tipos de ALV

### 1. **ALV Clássico (Function Modules)**
- `REUSE_ALV_GRID_DISPLAY`
- `REUSE_ALV_LIST_DISPLAY`
- `REUSE_ALV_HIERSEQ_LIST_DISPLAY`

### 2. **ALV Orientado a Objetos**
- `CL_SALV_TABLE` (simples e completo)
- `CL_GUI_ALV_GRID` (com container)

### 3. **ALV Tree**
- Apresentação hierárquica de dados

### 4. **ALV Popup**
- Janelas modais com dados

### 5. **ALV em Fullscreen**
- Ocupação total do ecrã

---

**Índice de Exemplos:**
1. [ALV Clássico Grid](alv_classico_grid.md)
2. [ALV Clássico List](alv_classico_list.md)
3. [ALV OO com CL_SALV_TABLE](alv_oo_salv_table.md)
4. [ALV OO com CL_GUI_ALV_GRID](alv_oo_gui_grid.md)
5. [ALV Tree Hierárquico](alv_tree.md)
6. [ALV Popup](alv_popup.md)
7. [ALV com Edição](alv_editavel.md)
8. [ALV com Eventos](alv_eventos.md)
9. [ALV com Botões Customizados](alv_botoes_custom.md)
10. [ALV com Layout e Variantes](alv_layout_variantes.md)
11. [ALV para Seleção de Valores](alv_selecao_valores.md)

---

## 🧩 Exemplos rápidos (in-page)

Aqui estão trechos de exemplo rápidos para cada tipo de ALV. Mantive a estrutura com páginas separadas — clique em "Ler mais" para ver a explicação completa e exemplos maiores.

### ALV Clássico - Grid (trecho)

```abap
REPORT z_alv_grid_classico.

DATA: lt_sflight TYPE TABLE OF sflight,
			lt_fieldcat TYPE slis_t_fieldcat_alv.

START-OF-SELECTION.
	SELECT * FROM sflight INTO TABLE lt_sflight UP TO 100 ROWS.
	CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
		EXPORTING i_structure_name = 'SFLIGHT'
		CHANGING  ct_fieldcat      = lt_fieldcat.
	CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
		EXPORTING it_fieldcat = lt_fieldcat
		TABLES   t_outtab    = lt_sflight.
```

[Ler mais »](alv_classico_grid.md)

### ALV Clássico - List (trecho)

```abap
REPORT z_alv_list_classico.
DATA: lt_scarr TYPE TABLE OF scarr, lt_fieldcat TYPE slis_t_fieldcat_alv.
START-OF-SELECTION.
	SELECT * FROM scarr INTO TABLE lt_scarr.
	CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE' EXPORTING i_structure_name = 'SCARR' CHANGING ct_fieldcat = lt_fieldcat.
	CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY' EXPORTING it_fieldcat = lt_fieldcat TABLES t_outtab = lt_scarr.
```

[Ler mais »](alv_classico_list.md)

### ALV OO - CL_SALV_TABLE (trecho)

```abap
DATA: lt_sflight TYPE TABLE OF sflight, lo_alv TYPE REF TO cl_salv_table.
START-OF-SELECTION.
	SELECT * FROM sflight INTO TABLE lt_sflight UP TO 100 ROWS.
	TRY.
			cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = lt_sflight ).
			lo_alv->display( ).
	CATCH cx_salv_msg INTO DATA(lx_error).
			MESSAGE lx_error->get_text( ) TYPE 'E'.
	ENDTRY.
```

[Ler mais »](alv_oo_salv_table.md)

### ALV OO - CL_GUI_ALV_GRID (trecho)

```abap
DATA: lo_container TYPE REF TO cl_gui_custom_container, lo_alv TYPE REF TO cl_gui_alv_grid, lt_sflight TYPE TABLE OF sflight.
CREATE OBJECT lo_container EXPORTING container_name = 'CONTAINER'.
CREATE OBJECT lo_alv EXPORTING i_parent = lo_container.
CALL METHOD lo_alv->set_table_for_first_display CHANGING it_outtab = lt_sflight.
```

[Ler mais »](alv_oo_gui_grid.md)

### ALV Tree - Exemplo dinâmico (trecho)

O exemplo dinâmico `Z_DYNAMIC_SALV_TREE` constrói recursivamente uma árvore a partir de qualquer estrutura (campos simples, estruturas internas e tabelas internas). Veja o exemplo completo na página.

[Ler mais »](alv_tree.md)

### ALV para Seleção de Valores (trecho)

```abap
" Seleção múltipla com popup e checkbox
CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
  EXPORTING
    i_title              = 'Selecione Clientes'
    i_selection          = 'X'
    i_checkbox_fieldname = 'SEL'
  IMPORTING
    e_exit               = DATA(lv_exit)
  TABLES
    t_outtab             = lt_clientes
    it_fieldcat          = lt_fieldcat.

IF sy-subrc = 0 AND lv_exit IS INITIAL.
  LOOP AT lt_clientes INTO DATA(ls_cliente) WHERE sel = 'X'.
    " Processar registos selecionados
  ENDLOOP.
ENDIF.
```

[Ler mais »](alv_selecao_valores.md)

---

Se quiser que algum destes trechos seja expandido (ex.: mostrar todo o exemplo do `Z_DYNAMIC_SALV_TREE` inline), digo e eu acrescento — por agora mantive trechos curtos na página principal e deixei os exemplos completos nas páginas individuais.
