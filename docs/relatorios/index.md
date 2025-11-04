---
tags:
  - Relatórios
  - ALV
  - SALV
  - Grid
  - UI
  - Interactive Reports
---

# Relatórios ABAP

Relatórios são programas que exibem dados em formatos estruturados. O SAP oferece diferentes tecnologias, sendo ALV (ABAP List Viewer) a mais utilizada.

## Tipos de Relatórios

| Tipo | Tecnologia | Uso | Modernidade |
|------|------------|-----|-------------|
| **Classical Report** | WRITE statements | Listagens simples | ❌ Legado |
| **ALV Function Module** | REUSE_ALV_* | Grids com funções básicas | ⚠️ Legado |
| **ALV OO (SALV)** | cl_salv_table | Moderno, simples | ✅ Recomendado |
| **ALV Grid Control** | cl_gui_alv_grid | Customização avançada | ✅ Avançado |

## SALV (ALV Orientado a Objetos)

### Relatório Básico

```abap
REPORT z_simple_report.

START-OF-SELECTION.
  " Buscar dados
  SELECT * FROM sflight
    INTO TABLE @DATA(lt_flights)
    UP TO 100 ROWS.

  " Criar ALV
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = DATA(lo_alv)
        CHANGING
          t_table      = lt_flights ).

      " Exibir
      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_error).
      WRITE: / lx_error->get_text( ).
  ENDTRY.
```

### ALV com Funcionalidades

```abap
DATA: lo_alv TYPE REF TO cl_salv_table.

SELECT * FROM scarr INTO TABLE @DATA(lt_carriers).

cl_salv_table=>factory(
  IMPORTING r_salv_table = lo_alv
  CHANGING  t_table      = lt_carriers ).

" Funções (All Buttons)
DATA(lo_functions) = lo_alv->get_functions( ).
lo_functions->set_all( abap_true ).

" Colunas
DATA(lo_columns) = lo_alv->get_columns( ).
lo_columns->set_optimize( abap_true ).  " Otimizar largura

" Coluna específica
DATA(lo_column) = CAST cl_salv_column_table(
  lo_columns->get_column( 'CARRID' ) ).
lo_column->set_short_text( 'ID' ).
lo_column->set_medium_text( 'Cia Aérea' ).
lo_column->set_long_text( 'Código Companhia' ).

" Display settings
DATA(lo_display) = lo_alv->get_display_settings( ).
lo_display->set_striped_pattern( abap_true ).  " Zebra
lo_display->set_list_header( 'Lista de Companhias Aéreas' ).

lo_alv->display( ).
```

### Ordenação e Filtros

```abap
" Sort
DATA(lo_sorts) = lo_alv->get_sorts( ).
TRY.
    lo_sorts->add_sort(
      columnname = 'CARRNAME'
      sequence   = if_salv_c_sort=>sort_up ).
  CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
ENDTRY.

" Filtros
DATA(lo_filters) = lo_alv->get_filters( ).
TRY.
    lo_filters->add_filter(
      columnname = 'CURRCODE'
      sign       = 'I'
      option     = 'EQ'
      low        = 'USD' ).
  CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
ENDTRY.
```

### Agregação (Subtotais)

```abap
DATA(lo_aggregations) = lo_alv->get_aggregations( ).

TRY.
    lo_aggregations->add_aggregation(
      columnname  = 'SEATSMAX'
      aggregation = if_salv_c_aggregation=>total ).
  CATCH cx_salv_data_error cx_salv_not_found cx_salv_existing.
ENDTRY.
```

### Cores em Células

```abap
" Definir estrutura com campo de cor
TYPES: BEGIN OF ty_flight,
         carrid    TYPE sflight-carrid,
         connid    TYPE sflight-connid,
         fldate    TYPE sflight-fldate,
         price     TYPE sflight-price,
         currency  TYPE sflight-currency,
         t_color   TYPE lvc_t_scol,  " Campo de cor
       END OF ty_flight.

DATA: lt_flights TYPE TABLE OF ty_flight.

" Preencher dados
SELECT carrid, connid, fldate, price, currency
  FROM sflight
  INTO CORRESPONDING FIELDS OF TABLE @lt_flights
  UP TO 50 ROWS.

" Definir cores
LOOP AT lt_flights ASSIGNING FIELD-SYMBOL(<fs_flight>).
  " Se preço > 500, vermelho
  IF <fs_flight>-price > 500.
    APPEND VALUE #(
      fname     = 'PRICE'
      color-col = col_negative  " Vermelho
      color-int = 0
    ) TO <fs_flight>-t_color.
  ENDIF.
ENDLOOP.

" Criar ALV
cl_salv_table=>factory(
  IMPORTING r_salv_table = DATA(lo_alv)
  CHANGING  t_table      = lt_flights ).

" Ativar cores
DATA(lo_columns) = lo_alv->get_columns( ).
lo_columns->set_color_column( 'T_COLOR' ).

lo_alv->display( ).
```

### Eventos (Duplo Clique)

```abap
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    READ TABLE lt_flights INDEX row INTO DATA(ls_flight).
    IF sy-subrc = 0.
      MESSAGE |Voo: { ls_flight-carrid } { ls_flight-connid }| TYPE 'I'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE @DATA(lt_flights) UP TO 100 ROWS.

  cl_salv_table=>factory(
    IMPORTING r_salv_table = DATA(lo_alv)
    CHANGING  t_table      = lt_flights ).

  " Registrar evento
  DATA(lo_events) = lo_alv->get_event( ).
  DATA(lo_handler) = NEW lcl_event_handler( ).
  SET HANDLER lo_handler->on_double_click FOR lo_events.

  lo_alv->display( ).
```

## ALV Grid Control (Avançado)

Para customização total, use `cl_gui_alv_grid`.

### ALV Grid Básico

```abap
REPORT z_alv_grid.

" Containers
DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alv       TYPE REF TO cl_gui_alv_grid.

DATA: gt_flights TYPE TABLE OF sflight.

START-OF-SELECTION.
  CALL SCREEN 100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MAIN'.
  SET TITLEBAR 'TITLE'.

  IF go_container IS NOT BOUND.
    " Criar container
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CONTAINER'.  " Nome do Custom Control

    " Criar ALV
    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_container.

    " Buscar dados
    SELECT * FROM sflight INTO TABLE gt_flights UP TO 100 ROWS.

    " Exibir
    go_alv->set_table_for_first_display(
      CHANGING
        it_outtab = gt_flights ).
  ENDIF.
ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
```

**Screen 100:**
- Criar Custom Control (SE51)
- Nome: `CONTAINER`

### Field Catalog

```abap
DATA: lt_fieldcat TYPE lvc_t_fcat.

" Definir colunas manualmente
lt_fieldcat = VALUE #(
  ( fieldname = 'CARRID'   coltext = 'Airline'    outputlen = 10 )
  ( fieldname = 'CONNID'   coltext = 'Connection' outputlen = 10 )
  ( fieldname = 'FLDATE'   coltext = 'Date'       outputlen = 10 )
  ( fieldname = 'PRICE'    coltext = 'Price'      outputlen = 15 )
  ( fieldname = 'CURRENCY' coltext = 'Currency'   outputlen = 5  )
).

go_alv->set_table_for_first_display(
  EXPORTING
    it_fieldcatalog = lt_fieldcat
  CHANGING
    it_outtab       = gt_flights ).
```

### Layout

```abap
DATA: ls_layout TYPE lvc_s_layo.

ls_layout-zebra      = abap_true.  " Linhas zebradas
ls_layout-cwidth_opt = abap_true.  " Otimizar largura
ls_layout-sel_mode   = 'A'.        " Seleção múltipla (A, B, C, D)

go_alv->set_table_for_first_display(
  EXPORTING
    is_layout = ls_layout
  CHANGING
    it_outtab = gt_flights ).
```

### Edição em ALV Grid

```abap
" Tornar campo editável
LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
  CASE <fs_fcat>-fieldname.
    WHEN 'PRICE'.
      <fs_fcat>-edit = abap_true.
  ENDCASE.
ENDLOOP.

" Registrar evento de data changed
SET HANDLER lcl_handler=>on_data_changed FOR go_alv.

" Handler
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: on_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_data_changed.
    LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_cell).
      " Processar mudanças
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

## Interactive Reports

Relatórios com drill-down (AT LINE-SELECTION).

```abap
REPORT z_interactive.

START-OF-SELECTION.
  " Lista de companhias
  SELECT * FROM scarr INTO TABLE @DATA(lt_carriers).

  LOOP AT lt_carriers INTO DATA(ls_carrier).
    WRITE: / ls_carrier-carrid, ls_carrier-carrname.
    HIDE ls_carrier-carrid.  " Esconder valor para retrieval
  ENDLOOP.

AT LINE-SELECTION.
  " Usuário clicou em linha
  DATA lv_carrid TYPE s_carr_id.
  lv_carrid = ls_carrier-carrid.  " Recuperar valor HIDE

  " Exibir voos dessa companhia
  SELECT * FROM sflight
    WHERE carrid = @lv_carrid
    INTO TABLE @DATA(lt_flights).

  SKIP 2.
  WRITE: / 'Voos da companhia:', lv_carrid.
  ULINE.

  LOOP AT lt_flights INTO DATA(ls_flight).
    WRITE: / ls_flight-connid,
             ls_flight-fldate,
             ls_flight-price,
             ls_flight-currency.
  ENDLOOP.
```

## Smartforms (Impressão)

Para documentos formatados (faturas, pedidos).

**Transação: SMARTFORMS**

```abap
" Chamar Smartform
DATA: lv_fm_name TYPE rs38l_fnam.

CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = 'Z_MY_SMARTFORM'
  IMPORTING
    fm_name            = lv_fm_name
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2.

IF sy-subrc = 0.
  CALL FUNCTION lv_fm_name
    EXPORTING
      iv_order_number = lv_order_number
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3.
ENDIF.
```

## Adobe Forms

Para PDFs complexos e interativos.

**Transação: SFP** (Form Builder)

```abap
DATA: lv_fm_name TYPE funcname,
      ls_output  TYPE sfpoutputparams.

" Configurar output
ls_output-dest     = 'PDF1'.
ls_output-nodialog = abap_true.

" Abrir job
CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams = ls_output.

" Get function name
CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
  EXPORTING
    i_name     = 'Z_MY_ADOBE_FORM'
  IMPORTING
    e_funcname = lv_fm_name.

" Chamar form
CALL FUNCTION lv_fm_name
  EXPORTING
    iv_customer_id = lv_customer_id.

" Fechar job
CALL FUNCTION 'FP_JOB_CLOSE'.
```

## Comparativo de Tecnologias

```abap
" ❌ Classical Report (Evite - sem funcionalidades)
WRITE: / 'Airline', 'Name'.
LOOP AT lt_carriers INTO DATA(ls_carrier).
  WRITE: / ls_carrier-carrid, ls_carrier-carrname.
ENDLOOP.

" ⚠️ ALV Function Module (Legado - use apenas se obrigatório)
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_structure_name = 'SCARR'
  TABLES
    t_outtab         = lt_carriers.

" ✅ SALV (Recomendado - simples e moderno)
cl_salv_table=>factory(
  IMPORTING r_salv_table = DATA(lo_alv)
  CHANGING  t_table      = lt_carriers ).
lo_alv->display( ).

" ✅ ALV Grid Control (Para customização avançada)
CREATE OBJECT go_alv EXPORTING i_parent = go_container.
go_alv->set_table_for_first_display( CHANGING it_outtab = gt_carriers ).
```

## Boas Práticas

!!! tip "Recomendações"
    - **Use SALV** para 90% dos casos (simples e poderoso)
    - **ALV Grid Control** apenas quando precisar edição ou eventos complexos
    - Sempre **otimize largura** de colunas: `lo_columns->set_optimize( )`
    - Use **zebra pattern** para melhor leitura
    - Defina **textos curtos, médios e longos** para colunas
    - Implemente **filtros e ordenação** quando útil

!!! warning "Performance"
    ```abap
    " ❌ Buscar dados em loop dentro do ALV
    LOOP AT lt_data INTO ls_data.
      SELECT SINGLE * FROM table WHERE key = ls_data-key.
    ENDLOOP.

    " ✅ Buscar tudo antes do ALV
    SELECT * FROM scarr INTO TABLE @DATA(lt_carriers).
    SELECT * FROM spfli INTO TABLE @DATA(lt_routes).
    " ... processar ...
    " Depois exibir ALV
    ```

!!! example "Exportar para Excel"
    SALV já inclui botão de exportar para Excel automaticamente!
    ```abap
    DATA(lo_functions) = lo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).  " Habilita todos botões
    " Usuário pode exportar para Excel, Word, etc.
    ```

---

**Tags:** #Relatórios #ALV #SALV #Grid #Interactive-Reports #Smartforms #UI
