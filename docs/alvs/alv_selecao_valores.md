# ALV para Seleção de Valores

## 📋 Visão Geral

Este tipo de ALV permite ao utilizador **selecionar um ou múltiplos registos** de uma lista apresentada, retornando os valores selecionados para processamento posterior. É ideal para:

- **Seleção de valores** em popup (como F4 customizado)
- **Escolha múltipla** de registos
- **Filtrar e selecionar** dados antes de processar

---

## 🎯 Método 1: ALV Popup com Seleção (Function Module)

### Exemplo Básico - Seleção Simples

```abap
REPORT z_alv_selecao_valores.

TYPES: BEGIN OF ty_material,
         matnr TYPE matnr,
         maktx TYPE maktx,
         mtart TYPE mtart,
         meins TYPE meins,
       END OF ty_material.

DATA: lt_material TYPE TABLE OF ty_material,
      ls_material TYPE ty_material,
      lt_return   TYPE TABLE OF ddshretval,
      ls_return   TYPE ddshretval.

START-OF-SELECTION.

  " Buscar dados
  SELECT mara~matnr, makt~maktx, mara~mtart, mara~meins
    FROM mara
    INNER JOIN makt ON mara~matnr = makt~matnr
    INTO TABLE @lt_material
    UP TO 100 ROWS
    WHERE makt~spras = @sy-langu.

  " Exibir ALV com seleção
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MATNR'           " Campo a retornar
      dynpprog        = sy-repid           " Programa
      dynpnr          = sy-dynnr           " Tela
      dynprofield     = 'P_MATNR'          " Campo de tela
      window_title    = 'Selecione um Material'
      value_org       = 'S'                " Estrutura
    TABLES
      value_tab       = lt_material        " Tabela de valores
      return_tab      = lt_return          " Valores retornados
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0 AND lt_return IS NOT INITIAL.
    READ TABLE lt_return INTO ls_return INDEX 1.
    WRITE: / 'Material selecionado:', ls_return-fieldval.
  ENDIF.
```

**Características:**
- ✅ Popup automático
- ✅ Busca integrada (F4)
- ✅ Retorna valor único
- ⚠️ Limitado a um campo de retorno

---

## 🎯 Método 2: ALV Popup com Checkbox (Seleção Múltipla)

### Exemplo - Múltipla Seleção

```abap
REPORT z_alv_multi_selecao.

TYPES: BEGIN OF ty_cliente,
         sel   TYPE char1,              " Checkbox
         kunnr TYPE kunnr,
         name1 TYPE name1,
         ort01 TYPE ort01,
         land1 TYPE land1,
       END OF ty_cliente.

DATA: lt_clientes TYPE TABLE OF ty_cliente,
      ls_cliente  TYPE ty_cliente,
      lt_fieldcat TYPE slis_t_fieldcat_alv,
      ls_fieldcat TYPE slis_fieldcat_alv,
      lt_selected TYPE TABLE OF ty_cliente.

START-OF-SELECTION.

  " Buscar clientes
  SELECT kunnr, name1, ort01, land1
    FROM kna1
    INTO CORRESPONDING FIELDS OF TABLE @lt_clientes
    UP TO 50 ROWS.

  " Preparar Field Catalog
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SEL'.
  ls_fieldcat-checkbox  = 'X'.
  ls_fieldcat-edit      = 'X'.
  ls_fieldcat-seltext_m = 'Sel'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'KUNNR'.
  ls_fieldcat-seltext_m = 'Cliente'.
  ls_fieldcat-checkbox  = space.
  ls_fieldcat-edit      = space.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'NAME1'.
  ls_fieldcat-seltext_m = 'Nome'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'ORT01'.
  ls_fieldcat-seltext_m = 'Cidade'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'LAND1'.
  ls_fieldcat-seltext_m = 'País'.
  APPEND ls_fieldcat TO lt_fieldcat.

  " Exibir ALV popup com checkbox
  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'Selecione Clientes'
      i_selection           = 'X'           " Permite seleção
      i_screen_start_column = 10
      i_screen_start_line   = 5
      i_screen_end_column   = 120
      i_screen_end_line     = 20
      i_checkbox_fieldname  = 'SEL'         " Campo checkbox
    IMPORTING
      e_exit                = DATA(lv_exit)
    TABLES
      t_outtab              = lt_clientes
      it_fieldcat           = lt_fieldcat
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

  IF sy-subrc = 0 AND lv_exit IS INITIAL.
    " Processar selecionados
    LOOP AT lt_clientes INTO ls_cliente WHERE sel = 'X'.
      APPEND ls_cliente TO lt_selected.
    ENDLOOP.

    " Mostrar resultado
    WRITE: / 'Registos selecionados:', lines( lt_selected ).
    LOOP AT lt_selected INTO ls_cliente.
      WRITE: / ls_cliente-kunnr, ls_cliente-name1.
    ENDLOOP.
  ELSE.
    WRITE: / 'Seleção cancelada'.
  ENDIF.
```

**Características:**
- ✅ Seleção múltipla com checkbox
- ✅ Popup configurável
- ✅ Retorna tabela completa
- ✅ Controlo sobre campos editáveis

---

## 🎯 Método 3: ALV OO com Seleção (CL_SALV_TABLE em Popup)

### Exemplo - Orientado a Objetos

```abap
REPORT z_alv_oo_selecao.

TYPES: BEGIN OF ty_fornecedor,
         sel   TYPE char1,
         lifnr TYPE lifnr,
         name1 TYPE name1,
         ort01 TYPE ort01,
       END OF ty_fornecedor.

DATA: lt_fornecedores TYPE TABLE OF ty_fornecedor,
      lo_alv          TYPE REF TO cl_salv_table,
      lo_selections   TYPE REF TO cl_salv_selections,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lo_column       TYPE REF TO cl_salv_column_table.

START-OF-SELECTION.

  " Buscar fornecedores
  SELECT lifnr, name1, ort01
    FROM lfa1
    INTO CORRESPONDING FIELDS OF TABLE @lt_fornecedores
    UP TO 50 ROWS.

  TRY.
      " Criar ALV
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false    " Fullscreen
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = lt_fornecedores ).

      " Ativar seleção múltipla
      lo_selections = lo_alv->get_selections( ).
      lo_selections->set_selection_mode(
        if_salv_c_selection_mode=>row_column ).

      " Configurar colunas
      lo_columns = lo_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      " Esconder campo SEL
      lo_column ?= lo_columns->get_column( 'SEL' ).
      lo_column->set_visible( abap_false ).

      " Exibir ALV
      lo_alv->display( ).

      " Obter linhas selecionadas
      DATA(lt_rows) = lo_selections->get_selected_rows( ).

      " Processar seleção
      IF lt_rows IS NOT INITIAL.
        WRITE: / 'Fornecedores selecionados:'.
        LOOP AT lt_rows INTO DATA(lv_row).
          READ TABLE lt_fornecedores INDEX lv_row INTO DATA(ls_fornecedor).
          IF sy-subrc = 0.
            WRITE: / ls_fornecedor-lifnr, ls_fornecedor-name1.
          ENDIF.
        ENDLOOP.
      ELSE.
        WRITE: / 'Nenhum fornecedor selecionado'.
      ENDIF.

    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.
```

**Características:**
- ✅ Totalmente orientado a objetos
- ✅ Seleção nativa do SALV
- ✅ Métodos para obter linhas selecionadas
- ✅ Mais controlo sobre aparência

---

## 🎯 Método 4: ALV com Duplo-Click para Seleção

### Exemplo - Seleção por Duplo Click

```abap
REPORT z_alv_doubleclick.

TYPES: BEGIN OF ty_pedido,
         vbeln TYPE vbeln_va,
         erdat TYPE erdat,
         netwr TYPE netwr,
         waerk TYPE waerk,
       END OF ty_pedido.

DATA: lt_pedidos TYPE TABLE OF ty_pedido,
      lo_alv     TYPE REF TO cl_salv_table,
      lo_events  TYPE REF TO cl_salv_events_table.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    READ TABLE lt_pedidos INDEX row INTO DATA(ls_pedido).
    IF sy-subrc = 0.
      MESSAGE |Pedido selecionado: { ls_pedido-vbeln }| TYPE 'I'.
      " Aqui pode processar o valor selecionado
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  " Buscar pedidos
  SELECT vbeln, erdat, netwr, waerk
    FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE @lt_pedidos
    UP TO 100 ROWS.

  TRY.
      " Criar ALV
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING t_table = lt_pedidos ).

      " Registar eventos
      lo_events = lo_alv->get_event( ).
      DATA(lo_handler) = NEW lcl_event_handler( ).
      SET HANDLER lo_handler->on_double_click FOR lo_events.

      " Configurar colunas
      lo_alv->get_columns( )->set_optimize( abap_true ).

      " Exibir
      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.
```

**Características:**
- ✅ Seleção intuitiva (duplo-click)
- ✅ Eventos customizáveis
- ✅ Fácil implementação
- ✅ Ideal para seleção única

---

## 📊 Comparação dos Métodos

| Método | Tipo | Múltipla Seleção | Complexidade | Uso Recomendado |
|--------|------|------------------|--------------|-----------------|
| **F4IF_INT_TABLE_VALUE_REQUEST** | Function | ❌ Não | ⭐ Baixa | Seleção rápida de valor único |
| **REUSE_ALV_POPUP_TO_SELECT** | Function | ✅ Sim (checkbox) | ⭐⭐ Média | Popup com múltipla seleção |
| **CL_SALV_TABLE com Seleções** | OO | ✅ Sim | ⭐⭐⭐ Alta | Seleção avançada, fullscreen |
| **Duplo-Click Event** | OO | ❌ Não* | ⭐⭐ Média | Seleção intuitiva única |

*Pode ser combinado com `CTRL+Click` para múltipla seleção

---

## 💡 Dicas Práticas

### ✅ Boas Práticas

1. **Para seleção única rápida**: Use `F4IF_INT_TABLE_VALUE_REQUEST`
2. **Para múltipla seleção em popup**: Use `REUSE_ALV_POPUP_TO_SELECT`
3. **Para controlo avançado**: Use `CL_SALV_TABLE` com eventos
4. **Validar sempre**: Verifique se `sy-subrc = 0` após chamadas
5. **User-friendly**: Adicione títulos descritivos às janelas

### ⚠️ Armadilhas Comuns

1. **Esquecer o campo checkbox**: Defina `i_checkbox_fieldname` corretamente
2. **Não processar cancelamento**: Sempre verificar `e_exit` em popups
3. **Performance**: Limite registos com `UP TO n ROWS` em SELECTs
4. **Field Catalog incompleto**: Certifique-se que todos os campos têm descrição

---

## 🔗 Exemplos Relacionados

- [ALV Popup](alv_popup.md) - Mais exemplos de popup
- [ALV com Eventos](alv_eventos.md) - Trabalhar com eventos
- [ALV Editável](alv_editavel.md) - Combinar seleção com edição

---

## 📚 Referências SAP

- Transaction: **SE37** → `F4IF_INT_TABLE_VALUE_REQUEST`
- Transaction: **SE37** → `REUSE_ALV_POPUP_TO_SELECT`
- Transaction: **SE24** → `CL_SALV_TABLE`, `CL_SALV_SELECTIONS`

---

**Tags:** `#ALV` `#Seleção` `#Popup` `#F4` `#Checkbox` `#Eventos`
