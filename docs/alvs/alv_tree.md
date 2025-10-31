# ALV Tree - Hierárquico
# ALV Tree - Hierárquico

Apresentação de dados em estrutura de árvore (hierarquia pai-filho).

---

## 🔹 Exemplo com CL_SALV_TREE

```abap
REPORT z_alv_tree.

DATA: lo_tree TYPE REF TO cl_salv_tree,
      lt_data TYPE TABLE OF sflight.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE lt_data.

  TRY.
      cl_salv_tree=>factory(
        IMPORTING r_salv_tree = lo_tree
        CHANGING  t_table     = lt_data ).

      lo_tree->display( ).

    CATCH cx_salv_error INTO DATA(lx_err).
      MESSAGE lx_err->get_text( ) TYPE 'E'.
  ENDTRY.
```

---

💡 Ideal para estruturas como organogramas, BOMs ou árvores de pastas.

---

## 🔹 Exemplo Dinâmico: Z_DYNAMIC_SALV_TREE

Exemplo completo de um report ABAP que monta dinamicamente um SALV Tree a partir de qualquer estrutura, processando campos simples, estruturas internas e tabelas internas. Útil como referência para criar árvores dinâmicas que refletem a estrutura de dados.

```abap
*&---------------------------------------------------------------------*
*& Report Z_DYNAMIC_SALV_TREE
*&---------------------------------------------------------------------*
*& SALV Tree Dinâmica - Processa qualquer estrutura automaticamente
*&---------------------------------------------------------------------*
REPORT z_dynamic_salv_tree.

*----------------------------------------------------------------------*
* Estrutura para a Tree
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tree_node,
         field_name   TYPE string,
         field_value  TYPE string,
         field_type   TYPE string,
       END OF ty_tree_node.

*----------------------------------------------------------------------*
* Declarações Globais
*----------------------------------------------------------------------*
DATA: gt_tree_data TYPE STANDARD TABLE OF ty_tree_node.

DATA: go_tree      TYPE REF TO cl_salv_tree,
      go_nodes     TYPE REF TO cl_salv_nodes,
      go_node      TYPE REF TO cl_salv_node,
      go_columns   TYPE REF TO cl_salv_columns_tree,
      go_functions TYPE REF TO cl_salv_functions_tree.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " Obter dados
  DATA lv_os TYPE aufnr VALUE '200000000355'.
  DATA(lo_so) = NEW zcl_wm_so( lv_os ).
  DATA(ls_os) = zcl_wm_putorder_fill_body=>get_put_order_data( lo_so ).

  " Criar SALV Tree
  TRY.
      CALL METHOD cl_salv_tree=>factory
        IMPORTING
          r_salv_tree = go_tree
        CHANGING
          t_table     = gt_tree_data.
    CATCH cx_salv_error INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  " Obter nós
  go_nodes = go_tree->get_nodes( ).

  " Construir árvore recursivamente
  PERFORM build_tree USING ls_os '' 0.

  " Configurar colunas
  PERFORM configure_columns.

  " Ativar funções
  go_functions = go_tree->get_functions( ).
  go_functions->set_all( abap_true ).

  " Colapsar todos os nós
  TRY.
      go_nodes->collapse_all( ).
    CATCH cx_salv_msg.
  ENDTRY.

  " Exibir
  go_tree->display( ).

*----------------------------------------------------------------------*
* FORM build_tree - Construir árvore recursivamente
*----------------------------------------------------------------------*
FORM build_tree USING p_data       TYPE any
                      p_parent_key TYPE salv_de_node_key
                      p_level      TYPE i.

  DATA: wa_node        TYPE ty_tree_node,
        lv_current_key TYPE salv_de_node_key.

  DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data ).

  IF lo_type->kind <> cl_abap_typedescr=>kind_struct.
    RETURN.
  ENDIF.

  DATA(lt_comp) = CAST cl_abap_structdescr( lo_type )->components.

  LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<fs_comp>).

    " Ignorar campos especiais
    IF <fs_comp>-name = 'CONTROLLER' OR <fs_comp>-name = 'MANDT'.
      CONTINUE.
    ENDIF.

    " Obter valor do campo
    FIELD-SYMBOLS: <fs_value> TYPE any.
    ASSIGN COMPONENT <fs_comp>-name OF STRUCTURE p_data TO <fs_value>.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    CLEAR wa_node.
    wa_node-field_name = <fs_comp>-name.

    CASE <fs_comp>-type_kind.

      " ========== CAMPO SIMPLES ==========
      WHEN cl_abap_typedescr=>typekind_string OR
           cl_abap_typedescr=>typekind_char   OR
           cl_abap_typedescr=>typekind_num    OR
           cl_abap_typedescr=>typekind_int    OR
           cl_abap_typedescr=>typekind_int1   OR
           cl_abap_typedescr=>typekind_int2   OR
           cl_abap_typedescr=>typekind_packed OR
           cl_abap_typedescr=>typekind_date   OR
           cl_abap_typedescr=>typekind_time.

        wa_node-field_value = <fs_value>.
        wa_node-field_type = 'Campo'.

        TRY.
            go_node = go_nodes->add_node(
              related_node = p_parent_key
              relationship = cl_gui_column_tree=>relat_last_child
              data_row     = wa_node ).
          CATCH cx_salv_msg.
        ENDTRY.

      " ========== ESTRUTURA ==========
      WHEN cl_abap_typedescr=>typekind_struct1 OR
           cl_abap_typedescr=>typekind_struct2.

        wa_node-field_value = '(Estrutura)'.
        wa_node-field_type = 'Estrutura'.

        TRY.
            go_node = go_nodes->add_node(
              related_node = p_parent_key
              relationship = cl_gui_column_tree=>relat_last_child
              data_row     = wa_node ).
            lv_current_key = go_node->get_key( ).
          CATCH cx_salv_msg.
            CONTINUE.
        ENDTRY.

        " Chamada recursiva
        DATA(lv_next_level) = p_level + 1.
        PERFORM build_tree USING <fs_value> lv_current_key lv_next_level.

      " ========== TABELA ==========
      WHEN cl_abap_typedescr=>typekind_table.

        FIELD-SYMBOLS: <ft_table> TYPE ANY TABLE.
        ASSIGN <fs_value> TO <ft_table>.

        DATA(lv_lines) = lines( <ft_table> ).
        wa_node-field_value = |Tabela ({ lv_lines } linhas)|.
        wa_node-field_type = 'Tabela'.

        TRY.
            go_node = go_nodes->add_node(
              related_node = p_parent_key
              relationship = cl_gui_column_tree=>relat_last_child
              data_row     = wa_node ).
            lv_current_key = go_node->get_key( ).
          CATCH cx_salv_msg.
            CONTINUE.
        ENDTRY.

        " Processar linhas
        FIELD-SYMBOLS: <fs_row> TYPE any.
        DATA: lv_row_counter TYPE i VALUE 0,
              lv_row_key     TYPE salv_de_node_key.

        LOOP AT <ft_table> ASSIGNING <fs_row>.
          lv_row_counter = lv_row_counter + 1.

          " Criar nó para a linha (sem adicionar como nó visível)
          " Processar diretamente os campos da linha
          DATA(lv_table_level) = p_level + 1.
          PERFORM build_tree USING <fs_row> lv_current_key lv_table_level.
        ENDLOOP.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
* FORM configure_columns - Configurar colunas
*----------------------------------------------------------------------*
FORM configure_columns.

  DATA: lo_column TYPE REF TO cl_salv_column_tree.

  go_columns = go_tree->get_columns( ).
  go_columns->set_optimize( abap_false ).

  " Definir largura da coluna de hierarquia (ícones)
  TRY.
      go_tree->get_tree_settings( )->set_hierarchy_size( 8 ).
    CATCH cx_salv_error.
  ENDTRY.

  " Coluna FIELD_NAME
  TRY.
      lo_column ?= go_columns->get_column( 'FIELD_NAME' ).
      lo_column->set_long_text( 'Campo' ).
      lo_column->set_medium_text( 'Campo' ).
      lo_column->set_short_text( 'Campo' ).
      lo_column->set_output_length( 40 ).
    CATCH cx_salv_not_found.
  ENDTRY.

  " Coluna FIELD_VALUE
  TRY.
      lo_column ?= go_columns->get_column( 'FIELD_VALUE' ).
      lo_column->set_long_text( 'Valor' ).
      lo_column->set_medium_text( 'Valor' ).
      lo_column->set_short_text( 'Valor' ).
      lo_column->set_output_length( 80 ).
    CATCH cx_salv_not_found.
  ENDTRY.

  " Coluna FIELD_TYPE
  TRY.
      lo_column ?= go_columns->get_column( 'FIELD_TYPE' ).
      lo_column->set_long_text( 'Tipo' ).
      lo_column->set_medium_text( 'Tipo' ).
      lo_column->set_short_text( 'Tipo' ).
      lo_column->set_output_length( 12 ).
    CATCH cx_salv_not_found.
  ENDTRY.

ENDFORM.
```
