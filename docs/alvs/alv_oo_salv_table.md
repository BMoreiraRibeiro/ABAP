# ALV OO - CL_SALV_TABLE

Abordagem moderna e simplificada para ALVs usando orientação a objetos.

---

## 🔹 Exemplo Básico

```abap
REPORT z_alv_oo_salv.

DATA: lt_sflight TYPE TABLE OF sflight,
      lo_alv TYPE REF TO cl_salv_table.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE lt_sflight UP TO 100 ROWS.

  TRY.
      " Factory method cria instância do ALV
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = lt_sflight ).

      " Exibir
      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.
```

---

## 🔹 Com Funcionalidades Ativadas

```abap
DATA: lo_functions TYPE REF TO cl_salv_functions_list,
      lo_columns   TYPE REF TO cl_salv_columns_table,
      lo_display   TYPE REF TO cl_salv_display_settings.

" Ativar todas as funções padrão (Excel, layout, filtros, etc.)
lo_functions = lo_alv->get_functions( ).
lo_functions->set_all( abap_true ).

" Otimizar colunas
lo_columns = lo_alv->get_columns( ).
lo_columns->set_optimize( abap_true ).

" Configurar título
lo_display = lo_alv->get_display_settings( ).
lo_display->set_list_header( 'Lista de Voos' ).
lo_display->set_striped_pattern( abap_true ).  " Zebrado

lo_alv->display( ).
```

---

## 🔹 Alterar Propriedades de Colunas

```abap
DATA: lo_column TYPE REF TO cl_salv_column_table.

TRY.
    " Obter coluna específica
    lo_column ?= lo_columns->get_column( 'PRICE' ).
    
    " Configurar
    lo_column->set_short_text( 'Preço' ).
    lo_column->set_medium_text( 'Preço do Voo' ).
    lo_column->set_long_text( 'Preço Total do Voo' ).
    
  CATCH cx_salv_not_found.
    " Coluna não encontrada
ENDTRY.
```

---

## 💡 Vantagens do CL_SALV_TABLE
- Código mais limpo e OO
- Funcionalidades automáticas (Export, Sort, Filter)
- Melhor manutenibilidade
