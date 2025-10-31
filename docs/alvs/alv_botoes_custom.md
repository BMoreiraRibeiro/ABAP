# ALV com Botões Customizados

Adicionar botões próprios na toolbar do ALV.

---

## 🔹 Com CL_SALV_TABLE

```abap
DATA: lo_functions TYPE REF TO cl_salv_functions_list.

lo_functions = lo_alv->get_functions( ).
lo_functions->set_all( abap_true ).

" Adicionar botão customizado
lo_functions->add_function(
  name     = 'BTN_EXPORT'
  icon     = '@49@'  " Ícone de exportar
  text     = 'Exportar Seleção'
  tooltip  = 'Exportar linhas selecionadas'
  position = if_salv_c_function_position=>right_of_salv_functions ).

" Capturar evento
DATA: lo_events TYPE REF TO cl_salv_events_table.
lo_events = lo_alv->get_event( ).
SET HANDLER handle_user_command FOR lo_events.

CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_user_command
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD handle_user_command.
    CASE e_salv_function.
      WHEN 'BTN_EXPORT'.
        MESSAGE 'Botão Export clicado!' TYPE 'I'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
```
