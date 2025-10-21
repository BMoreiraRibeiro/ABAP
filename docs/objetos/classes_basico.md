# Classe simples

```abap
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS show.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD show.
    WRITE: / 'Olá OO ABAP'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_obj) = NEW lcl_demo( ).
  lo_obj->show( ).
```
