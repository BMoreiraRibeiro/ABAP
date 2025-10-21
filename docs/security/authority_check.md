# Verificação de Autorização

```abap
AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SE38'.
IF sy-subrc <> 0.
  WRITE: / 'Sem permissão.'.
ENDIF.
```
