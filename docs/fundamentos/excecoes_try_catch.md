# Tratamento de Exceções (TRY / CATCH)

ABAP usa exceções para lidar com erros de execução de forma estruturada.

---

### 🔹 Exemplo

```abap
TRY.
    DATA(lv_x) = 1 / 0. " Gera exceção
  CATCH cx_sy_zerodivide INTO DATA(lx_error).
    WRITE: / lx_error->get_text( ).
ENDTRY.
```

---

### 💡 Dica
Usa classes CX_* para capturar tipos específicos de erro.  
