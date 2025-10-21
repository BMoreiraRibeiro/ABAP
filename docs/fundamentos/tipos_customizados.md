# Tipos Customizados

Podem ser definidos localmente (no programa) ou globalmente (via SE11).

---

### 🔹 Definição local

```abap
TYPES: BEGIN OF ty_cliente,
         id_cliente TYPE i,
         nome       TYPE string,
         cidade     TYPE string,
       END OF ty_cliente.

DATA ls_cliente TYPE ty_cliente VALUE #( id_cliente = 1 nome = 'Bruno' cidade = 'Porto' ).
WRITE: / ls_cliente-nome.
```

---

💡 Tipos customizados tornam o código mais organizado e reutilizável.
