# Field Symbols e Data References

Usados para apontar para endereços de memória dinamicamente (sem copiar dados).

---

### 🔹 Field-Symbols

```abap
DATA lv_valor TYPE i VALUE 10.
FIELD-SYMBOLS <fs> TYPE i.
ASSIGN lv_valor TO <fs>.

<fs> = <fs> + 5.
WRITE: / lv_valor. " 15
```

---

### 🔹 Data References

```abap
DATA lr_valor TYPE REF TO i.
CREATE DATA lr_valor.
lr_valor->* = 100.
WRITE: / lr_valor->*.
```

---

### 💡 Quando usar
- `FIELD-SYMBOLS` → apontar para variáveis/tabelas existentes  
- `REF TO` → criar referências novas em tempo de execução  
