# Variáveis e Constantes

Variáveis armazenam valores temporários. Constantes mantêm valores fixos durante toda a execução do programa.

---

### 🔹 Declaração básica

```abap
DATA lv_nome TYPE string VALUE 'Bruno'.
DATA lv_idade TYPE i VALUE 25.

WRITE: / |Nome: { lv_nome }|, / |Idade: { lv_idade }|.
```

---

### 🔹 Constantes

```abap
CONSTANTS:
  lc_pi TYPE decfloat16 VALUE '3.1415926535',
  lc_texto TYPE string VALUE 'Constante imutável'.

WRITE: / lc_pi, lc_texto.
```

---

### 🔹 Conversões de tipo

```abap
DATA(lv_texto) = '123'.
DATA(lv_numero) = lv_texto.
WRITE: / lv_numero + 1.
```

---

💡 **Boas práticas**
- Usa nomes descritivos (`lv_` = local variable, `lc_` = local constant)
- Evita valores mágicos: cria constantes nomeadas.
