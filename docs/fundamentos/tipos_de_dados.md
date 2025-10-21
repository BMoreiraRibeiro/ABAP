# Tipos de Dados em ABAP

Os tipos de dados definem **o formato e a natureza da informação** guardada numa variável.

---

### 🔹 Tipos Elementares

```abap
DATA: lv_i    TYPE i VALUE 42,          " Inteiro
      lv_p    TYPE p DECIMALS 2 VALUE '123.45',  " Decimal (packed)
      lv_c    TYPE c LENGTH 10 VALUE 'Texto',    " Cadeia de caracteres fixa
      lv_d    TYPE d VALUE sy-datum,             " Data (YYYYMMDD)
      lv_t    TYPE t VALUE sy-uzeit,             " Hora (HHMMSS)
      lv_str  TYPE string VALUE 'ABAP moderno'.  " Cadeia dinâmica
WRITE: / lv_i, lv_p, lv_c, lv_d, lv_t, lv_str.
```

---

### 🔹 Explicação
- `TYPE i`: inteiro (4 bytes)
- `TYPE p`: número decimal (com precisão)
- `TYPE c`: string de comprimento fixo
- `TYPE string`: string de comprimento variável
- `TYPE d` / `TYPE t`: data e hora, respetivamente

---

### 💡 Nota
Usa `TYPE` quando defines uma variável (declaração implícita) e `LIKE` para herdar o tipo de outro campo ou estrutura:
```abap
DATA lv_preco LIKE sflight-price.
```
