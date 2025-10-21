# Tabelas Internas

Tabelas internas são coleções de linhas em memória — o equivalente ABAP a arrays ou listas.

---

### 🔹 Exemplo básico

```abap
TYPES: BEGIN OF ty_voo,
         carrid TYPE s_carr_id,
         connid TYPE s_conn_id,
       END OF ty_voo.

DATA lt_voos TYPE TABLE OF ty_voo.
APPEND VALUE #( carrid = 'LH' connid = '0400' ) TO lt_voos.
APPEND VALUE #( carrid = 'AA' connid = '0010' ) TO lt_voos.

LOOP AT lt_voos INTO DATA(ls_voo).
  WRITE: / ls_voo-carrid, ls_voo-connid.
ENDLOOP.
```

---

### 🔹 Tipos de tabelas
| Tipo | Descrição |
|------|------------|
| `STANDARD TABLE` | Ordem indefinida (acesso sequencial) |
| `SORTED TABLE` | Ordenada por chave (acesso binário) |
| `HASHED TABLE` | Indexada por hash (acesso direto rápido) |

---

💡 Usa `READ TABLE ... WITH KEY` para procurar elementos específicos:
```abap
READ TABLE lt_voos INTO DATA(ls) WITH KEY carrid = 'LH'.
IF sy-subrc = 0.
  WRITE: / 'Encontrado', ls-connid.
ENDIF.
```
