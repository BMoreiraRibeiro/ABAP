---
tags:
  - ABAP
  - Utilitários
  - Number Ranges
---

# Number Ranges

## 📋 Visão Geral

Number ranges (intervalos numéricos) são usados para documentos e objetos que precisam de IDs únicos e sequenciais. Em SAP, há SNRO (Number Range Objects) e FM/Classes para obter o próximo número.

---

## Usar SNRO (NUMBER_GET_NEXT)

```abap
DATA: lv_number TYPE n LENGTH 10.

CALL FUNCTION 'NUMBER_GET_NEXT'
  EXPORTING
    nr_range_nr = '01'
    object      = 'Z_MY_NUMBER_RANGE'
  IMPORTING
    number      = lv_number
  EXCEPTIONS
    OTHERS      = 1.

IF sy-subrc <> 0.
  " Tratar erro
ENDIF.
```

- `Z_MY_NUMBER_RANGE` precisa ser criado via SNRO (transaction SNRO).
- Transporte e autorização cuidadosos.

---

## Boas Práticas

- Use number ranges para documentos que exigem sequência e controle.
- Proteja concorrência (SNRO lida com lock interno).
- Evite reiniciar ranges sem planeamento.

---

## Exemplos

- NF-e, Pedidos (Sales Order), Faturas, IDs internos

---

## Próximos Passos

- `7_locks.md` — mecanismo de lock

**Tags:** `#number-range` `#snro` `#unique-id`