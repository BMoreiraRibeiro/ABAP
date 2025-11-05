---
tags:
  - ABAP
  - Utilitários
  - Locks
---

# Locks (Enqueue / Dequeue)

## 📋 Visão Geral

Locks previnem atualizações concorrentes de dados críticos. Em ABAP usamos `ENQUEUE_<OBJ>` e `DEQUEUE_<OBJ>` (ou `ENQUEUE`/`DEQUEUE`).

---

## Lock Object

- Definido em `SE11` → Lock Objects
- Campos chave: ex: `ZLOCK_OBJ` com `MANDT`, `KUNNR`

### Exemplo de uso

```abap
DATA: lv_kunnr TYPE kunnr VALUE '1000'.

CALL FUNCTION 'ENQUEUE_EZLOCK'
  EXPORTING
    mode_zlock = 'S'
    kunnr      = lv_kunnr
  EXCEPTIONS
    foreign_lock = 1
    system_failure = 2
    OTHERS = 3.

IF sy-subrc <> 0.
  " Tratar lock não obtido
ENDIF.

" Fazer atualização segura

CALL FUNCTION 'DEQUEUE_EZLOCK'
  EXPORTING
    kunnr = lv_kunnr.
```

---

## ENQUEUE/DEQUEUE Moderno

```abap
ENQUEUE zlock_object ID lv_key.
" ...
DEQUEUE zlock_object ID lv_key.
```

---

## Tipos de Lock

- Shared (S) — leitura coordenada
- Exclusive (E) — escrita exclusiva

---

## Boas Práticas

- Mantenha o bloco de código lock o mais curto possível.
- Sempre liberar locks (`DEQUEUE`) em todas as rotas de saída (TRY/CATCH/FINAL).
- Use lock objects definidos em SE11, não ad-hoc.

---

## Próximos Passos

- Revisar `security` e `testes` para integrações com locks e number ranges.

**Tags:** `#locks` `#enqueue` `#concurrency`