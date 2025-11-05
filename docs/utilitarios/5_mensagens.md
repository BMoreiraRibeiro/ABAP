---
tags:
  - ABAP
  - Utilitários
  - Mensagens
---

# Mensagens e Logging

## 📋 Visão Geral

Mensagens em ABAP fornecem feedback ao utilizador (`S`, `I`, `W`, `E`, `A`) e há frameworks para log e auditoria (`BAL`, `SLG1`).

---

## MESSAGE

- Sintaxe básica: `MESSAGE 'texto' TYPE 'I'.`
- Tipos: `S` (sucesso), `I` (info), `W` (warning), `E` (erro), `A` (abend)

```abap
IF sy-subrc <> 0.
  MESSAGE 'Operação falhou' TYPE 'E'.
ENDIF.
```

---

## Classes de Mensagem

- Use mensagems classes (`SE91`) para textos reutilizáveis e tradução.

```abap
MESSAGE e001(zmy_class) WITH lv_param.
```

---

## Logging e Auditoria

- `BAL` / Application Log para operações críticas
- `SLG1` para visualizar logs

```abap
CALL FUNCTION 'BAL_LOG_MSG_ADD'
  EXPORTING
    i_s_msg = VALUE #( msgty = 'I' msgv1 = |Processo OK| ).
```

---

## Exceções vs Mensagens

- Use exceções (classes) para flows programáticos e `MESSAGE` para UX.
- Em APIs internas, preferir exceções que callers possam tratar.

---

## Boas Práticas

- Não expor dados sensíveis em mensagens
- Use classes de mensagem para i18n
- Logue operações críticas em `BAL` com contexto

---

## Próximos Passos

- `6_number_ranges.md` — gerar números sequenciais

**Tags:** `#message` `#logging` `#bal`