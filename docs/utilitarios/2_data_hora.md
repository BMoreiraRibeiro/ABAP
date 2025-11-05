---
tags:
  - ABAP
  - Utilitários
  - DateTime
---

# Data e Hora

## 📋 Visão Geral

Trabalhar com datas e horas em ABAP envolve tipos específicos (`d`, `t`, `timestamp`, `tz`) e utilitários para conversão, formatação e timezone.

---

## Tipos Comuns

- `d` (DATE) — formato interno YYYYMMDD
- `t` (TIME) — formato HHMMSS
- `timestamp` — geralmente `p` ou `string` dependendo da versão; use `cl_abap_tstmp`
- `tzoffset` / timezone handling com classes (ex: `cl_abap_tstmp`, `cl_abap_tstmp_util`)

---

## Conversões

```abap
DATA(lv_date) = sy-datum.    " hoje
DATA(lv_time) = sy-uzeit.    " agora

" String -> Date
DATA(lv_date_from_str) = |20251105|.

" Date -> YYYY-MM-DD
DATA(lv_formatted) = |{ lv_date(4) }-{ lv_date+4(2) }-{ lv_date+6(2) }|.
```

---

## Timezones

Use classes de timestamp para converter:

```abap
DATA(lv_tstmp) = cl_abap_tstmp=>get_current( ).
" Converter para timezone específica
DATA(lv_utc) = cl_abap_tstmp=>to_utc( iv_tstmp = lv_tstmp ).
```

---

## Formatação amigável

- Use templates para formatar
- Use `CONVERT TIME STAMP` para conversões clássicas em sistemas legados

```abap
DATA(lv_t) = sy-uzeit.
DATA(lv_hh) = lv_t(2).
DATA(lv_mm) = lv_t+2(2).
DATA(lv_ss) = lv_t+4(2).

WRITE: / |{ lv_hh }:{ lv_mm }:{ lv_ss }|.
```

---

## Boas Práticas

- Armazene datas no formato nativo (`d`) na base de dados.
- Não confie no relógio do cliente para autorizações/validações críticas; use `sy-datum`/`sy-uzeit` ou services centrais.
- Ao exibir, converta para timezone do usuário.

---

## Próximos Passos

- `3_conversoes.md` — conversões tipo a tipo

**Tags:** `#date` `#time` `#timezone`