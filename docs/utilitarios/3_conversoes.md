---
tags:
  - ABAP
  - Utilitários
  - Conversions
---

# Conversões de Tipos

## 📋 Visão Geral

Conversões entre tipos são comuns: números para texto, texto para números, cast entre referências e estruturas. Aqui estão as ferramentas e armadilhas.

---

## CONV e CAST

- `CONV` para conversões explícitas de valor
- `CAST` para referências e objetos

```abap
DATA(lv_num_str) = CONV string( 123 ).
DATA(lv_int) = CONV i( '00123' ).

" CAST
DATA(lo_obj) = CAST ref TO zcl_my_class( lv_ref ).
```

---

## Conversões de números/decimais

- Use `DECIMAL_SHIFT` e tipos `p` para cálculos financeiros
- Evite conversões implícitas que silenciosamente truncam

```abap
DATA(lv_amount) = '100,50'.
REPLACE ALL OCCURRENCES OF ',' IN lv_amount WITH '.'.
DATA(lv_dec) = CONV p( lv_amount ).
```

---

## Strings -> Estruturas

- Use `SPLIT` + `MOVE-CORRESPONDING` ou `cl_abap_structdescr`
- Para CSV, prefira `cl_abap_structdescr` ou parsing dedicado

---

## JSON / XML

- JSON: `cl_sxml_string_writer` / `cl_trex_json` (dependendo da versão)
- XML: `CALL TRANSFORMATION` ou `cl_xml_document`

Exemplo simples JSON → estrutura:

```abap
DATA: lv_json TYPE string.
CALL TRANSFORMATION id
  SOURCE XML lv_json
  RESULT my_struct = DATA(ls_struct).
```

---

## Boas Práticas

- Valide antes de converter (ex: `IS NUMERIC`)
- Use `CONV` explicitamente para mostrar intenção
- Evite depender de locais (comma vs dot) sem normalizar

---

## Próximos Passos

- `4_file_handling.md` — ficheiros no app/presentation server

**Tags:** `#conversoes` `#casting` `#json`