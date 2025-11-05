---
tags:
  - ABAP
  - Utilitários
  - Strings
---

# Funções de String

## 📋 Visão Geral

Strings são onipresentes em ABAP: nomes, IDs, descrições, mensagens. Esta página reúne as funções e técnicas mais úteis para manipular texto com segurança e desempenho.

---

## Operações Básicas

- CONCATENAR: `CONCATENATE` ou templates
- DIVIDIR: `SPLIT`
- SUBSTRING: `+`/`( )` ou `CONDENSE`/`SHIFT`
- REPLACE: `REPLACE` ou `REPLACE ALL OCCURRENCES OF`
- COMPARAR: `=` sensível, `=`/`NE` com `TO_LOWER`

### Exemplos

```abap
DATA(lv1) = 'ABAP'.
DATA(lv2) = 'Rocks'.

" Template (moderno)
DATA(lv_concat) = |{ lv1 } { lv2 }|.

" CONCATENATE (legacy)
CONCATENATE lv1 lv2 INTO DATA(lv_cat2) SEPARATED BY ' '.

" SPLIT
SPLIT lv_concat AT ' ' INTO TABLE DATA(lt_parts).
```

---

## Funções Úteis (ABAP 7.40+)

- `CONDENSE` remove espaços extras
- `TRANSLATE` para upper/lower
- `FIND` para procurar substrings
- `REPLACE` para substituir
- `REGEX` com `FIND REGEX` e `REPLACE REGEX`

```abap
" Remover espaços
CONDENSE lv_concat.

" Procurar
FIND 'Rocks' IN lv_concat.
IF sy-subrc = 0.
  " achou
ENDIF.

" Regex
FIND REGEX 'ABAP\s+Rocks' IN lv_concat.
```

---

## Strings e Performance

- Evite concatenações repetidas em loops (use `STRING( )` temporário ou `cl_abap_string_utils` se disponível).
- Prefira templates (`|{ var }|`) em vez de `CONCATENATE` para clareza.

---

## Internacionalização e Encoding

- Use tipos `string` para texto Unicode.
- Para manipular bytes use `xstring`.
- Ao trabalhar com arquivos, converta entre `string` e `xstring` conforme necessário.

---

## Boas Práticas

- Valide tamanho antes de gravar em campos de base de dados (ex: `CHAR10`).
- Use `REPLACE ALL OCCURRENCES OF` para substituições massivas.
- Para parsing complexo, prefira `FIND REGEX` e grupos de captura.

---

## Próximos passos

- `2_data_hora.md` — formatação de datas
- `3_conversoes.md` — conversões entre tipos

**Tags:** `#strings` `#text-processing` `#regex`