---
tags:
  - ABAP
  - Performance
  - Armadilhas
  - Anti-Patterns
  - Boas Práticas
---

# Armadilhas Comuns de Performance

Conheça os **erros mais comuns** que destroem a performance em ABAP e como evitá-los.

---

## 🔹 1. SELECT em LOOP

### ❌ O Pior Erro de Performance

```abap
" ❌❌❌ NUNCA FAÇA ISTO! ❌❌❌
SELECT * FROM vbak INTO TABLE @DATA(lt_orders) UP TO 1000 ROWS.

LOOP AT lt_orders INTO DATA(ls_order).
  " 1000 queries à base de dados! 😱
  SELECT * FROM vbap
    INTO TABLE @DATA(lt_items)
    WHERE vbeln = @ls_order-vbeln.
  
  " Processamento...
ENDLOOP.

" Resultado: 1001 acessos à BD
" Tempo: 15 segundos ❌
```

### ✅ Solução 1: FOR ALL ENTRIES

```abap
SELECT * FROM vbak INTO TABLE @DATA(lt_orders) UP TO 1000 ROWS.

" ✅ Uma única query
IF lt_orders IS NOT INITIAL.
  SELECT * FROM vbap
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_orders
    WHERE vbeln = @lt_orders-vbeln.
ENDIF.

" Resultado: 2 acessos à BD
" Tempo: 0.5 segundos ✅
" Ganho: 30x mais rápido!
```

### ✅ Solução 2: INNER JOIN

```abap
" ✅ Ainda melhor: 1 única query
SELECT h~vbeln,
       h~kunnr,
       i~posnr,
       i~matnr
  FROM vbak AS h
  INNER JOIN vbap AS i ON h~vbeln = i~vbeln
  INTO TABLE @DATA(lt_result).

" Resultado: 1 acesso à BD
" Tempo: 0.3 segundos ✅
```

---

## 🔹 2. SELECT * (Buscar Todos os Campos)

### ❌ Desperdício de Recursos

```abap
" ❌ Busca 150 campos mas usa só 3
SELECT * FROM vbak
  INTO TABLE @DATA(lt_orders).

LOOP AT lt_orders INTO DATA(ls_order).
  WRITE: / ls_order-vbeln, ls_order-kunnr.  " Só usa 2 campos!
ENDLOOP.
```

### ✅ Especificar Campos Necessários

```abap
" ✅ Busca apenas o necessário
SELECT vbeln, kunnr, erdat
  FROM vbak
  INTO TABLE @DATA(lt_orders).

" Benefícios:
" - Menos dados transferidos
" - Menos memória usada
" - Mais rápido
```

**Impacto:**
- SELECT * com 150 campos: **500KB** de dados
- SELECT 3 campos: **50KB** de dados
- **Ganho: 10x menos dados transferidos!**

---

## 🔹 3. FOR ALL ENTRIES Sem Validação

### ❌ Bug Perigoso

```abap
DATA lt_customers TYPE TABLE OF kna1.
" lt_customers está VAZIA!

" ❌ PERIGO: Retorna TODOS os registos da tabela!
SELECT * FROM vbak
  INTO TABLE @DATA(lt_orders)
  FOR ALL ENTRIES IN @lt_customers
  WHERE kunnr = @lt_customers-kunnr.

" Resultado: Milhões de registos! Sistema pode crashar! 😱
```

### ✅ SEMPRE Validar

```abap
DATA lt_customers TYPE TABLE OF kna1.

" ✅ Validação obrigatória
IF lt_customers IS NOT INITIAL.
  SELECT * FROM vbak
    INTO TABLE @DATA(lt_orders)
    FOR ALL ENTRIES IN @lt_customers
    WHERE kunnr = @lt_customers-kunnr.
ELSE.
  " Tabela vazia = nenhum resultado
  CLEAR lt_orders.
ENDIF.
```

---

## 🔹 4. NESTED LOOPs sem Otimização

### ❌ Complexidade O(n²)

```abap
" ❌ 1000 x 10000 = 10.000.000 iterações!
LOOP AT lt_orders INTO DATA(ls_order).      " 1.000 registos
  LOOP AT lt_items INTO DATA(ls_item).      " 10.000 registos
    IF ls_item-vbeln = ls_order-vbeln.
      " Processamento...
    ENDIF.
  ENDLOOP.
ENDLOOP.

" Tempo: 2 minutos ❌
```

### ✅ Usar HASHED Table

```abap
" ✅ Converter para HASHED
DATA lt_items_hash TYPE HASHED TABLE OF vbap
  WITH UNIQUE KEY vbeln posnr.

lt_items_hash = lt_items.

" ✅ Apenas 1.000 iterações + 1.000 READs rápidos
LOOP AT lt_orders INTO DATA(ls_order).
  LOOP AT lt_items_hash INTO DATA(ls_item)
    WHERE vbeln = ls_order-vbeln.
    " Processamento...
  ENDLOOP.
ENDLOOP.

" Tempo: 2 segundos ✅
" Ganho: 60x mais rápido!
```

---

## 🔹 5. Modificar Tabela Durante LOOP

### ❌ Comportamento Imprevisível

```abap
" ❌ Perigoso: modifica tabela durante iteração
LOOP AT lt_data INTO DATA(ls_data).
  IF ls_data-status = 'X'.
    DELETE lt_data INDEX sy-tabix.  " ❌ Pode saltar registos
  ENDIF.
ENDLOOP.
```

### ✅ Marcar e Deletar Depois

```abap
" ✅ Opção 1: Usar WHERE na loop
LOOP AT lt_data INTO DATA(ls_data) WHERE status <> 'X'.
  " Processar apenas registos válidos
ENDLOOP.

" ✅ Opção 2: DELETE com WHERE
DELETE lt_data WHERE status = 'X'.

" ✅ Opção 3: Nova tabela
DATA lt_filtered LIKE lt_data.
LOOP AT lt_data INTO DATA(ls_data) WHERE status <> 'X'.
  APPEND ls_data TO lt_filtered.
ENDLOOP.
lt_data = lt_filtered.
```

---

## 🔹 6. APPEND em LOOP Grande

### ❌ Realocação Constante

```abap
DATA lt_result TYPE TABLE OF ty_data.

" ❌ Cada APPEND pode realocar memória
DO 100000 TIMES.
  APPEND VALUE #( id = sy-index value = 'Test' ) TO lt_result.
ENDDO.

" Tempo: 5 segundos ❌
```

### ✅ VALUE com Lista

```abap
" ✅ Alocação única
DATA(lt_result) = VALUE ty_t_data(
  FOR i = 1 WHILE i <= 100000
  ( id = i value = 'Test' )
).

" Tempo: 0.5 segundos ✅
```

### ✅ INSERT LINES OF

```abap
" ✅ Para adicionar blocos
DATA lt_result TYPE TABLE OF ty_data.
DATA lt_chunk TYPE TABLE OF ty_data.

DO 10 TIMES.
  " Criar chunk de 10.000
  lt_chunk = VALUE #( FOR i = 1 WHILE i <= 10000 ( id = i ) ).
  
  " Adicionar chunk de uma vez
  INSERT LINES OF lt_chunk INTO TABLE lt_result.
ENDDO.
```

---

## 🔹 7. Converter Tipos Repetidamente

### ❌ Conversão em LOOP

```abap
" ❌ Converte 10.000 vezes
LOOP AT lt_data INTO DATA(ls_data).
  DATA(lv_string) = |{ ls_data-amount }|.  " Conversão repetida
  " Uso de lv_string...
ENDLOOP.
```

### ✅ Converter Uma Vez

```abap
" ✅ Adicionar campo calculado antes do LOOP
LOOP AT lt_data INTO DATA(ls_data).
  ls_data-amount_string = |{ ls_data-amount }|.
  MODIFY lt_data FROM ls_data.
ENDLOOP.

" Agora usar ls_data-amount_string sem conversão
```

---

## 🔹 8. SELECT SINGLE em LOOP

### ❌ Múltiplos Acessos

```abap
" ❌ 1.000 SELECTs
LOOP AT lt_materials INTO DATA(ls_mat).
  SELECT SINGLE maktx FROM makt
    INTO @DATA(lv_text)
    WHERE matnr = @ls_mat-matnr
      AND spras = @sy-langu.
ENDLOOP.
```

### ✅ Um SELECT com FOR ALL ENTRIES

```abap
" ✅ 1 SELECT apenas
IF lt_materials IS NOT INITIAL.
  SELECT matnr, maktx FROM makt
    INTO TABLE @DATA(lt_texts)
    FOR ALL ENTRIES IN @lt_materials
    WHERE matnr = @lt_materials-matnr
      AND spras = @sy-langu.
ENDIF.

" Converter para HASHED para lookups rápidos
DATA lt_texts_hash TYPE HASHED TABLE OF makt
  WITH UNIQUE KEY matnr.
lt_texts_hash = lt_texts.

" Usar no LOOP
LOOP AT lt_materials INTO DATA(ls_mat).
  READ TABLE lt_texts_hash INTO DATA(ls_text)
    WITH TABLE KEY matnr = ls_mat-matnr.
  " Usar ls_text-maktx
ENDLOOP.
```

---

## 🔹 9. Não Usar UP TO n ROWS

### ❌ Buscar Tudo Desnecessariamente

```abap
" ❌ Busca 10 milhões de registos mas usa só 100
SELECT * FROM bseg
  INTO TABLE @DATA(lt_items).

" Processar só 100
LOOP AT lt_items INTO DATA(ls_item) TO 100.
  " Processamento...
ENDLOOP.
```

### ✅ Limitar na Query

```abap
" ✅ Busca apenas o necessário
SELECT * FROM bseg
  INTO TABLE @DATA(lt_items)
  UP TO 100 ROWS.

LOOP AT lt_items INTO DATA(ls_item).
  " Processamento...
ENDLOOP.
```

---

## 🔹 10. String Concatenation Ineficiente

### ❌ Concatenação em LOOP

```abap
DATA lv_result TYPE string.

" ❌ Cada concatenação cria nova string
LOOP AT lt_data INTO DATA(ls_data).
  lv_result = lv_result && ls_data-text && cl_abap_char_utilities=>newline.
ENDLOOP.

" Com 10.000 registos = muito lento!
```

### ✅ Usar CONCATENATE LINES OF

```abap
" ✅ Método otimizado
DATA(lv_result) = REDUCE string(
  INIT result = ``
  FOR <line> IN lt_data
  NEXT result = result && <line>-text && cl_abap_char_utilities=>newline
).

" Ou simplesmente:
DATA lt_strings TYPE TABLE OF string.
LOOP AT lt_data INTO DATA(ls_data).
  APPEND ls_data-text TO lt_strings.
ENDLOOP.

CONCATENATE LINES OF lt_strings INTO lv_result
  SEPARATED BY cl_abap_char_utilities=>newline.
```

---

## 📊 Checklist de Performance

Antes de liberar código:

- [ ] ❌ SELECT em LOOP → ✅ FOR ALL ENTRIES ou JOIN
- [ ] ❌ SELECT * → ✅ Especificar campos
- [ ] ❌ FOR ALL ENTRIES sem validação → ✅ IF IS NOT INITIAL
- [ ] ❌ NESTED LOOPS → ✅ HASHED tables
- [ ] ❌ Modificar tabela em LOOP → ✅ WHERE ou nova tabela
- [ ] ❌ APPEND em loops grandes → ✅ VALUE ou INSERT LINES
- [ ] ❌ SELECT SINGLE em LOOP → ✅ Um SELECT + HASHED
- [ ] ❌ Sem UP TO ROWS → ✅ Limitar resultados
- [ ] ❌ Conversões repetidas → ✅ Calcular uma vez
- [ ] ❌ String concatenation → ✅ CONCATENATE LINES OF

---

## 💡 Regra de Ouro

> **"Se faz mais de 100 acessos à BD, está ERRADO!"**

**Metas:**
- Online programs: < 10 DB accesses
- Background jobs: < 100 DB accesses
- Reports: Depende do volume, mas sempre otimizar

---

## 🚀 Ferramentas de Validação

1. **SAT (Runtime Analysis)**: Medir performance
2. **ST05 (SQL Trace)**: Ver queries SQL
3. **Code Inspector (SCI)**: Detetar anti-patterns
4. **ST22**: Analisar dumps
5. **ST02**: Monitorizar buffer

---

## 🎯 Resumo

| Armadilha | Impacto | Solução |
|-----------|---------|---------|
| SELECT em LOOP | 😱😱😱 Crítico | FOR ALL ENTRIES |
| SELECT * | 😱😱 Alto | Especificar campos |
| FAE sem validação | 😱😱😱 Crítico | IF IS NOT INITIAL |
| NESTED LOOPS | 😱😱 Alto | HASHED tables |
| Modificar em LOOP | 😱 Médio | WHERE ou nova tabela |

---

## 🚀 Próximo Passo

Reveja todos os tópicos de performance e aplique no seu código:
1. [FOR ALL ENTRIES](1_for_all_entries.md)
2. [Índices](2_indices.md)
3. [Runtime Analysis](3_runtime_analysis.md)
4. [Buffering](4_buffering.md)
5. [Tabelas Internas](5_tabelas_internas.md)
