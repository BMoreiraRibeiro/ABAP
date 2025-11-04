---
tags:
  - ABAP
  - Performance
  - FOR ALL ENTRIES
  - SQL
---

# FOR ALL ENTRIES - Guia Completo

`FOR ALL ENTRIES` é uma técnica para **evitar SELECT dentro de LOOP**, melhorando drasticamente a performance.

---

## 🔹 Problema: SELECT em LOOP

### ❌ Código Ineficiente

```abap
" Problema: 1000 ordens = 1000 queries à BD! 😱
SELECT * FROM vbak INTO TABLE @DATA(lt_orders) UP TO 1000 ROWS.

DATA lt_items TYPE TABLE OF vbap.

LOOP AT lt_orders INTO DATA(ls_order).
  " Cada iteração faz uma query separada
  SELECT * FROM vbap
    INTO TABLE @DATA(lt_temp)
    WHERE vbeln = @ls_order-vbeln.
    
  APPEND LINES OF lt_temp TO lt_items.
ENDLOOP.

" Total: 1001 acessos à base de dados! 😱
```

**Problema**: Se tiver 1000 encomendas, faz **1000 SELECTs à base de dados**. Muito lento!

---

## 🔹 Solução: FOR ALL ENTRIES

### ✅ Código Otimizado

```abap
" Solução: Apenas 2 queries à BD! ✅
SELECT * FROM vbak INTO TABLE @DATA(lt_orders) UP TO 1000 ROWS.

DATA lt_items TYPE TABLE OF vbap.

" ✅ Validação obrigatória
IF lt_orders IS NOT INITIAL.
  SELECT * FROM vbap
    INTO TABLE @lt_items
    FOR ALL ENTRIES IN @lt_orders
    WHERE vbeln = @lt_orders-vbeln.
ENDIF.

" Total: 2 acessos à base de dados! ✅
```

**Vantagem**: Apenas **2 queries** independentemente do número de encomendas!

---

## 🔹 Como Funciona

O `FOR ALL ENTRIES` converte internamente para algo como:

```sql
SELECT * FROM vbap
WHERE vbeln IN ('0000001', '0000002', '0000003', ..., '0001000')
```

O SAP agrupa os valores e faz a query de forma otimizada.

---

## ⚠️ REGRA DE OURO

!!! danger "SEMPRE VALIDAR"
    **NUNCA** use FOR ALL ENTRIES sem validar se a tabela interna está vazia!

### ❌ ERRADO (sem validação)

```abap
DATA lt_orders TYPE TABLE OF vbak.
" lt_orders está vazia!

" PERIGO: Retorna TODOS os registos da tabela! 😱
SELECT * FROM vbap
  INTO TABLE @DATA(lt_items)
  FOR ALL ENTRIES IN @lt_orders
  WHERE vbeln = @lt_orders-vbeln.

" Resultado: milhões de registos! Sistema pode crashar!
```

### ✅ CORRETO (com validação)

```abap
DATA lt_orders TYPE TABLE OF vbak.

" ✅ Sempre validar
IF lt_orders IS NOT INITIAL.
  SELECT * FROM vbap
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_orders
    WHERE vbeln = @lt_orders-vbeln.
ENDIF.
```

---

## 🔹 Exemplos Práticos

### Exemplo 1: Encomendas e Itens

```abap
*&---------------------------------------------------------------------*
*& Report Z_FAE_ORDERS_ITEMS
*&---------------------------------------------------------------------*
REPORT z_fae_orders_items.

DATA: lt_orders TYPE TABLE OF vbak,
      lt_items  TYPE TABLE OF vbap.

START-OF-SELECTION.

  " 1. Buscar encomendas
  SELECT * FROM vbak
    INTO TABLE @lt_orders
    WHERE erdat >= '20240101'
    UP TO 100 ROWS.

  " 2. Validar antes de FOR ALL ENTRIES
  IF lt_orders IS NOT INITIAL.
    
    " 3. Buscar itens das encomendas
    SELECT * FROM vbap
      INTO TABLE @lt_items
      FOR ALL ENTRIES IN @lt_orders
      WHERE vbeln = @lt_orders-vbeln.
      
    WRITE: / |Encomendas: { lines( lt_orders ) }|.
    WRITE: / |Itens: { lines( lt_items ) }|.
  ELSE.
    WRITE: / 'Nenhuma encomenda encontrada.'.
  ENDIF.
```

### Exemplo 2: Clientes e Encomendas

```abap
" Buscar clientes portugueses
SELECT * FROM kna1
  INTO TABLE @DATA(lt_customers)
  WHERE land1 = 'PT'
  UP TO 50 ROWS.

" Buscar encomendas desses clientes
IF lt_customers IS NOT INITIAL.
  SELECT * FROM vbak
    INTO TABLE @DATA(lt_orders)
    FOR ALL ENTRIES IN @lt_customers
    WHERE kunnr = @lt_customers-kunnr.
ENDIF.
```

### Exemplo 3: Múltiplos Campos

```abap
" Pode usar múltiplos campos na condição
DATA: BEGIN OF ls_key,
        carrid TYPE s_carr_id,
        connid TYPE s_conn_id,
      END OF ls_key.

DATA lt_keys LIKE TABLE OF ls_key.

" Popular tabela de chaves
lt_keys = VALUE #(
  ( carrid = 'LH' connid = '0400' )
  ( carrid = 'LH' connid = '0402' )
  ( carrid = 'AA' connid = '0017' )
).

" FOR ALL ENTRIES com múltiplos campos
IF lt_keys IS NOT INITIAL.
  SELECT * FROM sflight
    INTO TABLE @DATA(lt_flights)
    FOR ALL ENTRIES IN @lt_keys
    WHERE carrid = @lt_keys-carrid
      AND connid = @lt_keys-connid.
ENDIF.
```

---

## 🔹 Limitações e Cuidados

### 1. Duplicados

FOR ALL ENTRIES **remove duplicados** automaticamente.

```abap
DATA lt_ids TYPE TABLE OF vbak-vbeln.

lt_ids = VALUE #( ( '0000001' ) ( '0000001' ) ( '0000002' ) ).

" Mesmo com 2x '0000001', o SELECT faz:
" WHERE vbeln IN ('0000001', '0000002')
IF lt_ids IS NOT INITIAL.
  SELECT * FROM vbap
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_ids
    WHERE vbeln = @lt_ids.
ENDIF.
```

### 2. Limite de Registos

Se a tabela interna for **muito grande** (> 10.000), pode haver problemas de performance.

**Solução**: Processar em lotes (chunks).

```abap
DATA: lv_lines TYPE i,
      lv_start TYPE i VALUE 1,
      lv_end   TYPE i,
      lt_chunk TYPE TABLE OF vbak.

CONSTANTS lc_chunk_size TYPE i VALUE 1000.

lv_lines = lines( lt_all_orders ).

WHILE lv_start <= lv_lines.
  lv_end = lv_start + lc_chunk_size - 1.
  
  " Processar chunk
  CLEAR lt_chunk.
  lt_chunk = lt_all_orders[ lv_start TO lv_end ].
  
  IF lt_chunk IS NOT INITIAL.
    SELECT * FROM vbap
      INTO TABLE @DATA(lt_items_temp)
      FOR ALL ENTRIES IN @lt_chunk
      WHERE vbeln = @lt_chunk-vbeln.
    
    APPEND LINES OF lt_items_temp TO lt_all_items.
  ENDIF.
  
  lv_start = lv_start + lc_chunk_size.
ENDWHILE.
```

### 3. Campos Vazios

Cuidado com campos vazios na tabela interna:

```abap
DATA: BEGIN OF ls_data,
        kunnr TYPE kunnr,
        land1 TYPE land1,
      END OF ls_data.

DATA lt_data LIKE TABLE OF ls_data.

" Se land1 estiver vazio em alguns registos...
lt_data = VALUE #(
  ( kunnr = '0001' land1 = 'PT' )
  ( kunnr = '0002' land1 = '' )    " Vazio!
).

" Pode retornar registos indesejados
IF lt_data IS NOT INITIAL.
  SELECT * FROM kna1
    INTO TABLE @DATA(lt_customers)
    FOR ALL ENTRIES IN @lt_data
    WHERE land1 = @lt_data-land1.  " Inclui vazios!
ENDIF.
```

**Solução**: Filtrar antes do FOR ALL ENTRIES.

```abap
DELETE lt_data WHERE land1 IS INITIAL.

IF lt_data IS NOT INITIAL.
  SELECT * FROM kna1...
ENDIF.
```

---

## 🔹 Alternativa: INNER JOIN

Nem sempre FOR ALL ENTRIES é a melhor opção. Se as tabelas estão relacionadas, use **INNER JOIN**:

```abap
" ❌ FOR ALL ENTRIES desnecessário
SELECT * FROM vbak INTO TABLE @DATA(lt_orders).
IF lt_orders IS NOT INITIAL.
  SELECT * FROM vbap
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_orders
    WHERE vbeln = @lt_orders-vbeln.
ENDIF.

" ✅ MELHOR: INNER JOIN
SELECT h~vbeln,
       h~kunnr,
       i~posnr,
       i~matnr
  FROM vbak AS h
  INNER JOIN vbap AS i ON h~vbeln = i~vbeln
  INTO TABLE @DATA(lt_result).
```

---

## 📊 Quando Usar

| Situação | Solução Recomendada |
|----------|---------------------|
| Tabelas relacionadas (FK) | **INNER JOIN** |
| Tabelas sem relação direta | **FOR ALL ENTRIES** |
| Lógica complexa entre queries | **FOR ALL ENTRIES** |
| Dados já em memória | **FOR ALL ENTRIES** |
| SELECT em LOOP | **SEMPRE EVITAR!** |

---

## 💡 Checklist

Antes de usar FOR ALL ENTRIES:

- [ ] Verificar se tabela interna `IS NOT INITIAL`
- [ ] Considerar INNER JOIN como alternativa
- [ ] Se tabela grande (>10k), usar processamento em chunks
- [ ] Filtrar campos vazios se necessário
- [ ] Testar com diferentes volumes de dados

---

## 🚀 Próximo Passo

Aprenda sobre [Índices de Base de Dados](2_indices.md) para otimizar ainda mais as queries.
