---
tags:
  - ABAP
  - Performance
  - Tabelas Internas
  - HASHED
  - SORTED
---

# Tabelas Internas Eficientes

Escolher o **tipo correto** de tabela interna pode melhorar drasticamente a performance.

---

## 🔹 Tipos de Tabelas Internas

### 1. STANDARD TABLE

Acesso **sequencial** (como um array normal).

```abap
DATA lt_customers TYPE STANDARD TABLE OF kna1.
```

**Características:**
- ✅ APPEND é rápido (adicionar no fim)
- ✅ Boa para LOOP sequencial
- ❌ READ lento (tem que procurar tudo)
- ❌ Permite duplicados

**Quando usar:**
- Processamento sequencial (LOOPs)
- Ordem de inserção importante
- Poucos registos (< 1000)

### 2. SORTED TABLE

Mantém dados **sempre ordenados**.

```abap
DATA lt_customers TYPE SORTED TABLE OF kna1
  WITH UNIQUE KEY kunnr.
```

**Características:**
- ✅ READ rápido (binary search automático)
- ✅ Ordenação mantida
- ❌ APPEND mais lento (tem que ordenar)
- ✅/❌ Pode ter chave UNIQUE ou NON-UNIQUE

**Quando usar:**
- Leituras frequentes (READ)
- Dados já ordenados
- Médio volume (1.000 - 100.000 registos)

### 3. HASHED TABLE

Acesso **por hash** (mais rápido).

```abap
DATA lt_customers TYPE HASHED TABLE OF kna1
  WITH UNIQUE KEY kunnr.
```

**Características:**
- ✅✅ READ **muito rápido** (tempo constante O(1))
- ✅ Única chave obrigatória
- ❌ Não permite LOOP com INDEX
- ❌ Ordem não garantida

**Quando usar:**
- Muitas leituras por chave
- Grande volume (> 10.000 registos)
- Ordem não importa

---

## 🔹 Comparação de Performance

### Cenário: Procurar 1 registo em 100.000

```abap
" Dados de teste
DATA: lt_standard TYPE STANDARD TABLE OF kna1,
      lt_sorted   TYPE SORTED TABLE OF kna1 WITH UNIQUE KEY kunnr,
      lt_hashed   TYPE HASHED TABLE OF kna1 WITH UNIQUE KEY kunnr.

" Popular com 100.000 clientes
DO 100000 TIMES.
  INSERT VALUE #( kunnr = sy-index name1 = |Cliente { sy-index }| ) 
    INTO TABLE lt_standard.
ENDDO.
```

### Performance

| Tipo | Operação | Tempo | Complexidade |
|------|----------|-------|--------------|
| **STANDARD** | READ | 50ms | O(n) - Linear |
| **SORTED** | READ | 0.5ms | O(log n) - Logarítmico |
| **HASHED** | READ | 0.05ms | O(1) - Constante |

**Resultado:**
- HASHED é **1000x mais rápido** que STANDARD! 🚀
- SORTED é **100x mais rápido** que STANDARD

---

## 🔹 Exemplo: STANDARD vs HASHED

### ❌ Ineficiente (STANDARD TABLE)

```abap
DATA: lt_customers TYPE STANDARD TABLE OF kna1,
      ls_customer  TYPE kna1.

" Popular 50.000 clientes
SELECT * FROM kna1 
  INTO TABLE lt_customers
  UP TO 50000 ROWS.

" Procurar 10.000 vezes
DO 10000 TIMES.
  READ TABLE lt_customers INTO ls_customer
    WITH KEY kunnr = '0000012345'.  " Lento! ❌
ENDDO.

" Tempo: ~25 segundos ❌
```

### ✅ Eficiente (HASHED TABLE)

```abap
DATA: lt_customers TYPE HASHED TABLE OF kna1 
        WITH UNIQUE KEY kunnr,
      ls_customer  TYPE kna1.

" Popular 50.000 clientes
SELECT * FROM kna1 
  INTO TABLE lt_customers
  UP TO 50000 ROWS.

" Procurar 10.000 vezes
DO 10000 TIMES.
  READ TABLE lt_customers INTO ls_customer
    WITH TABLE KEY kunnr = '0000012345'.  " Rápido! ✅
ENDDO.

" Tempo: ~0.25 segundos ✅
" Ganho: 100x mais rápido! 🚀
```

---

## 🔹 READ TABLE com BINARY SEARCH

Para **SORTED** tables, usar BINARY SEARCH:

```abap
DATA lt_sorted TYPE SORTED TABLE OF kna1
  WITH NON-UNIQUE KEY kunnr.

" ❌ Linear search (lento)
READ TABLE lt_sorted INTO DATA(ls_cust)
  WITH KEY kunnr = '0001234567'.

" ✅ Binary search (rápido) - automático em SORTED
" (não precisa especificar BINARY SEARCH)
READ TABLE lt_sorted INTO ls_cust
  WITH TABLE KEY kunnr = '0001234567'.
```

Para **STANDARD** tables, adicionar `BINARY SEARCH` manualmente:

```abap
DATA lt_standard TYPE STANDARD TABLE OF kna1.

" IMPORTANTE: Ordenar primeiro!
SORT lt_standard BY kunnr.

" ✅ Binary search manual
READ TABLE lt_standard INTO DATA(ls_cust)
  WITH KEY kunnr = '0001234567'
  BINARY SEARCH.

" ⚠️ Verificar se encontrou
IF sy-subrc = 0.
  " Processamento...
ENDIF.
```

---

## 🔹 LOOP com Performance

### STANDARD TABLE

```abap
" ✅ LOOP normal (eficiente)
LOOP AT lt_standard INTO DATA(ls_data).
  " Processamento...
ENDLOOP.

" ✅ LOOP com WHERE (eficiente em ABAP 7.40+)
LOOP AT lt_standard INTO ls_data WHERE status = 'A'.
  " Processamento...
ENDLOOP.
```

### HASHED TABLE

```abap
" ✅ LOOP sem índice (único modo)
LOOP AT lt_hashed INTO DATA(ls_data).
  " Processamento...
ENDLOOP.

" ❌ ERRO: HASHED não suporta índice
LOOP AT lt_hashed INTO ls_data FROM 1 TO 10.  " ERRO!
ENDLOOP.
```

---

## 🔹 Quando Usar Cada Tipo

### 📊 Matriz de Decisão

```
Volume Pequeno (< 1.000 registos)
├─ Ordem importante? 
│  ├─ Sim → STANDARD
│  └─ Não → STANDARD (simples)

Volume Médio (1.000 - 10.000)
├─ Muitos READs?
│  ├─ Sim → SORTED (com chave)
│  └─ Não → STANDARD

Volume Grande (> 10.000)
├─ Muitos READs por chave?
│  ├─ Sim → HASHED ✅
│  └─ Maioria LOOPs → SORTED
```

### Casos de Uso Comuns

| Cenário | Tipo Recomendado |
|---------|------------------|
| **Cache de validações** | HASHED |
| **Dados de configuração** | HASHED |
| **Resultados de SELECT** | STANDARD/SORTED |
| **Processamento sequencial** | STANDARD |
| **Lookup tables** | HASHED |
| **Dados ordenados** | SORTED |

---

## 🔹 Exemplo Completo: Otimização Real

### Cenário

Validar 100.000 documentos contra tabela de preços (50.000 itens).

### ❌ Versão Lenta

```abap
*&---------------------------------------------------------------------*
*& Report Z_SLOW_VALIDATION
*&---------------------------------------------------------------------*
REPORT z_slow_validation.

" STANDARD table (lento para READ)
DATA: lt_prices TYPE STANDARD TABLE OF zprices,
      lt_docs   TYPE STANDARD TABLE OF zdocs.

START-OF-SELECTION.

  " 1. Carregar preços
  SELECT * FROM zprices INTO TABLE lt_prices.

  " 2. Carregar documentos
  SELECT * FROM zdocs INTO TABLE lt_docs.

  " 3. Validar cada documento ❌
  LOOP AT lt_docs INTO DATA(ls_doc).
    " READ em STANDARD = lento!
    READ TABLE lt_prices INTO DATA(ls_price)
      WITH KEY matnr = ls_doc-matnr.
    
    IF sy-subrc = 0.
      ls_doc-price = ls_price-price.
      MODIFY lt_docs FROM ls_doc.
    ENDIF.
  ENDLOOP.

" Tempo: ~5 minutos ❌
```

### ✅ Versão Rápida

```abap
*&---------------------------------------------------------------------*
*& Report Z_FAST_VALIDATION
*&---------------------------------------------------------------------*
REPORT z_fast_validation.

" ✅ HASHED table para lookups rápidos
DATA: lt_prices TYPE HASHED TABLE OF zprices 
        WITH UNIQUE KEY matnr,
      lt_docs   TYPE STANDARD TABLE OF zdocs.

START-OF-SELECTION.

  " 1. Carregar preços em HASHED
  SELECT * FROM zprices INTO TABLE lt_prices.

  " 2. Carregar documentos
  SELECT * FROM zdocs INTO TABLE lt_docs.

  " 3. Validar cada documento ✅
  LOOP AT lt_docs INTO DATA(ls_doc).
    " READ em HASHED = muito rápido!
    READ TABLE lt_prices INTO DATA(ls_price)
      WITH TABLE KEY matnr = ls_doc-matnr.
    
    IF sy-subrc = 0.
      ls_doc-price = ls_price-price.
      MODIFY lt_docs FROM ls_doc.
    ENDIF.
  ENDLOOP.

" Tempo: ~3 segundos ✅
" Ganho: 100x mais rápido! 🚀
```

---

## 🔹 Operações por Tipo

### APPEND vs INSERT

```abap
" STANDARD: APPEND rápido
APPEND VALUE #( kunnr = '001' name1 = 'Cliente' ) TO lt_standard.

" SORTED: INSERT mantém ordem (mais lento)
INSERT VALUE #( kunnr = '001' name1 = 'Cliente' ) INTO TABLE lt_sorted.

" HASHED: INSERT por hash
INSERT VALUE #( kunnr = '001' name1 = 'Cliente' ) INTO TABLE lt_hashed.
```

### DELETE

```abap
" STANDARD: por índice ou condição
DELETE lt_standard INDEX 1.
DELETE lt_standard WHERE status = 'X'.

" SORTED: por chave ou condição
DELETE TABLE lt_sorted WITH TABLE KEY kunnr = '001'.

" HASHED: só por chave
DELETE TABLE lt_hashed WITH TABLE KEY kunnr = '001'.
```

---

## 🔹 Parallel Processing

Para tabelas muito grandes, processar em paralelo:

```abap
DATA: lt_data TYPE STANDARD TABLE OF zdata,
      lv_package_size TYPE i VALUE 10000.

" Processar em chunks
LOOP AT lt_data INTO DATA(ls_data)
  GROUP BY ( group_id = ( sy-tabix - 1 ) DIV lv_package_size ).
  
  " Processar grupo
  LOOP AT GROUP ls_data INTO DATA(ls_item).
    " Processamento...
  ENDLOOP.
ENDLOOP.
```

---

## 💡 Boas Práticas

| ✅ Fazer | ❌ Evitar |
|---------|-----------|
| HASHED para muitos READs | STANDARD com 100k+ READs |
| SORTED para dados ordenados | SORT repetido em STANDARD |
| WITH TABLE KEY em HASHED | WITH KEY kunnr = (sem TABLE) |
| Definir chave apropriada | Tabelas sem chave |
| BINARY SEARCH em STANDARD ordenado | READ sem BINARY SEARCH |

---

## 🚀 Resumo

- **STANDARD**: Default, bom para LOOPs
- **SORTED**: Dados ordenados, READs médios
- **HASHED**: Muitos READs, melhor performance
- Escolher tipo = **ganho de 100x** em performance! 🚀

---

## 🚀 Próximo Passo

Aprenda sobre [Armadilhas Comuns](6_armadilhas.md) em performance ABAP.
