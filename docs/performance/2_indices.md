---
tags:
  - ABAP
  - Performance
  - Database
  - Índices
---

# Índices de Base de Dados

Os **índices** são estruturas que aceleram consultas SQL, mas devem ser usados com cuidado.

---

## 🔹 O que é um Índice?

Um índice é como um **índice de livro** - permite encontrar dados rapidamente sem ler tudo.

**Analogia:**
- Sem índice: Ler todas as 1000 páginas para encontrar "ABAP"
- Com índice: Ir direto à página 234

---

## 🔹 Tipos de Índices no SAP

### 1. Índice Primário (Primary Index)

Criado automaticamente na **chave primária** da tabela.

```abap
" Tabela SCARR tem chave primária: CARRID
" SELECT usando índice primário (RÁPIDO ✅)
SELECT SINGLE * FROM scarr
  INTO @DATA(ls_carrier)
  WHERE carrid = 'LH'.  " Usa índice primário
```

### 2. Índice Secundário (Secondary Index)

Criado manualmente em campos frequentemente usados em WHERE.

```abap
" Se existir índice secundário em ERDAT
" SELECT usando índice secundário (RÁPIDO ✅)
SELECT * FROM vbak
  INTO TABLE @DATA(lt_orders)
  WHERE erdat = '20250101'.  " Usa índice em ERDAT
```

---

## 🔹 Como Verificar Índices

### SE11 - Dicionário ABAP

1. Transação **SE11**
2. Inserir nome da tabela (ex: `VBAK`)
3. Clicar em **Display**
4. Ver aba **Indexes**

### SE16 - Visualização de Dados

Ao executar SELECT, ver campo "Index used" na parte inferior.

---

## 🔹 Boas Práticas

### ✅ Usar Campos Indexados no WHERE

```abap
" ✅ BOM - usa índice primário
SELECT * FROM vbak
  INTO TABLE @DATA(lt_orders)
  WHERE vbeln = '0000001234'.

" ✅ BOM - usa índice secundário (se existir em ERDAT)
SELECT * FROM vbak
  INTO TABLE @lt_orders
  WHERE erdat >= '20250101'.

" ❌ MAU - campo não indexado
SELECT * FROM vbak
  INTO TABLE @lt_orders
  WHERE bstnk = 'PO12345'.  " Full table scan!
```

### ✅ Ordem Correta dos Campos

Para índices compostos, usar campos **na ordem do índice**:

```abap
" Índice composto: MANDT + ERDAT + AUART
" ✅ BOM - segue ordem do índice
SELECT * FROM vbak
  WHERE mandt = sy-mandt
    AND erdat = '20250101'
    AND auart = 'OR'.

" ❌ MENOS EFICIENTE - ordem incorreta
SELECT * FROM vbak
  WHERE auart = 'OR'      " Campo 3 do índice
    AND erdat = '20250101'  " Campo 2
    AND mandt = sy-mandt.   " Campo 1
```

---

## 🔹 Quando Criar Índices

### ✅ Criar Índice Quando:

1. **Campo usado frequentemente em WHERE**
2. **Tabela grande** (> 10.000 registos)
3. **Performance ruim** confirmada (via ST05)
4. **Seletividade alta** (campo tem muitos valores únicos)

### ❌ NÃO Criar Índice Quando:

1. Tabela pequena (< 1.000 registos)
2. Campo raramente usado
3. Campo com poucos valores (ex: STATUS com 3 valores)
4. Tabela com muitos INSERTs/UPDATEs (índices atrasam escrita)

---

## 🔹 Exemplo: Análise de Performance

### Cenário

Tabela `ZTRANSACTIONS` com 1.000.000 registos.

```abap
" ❌ LENTO - sem índice em CUSTOMER_ID
SELECT * FROM ztransactions
  INTO TABLE @DATA(lt_trans)
  WHERE customer_id = '12345'.

" Tempo: 15 segundos 😱 (full table scan)
```

### Solução: Criar Índice

**SE11 → ZTRANSACTIONS → Indexes → Create**

```
Index Name: Z01
Fields: CUSTOMER_ID, TRANS_DATE
```

```abap
" ✅ RÁPIDO - com índice
SELECT * FROM ztransactions
  INTO TABLE @DATA(lt_trans)
  WHERE customer_id = '12345'
    AND trans_date >= '20250101'.

" Tempo: 0.2 segundos ✅ (usando índice Z01)
```

---

## 🔹 Monitorizar Uso de Índices

### ST05 - SQL Trace

1. Executar **ST05**
2. Ativar trace SQL
3. Executar programa
4. Desativar trace
5. Analisar resultados

**Ver:**
- Qual índice foi usado
- Tempo de execução
- Número de registos lidos

### Exemplo de Análise

```
Table: VBAK
Accessed: 1.000.000 rows
Index used: None (Table Scan) ❌
Duration: 12.3 seconds

Recommendation: Create index on ERDAT
```

Após criar índice:

```
Table: VBAK
Accessed: 150 rows
Index used: Z01 (ERDAT) ✅
Duration: 0.05 seconds
```

---

## 🔹 Índices em Tabelas Standard SAP

!!! warning "Cuidado"
    **NUNCA modifique tabelas standard SAP!** Use índices secundários.

### Ver Índices Existentes

```abap
" Tabela VBAK tem vários índices secundários
" Ver em SE11 → VBAK → Indexes
```

Índices comuns em VBAK:
- `0` - Primário (VBELN)
- `1` - ERDAT, ERZET
- `2` - AUART, ERDAT
- `3` - KUNNR

---

## 🔹 Database Hints (Avançado)

Forçar uso de índice específico:

```abap
" Forçar uso do índice Z01
SELECT * FROM ztransactions
  INTO TABLE @DATA(lt_trans)
  %_HINTS ORACLE 'INDEX("ZTRANSACTIONS" "Z01")'
  WHERE customer_id = '12345'.
```

!!! danger "Atenção"
    Só use hints se souber o que está a fazer. Pode piorar performance!

---

## 🔹 Índices e FOR ALL ENTRIES

FOR ALL ENTRIES **não usa índices da mesma forma**:

```abap
DATA lt_customers TYPE TABLE OF kna1-kunnr.

" Popular com 1000 clientes
lt_customers = VALUE #( ( '0001' ) ( '0002' ) ... ( '1000' ) ).

" Converte internamente para:
" WHERE kunnr IN ('0001', '0002', ..., '1000')
IF lt_customers IS NOT INITIAL.
  SELECT * FROM vbak
    INTO TABLE @DATA(lt_orders)
    FOR ALL ENTRIES IN @lt_customers
    WHERE kunnr = @lt_customers-kunnr.
ENDIF.
```

Mesmo com índice em KUNNR, pode ser lento se lista for muito grande (>5000).

---

## 🔹 Exemplo Completo

```abap
*&---------------------------------------------------------------------*
*& Report Z_INDEX_DEMO
*&---------------------------------------------------------------------*
REPORT z_index_demo.

PARAMETERS: p_kunnr TYPE kunnr OBLIGATORY.

DATA: lt_orders TYPE TABLE OF vbak,
      lv_start  TYPE timestampl,
      lv_end    TYPE timestampl,
      lv_diff   TYPE int8.

START-OF-SELECTION.

  " Capturar tempo inicial
  GET TIME STAMP FIELD lv_start.

  " ✅ Query otimizada (usa índice em KUNNR - índice 3)
  SELECT * FROM vbak
    INTO TABLE @lt_orders
    WHERE kunnr = @p_kunnr
      AND erdat >= '20240101'
    UP TO 1000 ROWS.

  " Capturar tempo final
  GET TIME STAMP FIELD lv_end.

  " Calcular diferença em milissegundos
  lv_diff = cl_abap_tstmp=>subtract(
    tstmp1 = lv_end
    tstmp2 = lv_start
  ).

  " Mostrar resultados
  WRITE: / |Registos encontrados: { lines( lt_orders ) }|.
  WRITE: / |Tempo de execução: { lv_diff } microssegundos|.
  
  IF lv_diff > 1000000.  " > 1 segundo
    WRITE: / '⚠️ Performance ruim! Considerar otimização.'.
  ELSE.
    WRITE: / '✅ Performance boa!'.
  ENDIF.
```

---

## 💡 Checklist de Otimização

- [ ] Verificar índices existentes (SE11)
- [ ] Usar campos indexados no WHERE
- [ ] Seguir ordem dos campos do índice
- [ ] Testar com ST05 (SQL Trace)
- [ ] Considerar criar índice secundário se necessário
- [ ] Validar seletividade do índice
- [ ] Medir tempo antes e depois

---

## 🚀 Próximo Passo

Aprenda a usar [Runtime Analysis (SAT)](3_runtime_analysis.md) para análise profunda de performance.
