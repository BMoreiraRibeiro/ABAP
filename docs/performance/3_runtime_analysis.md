---
tags:
  - ABAP
  - Performance
  - SAT
  - Runtime Analysis
  - Profiling
---

# Runtime Analysis (SAT)

A transação **SAT** (antigamente SE30) permite **analisar performance** de programas ABAP em detalhe.

---

## 🔹 O que é o SAT?

O SAT (Runtime Analysis Tools) mede:
- **Tempo de execução** de cada método/função
- **Número de chamadas** a base de dados
- **Tempo gasto** em SQL vs processamento ABAP
- **Hotspots** (pontos mais lentos do código)

---

## 🔹 Como Usar

### Passo 1: Executar SAT

1. Transação **SAT**
2. Inserir nome do programa/transação
3. Clicar em **Execute** (F8)
4. Programa executa e dados são capturados
5. Ver análise automática

### Passo 2: Analisar Resultados

O SAT mostra:
- **Hit List**: Métodos mais lentos
- **Call Hierarchy**: Hierarquia de chamadas
- **Database Accesses**: Queries SQL executadas
- **Memory Usage**: Consumo de memória

---

## 🔹 Exemplo Prático

### Programa com Problema de Performance

```abap
*&---------------------------------------------------------------------*
*& Report Z_SLOW_PROGRAM
*&---------------------------------------------------------------------*
REPORT z_slow_program.

DATA: lt_orders TYPE TABLE OF vbak,
      lt_items  TYPE TABLE OF vbap.

START-OF-SELECTION.

  " 1. Buscar ordens
  SELECT * FROM vbak
    INTO TABLE lt_orders
    WHERE erdat >= '20240101'.

  " ❌ PROBLEMA: SELECT em LOOP
  LOOP AT lt_orders INTO DATA(ls_order).
    SELECT * FROM vbap
      INTO TABLE DATA(lt_temp)
      WHERE vbeln = ls_order-vbeln.
    
    APPEND LINES OF lt_temp TO lt_items.
  ENDLOOP.

  WRITE: / |Ordens: { lines( lt_orders ) }|.
  WRITE: / |Itens: { lines( lt_items ) }|.
```

### Executar no SAT

1. **SAT** → Inserir `Z_SLOW_PROGRAM`
2. **Execute**
3. Programa roda e dados são capturados

### Resultados do SAT

```
════════════════════════════════════════════════════════════
Runtime Analysis - Z_SLOW_PROGRAM
════════════════════════════════════════════════════════════

Total Runtime: 15.234 seconds

Hit List (Top Consumers):
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Rank | Time (s) | %    | Calls | Method/Function
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
1    | 14.500   | 95%  | 1,000 | SELECT FROM VBAP
2    | 0.500    | 3%   | 1     | SELECT FROM VBAK
3    | 0.234    | 2%   | 1,000 | LOOP AT LT_ORDERS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Database Accesses: 1,001 queries
  - VBAK: 1 query (0.5s)
  - VBAP: 1,000 queries (14.5s) ❌ PROBLEMA!

Recommendation:
⚠️ AVOID SELECT IN LOOP - Use FOR ALL ENTRIES or JOIN
```

---

## 🔹 Interpretar Resultados

### 1. Hit List

Mostra onde o tempo é gasto:

```
95% do tempo em SELECT de VBAP → PROBLEMA!
```

### 2. Database Accesses

```
1,000 SELECTs na mesma tabela → SELECT em LOOP ❌
```

### 3. Call Hierarchy

Visualizar hierarquia de chamadas:

```
Z_SLOW_PROGRAM
  └─ START-OF-SELECTION
      ├─ SELECT FROM VBAK (0.5s)
      └─ LOOP AT LT_ORDERS
          └─ SELECT FROM VBAP (14.5s) ❌ HOTSPOT!
```

---

## 🔹 Programa Otimizado

Após identificar o problema, corrigir:

```abap
*&---------------------------------------------------------------------*
*& Report Z_FAST_PROGRAM
*&---------------------------------------------------------------------*
REPORT z_fast_program.

DATA: lt_orders TYPE TABLE OF vbak,
      lt_items  TYPE TABLE OF vbap.

START-OF-SELECTION.

  " 1. Buscar ordens
  SELECT * FROM vbak
    INTO TABLE lt_orders
    WHERE erdat >= '20240101'.

  " ✅ SOLUÇÃO: FOR ALL ENTRIES
  IF lt_orders IS NOT INITIAL.
    SELECT * FROM vbap
      INTO TABLE lt_items
      FOR ALL ENTRIES IN lt_orders
      WHERE vbeln = lt_orders-vbeln.
  ENDIF.

  WRITE: / |Ordens: { lines( lt_orders ) }|.
  WRITE: / |Itens: { lines( lt_items ) }|.
```

### Executar no SAT Novamente

```
════════════════════════════════════════════════════════════
Runtime Analysis - Z_FAST_PROGRAM
════════════════════════════════════════════════════════════

Total Runtime: 0.812 seconds ✅ (era 15.234s)

Hit List:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Rank | Time (s) | %    | Calls | Method/Function
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
1    | 0.500    | 62%  | 1     | SELECT FROM VBAK
2    | 0.300    | 37%  | 1     | SELECT FROM VBAP ✅
3    | 0.012    | 1%   | -     | Other processing
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Database Accesses: 2 queries ✅ (era 1,001)
  - VBAK: 1 query (0.5s)
  - VBAP: 1 query (0.3s) ✅

Performance: EXCELLENT ✅
Improvement: 18.7x faster! 🚀
```

---

## 🔹 Funcionalidades Avançadas

### 1. Filtros

Filtrar análise por:
- **Package**: Apenas código custom
- **Program**: Específico programa
- **Time threshold**: Só mostrar > X ms

### 2. Comparação

Comparar 2 execuções:
1. Antes da otimização
2. Depois da otimização

```
Before: 15.234s
After:  0.812s
Gain:   94.7% ✅
```

### 3. Exportar Resultados

Salvar análise para documentação:
- **Download**: Excel, XML
- **Share**: Enviar para colega

---

## 🔹 Tipos de Análise

### Performance Trace

Medir tempo de execução geral.

```
SAT → Execution → Performance Trace
```

### Memory Trace

Analisar consumo de memória.

```
SAT → Execution → Memory Trace
```

### SQL Trace

Focar apenas em queries SQL (similar a ST05).

```
SAT → Execution → SQL Trace
```

---

## 🔹 Exemplo: Otimizar Relatório

### Cenário Real

Relatório demora **30 minutos** para executar.

### Passo 1: SAT

Executar SAT no relatório:

```
Top Consumers:
1. SELECT FROM BKPF (18 min) ❌
2. LOOP com cálculos (8 min)
3. SELECT FROM BSEG (4 min)
```

### Passo 2: Análise

**Problema 1**: SELECT sem WHERE efetivo
```abap
" ❌ Lê milhões de registos
SELECT * FROM bkpf
  INTO TABLE lt_bkpf
  WHERE bukrs = '1000'.  " Não selectivo!
```

**Solução**: Adicionar filtros
```abap
" ✅ Filtro mais selectivo
SELECT * FROM bkpf
  INTO TABLE lt_bkpf
  WHERE bukrs = '1000'
    AND gjahr = '2025'       " Ano
    AND monat IN ('01', '02', '03')  " Trimestre
    AND budat >= '20250101'.  " Data
```

**Problema 2**: Cálculos em LOOP sem necessidade
```abap
" ❌ Cálculo repetido
LOOP AT lt_data INTO DATA(ls_data).
  ls_data-total = ls_data-qty * ls_data-price * ls_data-tax.
  MODIFY lt_data FROM ls_data.
ENDLOOP.
```

**Solução**: Calcular só quando necessário
```abap
" ✅ Calcular sob demanda ou usar SELECT com expressões
SELECT menge,
       preis,
       steuer,
       menge * preis * steuer AS total
  FROM ztabela...
```

### Resultado

```
Before: 30 minutes
After:  2 minutes
Gain:   93% ✅
```

---

## 🔹 Métricas Importantes

### Tempo de Execução

```
< 1 segundo:    ✅ Excelente
1-5 segundos:   ✅ Bom
5-30 segundos:  ⚠️ Aceitável
> 30 segundos:  ❌ Otimizar!
```

### Database Calls

```
< 10 queries:   ✅ Excelente
10-50 queries:  ✅ Bom
50-100 queries: ⚠️ Revisar
> 100 queries:  ❌ Problema grave!
```

### DB Time vs ABAP Time

```
DB Time > 80%:  ❌ Otimizar SQL
ABAP Time > 80%: ❌ Otimizar código
50/50:          ✅ Balanceado
```

---

## 🔹 Checklist SAT

Antes de liberar código em produção:

- [ ] Executar SAT com volume realista de dados
- [ ] Verificar Hit List (nada > 50% do tempo)
- [ ] Contar database calls (< 50)
- [ ] Verificar se há SELECT em LOOP
- [ ] Medir tempo total (< 5s para jobs online)
- [ ] Comparar com versão anterior (se aplicável)
- [ ] Documentar melhorias

---

## 💡 Dicas

| ✅ Fazer | ❌ Evitar |
|---------|-----------|
| Executar SAT antes de liberar | Assumir que "parece rápido" |
| Testar com volume real | Testar só com 10 registos |
| Focar nos top 3 consumidores | Tentar otimizar tudo |
| Documentar melhorias | Não medir ganhos |
| Usar filtros para isolar código | Analisar código SAP standard |

---

## 🚀 Próximo Passo

Aprenda sobre [Table Buffering](4_buffering.md) para melhorar ainda mais a performance.
