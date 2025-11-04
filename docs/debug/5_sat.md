---
tags:
  - ABAP
  - Performance
  - SAT
  - Runtime Analysis
  - Otimização
---

# Runtime Analysis (SAT / SE30)

## 📋 Visão Geral

**SAT** (ou SE30 em sistemas antigos) é a ferramenta de análise de performance em runtime. Permite medir o tempo de execução de programas e identificar gargalos de performance.

---

## 🎯 Para que Serve

- Medir tempo de execução do programa
- Identificar código lento
- Analisar queries SQL
- Ver tempo gasto por método/função
- Encontrar gargalos de performance
- Comparar antes/depois de otimizações

---

## 🔹 Iniciar Análise

### Transaction Code
```
SAT   (sistemas novos)
SE30  (sistemas antigos)
```

### Passos Básicos

1. **SAT** → Nova medição
2. Inserir **nome do programa**
3. Pressionar **Execute**
4. Programa executa e é medido
5. Ver **resultados da análise**

---

## 🔹 Tipos de Medição

### 1️⃣ Medição de Programa

```abap
REPORT z_programa_teste.

START-OF-SELECTION.
  " Todo este código será medido
  SELECT * FROM sflight INTO TABLE @DATA(lt_voos).
  
  LOOP AT lt_voos INTO DATA(ls_voo).
    " Processar
  ENDLOOP.
```

**Como:**
1. SAT → Program: `Z_PROGRAMA_TESTE`
2. Execute
3. Ver resultados

### 2️⃣ Medição de Transaction

**Exemplo:** Medir SE16

1. SAT → Transaction: `SE16`
2. Execute
3. Executar ações na SE16
4. Voltar ao SAT para ver resultados

### 3️⃣ Medição Manual (Código)

```abap
REPORT z_manual_measure.

DATA: lv_start TYPE timestampl,
      lv_end TYPE timestampl,
      lv_diff TYPE p DECIMALS 6.

" Iniciar medição
GET TIME STAMP FIELD lv_start.

" Código a medir
SELECT * FROM sflight INTO TABLE @DATA(lt_voos) UP TO 10000 ROWS.

LOOP AT lt_voos INTO DATA(ls_voo).
  " Processar
ENDLOOP.

" Fim da medição
GET TIME STAMP FIELD lv_end.

" Calcular diferença
lv_diff = lv_end - lv_start.

WRITE: / |Tempo decorrido: { lv_diff } segundos|.
```

---

## 📊 Interpretar Resultados

### Visão Geral

```
Total Runtime: 2.5 segundos
Database Time: 2.0 segundos (80%)
ABAP Time: 0.5 segundos (20%)
```

**Interpretação:**
- Se Database > 70%: Otimizar SQL
- Se ABAP > 70%: Otimizar código ABAP

### Hit List (Lista de Campeões)

Mostra os **métodos/funções mais lentos**:

```
1. SELECT FROM SFLIGHT         - 1.8s (72%)
2. LOOP AT lt_voos             - 0.3s (12%)
3. APPEND TO lt_result         - 0.2s (8%)
4. PERFORM calcular_total      - 0.2s (8%)
```

**Ação:** Focar em otimizar os primeiros da lista!

### Call Hierarchy

Mostra a **árvore de chamadas**:

```
Z_PROGRAMA
├─ SELECT FROM SFLIGHT (1.8s)
├─ PERFORM processar_dados (0.5s)
│  ├─ LOOP AT lt_voos (0.3s)
│  └─ APPEND (0.2s)
└─ WRITE (0.2s)
```

---

## 💡 Exemplos de Otimização

### Exemplo 1: SELECT Lento

**Antes (Lento):**
```abap
" ❌ 5 segundos
SELECT * FROM sflight.
  " Processar linha a linha
ENDSELECT.
```

**Análise SAT:**
```
SELECT FROM SFLIGHT: 5.0s (100%)
```

**Depois (Rápido):**
```abap
" ✅ 0.5 segundos
SELECT * FROM sflight INTO TABLE @DATA(lt_voos).

LOOP AT lt_voos INTO DATA(ls_voo).
  " Processar
ENDLOOP.
```

**Nova Análise:**
```
SELECT FROM SFLIGHT: 0.5s (90%)
LOOP: 0.05s (10%)
```

**Melhoria:** 10x mais rápido!

---

### Exemplo 2: FOR ALL ENTRIES Ineficiente

**Antes:**
```abap
" ❌ Lento se lt_docs estiver vazia
SELECT * FROM ekpo
  INTO TABLE @DATA(lt_items)
  FOR ALL ENTRIES IN @lt_docs
  WHERE ebeln = @lt_docs-ebeln.
```

**Análise:** 3 segundos (tempo desperdiçado)

**Depois:**
```abap
" ✅ Verificar antes
IF lt_docs IS NOT INITIAL.
  SELECT * FROM ekpo
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_docs
    WHERE ebeln = @lt_docs-ebeln.
ENDIF.
```

**Melhoria:** Evita SELECT desnecessário

---

### Exemplo 3: LOOP Lento

**Antes:**
```abap
" ❌ Lento: O(n²)
LOOP AT lt_vendas INTO DATA(ls_venda).
  LOOP AT lt_clientes INTO DATA(ls_cliente).
    IF ls_cliente-kunnr = ls_venda-kunnr.
      " Processar
      EXIT.
    ENDIF.
  ENDLOOP.
ENDLOOP.
```

**Análise:** 10 segundos para 1000 vendas

**Depois:**
```abap
" ✅ Rápido: O(n)
LOOP AT lt_vendas INTO DATA(ls_venda).
  READ TABLE lt_clientes INTO DATA(ls_cliente)
    WITH KEY kunnr = ls_venda-kunnr
    BINARY SEARCH.
    
  IF sy-subrc = 0.
    " Processar
  ENDIF.
ENDLOOP.
```

**Melhoria:** 10s → 0.5s (20x mais rápido!)

---

## 🛠️ Funcionalidades Avançadas

### 1. Comparar Medições

**Cenário:** Antes e depois de otimização

1. SAT → Criar medição "ANTES"
2. Executar programa
3. Otimizar código
4. SAT → Criar medição "DEPOIS"
5. Comparar resultados

**Resultado:**
```
ANTES:  10.0s
DEPOIS:  1.0s
MELHORIA: 10x
```

### 2. SQL Trace

Ativar **SQL Trace** junto com SAT:

1. SAT → Settings → SQL Trace: ON
2. Executar medição
3. Ver queries SQL executadas

**Resultado:**
```
SELECT * FROM SFLIGHT - Executed 1 time - 2.0s
  - Records fetched: 1000
  - Identical selects: 0
```

### 3. Memory Analysis

Ver consumo de memória:

1. SAT → Settings → Memory Analysis: ON
2. Executar
3. Ver picos de memória

---

## 📈 Métricas Importantes

### Database Time

**Alto (>70%):**
- Otimizar SELECTs
- Adicionar índices
- Usar JOIN em vez de FOR ALL ENTRIES
- Limitar quantidade de dados

**Baixo (<30%):**
- Bom! Código ABAP está eficiente

### ABAP Processing Time

**Alto (>70%):**
- Otimizar LOOPs
- Usar tabelas HASHED/SORTED
- Evitar loops aninhados
- Usar expressões modernas

### Number of Database Accesses

**Alto (>100):**
- Consolidar SELECTs
- Usar JOIN
- Buffer tables quando possível

---

## 💡 Exemplo Completo

### Programa Lento

```abap
REPORT z_lento.

DATA: lt_vendas TYPE TABLE OF vbak,
      lt_items TYPE TABLE OF vbap,
      lt_clientes TYPE TABLE OF kna1.

START-OF-SELECTION.
  
  " ❌ Problema 1: SELECT sem limite
  SELECT * FROM vbak INTO TABLE lt_vendas.
  
  " ❌ Problema 2: SELECT dentro de LOOP
  LOOP AT lt_vendas INTO DATA(ls_venda).
    SELECT * FROM vbap INTO TABLE lt_items
      WHERE vbeln = ls_venda-vbeln.
      
    " ❌ Problema 3: LOOP aninhado sem otimização
    LOOP AT lt_items INTO DATA(ls_item).
      SELECT SINGLE * FROM kna1 INTO DATA(ls_cliente)
        WHERE kunnr = ls_venda-kunnr.
        
      WRITE: / ls_cliente-name1, ls_item-matnr.
    ENDLOOP.
  ENDLOOP.
```

**Análise SAT:**
```
Total: 45 segundos
Database: 40s (89%)
  - SELECT FROM VBAK: 5s
  - SELECT FROM VBAP: 20s (1000 vezes!)
  - SELECT FROM KNA1: 15s (5000 vezes!)
```

### Programa Otimizado

```abap
REPORT z_rapido.

DATA: lt_vendas TYPE TABLE OF vbak,
      lt_items TYPE TABLE OF vbap,
      lt_clientes TYPE HASHED TABLE OF kna1
                   WITH UNIQUE KEY kunnr.

START-OF-SELECTION.
  
  " ✅ Melhoria 1: Limitar quantidade
  SELECT * FROM vbak INTO TABLE lt_vendas
    UP TO 1000 ROWS.
  
  " ✅ Melhoria 2: SELECT único com FOR ALL ENTRIES
  IF lt_vendas IS NOT INITIAL.
    SELECT * FROM vbap INTO TABLE lt_items
      FOR ALL ENTRIES IN lt_vendas
      WHERE vbeln = lt_vendas-vbeln.
      
    " ✅ Melhoria 3: Buscar clientes de uma vez
    SELECT * FROM kna1 INTO TABLE lt_clientes
      FOR ALL ENTRIES IN lt_vendas
      WHERE kunnr = lt_vendas-kunnr.
  ENDIF.
  
  " ✅ Melhoria 4: Ordenar para BINARY SEARCH
  SORT lt_items BY vbeln.
  
  " ✅ Melhoria 5: Usar tabela HASHED
  LOOP AT lt_vendas INTO DATA(ls_venda).
    LOOP AT lt_items INTO DATA(ls_item)
      WHERE vbeln = ls_venda-vbeln.
      
      " ✅ Melhoria 6: READ TABLE em HASHED
      READ TABLE lt_clientes INTO DATA(ls_cliente)
        WITH TABLE KEY kunnr = ls_venda-kunnr.
        
      IF sy-subrc = 0.
        WRITE: / ls_cliente-name1, ls_item-matnr.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
```

**Nova Análise:**
```
Total: 2 segundos
Database: 1.5s (75%)
  - SELECT FROM VBAK: 0.5s
  - SELECT FROM VBAP: 0.5s (1 vez!)
  - SELECT FROM KNA1: 0.5s (1 vez!)
ABAP: 0.5s (25%)
```

**Resultado:** 45s → 2s = **22.5x mais rápido!**

---

## 🎓 Boas Práticas

### ✅ Fazer

```abap
" 1. Medir antes de otimizar
" Não otimize às cegas!

" 2. Focar nos 20% que causam 80% do problema
" Hit List no SAT mostra isto

" 3. Comparar antes/depois
DATA lv_antes TYPE p DECIMALS 6.
DATA lv_depois TYPE p DECIMALS 6.

" 4. Documentar melhorias
" Comentar: "Otimizado de 10s para 1s"
```

### ❌ Evitar

```abap
" 1. Otimizar sem medir
" "Acho que este código está lento" ❌

" 2. Micro-otimizações prematuras
lv_x = lv_y + 1.  " ✅ Legível
lv_x += 1.        " ✅ Também OK
" Diferença é insignificante!

" 3. Ignorar resultados
" SAT diz: "SELECT lento" mas não corrige ❌

" 4. Não testar após otimização
" Otimizou mas quebrou funcionalidade ❌
```

---

## 🔗 Transactions Relacionadas

- **SAT / SE30** - Runtime Analysis
- **ST05** - SQL Trace (detalhado)
- **ST12** - Performance Trace (completo)
- **ST22** - Dumps (após otimização falhar)

---

## 🔗 Próximos Passos

- **[Performance](../performance/index.md)** - Técnicas de otimização
- **[SQL Otimizações](../sql/6_otimizacoes.md)** - Otimizar queries
- **[FOR ALL ENTRIES](../performance/1_for_all_entries.md)** - Usar corretamente

---

**Tags:** `#Performance` `#SAT` `#RuntimeAnalysis` `#Otimização` `#ABAP`
