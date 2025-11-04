---
tags:
  - ABAP
  - Performance
  - Otimização
  - Boas Práticas
---

# ⚡ Performance e Boas Práticas

Guia completo para escrever **código ABAP eficiente e otimizado**.

---

## 📖 O que vais aprender

- Identificar e eliminar gargalos de performance
- Otimizar consultas SQL (SELECTs)
- Usar `FOR ALL ENTRIES` corretamente
- Trabalhar eficientemente com tabelas internas
- Evitar armadilhas comuns (SELECT em LOOP, etc.)
- Entender e usar índices de base de dados
- Analisar performance com SAT (Runtime Analysis)
- Configurar e usar buffering de tabelas
- Escolher tipos corretos de tabelas internas

---

## 🎯 Regras de Ouro

### ✅ Fazer

1. **Minimizar acessos à BD**: Agregar dados numa única query
2. **Usar campos indexados**: WHERE com campos que têm índices
3. **Limitar resultados**: `UP TO n ROWS` quando possível
4. **Especificar campos**: Evitar `SELECT *`
5. **Usar `FOR ALL ENTRIES`**: Para substituir SELECTs em LOOP
6. **Escolher tipo correto**: STANDARD vs SORTED vs HASHED

### ❌ Evitar

1. **SELECT dentro de LOOP**: Causa múltiplos acessos à BD ⚠️
2. **FOR ALL ENTRIES sem validação**: Pode retornar tudo! 😱
3. **APPEND em LOOPs grandes**: Usar `VALUE #()` ou `INSERT LINES OF`
4. **NESTED LOOPs**: Considerar tabelas HASHED
5. **Modificar tabela durante LOOP**: Comportamento inesperado
6. **SELECT ***: Desperdício de recursos

---

---

## 🔑 Tópicos Principais

### 1️⃣ [FOR ALL ENTRIES](1_for_all_entries.md)
Técnica essencial para **evitar SELECT em LOOP** e melhorar drasticamente a performance.

**Exemplo rápido:**
```abap
" ❌ ERRADO: 1000 queries
LOOP AT lt_orders INTO DATA(ls_order).
  SELECT * FROM vbap WHERE vbeln = ls_order-vbeln...
ENDLOOP.

" ✅ CORRETO: 2 queries
IF lt_orders IS NOT INITIAL.
  SELECT * FROM vbap
    FOR ALL ENTRIES IN @lt_orders
    WHERE vbeln = @lt_orders-vbeln...
ENDIF.
```

[Ler mais sobre FOR ALL ENTRIES »](1_for_all_entries.md)

---

### 2️⃣ [Índices de Base de Dados](2_indices.md)
Como usar índices para **acelerar queries SQL** até 1000x.

**Pontos-chave:**
- Usar campos indexados no WHERE
- Verificar índices existentes (SE11)
- Criar índices secundários quando necessário
- Medir impacto com ST05

[Ler mais sobre Índices »](2_indices.md)

---

### 3️⃣ [Runtime Analysis (SAT)](3_runtime_analysis.md)
Ferramenta para **analisar performance** em detalhe e identificar hotspots.

**O que mede:**
- Tempo de execução por método
- Número de queries à BD
- Tempo SQL vs processamento ABAP
- Consumo de memória

[Ler mais sobre SAT »](3_runtime_analysis.md)

---

### 4️⃣ [Table Buffering](4_buffering.md)
Usar **cache em memória** para dados que não mudam frequentemente.

**Tipos de buffering:**
- Full buffering (tabelas pequenas)
- Generic buffering (por grupos)
- Single record buffering (por chave)

**Ganho:** Até **100x mais rápido** para dados bufferizados!

[Ler mais sobre Buffering »](4_buffering.md)

---

### 5️⃣ [Tabelas Internas Eficientes](5_tabelas_internas.md)
Escolher o **tipo correto** de tabela interna pode melhorar performance em 100x.

**Comparação:**
| Tipo | READ (100k registos) | Quando Usar |
|------|---------------------|-------------|
| **STANDARD** | 50ms | LOOPs sequenciais |
| **SORTED** | 0.5ms | Dados ordenados, READs moderados |
| **HASHED** | 0.05ms | Muitos READs por chave |

[Ler mais sobre Tabelas Internas »](5_tabelas_internas.md)

---

### 6️⃣ [Armadilhas Comuns](6_armadilhas.md)
Os **10 erros mais comuns** que destroem performance.

**Top 3:**
1. ❌ SELECT em LOOP
2. ❌ SELECT * (todos os campos)
3. ❌ FOR ALL ENTRIES sem validação

[Ler mais sobre Armadilhas »](6_armadilhas.md)

---

## 📊 Comparação: Antes e Depois da Otimização

### Cenário Real: Relatório de Vendas

```abap
" ❌ ANTES: 15 minutos
LOOP AT lt_orders INTO DATA(ls_order).          " 10.000 orders
  SELECT * FROM vbap WHERE vbeln = ls_order-vbeln...  " 10.000 SELECTs
  LOOP AT lt_items INTO DATA(ls_item).
    SELECT SINGLE * FROM mara WHERE matnr = ls_item-matnr...  " 50.000 SELECTs
  ENDLOOP.
ENDLOOP.

" ✅ DEPOIS: 30 segundos
SELECT * FROM vbak INTO TABLE @DATA(lt_orders)...
IF lt_orders IS NOT INITIAL.
  SELECT * FROM vbap FOR ALL ENTRIES IN @lt_orders...  " 1 SELECT
ENDIF.

" Criar HASHED table para materiais
DATA lt_materials TYPE HASHED TABLE OF mara WITH UNIQUE KEY matnr.
SELECT * FROM mara INTO TABLE @lt_materials FOR ALL ENTRIES...

" Usar lookups rápidos
LOOP AT lt_items INTO DATA(ls_item).
  READ TABLE lt_materials WITH TABLE KEY matnr = ls_item-matnr...
ENDLOOP.

" Resultado: 30x mais rápido! 🚀
```

---

## 🧪 Ferramentas de Análise

### Transações Essenciais

| Transação | Propósito | Quando Usar |
|-----------|-----------|-------------|
| **SAT** | Runtime Analysis | Medir tempo de execução detalhado |
| **ST05** | SQL Trace | Ver queries SQL executadas |
| **ST02** | Buffer Statistics | Monitorizar buffers |
| **ST10** | Table Call Stats | Ver acessos a tabelas |
| **ST22** | ABAP Dumps | Analisar erros runtime |
| **SE11** | Data Dictionary | Ver índices de tabelas |

- Monitorizar todas as operações SQL
- Identificar SELECTs lentos
- Ver planos de execução

---

## 📚 Exercícios Práticos

Temos **10 exercícios** progressivos em `ex01.md` a `ex10.md`:

- **ex01**: Otimizar SELECT dentro de LOOP
- **ex02**: Usar FOR ALL ENTRIES corretamente
- **ex03**: Trabalhar com tabelas HASHED
- **ex04**: Evitar NESTED LOOPs
- **ex05**: Buffering de tabelas
- **ex06-ex10**: Casos práticos de otimização

---

## 💡 Exemplo Completo: Antes e Depois

### 🐌 Antes (Lento)

```abap
REPORT z_performance_bad.

DATA: lt_orders TYPE TABLE OF vbak,
      lt_items  TYPE TABLE OF vbap.

SELECT * FROM vbak INTO TABLE lt_orders UP TO 1000 ROWS.

LOOP AT lt_orders INTO DATA(ls_order).
  " ❌ SELECT dentro de LOOP - 1000 queries!
  SELECT * FROM vbap
    INTO TABLE @DATA(lt_temp)
    WHERE vbeln = @ls_order-vbeln.
  APPEND LINES OF lt_temp TO lt_items.
ENDLOOP.
```

**Performance**: ~5-10 segundos para 1000 encomendas

---

### 🚀 Depois (Rápido)

```abap
REPORT z_performance_good.

DATA: lt_orders TYPE TABLE OF vbak,
      lt_items  TYPE TABLE OF vbap.

SELECT * FROM vbak INTO TABLE lt_orders UP TO 1000 ROWS.

" ✅ Uma única query com FOR ALL ENTRIES
IF lt_orders IS NOT INITIAL.
  SELECT * FROM vbap
    INTO TABLE lt_items
    FOR ALL ENTRIES IN @lt_orders
    WHERE vbeln = @lt_orders-vbeln.
ENDIF.
```

**Performance**: ~0.5 segundos para 1000 encomendas  
**Melhoria**: 10-20x mais rápido! 🎯

---

## 🔍 Tabelas Internas: Standard vs. Sorted vs. Hashed

| Tipo | Acesso | Uso ideal |
|------|--------|-----------|
| `STANDARD TABLE` | Sequencial (lento) | Pequenas tabelas, sem buscas |
| `SORTED TABLE` | Binário (médio) | Acesso ordenado, buscas médias |
| `HASHED TABLE` | Hash (rápido) | Buscas frequentes com chave |

**Exemplo com HASHED:**
```abap
DATA lt_materials TYPE HASHED TABLE OF mara 
  WITH UNIQUE KEY matnr.

READ TABLE lt_materials WITH KEY matnr = 'MAT001' 
  INTO DATA(ls_mat).
" O(1) - acesso instantâneo! 
```

---

## 🚀 Próximos Passos

1. Comece por **[FOR ALL ENTRIES](1_for_all_entries.md)** para otimizar joins
2. Aprenda sobre **[Índices](2_indices.md)** para queries mais rápidas
3. Use **[SAT](3_runtime_analysis.md)** para analisar seus programas
4. Implemente **[Buffering](4_buffering.md)** quando apropriado
5. Otimize **[Tabelas Internas](5_tabelas_internas.md)** para melhor performance
6. Evite **[Armadilhas Comuns](6_armadilhas.md)** de performance
