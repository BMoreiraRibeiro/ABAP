# ⚡ Performance e Boas Práticas

Dicas e padrões para escrever **código ABAP eficiente**: otimização de consultas SQL, uso correto de tabelas internas e `FOR ALL ENTRIES`.

---

## 📖 O que vais aprender

- Identificar gargalos de performance
- Otimizar SELECTs e consultas SQL
- Usar `FOR ALL ENTRIES` corretamente
- Trabalhar eficientemente com tabelas internas
- Evitar armadilhas comuns (SELECT em LOOP, etc.)
- Usar índices de base de dados
- Análise com SAT (Runtime Analysis)
- Buffering de tabelas

---

## 🎯 Regras de Ouro

### ✅ Fazer

1. **Minimizar acessos à BD**: agregar dados numa única query
2. **Usar campos de índice**: WHERE com campos indexados
3. **Limitar resultados**: `UP TO n ROWS` quando possível
4. **Projetar apenas campos necessários**: evitar `SELECT *`
5. **Usar `FOR ALL ENTRIES`** para substituir SELECTs em LOOP

### ❌ Evitar

1. **SELECT dentro de LOOP**: causa múltiplos acessos à BD
2. **FOR ALL ENTRIES sem validação**: pode falhar com tabela vazia
3. **APPEND em LOOPs grandes**: usar `INSERT LINES OF` ou `VALUE #( )`
4. **NESTED LOOPs profundos**: considerar usar tabelas HASHED
5. **Modificar tabelas durante iteração**: pode causar comportamento inesperado

---

## 🔑 Conceitos Principais

### [FOR ALL ENTRIES](for_all_entries.md)
Como usar e armadilhas a evitar.

**Exemplo correto:**
```abap
SELECT * FROM scarr INTO TABLE @DATA(lt_carriers) UP TO 100 ROWS.

" ✅ Validar antes de usar FOR ALL ENTRIES
IF lt_carriers IS NOT INITIAL.
  SELECT * FROM spfli
    INTO TABLE @DATA(lt_connections)
    FOR ALL ENTRIES IN @lt_carriers
    WHERE carrid = @lt_carriers-carrid.
ENDIF.
```

**❌ Errado (sem validação):**
```abap
" Se lt_carriers estiver vazia, retorna TODOS os registos!
SELECT * FROM spfli
  INTO TABLE @DATA(lt_connections)
  FOR ALL ENTRIES IN @lt_carriers
  WHERE carrid = @lt_carriers-carrid.
```

---

## 📊 Comparação de Abordagens

### Cenário: Buscar detalhes de 1000 encomendas

#### ❌ Má prática (SELECT em LOOP)
```abap
LOOP AT lt_encomendas INTO DATA(ls_enc).
  SELECT SINGLE * FROM vbap 
    INTO @DATA(ls_item)
    WHERE vbeln = @ls_enc-vbeln.
  " 1000 queries à BD! 😱
ENDLOOP.
```

#### ✅ Boa prática (FOR ALL ENTRIES)
```abap
IF lt_encomendas IS NOT INITIAL.
  SELECT * FROM vbap
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_encomendas
    WHERE vbeln = @lt_encomendas-vbeln.
  " 1 query à BD! ✅
ENDIF.
```

---

## 🧪 Análise de Performance

### Runtime Analysis (SAT)

1. Executar transação **SAT**
2. Ativar medição
3. Executar o programa
4. Analisar resultados:
   - Tempo total
   - Tempo por método/função
   - Hits de base de dados
   - Tempo de processamento ABAP vs. BD

### SQL Trace (ST05)

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

1. Comece por [FOR ALL ENTRIES](for_all_entries.md)
2. Pratique com `ex01.md` a `ex10.md`
3. Use SAT para analisar seus programas
4. Aplique as boas práticas em código real
5. Avance para [SQL](../sql/index.md) para otimizações de queries
