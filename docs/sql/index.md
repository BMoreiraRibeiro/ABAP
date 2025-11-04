# 💾 SQL ABAP

Consultas básicas, JOINS, agregações e manipulação de dados na base de dados SAP.

---

## 📖 O que vais aprender

- Executar SELECTs básicos e avançados
- Usar JOINS para combinar tabelas
- Funções de agregação (COUNT, SUM, AVG, MAX, MIN)
- GROUP BY e HAVING
- Otimizações e boas práticas SQL
- Cláusulas WHERE dinâmicas
- INSERT, UPDATE, DELETE e MODIFY

---

## 🎯 Ordem de Aprendizagem

### 1️⃣ [SELECT Básico](1_select_basico.md)
Como buscar dados de tabelas SAP.

**Exemplo:**
```abap
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).
LOOP AT lt_scarr INTO DATA(ls_scarr).
  WRITE: / ls_scarr-carrid, ls_scarr-carrname.
ENDLOOP.
```

### 2️⃣ [JOINS](2_joins.md)
Combinar dados de múltiplas tabelas.

**Exemplo:**
```abap
SELECT a~carrid, a~connid, b~carrname
  FROM spfli AS a
  INNER JOIN scarr AS b ON a~carrid = b~carrid
  INTO TABLE @DATA(lt_join)
  UP TO 10 ROWS.
```

### 3️⃣ [Agregações e GROUP BY](3_agregacoes.md)
Funções estatísticas e agrupamento.

**Exemplo:**
```abap
SELECT carrid, 
       COUNT( * ) AS total_voos, 
       AVG( price AS DEC( 15,2 ) ) AS preco_medio
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_stats).
```

### 4️⃣ [INSERT, UPDATE, DELETE](4_insert_update_delete.md)
Manipular dados na base de dados.

**Exemplo:**
```abap
INSERT scarr FROM @( VALUE #(
  carrid = 'PT' carrname = 'TAP Air Portugal'
) ).

UPDATE sflight SET price = price * '1.10'
  WHERE carrid = 'LH'.
```

### 5️⃣ [WHERE Dinâmico](5_where_dinamico.md)
Consultas SQL flexíveis em tempo de execução.

**Exemplo:**
```abap
DATA(lv_where) = |carrid = '{ p_carrid }' AND price >= { p_preco }|.

SELECT * FROM sflight
  WHERE (lv_where)
  INTO TABLE @DATA(lt_voos).
```

### 6️⃣ [Otimizações SQL](6_otimizacoes.md)
Técnicas para melhorar performance.

**Exemplo:**
```abap
" ✅ Rápido: SELECT único com FOR ALL ENTRIES
IF lt_pedidos IS NOT INITIAL.
  SELECT * FROM vbap
    FOR ALL ENTRIES IN @lt_pedidos
    WHERE vbeln = @lt_pedidos-vbeln
    INTO TABLE @DATA(lt_itens).
ENDIF.
```

---

## 🚀 Boas Práticas

### ✅ Fazer
- Usar `@` para host variables (ABAP 7.40+)
- Limitar resultados com `UP TO n ROWS`
- Usar índices nas cláusulas WHERE
- Evitar SELECT * (especificar campos necessários)

### ❌ Evitar
- SELECTs dentro de LOOPs
- Não usar FOR ALL ENTRIES sem validação
- Evitar funções pesadas em WHERE

---

## 💡 Exemplo Completo

```abap
REPORT z_sql_demo.

PARAMETERS: p_carr TYPE s_carr_id DEFAULT 'LH'.

START-OF-SELECTION.

  " SELECT com JOIN e agregação
  SELECT a~carrid,
         a~carrname,
         COUNT( * ) AS num_flights,
         AVG( b~price ) AS avg_price
    FROM scarr AS a
    INNER JOIN sflight AS b ON a~carrid = b~carrid
    WHERE a~carrid = @p_carr
    GROUP BY a~carrid, a~carrname
    INTO TABLE @DATA(lt_result).

  " Exibir resultados
  LOOP AT lt_result INTO DATA(ls_result).
    WRITE: / ls_result-carrid, 
           ls_result-carrname, 
           ls_result-num_flights, 
           ls_result-avg_price.
  ENDLOOP.
```

---

## 🔗 Próximos Passos

1. Comece por [SELECT Básico](1_select_basico.md)
2. Depois siga para [JOINS](2_joins.md)
3. Continue com [Agregações](3_agregacoes.md)
4. Aprenda a [manipular dados](4_insert_update_delete.md)
5. Explore [WHERE Dinâmico](5_where_dinamico.md)
6. Domine [Otimizações SQL](6_otimizacoes.md)
7. Avance para [Performance](../performance/index.md) para otimizações avançadas
