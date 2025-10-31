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

### 1️⃣ [SELECT Básico](select_basico.md)
Como buscar dados de tabelas SAP.

**Exemplo:**
```abap
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).
LOOP AT lt_scarr INTO DATA(ls_scarr).
  WRITE: / ls_scarr-carrid, ls_scarr-carrname.
ENDLOOP.
```

### 2️⃣ [JOINS](joins.md)
Combinar dados de múltiplas tabelas.

**Exemplo:**
```abap
SELECT a~carrid, a~connid, b~carrname
  FROM spfli AS a
  INNER JOIN scarr AS b ON a~carrid = b~carrid
  INTO TABLE @DATA(lt_join)
  UP TO 10 ROWS.
```

### 3️⃣ [Agregações](agregacoes.md)
Funções estatísticas e agrupamento.

**Exemplo:**
```abap
SELECT carrid, 
       COUNT( * ) AS total_voos, 
       AVG( price ) AS preco_medio
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_stats).
```

---

## 📚 Exercícios Práticos

Temos **25 exercícios** progressivos em `ex01.md` a `ex25.md` que cobrem:

- SELECTs com filtros complexos
- Subconsultas (subqueries)
- CASE statements
- Agregações múltiplas
- JOINS de 3+ tabelas
- Otimização de performance SQL

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

1. Comece por [SELECT Básico](select_basico.md)
2. Depois siga para [JOINS](joins.md)
3. Pratique com os exercícios `ex01.md` a `ex25.md`
4. Avance para [Performance](../performance/index.md) para otimizações
