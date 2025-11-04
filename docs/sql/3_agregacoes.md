# Agregações e GROUP BY

## 📋 Visão Geral

As funções de agregação permitem realizar cálculos estatísticos sobre conjuntos de dados, como contar registos, calcular médias, somas, valores máximos e mínimos.

---

## 🔢 Funções de Agregação

### COUNT - Contar Registos

```abap
" Contar todos os registos
SELECT COUNT( * ) 
  FROM sflight
  INTO @DATA(lv_total).

WRITE: / 'Total de voos:', lv_total.

" Contar valores distintos
SELECT COUNT( DISTINCT carrid )
  FROM sflight
  INTO @DATA(lv_companhias).

WRITE: / 'Companhias diferentes:', lv_companhias.

" Contar com condição
SELECT COUNT( * )
  FROM sflight
  WHERE carrid = 'LH'
  INTO @DATA(lv_voos_lh).
```

### SUM - Somar Valores

```abap
" Soma total
SELECT SUM( price )
  FROM sflight
  WHERE currency = 'EUR'
  INTO @DATA(lv_total_preco).

WRITE: / 'Receita total:', lv_total_preco.

" Soma com várias moedas
SELECT currency,
       SUM( price ) AS total
  FROM sflight
  GROUP BY currency
  INTO TABLE @DATA(lt_totais).

LOOP AT lt_totais INTO DATA(ls_total).
  WRITE: / ls_total-currency, ls_total-total.
ENDLOOP.
```

### AVG - Calcular Média

```abap
" Média simples
SELECT AVG( price AS DEC( 15,2 ) )
  FROM sflight
  WHERE carrid = 'AA'
  INTO @DATA(lv_preco_medio).

WRITE: / 'Preço médio:', lv_preco_medio.

" Média por companhia
SELECT carrid,
       AVG( price AS DEC( 15,2 ) ) AS preco_medio
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_medias).
```

### MAX e MIN - Valores Extremos

```abap
" Máximo e mínimo
SELECT MAX( price ) AS max_preco,
       MIN( price ) AS min_preco
  FROM sflight
  INTO @DATA(ls_extremos).

WRITE: / 'Preço máximo:', ls_extremos-max_preco.
WRITE: / 'Preço mínimo:', ls_extremos-min_preco.

" Por grupo
SELECT carrid,
       MAX( price ) AS max_preco,
       MIN( price ) AS min_preco
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_ranges).
```

---

## 📊 GROUP BY - Agrupar Dados

### Agrupamento Simples

```abap
" Agrupar por companhia
SELECT carrid,
       COUNT( * ) AS num_voos
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_por_companhia).

LOOP AT lt_por_companhia INTO DATA(ls_comp).
  WRITE: / ls_comp-carrid, ls_comp-num_voos.
ENDLOOP.
```

### Agrupamento Múltiplo

```abap
" Agrupar por companhia e moeda
SELECT carrid,
       currency,
       COUNT( * ) AS quantidade,
       SUM( price ) AS total,
       AVG( price AS DEC( 15,2 ) ) AS media
  FROM sflight
  GROUP BY carrid, currency
  INTO TABLE @DATA(lt_estatisticas).

LOOP AT lt_estatisticas INTO DATA(ls_stat).
  WRITE: / ls_stat-carrid, 
         ls_stat-currency,
         ls_stat-quantidade,
         ls_stat-total,
         ls_stat-media.
ENDLOOP.
```

### GROUP BY com JOINs

```abap
" Estatísticas com informação de múltiplas tabelas
SELECT a~carrid,
       b~carrname,
       COUNT( * ) AS num_voos,
       AVG( a~price AS DEC( 15,2 ) ) AS preco_medio
  FROM sflight AS a
  INNER JOIN scarr AS b ON a~carrid = b~carrid
  GROUP BY a~carrid, b~carrname
  INTO TABLE @DATA(lt_completo).
```

---

## 🔍 HAVING - Filtrar Grupos

### Filtrar Após Agregação

```abap
" Apenas companhias com mais de 10 voos
SELECT carrid,
       COUNT( * ) AS num_voos
  FROM sflight
  GROUP BY carrid
  HAVING COUNT( * ) > 10
  INTO TABLE @DATA(lt_frequentes).

" Companhias com preço médio acima de 500
SELECT carrid,
       AVG( price AS DEC( 15,2 ) ) AS preco_medio
  FROM sflight
  GROUP BY carrid
  HAVING AVG( price ) > 500
  INTO TABLE @DATA(lt_premium).
```

### HAVING vs WHERE

```abap
" WHERE filtra ANTES do GROUP BY (mais eficiente)
SELECT carrid,
       COUNT( * ) AS num_voos
  FROM sflight
  WHERE fldate >= '20240101'  " Filtra primeiro
  GROUP BY carrid
  HAVING COUNT( * ) > 5      " Depois filtra grupos
  INTO TABLE @DATA(lt_resultado).

" Regra: Use WHERE para filtros de linhas individuais
"        Use HAVING para filtros de resultados agregados
```

---

## 📈 Agregações Avançadas

### Múltiplas Agregações

```abap
" Várias estatísticas de uma só vez
SELECT carrid,
       COUNT( * ) AS total_voos,
       SUM( seatsocc ) AS assentos_ocupados,
       SUM( seatsmax ) AS assentos_totais,
       AVG( price AS DEC( 15,2 ) ) AS preco_medio,
       MAX( price ) AS preco_maximo,
       MIN( price ) AS preco_minimo
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_analise_completa).
```

### Cálculos Derivados

```abap
" Taxa de ocupação calculada
SELECT carrid,
       SUM( seatsocc ) AS ocupados,
       SUM( seatsmax ) AS total,
       CAST( SUM( seatsocc ) AS DEC( 15,2 ) ) / 
       CAST( SUM( seatsmax ) AS DEC( 15,2 ) ) * 100 
         AS taxa_ocupacao
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_ocupacao).

LOOP AT lt_ocupacao INTO DATA(ls_ocup).
  WRITE: / ls_ocup-carrid, 
         'Ocupação:', ls_ocup-taxa_ocupacao, '%'.
ENDLOOP.
```

### CASE em Agregações

```abap
" Contar por categoria
SELECT carrid,
       COUNT( * ) AS total_voos,
       COUNT( CASE WHEN price < 500 THEN 1 END ) AS voos_economicos,
       COUNT( CASE WHEN price BETWEEN 500 AND 1000 THEN 1 END ) AS voos_normais,
       COUNT( CASE WHEN price > 1000 THEN 1 END ) AS voos_premium
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_categorias).
```

---

## 💡 Exemplos Práticos

### Dashboard de Vendas

```abap
REPORT z_dashboard_vendas.

START-OF-SELECTION.

  " Análise de vendas por mês
  SELECT SUBSTRING( fldate, 1, 6 ) AS mes,
         COUNT( * ) AS num_vendas,
         SUM( price ) AS receita_total,
         AVG( price AS DEC( 15,2 ) ) AS ticket_medio
    FROM sflight
    WHERE fldate >= '20240101'
    GROUP BY SUBSTRING( fldate, 1, 6 )
    ORDER BY mes
    INTO TABLE @DATA(lt_mensal).

  WRITE: / 'Dashboard Mensal'.
  WRITE: / '================'.
  
  LOOP AT lt_mensal INTO DATA(ls_mes).
    WRITE: / ls_mes-mes,
           'Vendas:', ls_mes-num_vendas,
           'Receita:', ls_mes-receita_total,
           'Ticket Médio:', ls_mes-ticket_medio.
  ENDLOOP.
```

### Top Companhias

```abap
METHOD obter_top_companhias.
  " Top 10 companhias por receita
  SELECT TOP 10
         a~carrid,
         b~carrname,
         COUNT( * ) AS num_voos,
         SUM( a~price ) AS receita_total
    FROM sflight AS a
    INNER JOIN scarr AS b ON a~carrid = b~carrid
    GROUP BY a~carrid, b~carrname
    ORDER BY receita_total DESCENDING
    INTO TABLE @DATA(lt_top).
    
  RETURN lt_top.
ENDMETHOD.
```

### Análise de Tendências

```abap
" Comparar performance ano a ano
SELECT SUBSTRING( fldate, 1, 4 ) AS ano,
       carrid,
       COUNT( * ) AS voos,
       AVG( seatsocc AS DEC( 15,2 ) ) AS ocupacao_media
  FROM sflight
  GROUP BY SUBSTRING( fldate, 1, 4 ), carrid
  HAVING COUNT( * ) > 50
  ORDER BY ano, carrid
  INTO TABLE @DATA(lt_tendencias).
```

---

## 🎯 Boas Práticas

### ✅ Fazer

```abap
" 1. Especificar todos os campos no GROUP BY
SELECT carrid, connid, COUNT( * )
  FROM sflight
  GROUP BY carrid, connid  " ✅ Correto
  INTO TABLE @DATA(lt_data).

" 2. Usar aliases para melhor legibilidade
SELECT carrid,
       COUNT( * ) AS total_voos,
       AVG( price AS DEC( 15,2 ) ) AS preco_medio  " ✅
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_stats).

" 3. Combinar WHERE e HAVING adequadamente
SELECT carrid, COUNT( * ) AS num
  FROM sflight
  WHERE fldate >= '20240101'  " ✅ Filtra antes (mais rápido)
  GROUP BY carrid
  HAVING COUNT( * ) > 10      " ✅ Filtra resultado agregado
  INTO TABLE @DATA(lt_result).
```

### ❌ Evitar

```abap
" 1. Campos não agregados sem GROUP BY
SELECT carrid, connid, COUNT( * )  " ❌ connid não está no GROUP BY
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_data).

" 2. SELECT * com GROUP BY
SELECT *, COUNT( * )  " ❌ Ambíguo
  FROM sflight
  GROUP BY carrid
  INTO TABLE @DATA(lt_data).

" 3. Agregações desnecessárias
SELECT carrid, COUNT( * )
  FROM sflight
  GROUP BY carrid
  HAVING carrid = 'LH'  " ❌ Deve estar no WHERE
  INTO TABLE @DATA(lt_data).

" Correto:
SELECT carrid, COUNT( * )
  FROM sflight
  WHERE carrid = 'LH'  " ✅
  GROUP BY carrid
  INTO TABLE @DATA(lt_data).
```

---

## 📊 Performance

### Otimizações

1. **Use índices no WHERE**: Filtre antes de agrupar
2. **Limite resultados**: Use `UP TO n ROWS` quando possível
3. **Evite HAVING complexos**: Use WHERE sempre que possível
4. **Agregações específicas**: Não use `COUNT(*)` se pode usar `COUNT(campo_indexado)`

```abap
" ❌ Lento
SELECT carrid, COUNT( * )
  FROM sflight
  GROUP BY carrid
  HAVING carrid IN ('AA', 'LH', 'BA')
  INTO TABLE @DATA(lt_slow).

" ✅ Rápido
SELECT carrid, COUNT( * )
  FROM sflight
  WHERE carrid IN ('AA', 'LH', 'BA')  " Usa índice
  GROUP BY carrid
  INTO TABLE @DATA(lt_fast).
```

---

## 🔗 Próximos Passos

- **[INSERT UPDATE DELETE](4_insert_update_delete.md)** - Manipular dados
- **[WHERE Dinâmico](5_where_dinamico.md)** - Consultas flexíveis
- **[Otimizações SQL](6_otimizacoes.md)** - Performance avançada

---

**Tags:** `#SQL` `#Agregações` `#GROUP-BY` `#HAVING` `#COUNT` `#SUM` `#AVG`
