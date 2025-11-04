# Otimizações SQL

## 📋 Visão Geral

Técnicas e boas práticas para escrever consultas SQL eficientes em ABAP, melhorando a performance e reduzindo o tempo de resposta das aplicações.

---

## 🎯 Regras de Ouro

### 1. Evitar SELECT em LOOPs

```abap
" ❌ MUITO LENTO - SELECT dentro de LOOP
LOOP AT lt_pedidos INTO DATA(ls_pedido).
  SELECT SINGLE * FROM kna1
    WHERE kunnr = @ls_pedido-kunnr
    INTO @DATA(ls_cliente).
  " Processar...
ENDLOOP.
" Se lt_pedidos tem 1000 linhas = 1000 SELECTs!

" ✅ RÁPIDO - SELECT único com FOR ALL ENTRIES
SELECT * FROM kna1
  FOR ALL ENTRIES IN @lt_pedidos
  WHERE kunnr = @lt_pedidos-kunnr
  INTO TABLE @DATA(lt_clientes).

" Depois fazer LOOP e READ TABLE
LOOP AT lt_pedidos INTO ls_pedido.
  READ TABLE lt_clientes INTO DATA(ls_cliente)
    WITH KEY kunnr = ls_pedido-kunnr.
  " Processar...
ENDLOOP.
```

### 2. Selecionar Apenas Campos Necessários

```abap
" ❌ Desnecessário - traz todos os campos
SELECT * FROM kna1
  WHERE land1 = 'PT'
  INTO TABLE @DATA(lt_clientes).

" ✅ Eficiente - apenas campos necessários
SELECT kunnr, name1, ort01
  FROM kna1
  WHERE land1 = 'PT'
  INTO TABLE @DATA(lt_clientes).

" Reduz:
" - Tráfego de rede
" - Uso de memória
" - Tempo de processamento
```

### 3. Usar Índices Corretamente

```abap
" ❌ Não usa índice - campo não indexado
SELECT * FROM vbak
  WHERE erdat = '20250101'
  INTO TABLE @DATA(lt_pedidos).

" ✅ Usa índice - campo chave/indexado
SELECT * FROM vbak
  WHERE vbeln = '0000000001'
  INTO TABLE @lt_pedidos.

" Ver índices: Transação SE11 > Display > Índices
```

---

## 🚀 FOR ALL ENTRIES

### Uso Correto

```abap
DATA lt_pedidos TYPE TABLE OF vbak.
DATA lt_itens   TYPE TABLE OF vbap.

" Buscar pedidos
SELECT * FROM vbak
  WHERE vkorg = '1000'
  INTO TABLE @lt_pedidos
  UP TO 1000 ROWS.

" ✅ IMPORTANTE: Verificar se tabela não está vazia!
IF lt_pedidos IS NOT INITIAL.
  SELECT * FROM vbap
    FOR ALL ENTRIES IN @lt_pedidos
    WHERE vbeln = @lt_pedidos-vbeln
    INTO TABLE @lt_itens.
ENDIF.

" Se lt_pedidos estiver vazia, SELECT retorna TUDO!
```

### Otimizar FOR ALL ENTRIES

```abap
" ✅ Remover duplicados antes de FOR ALL ENTRIES
DATA lt_pedidos_uniq LIKE lt_pedidos.

lt_pedidos_uniq = lt_pedidos.
SORT lt_pedidos_uniq BY vbeln.
DELETE ADJACENT DUPLICATES FROM lt_pedidos_uniq COMPARING vbeln.

" Agora usar a tabela sem duplicados
IF lt_pedidos_uniq IS NOT INITIAL.
  SELECT * FROM vbap
    FOR ALL ENTRIES IN @lt_pedidos_uniq
    WHERE vbeln = @lt_pedidos_uniq-vbeln
    INTO TABLE @DATA(lt_itens).
ENDIF.
```

---

## 📊 JOINs vs. FOR ALL ENTRIES

### Quando Usar JOIN

```abap
" ✅ JOIN - Melhor para relações 1:1 ou 1:N simples
SELECT a~vbeln,
       a~erdat,
       b~posnr,
       b~matnr
  FROM vbak AS a
  INNER JOIN vbap AS b ON a~vbeln = b~vbeln
  WHERE a~vkorg = '1000'
  INTO TABLE @DATA(lt_resultado).

" Vantagens:
" - Uma única query
" - Menos código
" - Processado na base de dados
```

### Quando Usar FOR ALL ENTRIES

```abap
" ✅ FOR ALL ENTRIES - Melhor quando:
" - Relações complexas
" - Precisa filtrar dados antes
" - Tabelas em bases de dados diferentes

" 1. Buscar e filtrar pedidos
SELECT * FROM vbak
  WHERE vkorg = '1000'
  INTO TABLE @DATA(lt_pedidos).

" Filtrar programaticamente
DELETE lt_pedidos WHERE erdat < '20240101'.

" 2. Buscar itens apenas dos pedidos filtrados
IF lt_pedidos IS NOT INITIAL.
  SELECT * FROM vbap
    FOR ALL ENTRIES IN @lt_pedidos
    WHERE vbeln = @lt_pedidos-vbeln
    INTO TABLE @DATA(lt_itens).
ENDIF.
```

---

## 🎯 UP TO n ROWS

### Limitar Resultados

```abap
" ✅ Sempre limitar quando não precisa de tudo
SELECT * FROM sflight
  WHERE carrid = 'LH'
  INTO TABLE @DATA(lt_voos)
  UP TO 100 ROWS.

" Especialmente útil em:
" - Telas de pesquisa (paginação)
" - Testes / Desenvolvimento
" - Validações (existe pelo menos um?)

" Verificar se existe pelo menos um registo
SELECT SINGLE carrid FROM scarr
  WHERE carrid = 'LH'
  INTO @DATA(lv_carrid).

IF sy-subrc = 0.
  " Companhia existe
ENDIF.
```

---

## 🔍 Agregações na Base de Dados

### Processar na BD vs. ABAP

```abap
" ❌ Processar em ABAP (lento)
SELECT * FROM sflight
  WHERE carrid = 'LH'
  INTO TABLE @DATA(lt_voos).

DATA lv_total TYPE p DECIMALS 2.
LOOP AT lt_voos INTO DATA(ls_voo).
  lv_total = lv_total + ls_voo-price.
ENDLOOP.

" ✅ Processar na base de dados (rápido)
SELECT SUM( price )
  FROM sflight
  WHERE carrid = 'LH'
  INTO @DATA(lv_total).

" Regra: Sempre que possível, processar na BD!
```

### Agregações Complexas

```abap
" ✅ Estatísticas calculadas na BD
SELECT carrid,
       COUNT( * ) AS num_voos,
       SUM( price ) AS receita,
       AVG( price AS DEC( 15,2 ) ) AS preco_medio,
       MAX( price ) AS preco_max,
       MIN( price ) AS preco_min
  FROM sflight
  GROUP BY carrid
  HAVING COUNT( * ) > 10
  INTO TABLE @DATA(lt_estatisticas).

" Muito mais rápido que fazer LOOPs em ABAP!
```

---

## 💾 Buffering

### Usar Buffer SAP

```abap
" Tabelas pequenas e raramente alteradas devem ter buffer
" Configurar em SE11 > Tabela > Technical Settings

" ✅ SELECT em tabelas bufferizadas é instantâneo
SELECT SINGLE * FROM t001  " Empresas (bufferizada)
  WHERE bukrs = '1000'
  INTO @DATA(ls_empresa).

" Verificar se tabela tem buffer: SE11 > Display

" Tabelas que devem ter buffer:
" - Tabelas de configuração
" - Tabelas pequenas (< 1000 linhas)
" - Tabelas raramente alteradas
" - Acedidas frequentemente
```

### BYPASSING BUFFER

```abap
" Quando precisa de dados em tempo real
SELECT * FROM ztabela
  BYPASSING BUFFER
  WHERE campo = @valor
  INTO TABLE @DATA(lt_data).

" Usar quando:
" - Dados devem estar atualizados
" - Após modificações recentes
" - Em processos críticos
```

---

## 🔧 Otimizações Avançadas

### SELECT com CASE

```abap
" ✅ Classificar na base de dados
SELECT carrid,
       connid,
       price,
       CASE
         WHEN price < 500 THEN 'Económico'
         WHEN price < 1000 THEN 'Normal'
         ELSE 'Premium'
       END AS categoria
  FROM sflight
  INTO TABLE @DATA(lt_classificados).

" Evita LOOP em ABAP para classificar
```

### Subconsultas (Subqueries)

```abap
" ✅ Filtrar com subquery
SELECT * FROM vbak
  WHERE vbeln IN (
    SELECT vbeln FROM vbap WHERE matnr = 'MAT001'
  )
  INTO TABLE @DATA(lt_pedidos).

" Alternativa com JOIN (geralmente mais rápido):
SELECT DISTINCT a~*
  FROM vbak AS a
  INNER JOIN vbap AS b ON a~vbeln = b~vbeln
  WHERE b~matnr = 'MAT001'
  INTO CORRESPONDING FIELDS OF TABLE @lt_pedidos.
```

### Uso de WITH (Common Table Expression)

```abap
" ✅ CTE para queries complexas (HANA)
WITH +pedidos_2024 AS (
  SELECT vbeln, kunnr, netwr
  FROM vbak
  WHERE erdat BETWEEN '20240101' AND '20241231'
)
SELECT p~vbeln,
       p~netwr,
       k~name1
  FROM +pedidos_2024 AS p
  INNER JOIN kna1 AS k ON p~kunnr = k~kunnr
  WHERE p~netwr > 10000
  INTO TABLE @DATA(lt_resultado).
```

---

## 📈 Ferramentas de Análise

### Runtime Analysis (SAT/SE30)

```abap
" Analisar performance do código
" 1. Transaction SAT (ou SE30)
" 2. Executar programa
" 3. Analisar resultados

" Ver:
" - Tempo de cada SELECT
" - Número de registos lidos
" - Uso de buffer
" - Operações mais lentas
```

### SQL Trace (ST05)

```abap
" Analisar queries SQL
" 1. Transaction ST05
" 2. Ativar SQL Trace
" 3. Executar programa
" 4. Desativar e analisar

" Identificar:
" - SELECTs sem índice
" - Table scans
" - Queries duplicadas
" - Tempo de execução
```

### Code Inspector (SCI)

```abap
" Verificar performance do código
" Transaction SCI ou ATC

" Detecta:
" - SELECT em LOOPs
" - SELECT * desnecessário
" - FOR ALL ENTRIES sem validação
" - Operações ineficientes
```

---

## 💡 Exemplos de Otimização

### Antes e Depois

#### Exemplo 1: Buscar Clientes e Pedidos

```abap
" ❌ ANTES - Muito lento (1000+ SELECTs)
SELECT * FROM kna1
  WHERE land1 = 'PT'
  INTO TABLE @DATA(lt_clientes).

LOOP AT lt_clientes INTO DATA(ls_cliente).
  SELECT COUNT( * ) FROM vbak
    WHERE kunnr = @ls_cliente-kunnr
    INTO @DATA(lv_num_pedidos).
  
  ls_cliente-num_pedidos = lv_num_pedidos.
  MODIFY lt_clientes FROM ls_cliente.
ENDLOOP.

" ✅ DEPOIS - Rápido (2 SELECTs apenas)
SELECT * FROM kna1
  WHERE land1 = 'PT'
  INTO TABLE @lt_clientes.

IF lt_clientes IS NOT INITIAL.
  SELECT kunnr, COUNT( * ) AS num_pedidos
    FROM vbak
    FOR ALL ENTRIES IN @lt_clientes
    WHERE kunnr = @lt_clientes-kunnr
    GROUP BY kunnr
    INTO TABLE @DATA(lt_contagens).
  
  LOOP AT lt_clientes ASSIGNING FIELD-SYMBOL(<ls_cliente>).
    READ TABLE lt_contagens INTO DATA(ls_count)
      WITH KEY kunnr = <ls_cliente>-kunnr.
    IF sy-subrc = 0.
      <ls_cliente>-num_pedidos = ls_count-num_pedidos.
    ENDIF.
  ENDLOOP.
ENDIF.
```

#### Exemplo 2: Relatório de Vendas

```abap
" ❌ ANTES - Processar tudo em ABAP
SELECT * FROM vbak INTO TABLE @DATA(lt_pedidos).

DATA: lv_total TYPE p DECIMALS 2,
      lv_count TYPE i.

LOOP AT lt_pedidos INTO DATA(ls_pedido) WHERE vkorg = '1000'.
  lv_total = lv_total + ls_pedido-netwr.
  lv_count = lv_count + 1.
ENDLOOP.

DATA(lv_media) = lv_total / lv_count.

" ✅ DEPOIS - Processar na base de dados
SELECT COUNT( * ) AS total_pedidos,
       SUM( netwr ) AS valor_total,
       AVG( netwr AS DEC( 15,2 ) ) AS valor_medio
  FROM vbak
  WHERE vkorg = '1000'
  INTO @DATA(ls_estatisticas).
```

---

## 🎯 Checklist de Otimização

### ✅ Verificar Sempre

- [ ] Não há SELECT dentro de LOOPs?
- [ ] SELECT usa campos indexados no WHERE?
- [ ] Seleciona apenas campos necessários (não SELECT *)?
- [ ] Usa UP TO ROWS quando apropriado?
- [ ] FOR ALL ENTRIES verifica se tabela não está vazia?
- [ ] Agregações são feitas na BD (não em LOOP)?
- [ ] Usa JOIN em vez de múltiplos SELECTs?
- [ ] Tabelas pequenas têm buffer ativado?

### 📊 Medir Performance

1. **Runtime Analysis (SAT)**: Tempo de execução
2. **SQL Trace (ST05)**: Análise detalhada de queries
3. **Code Inspector (SCI)**: Problemas de código
4. **ABAP Test Cockpit (ATC)**: Verificação automática

---

## 🔗 Próximos Passos

- **[Performance](../performance/index.md)** - Otimizações gerais ABAP
- **[FOR ALL ENTRIES](../performance/1_for_all_entries.md)** - Uso avançado
- **[Índices](../performance/2_indices.md)** - Como criar e usar

---

**Tags:** `#SQL` `#Performance` `#Otimização` `#Best-Practices` `#FOR-ALL-ENTRIES`
