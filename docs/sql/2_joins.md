---
tags:
  - ABAP
  - SQL
  - JOIN
  - INNER JOIN
  - LEFT JOIN
---

# JOINs em ABAP SQL

Os JOINs permitem combinar dados de múltiplas tabelas numa única consulta.

---

## 🔹 Tipos de JOIN

| Tipo | Descrição |
|------|-----------|
| **INNER JOIN** | Retorna apenas registos que existem em **ambas** as tabelas |
| **LEFT OUTER JOIN** | Retorna todos da esquerda + correspondentes da direita |
| **RIGHT OUTER JOIN** | Retorna todos da direita + correspondentes da esquerda |

!!! note "Nota"
    ABAP SQL suporta principalmente INNER JOIN e LEFT OUTER JOIN.

---

## 🔹 INNER JOIN

### Exemplo 1: Voos com Nome da Companhia

```abap
REPORT z_inner_join_demo.

DATA: BEGIN OF ls_flight,
        carrid   TYPE s_carr_id,
        connid   TYPE s_conn_id,
        carrname TYPE s_carrname,
        fldate   TYPE s_date,
        price    TYPE s_price,
      END OF ls_flight.

DATA lt_flights LIKE TABLE OF ls_flight.

START-OF-SELECTION.

  " Juntar tabelas SFLIGHT (voos) e SCARR (companhias)
  SELECT a~carrid,
         a~connid,
         b~carrname,
         a~fldate,
         a~price
    FROM sflight AS a
    INNER JOIN scarr AS b ON a~carrid = b~carrid
    INTO TABLE @lt_flights
    WHERE a~carrid = 'LH'
    UP TO 20 ROWS.

  LOOP AT lt_flights INTO ls_flight.
    WRITE: / ls_flight-carrid,
             ls_flight-connid,
             ls_flight-carrname,
             ls_flight-fldate,
             ls_flight-price.
  ENDLOOP.
```

### Exemplo 2: Conexões com Aeroportos

```abap
" Juntar SPFLI (conexões) com T005T (países)
SELECT a~carrid,
       a~connid,
       a~cityfrom,
       a~cityto,
       b~landx AS pais
  FROM spfli AS a
  INNER JOIN t005t AS b ON a~countryfr = b~land1
  INTO TABLE @DATA(lt_connections)
  WHERE b~spras = @sy-langu
  UP TO 50 ROWS.
```

---

## 🔹 LEFT OUTER JOIN

Retorna **todos** os registos da tabela à esquerda, mesmo que não haja correspondência na direita.

```abap
REPORT z_left_join_demo.

DATA: BEGIN OF ls_result,
        carrid   TYPE s_carr_id,
        carrname TYPE s_carrname,
        connid   TYPE s_conn_id,
        cityfrom TYPE s_from_cit,
        cityto   TYPE s_to_city,
      END OF ls_result.

DATA lt_results LIKE TABLE OF ls_result.

START-OF-SELECTION.

  " Buscar todas as companhias, mesmo que não tenham conexões
  SELECT a~carrid,
         a~carrname,
         b~connid,
         b~cityfrom,
         b~cityto
    FROM scarr AS a
    LEFT OUTER JOIN spfli AS b ON a~carrid = b~carrid
    INTO TABLE @lt_results
    UP TO 100 ROWS.

  LOOP AT lt_results INTO ls_result.
    WRITE: / ls_result-carrid,
             ls_result-carrname,
             ls_result-connid.
  ENDLOOP.
```

!!! tip "Uso"
    Use LEFT OUTER JOIN quando precisar de **todos** os registos da primeira tabela, independentemente de existirem na segunda.

---

## 🔹 JOIN com Múltiplas Tabelas

Pode juntar 3 ou mais tabelas:

```abap
" Juntar SFLIGHT + SPFLI + SCARR
SELECT f~carrid,
       c~carrname,
       f~connid,
       p~cityfrom,
       p~cityto,
       f~fldate,
       f~price,
       f~currency
  FROM sflight AS f
  INNER JOIN spfli AS p ON f~carrid = p~carrid
                       AND f~connid = p~connid
  INNER JOIN scarr AS c ON f~carrid = c~carrid
  INTO TABLE @DATA(lt_full_info)
  WHERE f~carrid = 'LH'
  UP TO 20 ROWS.
```

---

## 🔹 JOIN com Condições Complexas

```abap
" JOIN com múltiplas condições
SELECT a~vbeln,
       a~kunnr,
       b~name1,
       a~netwr
  FROM vbak AS a
  INNER JOIN kna1 AS b ON a~kunnr = b~kunnr
  INTO TABLE @DATA(lt_orders)
  WHERE a~erdat >= '20240101'
    AND a~auart = 'OR'
    AND b~land1 = 'PT'.
```

---

## 🔹 Comparação: JOIN vs FOR ALL ENTRIES

### ❌ Evitar (SELECT em LOOP)

```abap
" Má prática - múltiplas queries
SELECT * FROM scarr INTO TABLE @DATA(lt_carriers).

LOOP AT lt_carriers INTO DATA(ls_carrier).
  SELECT * FROM spfli
    INTO TABLE @DATA(lt_temp)
    WHERE carrid = @ls_carrier-carrid.
  " Processamento...
ENDLOOP.
```

### ✅ Opção 1: INNER JOIN

```abap
" Boa prática - uma única query
SELECT a~carrid,
       a~carrname,
       b~connid,
       b~cityfrom
  FROM scarr AS a
  INNER JOIN spfli AS b ON a~carrid = b~carrid
  INTO TABLE @DATA(lt_results).
```

### ✅ Opção 2: FOR ALL ENTRIES

```abap
" Alternativa quando JOIN não é possível
SELECT * FROM scarr INTO TABLE @DATA(lt_carriers) UP TO 100 ROWS.

IF lt_carriers IS NOT INITIAL.
  SELECT * FROM spfli
    INTO TABLE @DATA(lt_connections)
    FOR ALL ENTRIES IN @lt_carriers
    WHERE carrid = @lt_carriers-carrid.
ENDIF.
```

---

## 🔹 Exemplo Completo: Relatório de Voos

```abap
*&---------------------------------------------------------------------*
*& Report Z_JOIN_RELATORIO_VOOS
*&---------------------------------------------------------------------*
REPORT z_join_relatorio_voos.

PARAMETERS: p_carrid TYPE s_carr_id DEFAULT 'LH',
            p_datum  TYPE s_date DEFAULT sy-datum.

DATA: BEGIN OF ls_voo,
        carrid   TYPE s_carr_id,
        carrname TYPE s_carrname,
        connid   TYPE s_conn_id,
        cityfrom TYPE s_from_cit,
        cityto   TYPE s_to_city,
        fldate   TYPE s_date,
        price    TYPE s_price,
        currency TYPE s_currcode,
        seats    TYPE s_seatsmax,
      END OF ls_voo.

DATA lt_voos LIKE TABLE OF ls_voo.

START-OF-SELECTION.

  " JOIN triplo: SFLIGHT + SPFLI + SCARR
  SELECT f~carrid,
         c~carrname,
         f~connid,
         p~cityfrom,
         p~cityto,
         f~fldate,
         f~price,
         f~currency,
         f~seatsmax
    FROM sflight AS f
    INNER JOIN spfli AS p ON f~carrid = p~carrid
                         AND f~connid = p~connid
    INNER JOIN scarr AS c ON f~carrid = c~carrid
    INTO TABLE @lt_voos
    WHERE f~carrid = @p_carrid
      AND f~fldate >= @p_datum
    ORDER BY f~fldate ASCENDING.

  IF sy-subrc = 0.
    " Cabeçalho
    WRITE: / 'Companhia:', p_carrid.
    SKIP.
    WRITE: / 'Ligação', 12 'De', 25 'Para', 40 'Data', 55 'Preço', 70 'Lugares'.
    ULINE.

    " Dados
    LOOP AT lt_voos INTO ls_voo.
      WRITE: / ls_voo-connid,
               12 ls_voo-cityfrom,
               25 ls_voo-cityto,
               40 ls_voo-fldate,
               55 ls_voo-price,
               65 ls_voo-currency,
               70 ls_voo-seats.
    ENDLOOP.

    SKIP.
    WRITE: / |Total de voos: { lines( lt_voos ) }|.
  ELSE.
    WRITE: / 'Nenhum voo encontrado para os critérios selecionados.'.
  ENDIF.
```

---

## 💡 Boas Práticas

| ✅ Fazer | ❌ Evitar |
|---------|-----------|
| Usar INNER JOIN para performance | SELECT em LOOP |
| Especificar apenas campos necessários | JOIN de muitas tabelas sem necessidade |
| Usar aliases (AS a, AS b) | Nomes de tabela completos |
| WHERE nas condições principais | Filtros só no lado aplicacional |
| Verificar sy-subrc | Assumir que dados existem |

---

## 🚀 Próximo Passo

Aprenda sobre [Agregações e GROUP BY](3_agregacoes.md) para calcular totais, médias e contagens.
