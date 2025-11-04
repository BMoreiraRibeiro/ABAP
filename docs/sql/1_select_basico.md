---
tags:
  - ABAP
  - SQL
  - SELECT
  - Database
---

# SELECT Básico em ABAP

O comando `SELECT` é usado para ler dados das tabelas da base de dados SAP.

---

## 🔹 Sintaxe Básica

```abap
SELECT <campos>
  FROM <tabela>
  INTO <destino>
  [WHERE <condições>]
  [ORDER BY <campos>].
```

---

## 🔹 SELECT Simples

### Todos os campos, todas as linhas

```abap
REPORT z_select_basico.

DATA lt_scarr TYPE TABLE OF scarr.

START-OF-SELECTION.
  " Buscar todas as companhias aéreas
  SELECT * FROM scarr INTO TABLE @lt_scarr.
  
  " Mostrar resultados
  LOOP AT lt_scarr INTO DATA(ls_scarr).
    WRITE: / ls_scarr-carrid, ls_scarr-carrname.
  ENDLOOP.
```

---

## 🔹 SELECT com Campos Específicos

!!! tip "Boa Prática"
    Selecione apenas os campos que vai usar. Evite `SELECT *`.

```abap
DATA: BEGIN OF ls_flight,
        carrid TYPE s_carr_id,
        connid TYPE s_conn_id,
        price  TYPE s_price,
      END OF ls_flight.

DATA lt_flights LIKE TABLE OF ls_flight.

" ✅ Selecionar apenas campos necessários
SELECT carrid, connid, price
  FROM sflight
  INTO TABLE @lt_flights
  UP TO 100 ROWS.
```

---

## 🔹 SELECT SINGLE

Para buscar **apenas um registo**:

```abap
DATA ls_carrier TYPE scarr.

" Buscar companhia específica
SELECT SINGLE * FROM scarr
  INTO @ls_carrier
  WHERE carrid = 'LH'.

IF sy-subrc = 0.
  WRITE: / 'Encontrado:', ls_carrier-carrname.
ELSE.
  WRITE: / 'Não encontrado'.
ENDIF.
```

!!! warning "Atenção"
    Sempre verifique `sy-subrc` após SELECT SINGLE.
    - `sy-subrc = 0` → Registo encontrado
    - `sy-subrc ≠ 0` → Registo não encontrado

---

## 🔹 WHERE Condições

```abap
" Condição simples
SELECT * FROM sflight
  INTO TABLE @DATA(lt_flights)
  WHERE carrid = 'LH'
    AND fldate >= '20250101'.

" Múltiplas condições
SELECT * FROM sflight
  INTO TABLE @lt_flights
  WHERE carrid = 'LH'
    AND ( price > 500 OR currency = 'EUR' ).

" LIKE para padrões
SELECT * FROM scarr
  INTO TABLE @DATA(lt_carriers)
  WHERE carrname LIKE '%Airlines%'.

" IN para lista de valores
SELECT * FROM sflight
  INTO TABLE @lt_flights
  WHERE carrid IN ('LH', 'BA', 'AA').
```

---

## 🔹 Inline Declarations (ABAP 7.40+)

```abap
" Declaração inline da variável de destino
SELECT * FROM scarr INTO TABLE @DATA(lt_carriers).

" Inline em LOOP
SELECT * FROM sflight
  INTO TABLE @DATA(lt_flights)
  WHERE carrid = 'LH'.

LOOP AT lt_flights INTO DATA(ls_flight).
  WRITE: / ls_flight-connid, ls_flight-price.
ENDLOOP.
```

---

## 🔹 ORDER BY

```abap
" Ordenar por um campo
SELECT * FROM sflight
  INTO TABLE @DATA(lt_flights)
  WHERE carrid = 'LH'
  ORDER BY price ASCENDING.

" Ordenar por múltiplos campos
SELECT * FROM sflight
  INTO TABLE @lt_flights
  WHERE carrid = 'LH'
  ORDER BY fldate DESCENDING, connid ASCENDING.
```

---

## 🔹 DISTINCT (Valores Únicos)

```abap
" Buscar apenas valores únicos de carrid
SELECT DISTINCT carrid
  FROM sflight
  INTO TABLE @DATA(lt_carriers).
```

---

## 🔹 UP TO n ROWS

Limitar o número de registos retornados:

```abap
" Buscar apenas 10 registos
SELECT * FROM sflight
  INTO TABLE @DATA(lt_flights)
  UP TO 10 ROWS.
```

!!! tip "Performance"
    Use `UP TO n ROWS` quando não precisa de todos os dados para melhorar a performance.

---

## 🔹 INTO vs INTO TABLE vs APPENDING

```abap
" INTO TABLE - substitui o conteúdo
SELECT * FROM scarr INTO TABLE @lt_carriers.

" APPENDING TABLE - adiciona ao conteúdo existente
SELECT * FROM scarr APPENDING TABLE @lt_carriers WHERE carrid = 'LH'.

" INTO (linha a linha) - menos eficiente
SELECT * FROM scarr INTO @DATA(ls_carrier).
  WRITE: / ls_carrier-carrid.
ENDSELECT.
```

!!! warning "Performance"
    Evite `SELECT...ENDSELECT` (linha a linha). Use `INTO TABLE` sempre que possível.

---

## 🔹 Exemplo Completo

```abap
*&---------------------------------------------------------------------*
*& Report Z_SELECT_BASICO_DEMO
*&---------------------------------------------------------------------*
REPORT z_select_basico_demo.

PARAMETERS: p_carrid TYPE s_carr_id DEFAULT 'LH'.

DATA: BEGIN OF ls_flight,
        carrid TYPE s_carr_id,
        connid TYPE s_conn_id,
        fldate TYPE s_date,
        price  TYPE s_price,
        curr   TYPE s_currcode,
      END OF ls_flight.

DATA lt_flights LIKE TABLE OF ls_flight.

START-OF-SELECTION.

  " Buscar voos da companhia selecionada
  SELECT carrid, connid, fldate, price, currency
    FROM sflight
    INTO TABLE @lt_flights
    WHERE carrid = @p_carrid
      AND fldate >= @sy-datum
    ORDER BY fldate ASCENDING, price ASCENDING
    UP TO 50 ROWS.

  IF sy-subrc = 0.
    WRITE: / |Encontrados { lines( lt_flights ) } voos para { p_carrid }|.
    SKIP.
    
    LOOP AT lt_flights INTO ls_flight.
      WRITE: / ls_flight-connid,
               ls_flight-fldate,
               ls_flight-price,
               ls_flight-curr.
    ENDLOOP.
  ELSE.
    WRITE: / 'Nenhum voo encontrado.'.
  ENDIF.
```

---

## 💡 Boas Práticas

| ✅ Fazer | ❌ Evitar |
|---------|-----------|
| `SELECT campo1, campo2` | `SELECT *` |
| `INTO TABLE @lt_data` | `SELECT...ENDSELECT` |
| `WHERE` com campos indexados | WHERE sem índices |
| `UP TO n ROWS` | Buscar todos sem limite |
| Verificar `sy-subrc` | Assumir que dados existem |
| `@` para host variables (7.40+) | Sintaxe antiga |

---

## 🚀 Próximo Passo

Depois de dominar os SELECTs básicos, aprenda sobre [JOINs](2_joins.md) para combinar dados de múltiplas tabelas.
