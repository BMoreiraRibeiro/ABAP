# WHERE Dinâmico

## 📋 Visão Geral

WHERE dinâmico permite construir consultas SQL flexíveis em tempo de execução, essencial para criar pesquisas configuráveis, filtros de utilizador e queries adaptáveis.

---

## 🎯 Sintaxe Básica

### WHERE com String

```abap
DATA lv_where TYPE string.

" Construir cláusula WHERE dinamicamente
lv_where = |carrid = 'LH'|.

SELECT * FROM sflight
  WHERE (lv_where)
  INTO TABLE @DATA(lt_voos).

WRITE: / lines( lt_voos ), 'voos encontrados'.
```

### WHERE com Variáveis

```abap
PARAMETERS: p_carrid TYPE s_carr_id,
            p_connid TYPE s_conn_id.

DATA lv_where TYPE string.

" Construir WHERE baseado em parâmetros
IF p_carrid IS NOT INITIAL.
  lv_where = |carrid = '{ p_carrid }'|.
ENDIF.

IF p_connid IS NOT INITIAL.
  IF lv_where IS NOT INITIAL.
    lv_where = |{ lv_where } AND connid = '{ p_connid }'|.
  ELSE.
    lv_where = |connid = '{ p_connid }'|.
  ENDIF.
ENDIF.

IF lv_where IS NOT INITIAL.
  SELECT * FROM sflight
    WHERE (lv_where)
    INTO TABLE @DATA(lt_result).
ELSE.
  SELECT * FROM sflight
    INTO TABLE @lt_result
    UP TO 100 ROWS.
ENDIF.
```

---

## 🔧 Construção de WHERE Dinâmico

### Método Simples - String Concatenation

```abap
DATA lv_where TYPE string.
DATA lt_conditions TYPE TABLE OF string.

" Adicionar condições
IF p_carrid IS NOT INITIAL.
  APPEND |carrid = '{ p_carrid }'| TO lt_conditions.
ENDIF.

IF p_price_min IS NOT INITIAL.
  APPEND |price >= { p_price_min }| TO lt_conditions.
ENDIF.

IF p_price_max IS NOT INITIAL.
  APPEND |price <= { p_price_max }| TO lt_conditions.
ENDIF.

IF p_date_from IS NOT INITIAL.
  APPEND |fldate >= '{ p_date_from }'| TO lt_conditions.
ENDIF.

" Juntar com AND
IF lt_conditions IS NOT INITIAL.
  lv_where = concat_lines_of( table = lt_conditions sep = ' AND ' ).
  
  SELECT * FROM sflight
    WHERE (lv_where)
    INTO TABLE @DATA(lt_voos).
ENDIF.
```

### Método Avançado - Classe de Utilidade

```abap
CLASS lcl_where_builder DEFINITION.
  PUBLIC SECTION.
    METHODS:
      add_condition
        IMPORTING iv_field TYPE string
                  iv_operator TYPE string DEFAULT '='
                  iv_value TYPE any,
      get_where
        RETURNING VALUE(rv_where) TYPE string.
        
  PRIVATE SECTION.
    DATA mt_conditions TYPE TABLE OF string.
ENDCLASS.

CLASS lcl_where_builder IMPLEMENTATION.
  METHOD add_condition.
    DATA lv_condition TYPE string.
    
    " Tratar strings com aspas
    IF iv_value IS SUPPLIED AND iv_value IS NOT INITIAL.
      DATA lv_value_str TYPE string.
      lv_value_str = |{ iv_value }|.
      
      " Adicionar aspas se for texto
      IF iv_value CO '0123456789. '.
        lv_condition = |{ iv_field } { iv_operator } { lv_value_str }|.
      ELSE.
        lv_condition = |{ iv_field } { iv_operator } '{ lv_value_str }'|.
      ENDIF.
      
      APPEND lv_condition TO mt_conditions.
    ENDIF.
  ENDMETHOD.
  
  METHOD get_where.
    IF mt_conditions IS NOT INITIAL.
      rv_where = concat_lines_of( table = mt_conditions sep = ' AND ' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Uso:
START-OF-SELECTION.
  DATA(lo_where) = NEW lcl_where_builder( ).
  
  IF p_carrid IS NOT INITIAL.
    lo_where->add_condition( iv_field = 'carrid' iv_value = p_carrid ).
  ENDIF.
  
  IF p_price_min IS NOT INITIAL.
    lo_where->add_condition( 
      iv_field = 'price' 
      iv_operator = '>=' 
      iv_value = p_price_min ).
  ENDIF.
  
  DATA(lv_where) = lo_where->get_where( ).
  
  IF lv_where IS NOT INITIAL.
    SELECT * FROM sflight
      WHERE (lv_where)
      INTO TABLE @DATA(lt_voos).
  ENDIF.
```

---

## 🔍 Operadores Dinâmicos

### Operadores de Comparação

```abap
DATA: lv_field    TYPE string VALUE 'price',
      lv_operator TYPE string VALUE '>=',
      lv_value    TYPE string VALUE '500'.

DATA(lv_where) = |{ lv_field } { lv_operator } { lv_value }|.

SELECT * FROM sflight
  WHERE (lv_where)
  INTO TABLE @DATA(lt_result).

" Exemplos de operadores:
" =  : Igual
" <> : Diferente
" >  : Maior
" >= : Maior ou igual
" <  : Menor
" <= : Menor ou igual
```

### IN e BETWEEN

```abap
" Operador IN
DATA lv_carrids TYPE string VALUE |carrid IN ('AA', 'LH', 'BA')|.

SELECT * FROM sflight
  WHERE (lv_carrids)
  INTO TABLE @DATA(lt_voos).

" Operador BETWEEN
DATA lv_range TYPE string VALUE |price BETWEEN 100 AND 500|.

SELECT * FROM sflight
  WHERE (lv_range)
  INTO TABLE @lt_voos.
```

### LIKE para Padrões

```abap
DATA lv_pattern TYPE string.

" Construir padrão LIKE
lv_pattern = |carrname LIKE '%Air%'|.

SELECT * FROM scarr
  WHERE (lv_pattern)
  INTO TABLE @DATA(lt_carriers).

" Padrões:
" %    = qualquer sequência de caracteres
" _    = um único caractere
```

---

## 📦 SELECT-OPTIONS Dinâmico

### Usar SELECT-OPTIONS em WHERE

```abap
SELECT-OPTIONS: s_carrid FOR sflight-carrid,
                s_connid FOR sflight-connid,
                s_fldate FOR sflight-fldate,
                s_price  FOR sflight-price.

" SELECT-OPTIONS já funciona dinamicamente!
SELECT * FROM sflight
  WHERE carrid IN @s_carrid
    AND connid IN @s_connid
    AND fldate IN @s_fldate
    AND price  IN @s_price
  INTO TABLE @DATA(lt_voos).
```

### Construir WHERE com Ranges

```abap
DATA: lr_carrid TYPE RANGE OF s_carr_id,
      lr_price  TYPE RANGE OF s_price.

" Preencher ranges
lr_carrid = VALUE #(
  ( sign = 'I' option = 'EQ' low = 'AA' )
  ( sign = 'I' option = 'EQ' low = 'LH' )
).

lr_price = VALUE #(
  ( sign = 'I' option = 'BT' low = '100' high = '500' )
).

" Usar em SELECT
SELECT * FROM sflight
  WHERE carrid IN @lr_carrid
    AND price IN @lr_price
  INTO TABLE @DATA(lt_voos).
```

---

## 🎨 Exemplos Práticos

### Pesquisa Flexível de Clientes

```abap
REPORT z_pesquisa_clientes.

PARAMETERS: p_kunnr TYPE kunnr,
            p_name1 TYPE name1,
            p_ort01 TYPE ort01,
            p_land1 TYPE land1.

START-OF-SELECTION.
  DATA lt_where TYPE TABLE OF string.
  
  " Construir WHERE baseado em input
  IF p_kunnr IS NOT INITIAL.
    APPEND |kunnr = '{ p_kunnr }'| TO lt_where.
  ENDIF.
  
  IF p_name1 IS NOT INITIAL.
    APPEND |name1 LIKE '%{ p_name1 }%'| TO lt_where.
  ENDIF.
  
  IF p_ort01 IS NOT INITIAL.
    APPEND |ort01 = '{ p_ort01 }'| TO lt_where.
  ENDIF.
  
  IF p_land1 IS NOT INITIAL.
    APPEND |land1 = '{ p_land1 }'| TO lt_where.
  ENDIF.
  
  IF lt_where IS NOT INITIAL.
    DATA(lv_where) = concat_lines_of( table = lt_where sep = ' AND ' ).
    
    SELECT * FROM kna1
      WHERE (lv_where)
      INTO TABLE @DATA(lt_clientes)
      UP TO 100 ROWS.
    
    WRITE: / lines( lt_clientes ), 'clientes encontrados'.
  ELSE.
    WRITE: / 'Especifique pelo menos um critério'.
  ENDIF.
```

### Filtro de Relatório Configurável

```abap
METHOD executar_relatorio.
  DATA: lt_campos    TYPE TABLE OF string,
        lt_where     TYPE TABLE OF string,
        lv_from      TYPE string VALUE 'sflight',
        lv_where_str TYPE string,
        lv_campos_str TYPE string.
  
  " Campos a selecionar (configurável)
  APPEND 'carrid' TO lt_campos.
  APPEND 'connid' TO lt_campos.
  APPEND 'fldate' TO lt_campos.
  APPEND 'price' TO lt_campos.
  
  " Condições (baseadas em config)
  IF config-filtro_companhia IS NOT INITIAL.
    APPEND |carrid = '{ config-filtro_companhia }'| TO lt_where.
  ENDIF.
  
  IF config-data_inicio IS NOT INITIAL.
    APPEND |fldate >= '{ config-data_inicio }'| TO lt_where.
  ENDIF.
  
  IF config-preco_minimo IS NOT INITIAL.
    APPEND |price >= { config-preco_minimo }| TO lt_where.
  ENDIF.
  
  " Construir query
  lv_campos_str = concat_lines_of( table = lt_campos sep = ', ' ).
  lv_where_str  = concat_lines_of( table = lt_where sep = ' AND ' ).
  
  " Executar SELECT dinâmico completo
  IF lv_where_str IS NOT INITIAL.
    SELECT (lv_campos_str)
      FROM (lv_from)
      WHERE (lv_where_str)
      INTO TABLE @DATA(lt_result).
  ENDIF.
  
  RETURN lt_result.
ENDMETHOD.
```

### Query Builder Interativo

```abap
CLASS lcl_query_builder DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      adicionar_filtro
        IMPORTING iv_campo    TYPE string
                  iv_operador TYPE string
                  iv_valor    TYPE string,
      limpar_filtros,
      executar
        RETURNING VALUE(rt_dados) TYPE REF TO data.
        
  PRIVATE SECTION.
    DATA: mt_filtros TYPE TABLE OF string,
          mv_tabela  TYPE string.
ENDCLASS.

CLASS lcl_query_builder IMPLEMENTATION.
  METHOD constructor.
    mv_tabela = 'SFLIGHT'.
  ENDMETHOD.
  
  METHOD adicionar_filtro.
    DATA lv_filtro TYPE string.
    
    " Adicionar aspas a strings
    IF iv_valor CO '0123456789. '.
      lv_filtro = |{ iv_campo } { iv_operador } { iv_valor }|.
    ELSE.
      lv_filtro = |{ iv_campo } { iv_operador } '{ iv_valor }'|.
    ENDIF.
    
    APPEND lv_filtro TO mt_filtros.
  ENDMETHOD.
  
  METHOD limpar_filtros.
    CLEAR mt_filtros.
  ENDMETHOD.
  
  METHOD executar.
    DATA lv_where TYPE string.
    
    IF mt_filtros IS NOT INITIAL.
      lv_where = concat_lines_of( table = mt_filtros sep = ' AND ' ).
      
      SELECT * FROM (mv_tabela)
        WHERE (lv_where)
        INTO TABLE @DATA(lt_result).
      
      " Retornar referência aos dados
      GET REFERENCE OF lt_result INTO rt_dados.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Uso:
DATA(lo_query) = NEW lcl_query_builder( ).

lo_query->adicionar_filtro( 
  iv_campo = 'carrid' 
  iv_operador = '=' 
  iv_valor = 'LH' 
).

lo_query->adicionar_filtro( 
  iv_campo = 'price' 
  iv_operador = '>=' 
  iv_valor = '500' 
).

DATA(lr_dados) = lo_query->executar( ).
```

---

## ⚠️ Segurança - SQL Injection

### ❌ PERIGOSO - Vulnerável a SQL Injection

```abap
" NUNCA fazer isto em produção!
DATA lv_input TYPE string.
lv_input = p_user_input.  " Input do utilizador

DATA(lv_where) = |carrid = '{ lv_input }'|.  " ❌ PERIGOSO!

SELECT * FROM sflight
  WHERE (lv_where)
  INTO TABLE @DATA(lt_data).

" Utilizador malicioso pode inserir: ' OR '1'='1
" Resultado: carrid = '' OR '1'='1' (retorna tudo!)
```

### ✅ SEGURO - Validar e Sanitizar

```abap
METHOD validar_input.
  " Apenas permitir caracteres seguros
  IF iv_input CA ';''"`'.  " Caracteres perigosos
    RAISE EXCEPTION TYPE cx_invalid_input.
  ENDIF.
  
  " Validar contra lista permitida
  IF iv_campo NOT IN ('carrid', 'connid', 'fldate').
    RAISE EXCEPTION TYPE cx_invalid_field.
  ENDIF.
  
  RETURN iv_input.
ENDMETHOD.

" Uso seguro:
TRY.
    DATA(lv_campo_seguro) = validar_input( p_user_input ).
    DATA(lv_where) = |{ lv_campo_seguro } = '{ p_valor }'|.
  CATCH cx_invalid_input cx_invalid_field.
    MESSAGE 'Input inválido' TYPE 'E'.
    RETURN.
ENDTRY.
```

---

## 🎯 Boas Práticas

### ✅ Fazer

```abap
" 1. Validar sempre inputs do utilizador
IF lv_input CA ';''"`'.
  " Rejeitar input perigoso
ENDIF.

" 2. Usar lista branca de campos permitidos
DATA lt_campos_validos TYPE TABLE OF string.
lt_campos_validos = VALUE #( ( 'carrid' ) ( 'connid' ) ( 'price' ) ).

IF NOT line_exists( lt_campos_validos[ table_line = lv_campo ] ).
  " Campo não permitido
ENDIF.

" 3. Usar @ para host variables sempre que possível
SELECT * FROM sflight
  WHERE carrid = @p_carrid  " ✅ Mais seguro
  INTO TABLE @DATA(lt_data).

" 4. Construir WHERE de forma estruturada
DATA(lo_builder) = NEW lcl_where_builder( ).
lo_builder->add_condition( ... ).
```

### ❌ Evitar

```abap
" 1. Concatenação direta de input do utilizador
lv_where = |campo = '{ p_input }'|.  " ❌ Perigoso

" 2. WHERE complexo numa única string gigante
lv_where = |carrid = 'AA' AND connid = '001' AND ...|.  " ❌ Difícil manter

" 3. Não validar tipos de dados
lv_where = |price = { lv_texto }|.  " ❌ Se lv_texto não for número?
```

---

## 📊 Performance

### Otimizações

1. **Índices**: Certifique-se que campos em WHERE têm índices
2. **Caching**: Cache queries frequentes
3. **Limitar resultados**: Use `UP TO n ROWS`
4. **Preparar statements**: Reutilize WHERE quando possível

```abap
" ✅ Bom: Reutilizar WHERE
DATA(lv_where) = construir_where( ).

SELECT * FROM sflight WHERE (lv_where) INTO TABLE @lt_voos1.
SELECT * FROM spfli WHERE (lv_where) INTO TABLE @lt_ligacoes.

" ❌ Mau: Reconstruir sempre
LOOP AT lt_queries INTO DATA(lv_query).
  DATA(lv_where_temp) = construir_where( ).  " Desnecessário
  SELECT...
ENDLOOP.
```

---

## 🔗 Próximos Passos

- **[Otimizações SQL](6_otimizacoes.md)** - Performance avançada
- **[Performance](../performance/index.md)** - Boas práticas gerais
- **[Security](../security/index.md)** - Prevenir SQL Injection

---

**Tags:** `#SQL` `#WHERE-Dinâmico` `#Dynamic-SQL` `#Security` `#SQL-Injection`
