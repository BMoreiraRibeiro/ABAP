---
tags:
  - ABAP
  - Security
  - SQL Injection
  - Dynamic SQL
---

# SQL Injection

## 📋 Visão Geral

**SQL Injection** é uma técnica de ataque onde código SQL malicioso é inserido em queries, permitindo acesso não autorizado a dados.

---

## ⚠️ O Problema

### Código Vulnerável

```abap
PARAMETERS: p_name TYPE string.

" ❌ VULNERÁVEL!
DATA(lv_sql) = |SELECT * FROM kna1 WHERE name1 = '{ p_name }'|.

EXEC SQL.
  :lv_sql
ENDEXEC.
```

**Ataque:**
```
User digita: admin' OR '1'='1
Query gerada: SELECT * FROM kna1 WHERE name1 = 'admin' OR '1'='1'
Resultado: Retorna TODOS os clientes! (1=1 sempre verdade)
```

---

## 🔒 Soluções

### 1️⃣ Usar @-Escaping (Recomendado)

```abap
PARAMETERS: p_name TYPE string.

" ✅ SEGURO
SELECT * FROM kna1
  WHERE name1 = @p_name  " @ protege contra injection
  INTO TABLE @DATA(lt_customers).
```

**Por quê funciona?**
- `@p_name` é tratado como **valor**, não código SQL
- Caracteres especiais (`'`, `--`, `;`) são escapados automaticamente

---

### 2️⃣ Validar e Sanitizar Input

```abap
PARAMETERS: p_name TYPE string.

" Validar caracteres permitidos
IF p_name CA `';--*?/\`.
  MESSAGE 'Caracteres inválidos no nome' TYPE 'E'.
  RETURN.
ENDIF.

" Limitar tamanho
IF strlen( p_name ) > 35.
  MESSAGE 'Nome muito longo' TYPE 'E'.
  RETURN.
ENDIF.

" Usar com @-escaping
SELECT * FROM kna1
  WHERE name1 = @p_name
  INTO TABLE @DATA(lt_customers).
```

---

### 3️⃣ Prepared Statements (ADBC)

```abap
DATA: lo_sql    TYPE REF TO cl_sql_statement,
      lo_result TYPE REF TO cl_sql_result_set.

lo_sql = cl_sql_statement=>create_instance( ).

" ✅ Prepared statement
DATA(lv_sql) = 'SELECT * FROM kna1 WHERE name1 = ?'.

lo_result = lo_sql->execute_query(
  statement = lv_sql
  p_bnd_tab = VALUE #( ( p_name ) ) ).  " Parâmetro seguro

lo_result->next_package( IMPORTING et_data = lt_customers ).
```

---

## 💡 Exemplos de Ataques

### Ataque 1: Bypass de Autenticação

```abap
" Código vulnerável
SELECT SINGLE * FROM zusr_login
  WHERE username = p_user
    AND password = p_pass.

" Ataque:
p_user = "admin"
p_pass = "' OR '1'='1"

" Query gerada:
" SELECT * WHERE username = 'admin' AND password = '' OR '1'='1'
" Resultado: Login sem senha!
```

**Proteção:**
```abap
" ✅ SEGURO
SELECT SINGLE * FROM zusr_login
  WHERE username = @p_user
    AND password = @p_pass.
```

---

### Ataque 2: Data Exfiltration

```abap
" Vulnerável
p_id = "100001' UNION SELECT username, password, '0' FROM zusr_login--"

" Query gerada:
" SELECT name, city, country FROM kna1
"   WHERE kunnr = '100001'
"   UNION SELECT username, password, '0' FROM zusr_login--'

" Resultado: Expõe tabela de senhas!
```

**Proteção:**
```abap
" ✅ SEGURO
SELECT name1, ort01, land1 FROM kna1
  WHERE kunnr = @p_id
  INTO TABLE @DATA(lt_data).
```

---

### Ataque 3: Data Destruction

```abap
" Vulnerável
p_table = "kna1'; DELETE FROM zusr_login; SELECT * FROM kna1 WHERE '1'='1"

" Queries geradas:
" 1. SELECT * FROM kna1'
" 2. DELETE FROM zusr_login
" 3. SELECT * FROM kna1 WHERE '1'='1'

" Resultado: Tabela deletada!
```

**Proteção:**
```abap
" ✅ Validar nome da tabela
DATA(lt_allowed_tables) = VALUE stringtab( ( 'KNA1' ) ( 'MARA' ) ).

IF NOT line_exists( lt_allowed_tables[ table_value = to_upper( p_table ) ] ).
  MESSAGE 'Tabela não permitida' TYPE 'E'.
  RETURN.
ENDIF.

" Usar @-escaping
SELECT * FROM (p_table)
  UP TO 100 ROWS
  INTO TABLE @DATA(lt_data).
```

---

## 🛡️ Dynamic SQL Seguro

### WHERE Dinâmico - ERRADO

```abap
" ❌ VULNERÁVEL
DATA(lv_where) = |name1 = '{ p_name }'|.

SELECT * FROM kna1
  WHERE (lv_where)
  INTO TABLE @DATA(lt_data).
```

### WHERE Dinâmico - CORRETO

```abap
" ✅ SEGURO
DATA(lv_where) = |name1 = @p_name|.  " @ no string dinâmico

SELECT * FROM kna1
  WHERE (lv_where)
  INTO TABLE @DATA(lt_data).
```

### Validar Campos Dinâmicos

```abap
" ✅ Whitelist de campos permitidos
DATA(lt_allowed_fields) = VALUE stringtab(
  ( 'NAME1' ) ( 'ORT01' ) ( 'LAND1' ) ).

IF NOT line_exists( lt_allowed_fields[ table_value = to_upper( p_field ) ] ).
  MESSAGE 'Campo não permitido' TYPE 'E'.
  RETURN.
ENDIF.

" Usar campo validado
SELECT * FROM kna1
  ORDER BY (p_field)
  INTO TABLE @DATA(lt_data).
```

---

## 🎓 Exemplo Completo: Search Seguro

```abap
*&---------------------------------------------------------------------*
*& Report Z_SECURE_SEARCH
*&---------------------------------------------------------------------*
REPORT z_secure_search.

PARAMETERS: p_search TYPE string.

START-OF-SELECTION.
  
  " ═══ VALIDAÇÃO DE INPUT ═══
  
  " 1. Não vazio
  IF p_search IS INITIAL.
    MESSAGE 'Digite algo para buscar' TYPE 'I'.
    RETURN.
  ENDIF.
  
  " 2. Tamanho máximo
  IF strlen( p_search ) > 35.
    MESSAGE 'Busca muito longa (máx 35 caracteres)' TYPE 'E'.
    RETURN.
  ENDIF.
  
  " 3. Caracteres perigosos
  IF p_search CA `';--*/\`.
    MESSAGE 'Caracteres não permitidos: ; -- * / \' TYPE 'E'.
    RETURN.
  ENDIF.
  
  " ═══ QUERY SEGURA ═══
  
  SELECT * FROM kna1
    WHERE name1 LIKE @p_search  " ✅ @-escaping
       OR ort01 LIKE @p_search
    INTO TABLE @DATA(lt_customers)
    UP TO 100 ROWS.
  
  " ═══ RESULTADO ═══
  
  IF lt_customers IS INITIAL.
    MESSAGE 'Nenhum cliente encontrado' TYPE 'I'.
  ELSE.
    LOOP AT lt_customers INTO DATA(ls_customer).
      WRITE: / ls_customer-kunnr, ls_customer-name1, ls_customer-ort01.
    ENDLOOP.
    
    WRITE: / |Total: { lines( lt_customers ) } clientes|.
  ENDIF.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. SEMPRE usar @-escaping
SELECT * FROM kna1
  WHERE kunnr = @p_customer.  " ✅

" 2. Validar inputs
IF p_input CA `';--*`.
  MESSAGE 'Caracteres inválidos' TYPE 'E'.
ENDIF.

" 3. Whitelist para valores dinâmicos
IF NOT line_exists( lt_allowed[ table_value = p_table ] ).
  MESSAGE 'Não permitido' TYPE 'E'.
ENDIF.

" 4. Limitar resultados
SELECT * ... UP TO 100 ROWS.  " ✅

" 5. Prepared statements para ADBC
lo_sql->execute_query(
  statement = 'SELECT * FROM kna1 WHERE kunnr = ?'
  p_bnd_tab = VALUE #( ( p_customer ) ) ).
```

### ❌ Evitar

```abap
" 1. SQL dinâmico sem @-escaping
DATA(lv_sql) = |SELECT * WHERE id = '{ p_id }'|.  " ❌

" 2. Concatenar strings em WHERE
lv_where = |name = '{ p_name }'|.  " ❌

" 3. EXEC SQL com input do usuário
EXEC SQL.
  SELECT * WHERE :p_field = :p_value
ENDEXEC.  " ❌

" 4. Não validar inputs
SELECT * WHERE (p_field) = @p_value.  " ❌ p_field não validado

" 5. Confiar no frontend
" ❌ SEMPRE validar no backend também
```

---

## 🔍 Detectar Vulnerabilidades

### Code Inspector (SCI)

```
SE38 → Code Inspector (Ctrl+Shift+F3)
Check Variant: SECURITY
```

### Revisão Manual

Procurar por:
- `WHERE (lv_variable)` sem validação
- Concatenação de strings em SQL
- `EXEC SQL` com inputs dinâmicos
- Falta de `@` antes de variáveis

---

## 🔗 Próximos Passos

- **[Práticas Seguras](5_praticas_seguras.md)** - Mais segurança no código
- **[AUTHORITY-CHECK](2_authority_check.md)** - Controle de acesso
- **[SQL Otimizado](../sql/6_otimizacoes.md)** - Performance + Segurança

---

**Tags:** `#SQL-Injection` `#Security` `#Dynamic-SQL` `#Input-Validation`
