---
tags:
  - ABAP
  - Testing
  - Assertions
  - Verification
---

# Assertions

## 📋 Visão Geral

**Assertions** são verificações que determinam se um teste **passou** ou **falhou**. Classe `cl_abap_unit_assert` fornece métodos estáticos para validar resultados.

---

## ✅ Assertions Principais

### assert_equals

Verifica se dois valores são **iguais**.

```abap
cl_abap_unit_assert=>assert_equals(
  act = lv_resultado     " Valor atual
  exp = lv_esperado      " Valor esperado
  msg = 'Mensagem de erro se falhar' ).

" Exemplo
DATA(lv_soma) = 5 + 3.

cl_abap_unit_assert=>assert_equals(
  act = lv_soma
  exp = 8
  msg = '5 + 3 deveria ser 8' ).
```

---

### assert_not_equals

Verifica se dois valores são **diferentes**.

```abap
cl_abap_unit_assert=>assert_not_equals(
  act = lv_new_id
  exp = lv_old_id
  msg = 'Novo ID deveria ser diferente do antigo' ).
```

---

### assert_true / assert_false

Verifica condição booleana.

```abap
" assert_true
cl_abap_unit_assert=>assert_true(
  act = lv_is_valid
  msg = 'Validação deveria retornar true' ).

" assert_false
cl_abap_unit_assert=>assert_false(
  act = lv_has_errors
  msg = 'Não deveria ter erros' ).
```

---

### assert_initial / assert_not_initial

Verifica se variável está vazia ou preenchida.

```abap
" assert_initial (vazio)
cl_abap_unit_assert=>assert_initial(
  act = lv_result
  msg = 'Resultado deveria estar vazio' ).

" assert_not_initial (preenchido)
cl_abap_unit_assert=>assert_not_initial(
  act = lv_customer_id
  msg = 'Customer ID não deveria estar vazio' ).
```

---

### assert_bound / assert_not_bound

Verifica se referência está atribuída.

```abap
" assert_bound (objeto existe)
cl_abap_unit_assert=>assert_bound(
  act = mo_processor
  msg = 'Objeto deveria estar criado' ).

" assert_not_bound (objeto não existe)
cl_abap_unit_assert=>assert_not_bound(
  act = mo_connection
  msg = 'Conexão não deveria estar ativa' ).
```

---

### assert_subrc

Verifica sy-subrc.

```abap
SELECT SINGLE * FROM kna1
  WHERE kunnr = lv_customer
  INTO @DATA(ls_customer).

cl_abap_unit_assert=>assert_subrc(
  act = sy-subrc
  exp = 0
  msg = |Cliente { lv_customer } deveria existir| ).
```

---

### assert_table_contains

Verifica se tabela contém linha específica.

```abap
cl_abap_unit_assert=>assert_table_contains(
  line  = VALUE ty_customer( kunnr = '100001' name1 = 'Test' )
  table = lt_customers
  msg   = 'Cliente de teste deveria estar na tabela' ).
```

---

### assert_differs

Verifica se valores são **fisicamente** diferentes (não apenas conteúdo).

```abap
DATA: lv_ref1 TYPE REF TO data,
      lv_ref2 TYPE REF TO data.

CREATE DATA lv_ref1.
CREATE DATA lv_ref2.

cl_abap_unit_assert=>assert_differs(
  act = lv_ref1
  exp = lv_ref2
  msg = 'Deveriam ser referências diferentes' ).
```

---

## 🚨 fail()

Forçar **falha** do teste.

```abap
IF lv_error_occurred = abap_true.
  cl_abap_unit_assert=>fail(
    msg = 'Operação não deveria ter falhado' ).
ENDIF.

" Uso comum: verificar se exceção foi lançada
TRY.
    mo_cut->dangerous_operation( ).
    
    " Se chegou aqui, não lançou exceção = FALHA
    cl_abap_unit_assert=>fail(
      msg = 'Deveria ter lançado exceção' ).
      
  CATCH cx_custom_error.
    " OK - exceção foi lançada
ENDTRY.
```

---

## 💡 Exemplos Práticos

### Teste com Múltiplas Assertions

```abap
METHOD test_create_order.
  " Arrange
  DATA(lv_customer) = '100001'.
  
  " Act
  DATA(ls_order) = mo_cut->create_order(
    iv_customer = lv_customer
    iv_material = 'MAT001'
    iv_quantity = 10 ).
  
  " Assert - múltiplas verificações
  cl_abap_unit_assert=>assert_not_initial(
    act = ls_order-vbeln
    msg = 'Order ID deveria ser gerado' ).
    
  cl_abap_unit_assert=>assert_equals(
    act = ls_order-kunnr
    exp = lv_customer
    msg = 'Customer deveria ser o mesmo' ).
    
  cl_abap_unit_assert=>assert_equals(
    act = ls_order-status
    exp = 'NEW'
    msg = 'Status inicial deveria ser NEW' ).
    
  cl_abap_unit_assert=>assert_true(
    act = xsdbool( ls_order-netwr > 0 )
    msg = 'Valor líquido deveria ser maior que zero' ).
ENDMETHOD.
```

---

### Testar Exceções

```abap
METHOD test_divide_by_zero_raises_exception.
  " Arrange
  DATA(lv_a) = 10.
  DATA(lv_b) = 0.
  
  " Act & Assert
  TRY.
      DATA(lv_result) = mo_calculator->divide( iv_a = lv_a iv_b = lv_b ).
      
      " Se chegou aqui = FALHA
      cl_abap_unit_assert=>fail(
        msg = 'Deveria lançar cx_sy_zerodivide' ).
        
    CATCH cx_sy_zerodivide.
      " Sucesso! Exceção esperada foi lançada
  ENDTRY.
ENDMETHOD.
```

---

### Testar Tabelas

```abap
METHOD test_filter_returns_correct_items.
  " Arrange
  DATA(lt_input) = VALUE ty_items_tab(
    ( id = '1' status = 'ACTIVE' )
    ( id = '2' status = 'INACTIVE' )
    ( id = '3' status = 'ACTIVE' )
  ).
  
  " Act
  DATA(lt_result) = mo_cut->filter_active( lt_input ).
  
  " Assert
  cl_abap_unit_assert=>assert_equals(
    act = lines( lt_result )
    exp = 2
    msg = 'Deveria retornar 2 itens ativos' ).
    
  " Verificar conteúdo
  cl_abap_unit_assert=>assert_table_contains(
    line  = VALUE ty_item( id = '1' status = 'ACTIVE' )
    table = lt_result
    msg   = 'Deveria conter item 1' ).
    
  cl_abap_unit_assert=>assert_table_contains(
    line  = VALUE ty_item( id = '3' status = 'ACTIVE' )
    table = lt_result
    msg   = 'Deveria conter item 3' ).
ENDMETHOD.
```

---

### Comparar Estruturas

```abap
METHOD test_calculate_returns_correct_structure.
  " Arrange
  DATA(lv_price) = 1000.
  DATA(lv_tax_rate) = '0.23'.
  
  " Act
  DATA(ls_result) = mo_cut->calculate_with_tax(
    iv_price    = lv_price
    iv_tax_rate = lv_tax_rate ).
  
  " Assert - estrutura esperada
  DATA(ls_expected) = VALUE ty_result(
    net_price   = 1000
    tax_amount  = 230
    gross_price = 1230
    currency    = 'EUR' ).
  
  cl_abap_unit_assert=>assert_equals(
    act = ls_result
    exp = ls_expected
    msg = 'Cálculo deveria retornar estrutura correta' ).
ENDMETHOD.
```

---

## 📊 Tabela de Assertions

| Assertion | Uso | Exemplo |
|-----------|-----|---------|
| `assert_equals` | Igualdade | `act = 8, exp = 8` |
| `assert_not_equals` | Diferença | `act = 'A', exp = 'B'` |
| `assert_true` | Condição verdadeira | `act = abap_true` |
| `assert_false` | Condição falsa | `act = abap_false` |
| `assert_initial` | Valor vazio | `act = ''` |
| `assert_not_initial` | Valor preenchido | `act = 'XYZ'` |
| `assert_bound` | Objeto existe | `act = mo_obj` |
| `assert_not_bound` | Objeto não existe | `act = mo_null` |
| `assert_subrc` | sy-subrc | `act = 0, exp = 0` |
| `assert_table_contains` | Linha em tabela | `line in table` |
| `assert_differs` | Referências diferentes | `ref1 <> ref2` |
| `fail` | Forçar falha | Quando condição inesperada |

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Mensagens descritivas
cl_abap_unit_assert=>assert_equals(
  act = lv_count
  exp = 5
  msg = |Esperava 5 itens mas encontrou { lv_count }| ).  " ✅

" 2. Um conceito por teste
METHOD test_add_returns_sum.
  " ✅ Testa APENAS soma
  cl_abap_unit_assert=>assert_equals( act = lv_result exp = 8 ).
ENDMETHOD.

" 3. Valores esperados explícitos
cl_abap_unit_assert=>assert_equals(
  act = ls_order-status
  exp = 'APPROVED' ).  " ✅ Literal claro

" 4. assert_true para condições complexas
cl_abap_unit_assert=>assert_true(
  act = xsdbool( lv_date BETWEEN '20250101' AND '20251231' )
  msg = 'Data deveria estar em 2025' ).  " ✅
```

### ❌ Evitar

```abap
" 1. Mensagens genéricas
cl_abap_unit_assert=>assert_equals(
  act = lv_result
  exp = 8
  msg = 'Erro' ).  " ❌ Não ajuda a debugar

" 2. Múltiplos conceitos num teste
METHOD test_everything.
  cl_abap_unit_assert=>assert_equals( ... ).  " Testa A
  cl_abap_unit_assert=>assert_true( ... ).    " Testa B
  cl_abap_unit_assert=>assert_bound( ... ).   " Testa C
  " ❌ Difícil saber o que falhou
ENDMETHOD.

" 3. Calcular valor esperado
DATA(lv_expected) = lv_input * 2 + 5.  " ❌ Pode ter mesmo bug
cl_abap_unit_assert=>assert_equals(
  act = mo_cut->calculate( lv_input )
  exp = lv_expected ).

" 4. Testes sem assertions
METHOD test_process.
  mo_cut->process( ).  " ❌ Não verifica nada!
ENDMETHOD.
```

---

## 🎯 Assertions Customizadas

Criar assertions próprias para reutilização:

```abap
CLASS lcl_custom_assert DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      assert_order_valid
        IMPORTING is_order TYPE ty_order
        RAISING   cx_static_check.
ENDCLASS.

CLASS lcl_custom_assert IMPLEMENTATION.
  METHOD assert_order_valid.
    cl_abap_unit_assert=>assert_not_initial(
      act = is_order-vbeln
      msg = 'Order ID é obrigatório' ).
      
    cl_abap_unit_assert=>assert_not_initial(
      act = is_order-kunnr
      msg = 'Customer é obrigatório' ).
      
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( is_order-netwr > 0 )
      msg = 'Valor deve ser maior que zero' ).
  ENDMETHOD.
ENDCLASS.

" Uso
METHOD test_create_order.
  DATA(ls_order) = mo_cut->create( ... ).
  lcl_custom_assert=>assert_order_valid( ls_order ).  " ✅ Reutilizável
ENDMETHOD.
```

---

## 🔗 Próximos Passos

- **[ABAP Unit Básico](1_abap_unit_basico.md)** - Fundamentos
- **[Test Fixtures](2_test_fixtures.md)** - Setup e teardown
- **[Test Doubles](4_test_doubles.md)** - Mockar dependências

---

**Tags:** `#Assertions` `#Verification` `#Testing` `#ABAP-Unit`
