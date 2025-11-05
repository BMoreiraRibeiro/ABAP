---
tags:
  - ABAP
  - TDD
  - Test Driven Development
  - Agile
---

# TDD - Test-Driven Development

## 📋 Visão Geral

**TDD** (Test-Driven Development) é uma prática onde você escreve o **teste ANTES** do código de produção, seguindo o ciclo **Red-Green-Refactor**.

---

## 🔄 Ciclo Red-Green-Refactor

```
1. 🔴 RED: Escrever teste que FALHA
         ↓
2. 🟢 GREEN: Escrever código mínimo para PASSAR
         ↓
3. 🔵 REFACTOR: Melhorar código SEM alterar comportamento
         ↓
     (Repetir)
```

---

## 🎯 Regras do TDD

### 1. Não escrever código de produção sem teste falhando
### 2. Escrever apenas teste suficiente para falhar
### 3. Escrever apenas código suficiente para passar no teste

---

## 💡 Exemplo Completo: Calculadora

### 🔴 PASSO 1: Red - Teste Falha

```abap
*&---------------------------------------------------------------------*
*& TDD Exemplo: Calculadora
*&---------------------------------------------------------------------*

" ═══ TESTE (escrever PRIMEIRO) ═══
CLASS ltc_calculator DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_calculator.
    
    METHODS:
      setup,
      test_add_two_numbers FOR TESTING.
ENDCLASS.

CLASS ltc_calculator IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.
  
  METHOD test_add_two_numbers.
    " Arrange
    DATA(lv_a) = 5.
    DATA(lv_b) = 3.
    
    " Act
    DATA(lv_result) = mo_cut->add( iv_a = lv_a iv_b = lv_b ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 8
      msg = '5 + 3 deveria ser 8' ).
  ENDMETHOD.
ENDCLASS.
```

**Executar:** ❌ **FALHA** (método `add` não existe ainda!)

---

### 🟢 PASSO 2: Green - Código Mínimo

```abap
" ═══ CÓDIGO DE PRODUÇÃO (escrever DEPOIS) ═══
CLASS zcl_calculator DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS add
      IMPORTING iv_a TYPE i
                iv_b TYPE i
      RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS zcl_calculator IMPLEMENTATION.
  METHOD add.
    " Código MÍNIMO para passar
    rv_result = iv_a + iv_b.
  ENDMETHOD.
ENDCLASS.
```

**Executar:** ✅ **PASSA!**

---

### 🔵 PASSO 3: Refactor - Melhorar

```abap
" Neste caso simples, já está OK
" Refactor seria: renomear variáveis, extrair métodos, etc.
```

**Executar novamente:** ✅ **AINDA PASSA!**

---

### 🔄 REPETIR: Próximo Teste

```abap
" ═══ ADICIONAR TESTE PARA SUBTRAÇÃO ═══
METHOD test_subtract_two_numbers FOR TESTING.
  " Arrange
  DATA(lv_a) = 10.
  DATA(lv_b) = 4.
  
  " Act
  DATA(lv_result) = mo_cut->subtract( iv_a = lv_a iv_b = lv_b ).
  
  " Assert
  cl_abap_unit_assert=>assert_equals(
    act = lv_result
    exp = 6
    msg = '10 - 4 deveria ser 6' ).
ENDMETHOD.
```

**Executar:** ❌ **FALHA** (método `subtract` não existe)

```abap
" ═══ IMPLEMENTAR subtract ═══
METHOD subtract.
  rv_result = iv_a - iv_b.
ENDMETHOD.
```

**Executar:** ✅ **PASSA!**

---

## 🏗️ Exemplo Avançado: Validador de Email

### Iteração 1: Email Vazio

#### 🔴 Red

```abap
METHOD test_empty_email_is_invalid FOR TESTING.
  " Act
  DATA(lv_valid) = mo_validator->is_valid_email( '' ).
  
  " Assert
  cl_abap_unit_assert=>assert_false(
    act = lv_valid
    msg = 'Email vazio deveria ser inválido' ).
ENDMETHOD.
```

❌ Falha: método não existe

#### 🟢 Green

```abap
CLASS zcl_email_validator IMPLEMENTATION.
  METHOD is_valid_email.
    IF iv_email IS INITIAL.
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.  " Implementação mínima
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

✅ Passa!

---

### Iteração 2: Email sem @

#### 🔴 Red

```abap
METHOD test_email_without_at_is_invalid FOR TESTING.
  " Act
  DATA(lv_valid) = mo_validator->is_valid_email( 'teste.com' ).
  
  " Assert
  cl_abap_unit_assert=>assert_false(
    act = lv_valid
    msg = 'Email sem @ deveria ser inválido' ).
ENDMETHOD.
```

❌ Falha: retorna `true` (implementação atual)

#### 🟢 Green

```abap
METHOD is_valid_email.
  IF iv_email IS INITIAL.
    rv_valid = abap_false.
    RETURN.
  ENDIF.
  
  " Verificar @
  IF NOT iv_email CA '@'.
    rv_valid = abap_false.
    RETURN.
  ENDIF.
  
  rv_valid = abap_true.
ENDMETHOD.
```

✅ Passa!

---

### Iteração 3: Email Válido

#### 🔴 Red

```abap
METHOD test_valid_email FOR TESTING.
  " Act
  DATA(lv_valid) = mo_validator->is_valid_email( 'user@example.com' ).
  
  " Assert
  cl_abap_unit_assert=>assert_true(
    act = lv_valid
    msg = 'Email válido deveria ser aceite' ).
ENDMETHOD.
```

✅ Já passa! (implementação atual aceita)

---

### 🔵 Refactor

```abap
METHOD is_valid_email.
  " Extrair validações para métodos privados
  IF iv_email IS INITIAL OR
     NOT contains_at_sign( iv_email ) OR
     NOT contains_domain( iv_email ).
    rv_valid = abap_false.
  ELSE.
    rv_valid = abap_true.
  ENDIF.
ENDMETHOD.

METHOD contains_at_sign.
  rv_result = xsdbool( iv_email CA '@' ).
ENDMETHOD.

METHOD contains_domain.
  SPLIT iv_email AT '@' INTO DATA(lv_user) DATA(lv_domain).
  rv_result = xsdbool( lv_domain IS NOT INITIAL ).
ENDMETHOD.
```

✅ Testes ainda passam!

---

## 🎯 Vantagens do TDD

### ✅ Benefícios

1. **Design melhor** - Código testável = código desacoplado
2. **Menos bugs** - Testes desde início
3. **Documentação** - Testes mostram uso esperado
4. **Confiança** - Refatorar sem medo
5. **Coverage alto** - Naturalmente >80%
6. **Foco** - Um problema de cada vez

---

## ⚠️ Desafios do TDD

### Dificuldades

1. **Curva aprendizado** - Requer prática
2. **Mais tempo inicial** - Compensa no longo prazo
3. **Difícil com código legado** - Sem testes existentes
4. **Requer disciplina** - Não pular Red-Green-Refactor

---

## 💡 Baby Steps

**Regra:** Passos **pequenos** e **incrementais**.

```abap
" ❌ RUIM: Implementar tudo de uma vez
METHOD validate_order.
  " 200 linhas de validação complexa sem testes
ENDMETHOD.

" ✅ BOM: Um caso de cada vez
METHOD test_order_without_customer_is_invalid FOR TESTING.
  " Testar APENAS customer vazio
ENDMETHOD.

METHOD test_order_without_items_is_invalid FOR TESTING.
  " Testar APENAS items vazios (PRÓXIMO passo)
ENDMETHOD.
```

---

## 🎓 Exemplo Completo: Processador de Pedidos

```abap
*&---------------------------------------------------------------------*
*& TDD Completo: Processador de Pedidos
*&---------------------------------------------------------------------*

" ═══════════════════════════════════════════════════════════
" CLASSE DE TESTE (escrever PRIMEIRO)
" ═══════════════════════════════════════════════════════════
CLASS ltc_order_processor DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_order_processor.
    
    METHODS:
      setup,
      
      " Iteração 1
      test_order_with_valid_data_is_created FOR TESTING,
      
      " Iteração 2
      test_order_without_customer_raises_error FOR TESTING,
      
      " Iteração 3
      test_order_without_items_raises_error FOR TESTING.
      
ENDCLASS.

CLASS ltc_order_processor IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.
  
  " ═══ ITERAÇÃO 1: Happy Path ═══
  METHOD test_order_with_valid_data_is_created.
    " Arrange
    DATA(ls_order) = VALUE ty_order(
      customer_id = '100001'
      items       = VALUE #( ( material = 'MAT001' quantity = 10 ) ) ).
    
    " Act
    DATA(lv_order_id) = mo_cut->create_order( ls_order ).
    
    " Assert
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_order_id
      msg = 'Order ID deveria ser gerado' ).
  ENDMETHOD.
  
  " ═══ ITERAÇÃO 2: Validação Customer ═══
  METHOD test_order_without_customer_raises_error.
    " Arrange
    DATA(ls_order) = VALUE ty_order(
      customer_id = ''  " Vazio!
      items       = VALUE #( ( material = 'MAT001' quantity = 10 ) ) ).
    
    " Act & Assert
    TRY.
        mo_cut->create_order( ls_order ).
        cl_abap_unit_assert=>fail( 'Deveria lançar cx_invalid_order' ).
      CATCH cx_invalid_order.
        " OK
    ENDTRY.
  ENDMETHOD.
  
  " ═══ ITERAÇÃO 3: Validação Items ═══
  METHOD test_order_without_items_raises_error.
    " Arrange
    DATA(ls_order) = VALUE ty_order(
      customer_id = '100001'
      items       = VALUE #( ) ).  " Vazio!
    
    " Act & Assert
    TRY.
        mo_cut->create_order( ls_order ).
        cl_abap_unit_assert=>fail( 'Deveria lançar cx_invalid_order' ).
      CATCH cx_invalid_order.
        " OK
    ENDTRY.
  ENDMETHOD.
  
ENDCLASS.

" ═══════════════════════════════════════════════════════════
" CLASSE DE PRODUÇÃO (escrever DEPOIS, iterativamente)
" ═══════════════════════════════════════════════════════════
CLASS zcl_order_processor DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS create_order
      IMPORTING is_order TYPE ty_order
      RETURNING VALUE(rv_order_id) TYPE vbeln
      RAISING   cx_invalid_order.
ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.
  METHOD create_order.
    " Iteração 2: Validar customer
    IF is_order-customer_id IS INITIAL.
      RAISE EXCEPTION TYPE cx_invalid_order
        EXPORTING textid = cx_invalid_order=>customer_required.
    ENDIF.
    
    " Iteração 3: Validar items
    IF is_order-items IS INITIAL.
      RAISE EXCEPTION TYPE cx_invalid_order
        EXPORTING textid = cx_invalid_order=>items_required.
    ENDIF.
    
    " Iteração 1: Gerar ID
    rv_order_id = |ORD{ sy-datum }{ sy-uzeit }|.
    
    " TODO (próximas iterações):
    " - Salvar no BD
    " - Enviar notificação
    " - Atualizar estoque
  ENDMETHOD.
ENDCLASS.
```

---

## ⚡ Boas Práticas TDD

### ✅ Fazer

```abap
" 1. Red-Green-Refactor sempre
" 🔴 Teste falha → 🟢 Código passa → 🔵 Refactor

" 2. Baby steps
" ✅ Um requisito de cada vez

" 3. Testar comportamento, não implementação
cl_abap_unit_assert=>assert_equals(
  act = lo_order->get_total( )  " ✅ Comportamento público
  exp = 1000 ).

" 4. Refactor quando testes passam
" 🟢 Verde = seguro refatorar

" 5. Commits frequentes
" Commit após cada ciclo Red-Green-Refactor
```

### ❌ Evitar

```abap
" 1. Pular para Green sem Red
" ❌ Escrever código antes do teste

" 2. Testes complexos demais
METHOD test_entire_workflow FOR TESTING.
  " ❌ Testar 10 coisas de uma vez
ENDMETHOD.

" 3. Não refatorar
" ❌ Código funciona mas fica bagunçado

" 4. Alterar teste para passar
" ❌ Teste falha? Corrigir código, não teste!
```

---

## 🔗 Próximos Passos

- **[ABAP Unit Básico](1_abap_unit_basico.md)** - Fundamentos de testes
- **[Injeção de Dependências](5_injecao_dependencias.md)** - Design testável
- **[Code Coverage](6_code_coverage.md)** - TDD = coverage natural

---

**Tags:** `#TDD` `#Test-Driven-Development` `#Red-Green-Refactor` `#Agile`
