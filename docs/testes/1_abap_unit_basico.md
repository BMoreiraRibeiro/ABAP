---
tags:
  - ABAP
  - Testing
  - ABAP Unit
  - Unit Test
---

# ABAP Unit Básico

## 📋 Visão Geral

**ABAP Unit** é o framework de testes unitários integrado ao SAP NetWeaver, permitindo criar testes automatizados diretamente no código ABAP.

---

## 🎯 O que é um Teste Unitário?

Um teste unitário verifica uma **pequena parte do código** (método, função) de forma **isolada**, garantindo que funciona conforme esperado.

**Características:**
- ✅ Rápido (milissegundos)
- ✅ Isolado (não depende de BD, sistema externo)
- ✅ Repetível (sempre mesmo resultado)
- ✅ Automatizado (executa sem intervenção)

---

## 🏗️ Anatomia de uma Classe de Teste

### Estrutura Básica

```abap
*&---------------------------------------------------------------------*
*& Include ou Classe de Teste
*&---------------------------------------------------------------------*
CLASS ltc_my_test DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_my_class.  " Class Under Test
    
    METHODS:
      " Executado antes de CADA teste
      setup,
      
      " Testes (um método por cenário)
      test_scenario_1 FOR TESTING,
      test_scenario_2 FOR TESTING.
      
ENDCLASS.

CLASS ltc_my_test IMPLEMENTATION.
  
  METHOD setup.
    " Preparar ambiente
    CREATE OBJECT mo_cut.
  ENDMETHOD.
  
  METHOD test_scenario_1.
    " Arrange (preparar)
    DATA(lv_input) = 'value'.
    
    " Act (executar)
    DATA(lv_result) = mo_cut->process( lv_input ).
    
    " Assert (verificar)
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'expected'
      msg = 'Resultado diferente do esperado' ).
  ENDMETHOD.
  
  METHOD test_scenario_2.
    " Outro cenário de teste
  ENDMETHOD.
  
ENDCLASS.
```

---

## 🏷️ Atributos de Teste

### FOR TESTING

Marca método como teste:

```abap
METHODS test_add_numbers FOR TESTING.
```

---

### RISK LEVEL

Indica **risco** se teste falhar:

```abap
CLASS ltc_test DEFINITION FOR TESTING
  RISK LEVEL HARMLESS.    " Sem risco
  " ou
  RISK LEVEL DANGEROUS.   " Pode afetar dados
  " ou
  RISK LEVEL CRITICAL.    " Pode causar danos graves
```

| Level | Uso |
|-------|-----|
| **HARMLESS** | Testes puros, sem efeitos colaterais |
| **DANGEROUS** | Pode gravar em BD, chamar RFCs |
| **CRITICAL** | Pode alterar customizing, master data |

---

### DURATION

Tempo esperado de execução:

```abap
CLASS ltc_test DEFINITION FOR TESTING
  DURATION SHORT.      " < 1 minuto
  " ou
  DURATION MEDIUM.     " 1-5 minutos
  " ou
  DURATION LONG.       " > 5 minutos
```

**Recomendação:** Mantenha testes **SHORT** sempre que possível!

---

## 🎨 Padrão Arrange-Act-Assert (AAA)

Estrutura padrão para organizar testes:

```abap
METHOD test_calculate_discount.
  
  " ═══ ARRANGE (Preparar) ═══
  " Definir dados de entrada
  DATA(lv_price) = 1000.
  DATA(lv_discount_percent) = 10.
  
  " ═══ ACT (Executar) ═══
  " Chamar método sob teste
  DATA(lv_final_price) = mo_cut->calculate_discount(
    iv_price    = lv_price
    iv_discount = lv_discount_percent ).
  
  " ═══ ASSERT (Verificar) ═══
  " Validar resultado
  cl_abap_unit_assert=>assert_equals(
    act = lv_final_price
    exp = 900
    msg = |Desconto de { lv_discount_percent }% em { lv_price } deveria ser 900| ).
    
ENDMETHOD.
```

---

## 💡 Exemplo Completo: Calculadora

### Classe a Testar

```abap
*&---------------------------------------------------------------------*
*& Classe: ZCL_CALCULATOR
*&---------------------------------------------------------------------*
CLASS zcl_calculator DEFINITION PUBLIC FINAL CREATE PUBLIC.
  
  PUBLIC SECTION.
    METHODS:
      add IMPORTING iv_a TYPE i
                    iv_b TYPE i
          RETURNING VALUE(rv_result) TYPE i,
          
      subtract IMPORTING iv_a TYPE i
                         iv_b TYPE i
               RETURNING VALUE(rv_result) TYPE i,
               
      divide IMPORTING iv_a TYPE i
                       iv_b TYPE i
             RETURNING VALUE(rv_result) TYPE f
             RAISING   cx_sy_zerodivide.
             
ENDCLASS.

CLASS zcl_calculator IMPLEMENTATION.
  
  METHOD add.
    rv_result = iv_a + iv_b.
  ENDMETHOD.
  
  METHOD subtract.
    rv_result = iv_a - iv_b.
  ENDMETHOD.
  
  METHOD divide.
    IF iv_b = 0.
      RAISE EXCEPTION TYPE cx_sy_zerodivide.
    ENDIF.
    rv_result = iv_a / iv_b.
  ENDMETHOD.
  
ENDCLASS.
```

---

### Classe de Teste

```abap
*&---------------------------------------------------------------------*
*& Testes: ZCL_CALCULATOR
*&---------------------------------------------------------------------*
CLASS ltc_calculator DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: mo_calculator TYPE REF TO zcl_calculator.
    
    METHODS:
      setup,
      test_add_positive_numbers FOR TESTING,
      test_add_negative_numbers FOR TESTING,
      test_subtract FOR TESTING,
      test_divide_normal FOR TESTING,
      test_divide_by_zero FOR TESTING.
      
ENDCLASS.

CLASS ltc_calculator IMPLEMENTATION.
  
  METHOD setup.
    " Executado antes de CADA teste
    CREATE OBJECT mo_calculator.
  ENDMETHOD.
  
  METHOD test_add_positive_numbers.
    " Arrange
    DATA(lv_a) = 5.
    DATA(lv_b) = 3.
    
    " Act
    DATA(lv_result) = mo_calculator->add( iv_a = lv_a iv_b = lv_b ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 8
      msg = '5 + 3 deveria ser 8' ).
  ENDMETHOD.
  
  METHOD test_add_negative_numbers.
    " Arrange
    DATA(lv_a) = -5.
    DATA(lv_b) = 3.
    
    " Act
    DATA(lv_result) = mo_calculator->add( iv_a = lv_a iv_b = lv_b ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = -2
      msg = '-5 + 3 deveria ser -2' ).
  ENDMETHOD.
  
  METHOD test_subtract.
    " Arrange
    DATA(lv_a) = 10.
    DATA(lv_b) = 4.
    
    " Act
    DATA(lv_result) = mo_calculator->subtract( iv_a = lv_a iv_b = lv_b ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 6
      msg = '10 - 4 deveria ser 6' ).
  ENDMETHOD.
  
  METHOD test_divide_normal.
    " Arrange
    DATA(lv_a) = 10.
    DATA(lv_b) = 2.
    
    " Act
    DATA(lv_result) = mo_calculator->divide( iv_a = lv_a iv_b = lv_b ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '5.0'
      msg = '10 / 2 deveria ser 5.0' ).
  ENDMETHOD.
  
  METHOD test_divide_by_zero.
    " Arrange
    DATA(lv_a) = 10.
    DATA(lv_b) = 0.
    
    " Act & Assert (espera exceção)
    TRY.
        DATA(lv_result) = mo_calculator->divide( iv_a = lv_a iv_b = lv_b ).
        
        " Se chegou aqui, não lançou exceção (FALHA!)
        cl_abap_unit_assert=>fail(
          msg = 'Deveria lançar exceção ao dividir por zero' ).
          
      CATCH cx_sy_zerodivide.
        " Sucesso! Exceção foi lançada como esperado
    ENDTRY.
  ENDMETHOD.
  
ENDCLASS.
```

---

## ▶️ Executar Testes

### Eclipse ADT (Recomendado)

1. **Abrir classe** no editor
2. **Ctrl+Shift+F10** - Executar testes
3. **Ver resultados** na view "ABAP Unit"

**Atalhos:**
- `Ctrl+Shift+F10` - Run Unit Tests
- `Ctrl+Shift+F11` - Run with Coverage
- `F5` - Re-run Last Test

---

### SAP GUI

1. **SE80** ou **SE24** - Abrir classe
2. **Classe → Executar → Testes Unitários** (ou `F9`)
3. Ver resultados na janela popup

---

## 📊 Interpretar Resultados

### Testes Passando ✅

```
Run: 5 tests
Status: OK (Green)
Duration: 0.023s
```

### Testes Falhando ❌

```
Run: 5 tests
Failed: 1
Duration: 0.031s

test_add_positive_numbers - FAILED
  Expected: 8
  Actual: 7
  Message: 5 + 3 deveria ser 8
```

---

## 🎯 Onde Colocar Testes?

### Opção 1: Test Include (Classes Globais)

```
ZCL_MY_CLASS
├── Definitions
├── Implementations
└── Test Classes (Include Automático)
```

**Vantagem:** Testes junto com código  
**Uso:** Classes globais (SE24)

---

### Opção 2: Include Separado (Reports)

```abap
*&---------------------------------------------------------------------*
*& Report Z_MY_REPORT
*&---------------------------------------------------------------------*
REPORT z_my_report.

" ... código do report ...

*&---------------------------------------------------------------------*
*& Test Include
*&---------------------------------------------------------------------*
INCLUDE z_my_report_test IF FOUND.
```

**Arquivo: Z_MY_REPORT_TEST**
```abap
CLASS ltc_test DEFINITION FOR TESTING ...
```

---

### Opção 3: Classe de Teste Separada

```abap
" Classe principal: ZCL_ORDER_PROCESSOR
" Classe de teste: ZCL_ORDER_PROCESSOR_TEST (sufixo _TEST)
```

**Uso:** Testes muito complexos

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Nomes descritivos
METHOD test_calculate_discount_returns_reduced_price.

" 2. Um assert por teste (ideal)
cl_abap_unit_assert=>assert_equals( act = lv_result exp = 100 ).

" 3. Sempre limpar em teardown
METHOD teardown.
  CLEAR mo_cut.
  " Deletar dados temporários
ENDMETHOD.

" 4. Testar casos extremos
METHOD test_add_max_integer.
  " Testa limite superior
ENDMETHOD.

METHOD test_add_zero.
  " Testa zero
ENDMETHOD.

" 5. Mensagens informativas
cl_abap_unit_assert=>assert_equals(
  act = lv_count
  exp = 5
  msg = |Esperava 5 itens mas encontrou { lv_count }| ).
```

### ❌ Evitar

```abap
" 1. Nomes genéricos
METHOD test1.  " ❌ Não diz o que testa

" 2. Muitas responsabilidades
METHOD test_everything.
  " Testa 10 coisas diferentes  ❌
ENDMETHOD.

" 3. Dependências externas
METHOD test_read_database.
  SELECT * FROM kna1...  " ❌ Não é teste unitário!
ENDMETHOD.

" 4. Sem assertions
METHOD test_process.
  mo_cut->process( ).  " ❌ Não verifica nada
ENDMETHOD.

" 5. Testes interdependentes
DATA: gv_shared_state.  " ❌ Testes compartilhando estado
```

---

## 🔗 Próximos Passos

- **[Test Fixtures](2_test_fixtures.md)** - Setup e teardown avançados
- **[Assertions](3_assertions.md)** - Todas as verificações disponíveis
- **[Test Doubles](4_test_doubles.md)** - Mockar dependências

---

**Tags:** `#ABAP-Unit` `#Testing` `#Basics` `#AAA-Pattern`
