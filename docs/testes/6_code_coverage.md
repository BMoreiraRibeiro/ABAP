---
tags:
  - ABAP
  - Testing
  - Code Coverage
  - Quality Metrics
---

# Cobertura de Código (Code Coverage)

## 📋 Visão Geral

**Code Coverage** mede a **percentagem de código executada** pelos testes, indicando qualidade da suite de testes.

---

## 📊 Tipos de Cobertura

### Statement Coverage
Percentagem de **linhas** executadas.

```abap
METHOD calculate_discount.
  DATA(lv_discount) = 0.
  
  IF iv_price > 1000.        " Linha 1
    lv_discount = iv_price * '0.1'.  " Linha 2
  ELSE.                      " Linha 3
    lv_discount = 0.         " Linha 4
  ENDIF.
  
  rv_result = iv_price - lv_discount.  " Linha 5
ENDMETHOD.

" Teste: calculate_discount( 1500 ) 
" Cobre: Linhas 1, 2, 5 = 60% coverage (3/5)
" NÃO cobre: Linhas 3, 4 = branch ELSE não testado
```

---

### Branch Coverage
Percentagem de **caminhos** (branches) executados.

```abap
" 2 branches: IF (true) e ELSE (false)
" Coverage 100% requer AMBOS os caminhos testados

METHOD test_high_price.
  " Testa branch IF
  DATA(lv_discount) = mo_cut->calculate_discount( 1500 ).
ENDMETHOD.

METHOD test_low_price.
  " Testa branch ELSE
  DATA(lv_discount) = mo_cut->calculate_discount( 500 ).
ENDMETHOD.
```

---

## 🎯 Metas de Cobertura

| Nível | Coverage | Classificação |
|-------|----------|---------------|
| **Crítico** | < 40% | ❌ Insuficiente |
| **Baixo** | 40-60% | ⚠️ Melhorar |
| **Aceitável** | 60-70% | 🟡 OK |
| **Bom** | 70-80% | ✅ Bom |
| **Excelente** | 80-90% | 🌟 Excelente |
| **Perfeito** | > 90% | 💎 Perfeito |

**Meta Recomendada:** **70-80%**

⚠️ **100% nem sempre é necessário!** (código gerado, getters/setters triviais)

---

## ▶️ Medir Coverage

### Eclipse ADT (Recomendado)

1. **Executar com coverage:** `Ctrl+Shift+F11`
2. **Ver resultado:** View "Coverage"
3. **Código destacado:**
   - 🟢 Verde = executado
   - 🔴 Vermelho = NÃO executado

---

### SAP GUI

1. **SE80/SE24** - Abrir classe
2. **F8** ou **Coverage Analyzer (SCOV)**
3. **Executar testes**
4. **Ver relatório** de cobertura

---

## 💡 Exemplo Prático

### Código a Testar

```abap
CLASS zcl_price_calculator DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS calculate_final_price
      IMPORTING iv_base_price TYPE p
                iv_customer_type TYPE char1  " 'P'=Premium, 'R'=Regular
      RETURNING VALUE(rv_price) TYPE p.
ENDCLASS.

CLASS zcl_price_calculator IMPLEMENTATION.
  METHOD calculate_final_price.
    rv_price = iv_base_price.
    
    " Branch 1: Desconto Premium
    IF iv_customer_type = 'P'.
      rv_price = rv_price * '0.9'.  " 10% desconto
      
    " Branch 2: Desconto Regular  
    ELSEIF iv_customer_type = 'R'.
      rv_price = rv_price * '0.95'.  " 5% desconto
      
    " Branch 3: Sem desconto
    ELSE.
      " Sem alteração
    ENDIF.
    
    " Taxa de serviço
    IF rv_price > 1000.
      rv_price = rv_price + 50.  " Taxa fixa
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

### Testes com Coverage Baixo ❌

```abap
CLASS ltc_calculator DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_price_calculator.
    
    METHODS:
      setup,
      test_premium_customer FOR TESTING.
ENDCLASS.

CLASS ltc_calculator IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.
  
  METHOD test_premium_customer.
    " Testa APENAS Premium
    DATA(lv_price) = mo_cut->calculate_final_price(
      iv_base_price    = 1000
      iv_customer_type = 'P' ).
    
    cl_abap_unit_assert=>assert_equals(
      act = lv_price
      exp = 900
      msg = 'Premium deveria ter 10% desconto' ).
  ENDMETHOD.
ENDCLASS.

" Coverage: ~40% ❌
" Branches não cobertos:
"   - Customer type 'R'
"   - Customer type outro
"   - Preço > 1000 (taxa)
```

---

### Testes com Coverage Alto ✅

```abap
CLASS ltc_calculator DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_price_calculator.
    
    METHODS:
      setup,
      test_premium_customer FOR TESTING,
      test_regular_customer FOR TESTING,
      test_standard_customer FOR TESTING,
      test_high_price_adds_service_fee FOR TESTING,
      test_low_price_no_service_fee FOR TESTING.
ENDCLASS.

CLASS ltc_calculator IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.
  
  METHOD test_premium_customer.
    " Branch: customer_type = 'P'
    DATA(lv_price) = mo_cut->calculate_final_price(
      iv_base_price    = 1000
      iv_customer_type = 'P' ).
    
    cl_abap_unit_assert=>assert_equals( act = lv_price exp = 900 ).
  ENDMETHOD.
  
  METHOD test_regular_customer.
    " Branch: customer_type = 'R'
    DATA(lv_price) = mo_cut->calculate_final_price(
      iv_base_price    = 1000
      iv_customer_type = 'R' ).
    
    cl_abap_unit_assert=>assert_equals( act = lv_price exp = 950 ).
  ENDMETHOD.
  
  METHOD test_standard_customer.
    " Branch: customer_type = outro
    DATA(lv_price) = mo_cut->calculate_final_price(
      iv_base_price    = 1000
      iv_customer_type = 'S' ).
    
    cl_abap_unit_assert=>assert_equals( act = lv_price exp = 1000 ).
  ENDMETHOD.
  
  METHOD test_high_price_adds_service_fee.
    " Branch: price > 1000
    DATA(lv_price) = mo_cut->calculate_final_price(
      iv_base_price    = 1200
      iv_customer_type = 'S' ).
    
    cl_abap_unit_assert=>assert_equals(
      act = lv_price
      exp = 1250  " 1200 + 50 taxa
      msg = 'Preço alto deveria incluir taxa de serviço' ).
  ENDMETHOD.
  
  METHOD test_low_price_no_service_fee.
    " Branch: price <= 1000
    DATA(lv_price) = mo_cut->calculate_final_price(
      iv_base_price    = 500
      iv_customer_type = 'S' ).
    
    cl_abap_unit_assert=>assert_equals(
      act = lv_price
      exp = 500
      msg = 'Preço baixo não deveria ter taxa' ).
  ENDMETHOD.
ENDCLASS.

" Coverage: ~95% ✅
" Todos os branches cobertos!
```

---

## 📈 Melhorar Coverage

### 1. Identificar Gaps

```
Coverage Report:
├── calculate_final_price: 60%
│   ├── Line 5-6: ✅ Covered
│   ├── Line 8-9: ❌ Not covered (ELSEIF branch)
│   └── Line 15-16: ❌ Not covered (high price)
```

### 2. Criar Testes para Gaps

```abap
METHOD test_missing_branch.
  " Cobrir linha 8-9
  DATA(lv_price) = mo_cut->calculate_final_price(
    iv_base_price    = 1000
    iv_customer_type = 'R' ).  " ✅ Agora cobre ELSEIF
ENDMETHOD.
```

### 3. Re-executar Coverage

```
Coverage Report:
├── calculate_final_price: 85% ✅ Melhorado!
```

---

## 🎯 O que NÃO Contar

Código que pode ter baixo coverage:

```abap
" 1. Getters/Setters triviais
METHOD get_value.
  rv_value = mv_value.  " OK ter 0% coverage
ENDMETHOD.

" 2. Código gerado automaticamente
" (ex: conversões automáticas)

" 3. Construtores vazios
METHOD constructor.
  " Vazio
ENDMETHOD.

" 4. Exception handlers de erros improváveis
CATCH cx_sy_itab_line_not_found.
  " Nunca deveria acontecer = OK não testar
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Testar todos os branches críticos
IF iv_status = 'CRITICAL'.
  " ✅ Deve ter teste
ENDIF.

" 2. Foco em lógica de negócio
METHOD calculate_complex_discount.
  " ✅ 90%+ coverage aqui
ENDMETHOD.

" 3. Coverage como métrica, não meta
" 80% com testes bons > 100% com testes ruins

" 4. Revisar código não coberto
" Será que é código morto?
```

### ❌ Evitar

```abap
" 1. Testes só para coverage
METHOD test_for_coverage.
  mo_cut->do_something( ).  " ❌ Sem assertion!
ENDMETHOD.

" 2. Obcecar com 100%
" ❌ Gastar horas testando getters triviais

" 3. Ignorar coverage baixo em código crítico
METHOD process_payment.
  " ❌ 30% coverage aqui é GRAVE!
ENDMETHOD.
```

---

## 📊 Relatórios Coverage

### Ver em Eclipse ADT

```
Window → Show View → Other → ABAP → Coverage
```

### Exportar Relatório

```
Coverage View → Export → HTML/CSV
```

### CI/CD Integration

```yaml
# Falhar build se coverage < 70%
abap_unit:
  min_coverage: 70
```

---

## 🔗 Próximos Passos

- **[ABAP Unit Básico](1_abap_unit_basico.md)** - Criar testes
- **[TDD](7_tdd.md)** - Coverage natural com TDD
- **[Assertions](3_assertions.md)** - Verificações eficazes

---

**Tags:** `#Code-Coverage` `#Quality-Metrics` `#Testing` `#SCOV`
