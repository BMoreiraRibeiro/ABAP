---
tags:
  - ABAP
  - Testing
  - Test Fixtures
  - Setup
---

# Test Fixtures

## 📋 Visão Geral

**Test Fixtures** são métodos especiais que preparam e limpam o ambiente de teste, garantindo que cada teste execute em condições controladas.

---

## 🔄 Ciclo de Vida dos Testes

```
class_setup()           ← Executado 1x antes de TODOS os testes
    ↓
    setup()             ← Executado antes de CADA teste
        ↓
        test_1()        ← Teste 1
        ↓
    teardown()          ← Executado após CADA teste
    ↓
    setup()
        ↓
        test_2()        ← Teste 2
        ↓
    teardown()
    ↓
class_teardown()        ← Executado 1x após TODOS os testes
```

---

## 🛠️ Métodos de Fixture

### setup()

Executado **antes de cada teste**.

```abap
CLASS ltc_test DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_order_processor.
    
    METHODS:
      setup,
      test_process_order FOR TESTING,
      test_cancel_order FOR TESTING.
      
ENDCLASS.

CLASS ltc_test IMPLEMENTATION.
  
  METHOD setup.
    " Executado antes de test_process_order
    " E novamente antes de test_cancel_order
    
    CREATE OBJECT mo_cut.
    mo_cut->set_mode( 'TEST' ).
  ENDMETHOD.
  
  METHOD test_process_order.
    " mo_cut já está criado e configurado
    DATA(lv_result) = mo_cut->process( '12345' ).
    cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'OK' ).
  ENDMETHOD.
  
  METHOD test_cancel_order.
    " mo_cut recriado novamente (isolamento!)
    DATA(lv_result) = mo_cut->cancel( '12345' ).
    cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'CANCELLED' ).
  ENDMETHOD.
  
ENDCLASS.
```

**Uso:** Criar objetos, inicializar dados comuns a todos os testes.

---

### teardown()

Executado **após cada teste**.

```abap
METHOD teardown.
  " Limpar após cada teste
  CLEAR mo_cut.
  
  " Deletar dados temporários
  DELETE FROM ztmp_test WHERE session_id = gv_session_id.
  
  " Liberar recursos
  IF mo_connection IS BOUND.
    mo_connection->close( ).
  ENDIF.
ENDMETHOD.
```

**Uso:** Limpar memória, deletar dados temporários, fechar conexões.

---

### class_setup()

Executado **1 vez antes de todos os testes** da classe.

```abap
CLASS ltc_test DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    CLASS-DATA: go_shared_config TYPE REF TO zcl_config.
    
    CLASS-METHODS:
      class_setup,
      class_teardown.
      
    METHODS:
      test_scenario_1 FOR TESTING,
      test_scenario_2 FOR TESTING.
      
ENDCLASS.

CLASS ltc_test IMPLEMENTATION.
  
  METHOD class_setup.
    " Executado 1x no início
    " Operações pesadas/lentas aqui
    
    CREATE OBJECT go_shared_config.
    go_shared_config->load_from_file( '/tmp/test_config.json' ).
    
    " Criar dados master de teste
    INSERT ztmp_test_data FROM TABLE @gt_master_data.
  ENDMETHOD.
  
  METHOD class_teardown.
    " Executado 1x no final
    DELETE FROM ztmp_test_data WHERE test_session = 'ABC123'.
  ENDMETHOD.
  
ENDCLASS.
```

**Uso:** Setup pesado (carregar configs, criar master data), compartilhado entre testes.

⚠️ **Cuidado:** Dados compartilhados podem causar dependências entre testes!

---

### class_teardown()

Executado **1 vez após todos os testes** da classe.

```abap
METHOD class_teardown.
  " Limpar dados criados em class_setup
  DELETE FROM ztmp_test_master WHERE session_id = gv_session_id.
  
  " Fechar conexões globais
  CLEAR go_shared_config.
ENDMETHOD.
```

---

## 💡 Exemplo Completo

```abap
*&---------------------------------------------------------------------*
*& Teste com Fixtures Completo
*&---------------------------------------------------------------------*
CLASS ltc_order_test DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    " Dados de instância (recriados em cada teste)
    DATA: mo_processor TYPE REF TO zcl_order_processor,
          mv_order_id  TYPE vbeln.
    
    " Dados de classe (compartilhados)
    CLASS-DATA: gt_test_customers TYPE TABLE OF kna1,
                gv_session_id     TYPE char32.
    
    CLASS-METHODS:
      class_setup,
      class_teardown.
      
    METHODS:
      setup,
      teardown,
      test_create_order FOR TESTING,
      test_cancel_order FOR TESTING.
      
ENDCLASS.

CLASS ltc_order_test IMPLEMENTATION.
  
  " ═══════════════════════════════════════════════════════════════
  " CLASS FIXTURES (executam 1x para toda classe)
  " ═══════════════════════════════════════════════════════════════
  
  METHOD class_setup.
    " Gerar ID único para sessão de teste
    gv_session_id = cl_system_uuid=>create_uuid_c32_static( ).
    
    " Criar clientes de teste
    gt_test_customers = VALUE #(
      ( kunnr = '0000100001' name1 = 'Test Customer 1' land1 = 'PT' )
      ( kunnr = '0000100002' name1 = 'Test Customer 2' land1 = 'BR' )
    ).
    
    " Inserir em tabela temporária
    INSERT kna1 FROM TABLE @gt_test_customers.
    
    WRITE: / |Class Setup - Sessão: { gv_session_id }|.
  ENDMETHOD.
  
  METHOD class_teardown.
    " Deletar clientes de teste
    DELETE FROM kna1 WHERE kunnr IN ( '0000100001', '0000100002' ).
    
    WRITE: / 'Class Teardown - Limpeza completa'.
  ENDMETHOD.
  
  " ═══════════════════════════════════════════════════════════════
  " INSTANCE FIXTURES (executam antes/depois de CADA teste)
  " ═══════════════════════════════════════════════════════════════
  
  METHOD setup.
    " Criar nova instância para cada teste (isolamento)
    CREATE OBJECT mo_processor.
    
    " Gerar novo order ID
    mv_order_id = |TEST{ sy-datum }{ sy-uzeit }|.
    
    WRITE: / |Setup - Teste iniciando com ordem { mv_order_id }|.
  ENDMETHOD.
  
  METHOD teardown.
    " Limpar dados da ordem
    DELETE FROM vbak WHERE vbeln = mv_order_id.
    DELETE FROM vbap WHERE vbeln = mv_order_id.
    
    " Liberar objeto
    CLEAR mo_processor.
    
    WRITE: / |Teardown - Ordem { mv_order_id } limpa|.
  ENDMETHOD.
  
  " ═══════════════════════════════════════════════════════════════
  " TESTES
  " ═══════════════════════════════════════════════════════════════
  
  METHOD test_create_order.
    " Arrange
    DATA(lv_customer) = gt_test_customers[ 1 ]-kunnr.
    
    " Act
    DATA(lv_result) = mo_processor->create_order(
      iv_order_id  = mv_order_id
      iv_customer  = lv_customer
      iv_material  = 'MAT001'
      iv_quantity  = 10 ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'SUCCESS'
      msg = |Criação de ordem { mv_order_id } deveria ter sucesso| ).
      
    " Verificar que ordem foi criada no BD
    SELECT SINGLE @abap_true FROM vbak
      WHERE vbeln = @mv_order_id
      INTO @DATA(lv_exists).
      
    cl_abap_unit_assert=>assert_true(
      act = lv_exists
      msg = 'Ordem deveria existir no BD' ).
  ENDMETHOD.
  
  METHOD test_cancel_order.
    " Arrange - Criar ordem primeiro
    mo_processor->create_order(
      iv_order_id  = mv_order_id
      iv_customer  = gt_test_customers[ 1 ]-kunnr
      iv_material  = 'MAT001'
      iv_quantity  = 5 ).
    
    " Act - Cancelar
    DATA(lv_result) = mo_processor->cancel_order( mv_order_id ).
    
    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'CANCELLED'
      msg = 'Cancelamento deveria ter sucesso' ).
  ENDMETHOD.
  
ENDCLASS.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Setup mínimo necessário
METHOD setup.
  CREATE OBJECT mo_cut.  " ✅ Apenas o essencial
ENDMETHOD.

" 2. Sempre limpar em teardown
METHOD teardown.
  DELETE FROM ztmp_test WHERE session_id = gv_session.  " ✅
  CLEAR mo_cut.
ENDMETHOD.

" 3. Usar class_setup para operações pesadas
METHOD class_setup.
  " ✅ Carregar config 1x, não a cada teste
  go_config->load_from_file( '/config/test.json' ).
ENDMETHOD.

" 4. Cada teste deve ser independente
METHOD setup.
  CREATE OBJECT mo_cut.  " ✅ Nova instância = isolamento
ENDMETHOD.

" 5. Dados de teste descritivos
gt_test_customers = VALUE #(
  ( kunnr = 'TESTCUST01' name1 = 'Valid Customer' )  " ✅ Nome claro
).
```

### ❌ Evitar

```abap
" 1. Setup complexo demais
METHOD setup.
  " ❌ Muita lógica = testes lentos
  DO 1000 TIMES.
    " processar...
  ENDDO.
ENDMETHOD.

" 2. Compartilhar estado mutável
CLASS-DATA: go_shared TYPE REF TO zcl_processor.  " ❌ Perigoso!

METHOD test_1.
  go_shared->set_value( 'A' ).  " Afeta test_2!
ENDMETHOD.

" 3. Não limpar após testes
METHOD teardown.
  " ❌ Vazio! Dados ficam na BD
ENDMETHOD.

" 4. Testes dependentes
METHOD test_step1.
  gv_order_id = mo_cut->create( ).  " ❌ test_step2 depende
ENDMETHOD.

METHOD test_step2.
  mo_cut->process( gv_order_id ).  " ❌ Falha se test_step1 não executou
ENDMETHOD.
```

---

## 🎯 Quando Usar Cada Fixture

| Fixture | Quando Usar | Exemplo |
|---------|-------------|---------|
| **setup()** | Sempre! Preparação padrão | Criar objetos, inicializar variáveis |
| **teardown()** | Quando grava em BD/arquivo | Deletar dados temporários |
| **class_setup()** | Operações pesadas/lentas | Carregar configs, criar master data |
| **class_teardown()** | Limpar dados globais | Deletar master data de teste |

---

## 🔗 Próximos Passos

- **[ABAP Unit Básico](1_abap_unit_basico.md)** - Fundamentos
- **[Assertions](3_assertions.md)** - Verificações
- **[Test Doubles](4_test_doubles.md)** - Mockar dependências

---

**Tags:** `#Test-Fixtures` `#Setup` `#Teardown` `#Test-Lifecycle`
