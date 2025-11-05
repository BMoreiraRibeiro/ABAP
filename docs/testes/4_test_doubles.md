---
tags:
  - ABAP
  - Testing
  - Test Doubles
  - Mocking
---

# Test Doubles e Mocking

## 📋 Visão Geral

**Test Doubles** são objetos falsos que substituem dependências reais durante testes, permitindo **isolar** o código sob teste.

---

## 🎭 Tipos de Test Doubles

### 1️⃣ Dummy
Objeto passado mas nunca usado.

```abap
mo_service->process(
  iv_data = 'real_data'
  io_logger = lo_dummy_logger ).  " Nunca chamado
```

### 2️⃣ Stub
Retorna respostas pré-programadas.

```abap
" Stub sempre retorna 'OK'
lo_stub->is_valid( ) = abap_true.
```

### 3️⃣ Mock
Verifica que foi chamado corretamente.

```abap
" Mock verifica que send_email foi chamado com parâmetros corretos
VERIFY lo_mock->send_email( iv_to = 'test@example.com' ).
```

### 4️⃣ Fake
Implementação simplificada funcional.

```abap
" Fake database usa tabela interna em vez de BD real
CLASS lcl_fake_db IMPLEMENTATION.
  METHOD save.
    APPEND is_data TO mt_memory_storage.
  ENDMETHOD.
ENDCLASS.
```

---

## 🔧 ABAP Test Double Framework

Framework nativo para criar mocks/stubs em ABAP.

### Criar Test Double

```abap
DATA: lo_double TYPE REF TO if_my_interface.

" Criar test double de interface
lo_double = CAST if_my_interface(
  cl_abap_testdouble=>create( 'IF_MY_INTERFACE' ) ).
```

---

### Configurar Comportamento (Stub)

```abap
METHOD test_with_stub.
  " Criar double
  DATA(lo_customer_repo) = CAST if_customer_repository(
    cl_abap_testdouble=>create( 'IF_CUSTOMER_REPOSITORY' ) ).
  
  " Configurar: quando get_customer('100001') for chamado, retornar...
  cl_abap_testdouble=>configure_call( lo_customer_repo )->returning(
    VALUE ty_customer( kunnr = '100001' name1 = 'Test Customer' ) ).
  
  lo_customer_repo->get_customer( '100001' ).
  
  " Injetar double no objeto sob teste
  mo_cut = NEW zcl_order_processor( io_customer_repo = lo_customer_repo ).
  
  " Testar
  DATA(ls_order) = mo_cut->create_order(
    iv_customer = '100001'
    iv_material = 'MAT001' ).
  
  " Verificar que usou dados do stub
  cl_abap_unit_assert=>assert_equals(
    act = ls_order-customer_name
    exp = 'Test Customer' ).
ENDMETHOD.
```

---

### Verificar Chamadas (Mock)

```abap
METHOD test_sends_notification.
  " Criar mock
  DATA(lo_notification) = CAST if_notification_service(
    cl_abap_testdouble=>create( 'IF_NOTIFICATION_SERVICE' ) ).
  
  " Injetar no objeto
  mo_cut = NEW zcl_order_processor( io_notifier = lo_notification ).
  
  " Executar
  mo_cut->process_order( '12345' ).
  
  " Verificar que send() foi chamado exatamente 1 vez
  cl_abap_testdouble=>verify_expectations( lo_notification ).
ENDMETHOD.
```

---

### Configurar Múltiplas Chamadas

```abap
METHOD test_multiple_calls.
  DATA(lo_db) = CAST if_database(
    cl_abap_testdouble=>create( 'IF_DATABASE' ) ).
  
  " Primeira chamada retorna 'A'
  cl_abap_testdouble=>configure_call( lo_db )->returning( 'A' ).
  lo_db->read( ).
  
  " Segunda chamada retorna 'B'
  cl_abap_testdouble=>configure_call( lo_db )->returning( 'B' ).
  lo_db->read( ).
  
  " Usar
  mo_cut = NEW zcl_processor( io_db = lo_db ).
  
  DATA(lv_first) = mo_cut->get_next( ).   " Retorna 'A'
  DATA(lv_second) = mo_cut->get_next( ).  " Retorna 'B'
  
  cl_abap_unit_assert=>assert_equals( act = lv_first exp = 'A' ).
  cl_abap_unit_assert=>assert_equals( act = lv_second exp = 'B' ).
ENDMETHOD.
```

---

### Configurar Exceções

```abap
METHOD test_handles_database_error.
  DATA(lo_db) = CAST if_database(
    cl_abap_testdouble=>create( 'IF_DATABASE' ) ).
  
  " Configurar para lançar exceção
  cl_abap_testdouble=>configure_call( lo_db )->raise_exception(
    NEW cx_database_error( ) ).
  
  lo_db->save( is_data = ls_data ).
  
  " Injetar
  mo_cut = NEW zcl_processor( io_db = lo_db ).
  
  " Testar tratamento de erro
  TRY.
      mo_cut->process( ).
      cl_abap_unit_assert=>fail( 'Deveria propagar exceção' ).
    CATCH cx_database_error.
      " OK - tratou corretamente
  ENDTRY.
ENDMETHOD.
```

---

### times() - Verificar Quantidade de Chamadas

```abap
METHOD test_caches_data.
  DATA(lo_db) = CAST if_database(
    cl_abap_testdouble=>create( 'IF_DATABASE' ) ).
  
  " Configurar que read() deve ser chamado exatamente 1 vez
  cl_abap_testdouble=>configure_call( lo_db )->times( 1 ).
  lo_db->read( iv_id = '123' ).
  
  mo_cut = NEW zcl_caching_processor( io_db = lo_db ).
  
  " Chamar 3 vezes
  mo_cut->get_data( '123' ).
  mo_cut->get_data( '123' ).  " Deveria usar cache
  mo_cut->get_data( '123' ).  " Deveria usar cache
  
  " Verificar que BD foi consultado apenas 1 vez
  cl_abap_testdouble=>verify_expectations( lo_db ).
ENDMETHOD.
```

---

## 💡 Exemplo Completo

### Interface de Dependência

```abap
INTERFACE if_email_service PUBLIC.
  METHODS send_email
    IMPORTING iv_to      TYPE string
              iv_subject TYPE string
              iv_body    TYPE string
    RETURNING VALUE(rv_success) TYPE abap_bool.
ENDINTERFACE.
```

### Classe a Testar

```abap
CLASS zcl_order_processor DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_email_service TYPE REF TO if_email_service.
      
    METHODS process_order
      IMPORTING iv_order_id TYPE vbeln
      RETURNING VALUE(rv_success) TYPE abap_bool.
      
  PRIVATE SECTION.
    DATA mo_email_service TYPE REF TO if_email_service.
ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.
  METHOD constructor.
    mo_email_service = io_email_service.
  ENDMETHOD.
  
  METHOD process_order.
    " Processar ordem...
    rv_success = abap_true.
    
    " Enviar notificação
    mo_email_service->send_email(
      iv_to      = 'manager@company.com'
      iv_subject = |Ordem { iv_order_id } processada|
      iv_body    = 'Ordem processada com sucesso' ).
  ENDMETHOD.
ENDCLASS.
```

### Teste com Mock

```abap
CLASS ltc_order_processor DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: mo_cut           TYPE REF TO zcl_order_processor,
          mo_email_service TYPE REF TO if_email_service.
    
    METHODS:
      setup,
      test_sends_notification FOR TESTING.
ENDCLASS.

CLASS ltc_order_processor IMPLEMENTATION.
  METHOD setup.
    " Criar mock de email service
    mo_email_service = CAST if_email_service(
      cl_abap_testdouble=>create( 'IF_EMAIL_SERVICE' ) ).
    
    " Configurar: send_email retorna true
    cl_abap_testdouble=>configure_call( mo_email_service )->returning( abap_true ).
    mo_email_service->send_email(
      iv_to      = cl_abap_testdouble=>any_value( )
      iv_subject = cl_abap_testdouble=>any_value( )
      iv_body    = cl_abap_testdouble=>any_value( ) ).
    
    " Injetar mock
    mo_cut = NEW zcl_order_processor( mo_email_service ).
  ENDMETHOD.
  
  METHOD test_sends_notification.
    " Act
    DATA(lv_success) = mo_cut->process_order( '12345' ).
    
    " Assert - processamento teve sucesso
    cl_abap_unit_assert=>assert_true(
      act = lv_success
      msg = 'Processamento deveria ter sucesso' ).
    
    " Verificar que email foi enviado
    cl_abap_testdouble=>verify_expectations( mo_email_service ).
  ENDMETHOD.
ENDCLASS.
```

---

## 🛠️ Fake Manual

Quando test double framework não é suficiente:

```abap
" Interface
INTERFACE if_customer_repository.
  METHODS get_customer
    IMPORTING iv_id TYPE kunnr
    RETURNING VALUE(rs_customer) TYPE kna1.
ENDINTERFACE.

" Fake (implementação simplificada)
CLASS lcl_fake_customer_repo DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_customer_repository.
    
    METHODS add_test_customer
      IMPORTING is_customer TYPE kna1.
      
  PRIVATE SECTION.
    DATA mt_customers TYPE TABLE OF kna1.
ENDCLASS.

CLASS lcl_fake_customer_repo IMPLEMENTATION.
  METHOD if_customer_repository~get_customer.
    READ TABLE mt_customers INTO rs_customer
      WITH KEY kunnr = iv_id.
  ENDMETHOD.
  
  METHOD add_test_customer.
    APPEND is_customer TO mt_customers.
  ENDMETHOD.
ENDCLASS.

" Uso no teste
METHOD test_with_fake.
  DATA(lo_fake_repo) = NEW lcl_fake_customer_repo( ).
  
  " Adicionar clientes de teste
  lo_fake_repo->add_test_customer(
    VALUE #( kunnr = '100001' name1 = 'Test Customer' ) ).
  
  " Injetar
  mo_cut = NEW zcl_order_processor( lo_fake_repo ).
  
  " Testar
  DATA(ls_order) = mo_cut->create_order( iv_customer = '100001' ).
  
  cl_abap_unit_assert=>assert_equals(
    act = ls_order-customer_name
    exp = 'Test Customer' ).
ENDMETHOD.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Usar interfaces para dependências
CLASS zcl_processor DEFINITION.
  METHODS constructor
    IMPORTING io_db TYPE REF TO if_database.  " ✅ Interface
ENDCLASS.

" 2. Injeção de dependências
mo_cut = NEW zcl_processor(
  io_db     = lo_mock_db
  io_logger = lo_stub_logger ).  " ✅ Injetado

" 3. Isolar uma dependência por vez
METHOD test_database_error.
  " ✅ Mock apenas database, resto real
  mo_cut = NEW zcl_processor( io_db = lo_mock_db ).
ENDMETHOD.

" 4. Verificar apenas comportamentos importantes
cl_abap_testdouble=>verify_expectations( lo_notification ).  " ✅
```

### ❌ Evitar

```abap
" 1. Dependências hardcoded
CLASS zcl_processor IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_db TYPE zcl_database.  " ❌ Não testável
  ENDMETHOD.
ENDCLASS.

" 2. Mockar tudo
METHOD test_something.
  " ❌ Mocks demais = teste frágil
  lo_mock1 = ...
  lo_mock2 = ...
  lo_mock3 = ...
ENDMETHOD.

" 3. Testar implementação de mocks
METHOD test_mock_works.
  DATA(lv_result) = lo_mock->get_data( ).
  cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'mocked' ).
  " ❌ Testa o mock, não o código real!
ENDMETHOD.
```

---

## 🔗 Próximos Passos

- **[Injeção de Dependências](5_injecao_dependencias.md)** - Tornar código testável
- **[ABAP Unit Básico](1_abap_unit_basico.md)** - Fundamentos
- **[TDD](7_tdd.md)** - Test-Driven Development

---

**Tags:** `#Test-Doubles` `#Mocking` `#Stubbing` `#Isolation`
