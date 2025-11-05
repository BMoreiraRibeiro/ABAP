---
tags:
  - ABAP
  - Testing
  - Dependency Injection
  - Design Patterns
---

# Injeção de Dependências

## 📋 Visão Geral

**Injeção de Dependências** (Dependency Injection - DI) é um padrão que torna código **testável** ao permitir substituir dependências reais por test doubles.

---

## 🎯 Problema: Código Não-Testável

### ❌ Ruim: Dependências Hardcoded

```abap
CLASS zcl_order_processor DEFINITION.
  PRIVATE SECTION.
    DATA mo_database TYPE REF TO zcl_database.
    DATA mo_email    TYPE REF TO zcl_email_service.
ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.
  METHOD constructor.
    " ❌ Cria dependências internamente
    CREATE OBJECT mo_database.
    CREATE OBJECT mo_email.
  ENDMETHOD.
  
  METHOD process_order.
    " Usa mo_database e mo_email...
  ENDMETHOD.
ENDCLASS.
```

**Problema:** Impossível testar sem BD real e enviar emails reais!

---

## ✅ Solução: Injetar Dependências

### 1️⃣ Constructor Injection (Recomendado)

```abap
INTERFACE if_database.
  METHODS save IMPORTING is_data TYPE any.
ENDINTERFACE.

INTERFACE if_email_service.
  METHODS send IMPORTING iv_to TYPE string.
ENDINTERFACE.

CLASS zcl_order_processor DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_database      TYPE REF TO if_database
                io_email_service TYPE REF TO if_email_service.
                
  PRIVATE SECTION.
    DATA: mo_database      TYPE REF TO if_database,
          mo_email_service TYPE REF TO if_email_service.
ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.
  METHOD constructor.
    " ✅ Recebe dependências externas
    mo_database      = io_database.
    mo_email_service = io_email_service.
  ENDMETHOD.
  
  METHOD process_order.
    mo_database->save( ls_order ).
    mo_email_service->send( iv_to = 'manager@company.com' ).
  ENDMETHOD.
ENDCLASS.
```

**Produção:**
```abap
DATA(lo_processor) = NEW zcl_order_processor(
  io_database      = NEW zcl_real_database( )
  io_email_service = NEW zcl_real_email_service( ) ).
```

**Teste:**
```abap
METHOD test_process_order.
  " ✅ Injeta mocks
  DATA(lo_processor) = NEW zcl_order_processor(
    io_database      = lo_mock_database
    io_email_service = lo_mock_email ).
  
  lo_processor->process_order( '12345' ).
  
  " Verificar sem tocar BD/email real!
ENDMETHOD.
```

---

### 2️⃣ Setter Injection

Quando constructor tem muitos parâmetros:

```abap
CLASS zcl_order_processor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      set_database IMPORTING io_db TYPE REF TO if_database,
      set_email_service IMPORTING io_email TYPE REF TO if_email_service.
      
  PRIVATE SECTION.
    DATA: mo_database      TYPE REF TO if_database,
          mo_email_service TYPE REF TO if_email_service.
ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.
  METHOD set_database.
    mo_database = io_db.
  ENDMETHOD.
  
  METHOD set_email_service.
    mo_email_service = io_email.
  ENDMETHOD.
ENDCLASS.
```

**Uso:**
```abap
" Produção
DATA(lo_processor) = NEW zcl_order_processor( ).
lo_processor->set_database( NEW zcl_real_database( ) ).
lo_processor->set_email_service( NEW zcl_real_email( ) ).

" Teste
DATA(lo_processor) = NEW zcl_order_processor( ).
lo_processor->set_database( lo_mock_database ).
lo_processor->set_email_service( lo_mock_email ).
```

---

### 3️⃣ Interface Injection (Raro em ABAP)

```abap
INTERFACE if_injectable.
  METHODS inject_dependencies
    IMPORTING io_db TYPE REF TO if_database.
ENDINTERFACE.
```

---

## 💡 Exemplo Completo

### Interfaces

```abap
INTERFACE if_customer_repository.
  METHODS get_customer
    IMPORTING iv_id TYPE kunnr
    RETURNING VALUE(rs_customer) TYPE kna1.
ENDINTERFACE.

INTERFACE if_notification_service.
  METHODS notify
    IMPORTING iv_customer TYPE kunnr
              iv_message  TYPE string.
ENDINTERFACE.
```

### Implementações Reais

```abap
CLASS zcl_db_customer_repo DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_customer_repository.
ENDCLASS.

CLASS zcl_db_customer_repo IMPLEMENTATION.
  METHOD if_customer_repository~get_customer.
    SELECT SINGLE * FROM kna1
      WHERE kunnr = @iv_id
      INTO @rs_customer.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_email_notification DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_notification_service.
ENDCLASS.

CLASS zcl_email_notification IMPLEMENTATION.
  METHOD if_notification_service~notify.
    " Enviar email real
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1' ...
  ENDMETHOD.
ENDCLASS.
```

### Classe Testável

```abap
CLASS zcl_order_service DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_customer_repo TYPE REF TO if_customer_repository
                io_notifier      TYPE REF TO if_notification_service.
                
    METHODS create_order
      IMPORTING iv_customer TYPE kunnr
                iv_material TYPE matnr
      RETURNING VALUE(rv_order_id) TYPE vbeln.
      
  PRIVATE SECTION.
    DATA: mo_customer_repo TYPE REF TO if_customer_repository,
          mo_notifier      TYPE REF TO if_notification_service.
ENDCLASS.

CLASS zcl_order_service IMPLEMENTATION.
  METHOD constructor.
    mo_customer_repo = io_customer_repo.
    mo_notifier      = io_notifier.
  ENDMETHOD.
  
  METHOD create_order.
    " Validar cliente
    DATA(ls_customer) = mo_customer_repo->get_customer( iv_customer ).
    
    IF ls_customer-kunnr IS INITIAL.
      RAISE EXCEPTION TYPE cx_invalid_customer.
    ENDIF.
    
    " Criar ordem
    rv_order_id = |ORD{ sy-datum }{ sy-uzeit }|.
    
    " Notificar
    mo_notifier->notify(
      iv_customer = iv_customer
      iv_message  = |Ordem { rv_order_id } criada| ).
  ENDMETHOD.
ENDCLASS.
```

### Uso em Produção

```abap
DATA(lo_service) = NEW zcl_order_service(
  io_customer_repo = NEW zcl_db_customer_repo( )
  io_notifier      = NEW zcl_email_notification( ) ).

DATA(lv_order_id) = lo_service->create_order(
  iv_customer = '100001'
  iv_material = 'MAT001' ).
```

### Teste

```abap
CLASS ltc_order_service DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: mo_cut           TYPE REF TO zcl_order_service,
          mo_customer_repo TYPE REF TO if_customer_repository,
          mo_notifier      TYPE REF TO if_notification_service.
    
    METHODS:
      setup,
      test_create_order_success FOR TESTING,
      test_invalid_customer_raises_error FOR TESTING.
ENDCLASS.

CLASS ltc_order_service IMPLEMENTATION.
  METHOD setup.
    " Criar mocks
    mo_customer_repo = CAST if_customer_repository(
      cl_abap_testdouble=>create( 'IF_CUSTOMER_REPOSITORY' ) ).
      
    mo_notifier = CAST if_notification_service(
      cl_abap_testdouble=>create( 'IF_NOTIFICATION_SERVICE' ) ).
    
    " Injetar mocks
    mo_cut = NEW zcl_order_service(
      io_customer_repo = mo_customer_repo
      io_notifier      = mo_notifier ).
  ENDMETHOD.
  
  METHOD test_create_order_success.
    " Stub: get_customer retorna cliente válido
    cl_abap_testdouble=>configure_call( mo_customer_repo )->returning(
      VALUE kna1( kunnr = '100001' name1 = 'Test Customer' ) ).
    mo_customer_repo->get_customer( '100001' ).
    
    " Stub: notify retorna sucesso
    cl_abap_testdouble=>configure_call( mo_notifier ).
    mo_notifier->notify(
      iv_customer = cl_abap_testdouble=>any_value( )
      iv_message  = cl_abap_testdouble=>any_value( ) ).
    
    " Act
    DATA(lv_order_id) = mo_cut->create_order(
      iv_customer = '100001'
      iv_material = 'MAT001' ).
    
    " Assert
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_order_id
      msg = 'Order ID deveria ser gerado' ).
    
    " Verificar que notify foi chamado
    cl_abap_testdouble=>verify_expectations( mo_notifier ).
  ENDMETHOD.
  
  METHOD test_invalid_customer_raises_error.
    " Stub: get_customer retorna vazio (cliente inválido)
    cl_abap_testdouble=>configure_call( mo_customer_repo )->returning(
      VALUE kna1( ) ).
    mo_customer_repo->get_customer( '999999' ).
    
    " Act & Assert
    TRY.
        mo_cut->create_order(
          iv_customer = '999999'
          iv_material = 'MAT001' ).
        
        cl_abap_unit_assert=>fail(
          msg = 'Deveria lançar cx_invalid_customer' ).
          
      CATCH cx_invalid_customer.
        " Sucesso!
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

---

## 🏗️ Padrão Factory (Alternativa)

Quando DI não é viável:

```abap
" Factory retorna interface
CLASS zcl_database_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_db) TYPE REF TO if_database.
ENDCLASS.

CLASS zcl_database_factory IMPLEMENTATION.
  METHOD get_instance.
    IF sy-sysid = 'TST'.
      ro_db = NEW zcl_test_database( ).
    ELSE.
      ro_db = NEW zcl_production_database( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Uso
DATA(lo_db) = zcl_database_factory=>get_instance( ).
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre usar interfaces
METHODS constructor
  IMPORTING io_repo TYPE REF TO if_repository.  " ✅

" 2. Constructor injection (preferível)
mo_cut = NEW zcl_processor(
  io_db     = lo_mock_db
  io_logger = lo_stub_logger ).  " ✅

" 3. Dependências explícitas
" Fácil ver o que a classe precisa

" 4. Um nível de abstração
INTERFACE if_email_service.  " ✅ Alto nível
  METHODS send_email.
ENDINTERFACE.
```

### ❌ Evitar

```abap
" 1. Criar dependências internamente
CREATE OBJECT mo_db TYPE zcl_database.  " ❌

" 2. Dependências concretas
METHODS constructor
  IMPORTING io_db TYPE REF TO zcl_specific_database.  " ❌ Concreto

" 3. Muitas dependências (>5)
METHODS constructor
  IMPORTING io_dep1 ... io_dep10.  " ❌ Classe faz demais

" 4. Dependências opcionais sem default
METHODS set_logger
  IMPORTING io_logger TYPE REF TO if_logger.  " ⚠️ E se não chamar?
```

---

## 🔗 Próximos Passos

- **[Test Doubles](4_test_doubles.md)** - Mockar dependências
- **[TDD](7_tdd.md)** - Desenvolver com testes
- **[Code Coverage](6_code_coverage.md)** - Medir qualidade

---

**Tags:** `#Dependency-Injection` `#Design-Patterns` `#Testability`
