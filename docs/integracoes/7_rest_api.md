---
tags:
  - ABAP
  - REST API
  - HTTP
  - JSON
  - Integrações
---

# REST API em ABAP

## 📋 Visão Geral

**REST** (Representational State Transfer) é um estilo arquitetural para **APIs web** baseado em **HTTP**. Em ABAP, podemos **criar** e **consumir** APIs RESTful modernas.

**Características REST:**
- ✅ Stateless (sem estado)
- ✅ Baseado em recursos (URLs)
- ✅ Métodos HTTP (GET, POST, PUT, DELETE)
- ✅ Formato JSON (geralmente)
- ✅ Leve e performático

---

## 🎯 REST vs SOAP vs OData

| Aspecto | REST | SOAP | OData |
|---------|------|------|-------|
| **Protocolo** | HTTP | HTTP/SMTP | HTTP |
| **Formato** | JSON, XML | XML | JSON, XML |
| **Complexidade** | **Baixa** | Alta | Média |
| **Performance** | **Rápida** | Lenta | Rápida |
| **Flexibilidade** | **Alta** | Baixa | Média |
| **Padrão SAP** | Customizado | Legacy | **Fiori** |

---

## 🛠️ Criar REST API (Provider)

### Transaction SICF - HTTP Service

**Criar serviço HTTP customizado:**

#### Passo 1: Criar Handler Class

```abap
*&---------------------------------------------------------------------*
*& Class ZCL_REST_PRODUCTS
*&---------------------------------------------------------------------*
CLASS zcl_rest_products DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.
ENDCLASS.

CLASS zcl_rest_products IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    
    DATA: lv_method TYPE string,
          lv_path   TYPE string,
          lv_body   TYPE string,
          lv_response TYPE string.
    
    " Obter método HTTP
    lv_method = server->request->get_method( ).
    
    " Obter path
    lv_path = server->request->get_header_field( '~path_info' ).
    
    " Processar request
    CASE lv_method.
      WHEN 'GET'.
        " GET /api/products
        IF lv_path CP '/api/products*'.
          me->get_products(
            EXPORTING iv_path = lv_path
            IMPORTING ev_json = lv_response ).
        ENDIF.
        
      WHEN 'POST'.
        " POST /api/products
        lv_body = server->request->get_cdata( ).
        me->create_product(
          EXPORTING iv_json = lv_body
          IMPORTING ev_json = lv_response ).
          
      WHEN 'PUT'.
        " PUT /api/products/{id}
        lv_body = server->request->get_cdata( ).
        me->update_product(
          EXPORTING
            iv_path = lv_path
            iv_json = lv_body
          IMPORTING
            ev_json = lv_response ).
            
      WHEN 'DELETE'.
        " DELETE /api/products/{id}
        me->delete_product(
          EXPORTING iv_path = lv_path
          IMPORTING ev_json = lv_response ).
          
      WHEN OTHERS.
        lv_response = '{"error": "Method not allowed"}'.
        server->response->set_status( code = 405 reason = 'Method Not Allowed' ).
    ENDCASE.
    
    " Set response
    server->response->set_header_field(
      name  = 'Content-Type'
      value = 'application/json' ).
    server->response->set_cdata( lv_response ).
    
  ENDMETHOD.
  
  METHOD get_products.
    " Implementação GET
    TYPES: BEGIN OF ty_product,
             id       TYPE matnr,
             name     TYPE maktx,
             category TYPE mtart,
             price    TYPE netpr,
           END OF ty_product.
    
    DATA: lt_products TYPE TABLE OF ty_product,
          lv_id       TYPE matnr.
    
    " Verificar se é GET de um produto específico
    " /api/products/MAT-001
    IF iv_path CP '/api/products/*'.
      lv_id = substring_after( val = iv_path sub = '/api/products/' ).
      
      " Buscar produto específico
      SELECT SINGLE
        mara~matnr AS id,
        makt~maktx AS name,
        mara~mtart AS category,
        mbew~verpr AS price
      FROM mara
      INNER JOIN makt ON mara~matnr = makt~matnr
      INNER JOIN mbew ON mara~matnr = mbew~matnr
      WHERE mara~matnr = @lv_id
        AND makt~spras = @sy-langu
      INTO @DATA(ls_product).
      
      IF sy-subrc = 0.
        " Produto encontrado
        ev_json = /ui2/cl_json=>serialize(
          data = ls_product
          compress = abap_false ).
      ELSE.
        " 404 Not Found
        ev_json = |{{ "error": "Product { lv_id } not found" }}|.
      ENDIF.
      
    ELSE.
      " GET todos os produtos
      SELECT
        mara~matnr AS id,
        makt~maktx AS name,
        mara~mtart AS category,
        mbew~verpr AS price
      FROM mara
      INNER JOIN makt ON mara~matnr = makt~matnr
      INNER JOIN mbew ON mara~matnr = mbew~matnr
      WHERE makt~spras = @sy-langu
      INTO TABLE @lt_products
      UP TO 100 ROWS.
      
      " Converter para JSON
      ev_json = /ui2/cl_json=>serialize(
        data = lt_products
        compress = abap_false ).
    ENDIF.
  ENDMETHOD.
  
  METHOD create_product.
    " Implementação POST
    TYPES: BEGIN OF ty_product_in,
             name     TYPE string,
             category TYPE string,
             price    TYPE string,
           END OF ty_product_in.
    
    DATA: ls_product TYPE ty_product_in,
          lv_matnr   TYPE matnr.
    
    TRY.
        " Parse JSON
        /ui2/cl_json=>deserialize(
          EXPORTING json = iv_json
          CHANGING data = ls_product ).
        
        " Gerar número de material
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'MATERIALNR'
          IMPORTING
            number      = lv_matnr.
        
        " Inserir produto (simplificado)
        INSERT mara FROM VALUE #(
          matnr = lv_matnr
          mtart = CONV mtart( ls_product-category )
          meins = 'PC'
        ).
        
        IF sy-subrc = 0.
          " Sucesso - 201 Created
          ev_json = |{{ "id": "{ lv_matnr }", "message": "Product created" }}|.
        ELSE.
          " Erro
          ev_json = '{"error": "Failed to create product"}'.
        ENDIF.
        
      CATCH cx_root INTO DATA(lo_ex).
        ev_json = |{{ "error": "{ lo_ex->get_text( ) }" }}|.
    ENDTRY.
  ENDMETHOD.
  
  METHOD update_product.
    " Implementação PUT
    DATA: lv_id TYPE matnr,
          ls_product TYPE ty_product_in.
    
    " Extrair ID da URL
    lv_id = substring_after( val = iv_path sub = '/api/products/' ).
    
    TRY.
        " Parse JSON
        /ui2/cl_json=>deserialize(
          EXPORTING json = iv_json
          CHANGING data = ls_product ).
        
        " Atualizar (simplificado)
        UPDATE mara SET mtart = CONV mtart( ls_product-category )
          WHERE matnr = lv_id.
        
        IF sy-subrc = 0.
          ev_json = |{{ "message": "Product { lv_id } updated" }}|.
        ELSE.
          ev_json = |{{ "error": "Product { lv_id } not found" }}|.
        ENDIF.
        
      CATCH cx_root INTO DATA(lo_ex).
        ev_json = |{{ "error": "{ lo_ex->get_text( ) }" }}|.
    ENDTRY.
  ENDMETHOD.
  
  METHOD delete_product.
    " Implementação DELETE
    DATA lv_id TYPE matnr.
    
    lv_id = substring_after( val = iv_path sub = '/api/products/' ).
    
    " Deletar
    DELETE FROM mara WHERE matnr = lv_id.
    
    IF sy-subrc = 0.
      ev_json = |{{ "message": "Product { lv_id } deleted" }}|.
    ELSE.
      ev_json = |{{ "error": "Product { lv_id } not found" }}|.
    ENDIF.
  ENDMETHOD.
  
ENDCLASS.
```

---

#### Passo 2: Criar HTTP Service (SICF)

1. **SICF** - Maintain Services
2. **Navegar:** default_host → sap → **Create Sub-Element**
3. **Service Name:** `zapi`
4. **Description:** "REST API"
5. **Handler List:** `ZCL_REST_PRODUCTS`
6. **Save** e **Activate**

**URL resultante:**
```
http://server:port/sap/zapi/products
```

---

### Testar API

#### cURL

```bash
# GET - Listar produtos
curl -X GET http://server:8000/sap/zapi/products \
  -u username:password \
  -H "Accept: application/json"

# GET - Produto específico
curl -X GET http://server:8000/sap/zapi/products/MAT-001 \
  -u username:password

# POST - Criar produto
curl -X POST http://server:8000/sap/zapi/products \
  -u username:password \
  -H "Content-Type: application/json" \
  -d '{"name":"Produto Novo","category":"FERT","price":"99.90"}'

# PUT - Atualizar produto
curl -X PUT http://server:8000/sap/zapi/products/MAT-001 \
  -u username:password \
  -H "Content-Type: application/json" \
  -d '{"name":"Produto Atualizado","price":"149.90"}'

# DELETE - Deletar produto
curl -X DELETE http://server:8000/sap/zapi/products/MAT-001 \
  -u username:password
```

---

## 🔐 Autenticação e Segurança

### Basic Authentication

**Já implementado via HTTP:**
```bash
curl -u username:password http://server/sap/zapi/products
```

---

### OAuth 2.0

**Implementar validação de token:**

```abap
METHOD validate_oauth_token.
  DATA: lv_token TYPE string,
        lv_auth_header TYPE string.
  
  " Obter header Authorization
  lv_auth_header = server->request->get_header_field( 'Authorization' ).
  
  " Bearer <token>
  IF lv_auth_header CP 'Bearer *'.
    lv_token = substring_after( val = lv_auth_header sub = 'Bearer ' ).
    
    " Validar token (exemplo simplificado)
    " Em produção: validar com servidor OAuth
    IF lv_token = 'valid-token-123'.
      " Token válido
      RETURN.
    ELSE.
      " Token inválido - 401 Unauthorized
      server->response->set_status( code = 401 reason = 'Unauthorized' ).
      server->response->set_cdata( '{"error":"Invalid token"}' ).
      " Abortar
    ENDIF.
  ELSE.
    " Sem token - 401
    server->response->set_status( code = 401 reason = 'Unauthorized' ).
  ENDIF.
ENDMETHOD.
```

---

### API Key

```abap
METHOD validate_api_key.
  DATA lv_api_key TYPE string.
  
  " Obter API key do header
  lv_api_key = server->request->get_header_field( 'X-API-Key' ).
  
  " Validar
  IF lv_api_key <> 'secret-api-key-abc123'.
    server->response->set_status( code = 403 reason = 'Forbidden' ).
    server->response->set_cdata( '{"error":"Invalid API key"}' ).
  ENDIF.
ENDMETHOD.
```

---

### CORS (Cross-Origin Resource Sharing)

**Permitir acesso de outros domínios:**

```abap
METHOD set_cors_headers.
  " Permitir qualquer origem (desenvolvimento)
  server->response->set_header_field(
    name  = 'Access-Control-Allow-Origin'
    value = '*' ).
  
  " Métodos permitidos
  server->response->set_header_field(
    name  = 'Access-Control-Allow-Methods'
    value = 'GET, POST, PUT, DELETE, OPTIONS' ).
  
  " Headers permitidos
  server->response->set_header_field(
    name  = 'Access-Control-Allow-Headers'
    value = 'Content-Type, Authorization, X-API-Key' ).
  
  " Preflight OPTIONS request
  IF server->request->get_method( ) = 'OPTIONS'.
    server->response->set_status( code = 200 reason = 'OK' ).
    " Não processar mais
  ENDIF.
ENDMETHOD.
```

---

## 📊 Trabalhar com JSON

### Serializar ABAP → JSON

```abap
" Estrutura simples
DATA: BEGIN OF ls_user,
        id    TYPE i VALUE 1,
        name  TYPE string VALUE 'João Silva',
        email TYPE string VALUE 'joao@example.com',
      END OF ls_user.

DATA(lv_json) = /ui2/cl_json=>serialize(
  data = ls_user
  compress = abap_false  " Formatado
  pretty_name = /ui2/cl_json=>pretty_mode-low_case ).  " camelCase

" Resultado:
" {"id":1,"name":"João Silva","email":"joao@example.com"}
```

### Deserializar JSON → ABAP

```abap
DATA: lv_json TYPE string,
      ls_user TYPE ty_user.

lv_json = '{"id":1,"name":"João Silva","email":"joao@example.com"}'.

/ui2/cl_json=>deserialize(
  EXPORTING
    json = lv_json
    pretty_name = /ui2/cl_json=>pretty_mode-camel_case
  CHANGING
    data = ls_user ).

WRITE: / ls_user-name.  " João Silva
```

### Arrays JSON

```abap
" Tabela interna → JSON array
DATA: lt_products TYPE TABLE OF ty_product.

SELECT * FROM mara INTO TABLE @lt_products UP TO 10 ROWS.

DATA(lv_json_array) = /ui2/cl_json=>serialize( data = lt_products ).

" Resultado: [{"id":"MAT-001",...},{"id":"MAT-002",...}]
```

---

## ⚡ Features Avançadas

### Paginação

```abap
METHOD get_products_paginated.
  DATA: lv_page TYPE i,
        lv_per_page TYPE i VALUE 20,
        lv_offset TYPE i.
  
  " Query parameters: ?page=2&per_page=10
  lv_page = server->request->get_form_field( 'page' ).
  IF lv_page IS INITIAL.
    lv_page = 1.
  ENDIF.
  
  lv_per_page = server->request->get_form_field( 'per_page' ).
  IF lv_per_page IS INITIAL.
    lv_per_page = 20.
  ENDIF.
  
  lv_offset = ( lv_page - 1 ) * lv_per_page.
  
  " Buscar com paginação
  SELECT * FROM mara
    INTO TABLE @DATA(lt_products)
    UP TO @lv_per_page ROWS
    OFFSET @lv_offset.
  
  " Resposta com metadata de paginação
  DATA(lv_json) = |{{ "page": { lv_page }, | &&
                  |"per_page": { lv_per_page }, | &&
                  |"data": { /ui2/cl_json=>serialize( lt_products ) } }}|.
  
  ev_json = lv_json.
ENDMETHOD.
```

---

### Filtros e Ordenação

```abap
METHOD get_products_filtered.
  DATA: lv_category TYPE mtart,
        lv_sort     TYPE string,
        lv_order    TYPE string.
  
  " Query: ?category=FERT&sort=name&order=desc
  lv_category = server->request->get_form_field( 'category' ).
  lv_sort     = server->request->get_form_field( 'sort' ).
  lv_order    = server->request->get_form_field( 'order' ).
  
  " SELECT dinâmico
  SELECT * FROM mara
    WHERE mtart = @lv_category
    ORDER BY (lv_sort) (lv_order)
    INTO TABLE @DATA(lt_products)
    UP TO 100 ROWS.
  
  ev_json = /ui2/cl_json=>serialize( data = lt_products ).
ENDMETHOD.
```

---

### HTTP Status Codes

```abap
METHOD set_http_status.
  " 200 OK
  server->response->set_status( code = 200 reason = 'OK' ).
  
  " 201 Created
  server->response->set_status( code = 201 reason = 'Created' ).
  
  " 400 Bad Request
  server->response->set_status( code = 400 reason = 'Bad Request' ).
  
  " 401 Unauthorized
  server->response->set_status( code = 401 reason = 'Unauthorized' ).
  
  " 403 Forbidden
  server->response->set_status( code = 403 reason = 'Forbidden' ).
  
  " 404 Not Found
  server->response->set_status( code = 404 reason = 'Not Found' ).
  
  " 500 Internal Server Error
  server->response->set_status( code = 500 reason = 'Internal Server Error' ).
ENDMETHOD.
```

---

### Rate Limiting

```abap
CLASS zcl_rate_limiter DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_limit
        IMPORTING iv_user TYPE string
        RETURNING VALUE(rv_allowed) TYPE abap_bool.
ENDCLASS.

CLASS zcl_rate_limiter IMPLEMENTATION.
  METHOD check_limit.
    " Verificar requisições por minuto
    " (Implementação simplificada - usar cache ou DB)
    
    DATA: lv_count TYPE i,
          lv_timestamp TYPE timestamp.
    
    GET TIME STAMP FIELD lv_timestamp.
    
    " Contar requisições do usuário no último minuto
    SELECT COUNT(*) FROM zapi_requests
      INTO @lv_count
      WHERE user = @iv_user
        AND timestamp > @( lv_timestamp - 60 ).
    
    IF lv_count < 100.  " Limite: 100 req/min
      rv_allowed = abap_true.
    ELSE.
      rv_allowed = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Uso:
IF zcl_rate_limiter=>check_limit( sy-uname ) = abap_false.
  server->response->set_status( code = 429 reason = 'Too Many Requests' ).
  server->response->set_cdata( '{"error":"Rate limit exceeded"}' ).
  RETURN.
ENDIF.
```

---

## 💡 Exemplo Completo: API de Pedidos

### Handler Class

```abap
*&---------------------------------------------------------------------*
*& Class ZCL_REST_ORDERS_API
*&---------------------------------------------------------------------*
CLASS zcl_rest_orders_api DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_http_extension.
    
  PRIVATE SECTION.
    DATA: mo_server TYPE REF TO if_http_server.
    
    METHODS:
      get_orders IMPORTING iv_path TYPE string
                 EXPORTING ev_json TYPE string,
      create_order IMPORTING iv_json TYPE string
                   EXPORTING ev_json TYPE string,
      get_order_details IMPORTING iv_order_id TYPE vbeln_va
                        EXPORTING ev_json TYPE string.
ENDCLASS.

CLASS zcl_rest_orders_api IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    DATA: lv_method TYPE string,
          lv_path   TYPE string.
    
    mo_server = server.
    
    " Método e path
    lv_method = server->request->get_method( ).
    lv_path = server->request->get_header_field( '~path_info' ).
    
    " CORS
    server->response->set_header_field(
      name = 'Access-Control-Allow-Origin' value = '*' ).
    
    " Roteamento
    CASE lv_method.
      WHEN 'GET'.
        IF lv_path = '/api/orders' OR lv_path = '/api/orders/'.
          " GET /api/orders - Listar ordens
          me->get_orders(
            EXPORTING iv_path = lv_path
            IMPORTING ev_json = DATA(lv_response) ).
            
        ELSEIF lv_path CP '/api/orders/*'.
          " GET /api/orders/{id} - Ordem específica
          DATA(lv_order_id) = substring_after( val = lv_path sub = '/api/orders/' ).
          me->get_order_details(
            EXPORTING iv_order_id = CONV vbeln_va( lv_order_id )
            IMPORTING ev_json = lv_response ).
        ENDIF.
        
      WHEN 'POST'.
        " POST /api/orders - Criar ordem
        DATA(lv_body) = server->request->get_cdata( ).
        me->create_order(
          EXPORTING iv_json = lv_body
          IMPORTING ev_json = lv_response ).
          
      WHEN OTHERS.
        lv_response = '{"error":"Method not allowed"}'.
        server->response->set_status( code = 405 reason = 'Method Not Allowed' ).
    ENDCASE.
    
    " Response
    server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    server->response->set_cdata( lv_response ).
  ENDMETHOD.
  
  METHOD get_orders.
    TYPES: BEGIN OF ty_order,
             order_id   TYPE vbeln_va,
             customer   TYPE kunnr,
             order_date TYPE erdat,
             net_value  TYPE netwr,
           END OF ty_order.
    
    DATA lt_orders TYPE TABLE OF ty_order.
    
    " Buscar ordens
    SELECT vbeln AS order_id,
           kunnr AS customer,
           erdat AS order_date,
           netwr AS net_value
    FROM vbak
    INTO TABLE @lt_orders
    UP TO 50 ROWS
    ORDER BY erdat DESCENDING.
    
    " JSON
    ev_json = /ui2/cl_json=>serialize(
      data = lt_orders
      compress = abap_false ).
  ENDMETHOD.
  
  METHOD get_order_details.
    TYPES: BEGIN OF ty_order_detail,
             order_id   TYPE vbeln_va,
             customer   TYPE kunnr,
             order_date TYPE erdat,
             net_value  TYPE netwr,
             items      TYPE TABLE OF ty_item WITH DEFAULT KEY,
           END OF ty_order_detail,
           BEGIN OF ty_item,
             item_number TYPE posnr_va,
             material    TYPE matnr,
             quantity    TYPE kwmeng,
             price       TYPE netpr,
           END OF ty_item.
    
    DATA: ls_order TYPE ty_order_detail,
          lt_items TYPE TABLE OF ty_item.
    
    " Cabeçalho da ordem
    SELECT SINGLE
      vbeln AS order_id,
      kunnr AS customer,
      erdat AS order_date,
      netwr AS net_value
    FROM vbak
    WHERE vbeln = @iv_order_id
    INTO CORRESPONDING FIELDS OF @ls_order.
    
    IF sy-subrc <> 0.
      ev_json = |{{ "error": "Order { iv_order_id } not found" }}|.
      mo_server->response->set_status( code = 404 reason = 'Not Found' ).
      RETURN.
    ENDIF.
    
    " Itens da ordem
    SELECT
      posnr AS item_number,
      matnr AS material,
      kwmeng AS quantity,
      netpr AS price
    FROM vbap
    WHERE vbeln = @iv_order_id
    INTO TABLE @lt_items.
    
    ls_order-items = lt_items.
    
    " JSON
    ev_json = /ui2/cl_json=>serialize(
      data = ls_order
      compress = abap_false ).
  ENDMETHOD.
  
  METHOD create_order.
    TYPES: BEGIN OF ty_order_request,
             customer TYPE kunnr,
             items    TYPE TABLE OF ty_item_req WITH DEFAULT KEY,
           END OF ty_order_request,
           BEGIN OF ty_item_req,
             material TYPE matnr,
             quantity TYPE kwmeng,
           END OF ty_item_req.
    
    DATA: ls_request TYPE ty_order_request,
          lv_order_id TYPE vbeln_va.
    
    TRY.
        " Parse JSON
        /ui2/cl_json=>deserialize(
          EXPORTING json = iv_json
          CHANGING data = ls_request ).
        
        " Criar ordem via BAPI (simplificado)
        DATA: ls_header TYPE bapisdhd1,
              lt_items TYPE TABLE OF bapisditm,
              lt_return TYPE TABLE OF bapiret2.
        
        ls_header-doc_type = 'TA'.
        ls_header-sales_org = '1000'.
        ls_header-distr_chan = '10'.
        ls_header-division = '00'.
        
        LOOP AT ls_request-items INTO DATA(ls_item_req).
          APPEND VALUE #(
            itm_number = sy-tabix * 10
            material   = ls_item_req-material
            target_qty = ls_item_req-quantity
          ) TO lt_items.
        ENDLOOP.
        
        CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
          EXPORTING
            order_header_in = ls_header
          IMPORTING
            salesdocument = lv_order_id
          TABLES
            return = lt_return
            order_items_in = lt_items.
        
        READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ev_json = '{"error":"Failed to create order"}'.
          mo_server->response->set_status( code = 400 reason = 'Bad Request' ).
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
          ev_json = |{{ "order_id": "{ lv_order_id }", "message": "Order created" }}|.
          mo_server->response->set_status( code = 201 reason = 'Created' ).
        ENDIF.
        
      CATCH cx_root INTO DATA(lo_ex).
        ev_json = |{{ "error": "{ lo_ex->get_text( ) }" }}|.
        mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

---

## 🔗 Documentação API (Swagger/OpenAPI)

### Gerar Swagger JSON

```json
{
  "swagger": "2.0",
  "info": {
    "title": "SAP Products API",
    "version": "1.0.0"
  },
  "host": "server:8000",
  "basePath": "/sap/zapi",
  "schemes": ["https"],
  "paths": {
    "/products": {
      "get": {
        "summary": "List all products",
        "responses": {
          "200": {
            "description": "Successful"
          }
        }
      },
      "post": {
        "summary": "Create product",
        "parameters": [{
          "in": "body",
          "name": "product",
          "schema": {
            "$ref": "#/definitions/Product"
          }
        }],
        "responses": {
          "201": {"description": "Created"}
        }
      }
    }
  },
  "definitions": {
    "Product": {
      "type": "object",
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "price": {"type": "number"}
      }
    }
  }
}
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Versionamento da API
" /api/v1/products

" 2. Usar códigos HTTP corretos
server->response->set_status( code = 201 reason = 'Created' ).

" 3. Validação de input
IF ls_product-name IS INITIAL.
  ev_json = '{"error":"Name is required"}'.
  server->response->set_status( code = 400 reason = 'Bad Request' ).
  RETURN.
ENDIF.

" 4. Tratamento de exceções
TRY.
    " ... lógica ...
  CATCH cx_root INTO DATA(lo_ex).
    ev_json = |{{ "error": "{ lo_ex->get_text( ) }" }}|.
    server->response->set_status( code = 500 reason = 'Internal Server Error' ).
ENDTRY.

" 5. CORS headers
server->response->set_header_field(
  name = 'Access-Control-Allow-Origin' value = '*' ).
```

### ❌ Evitar

```abap
" 1. Expor erros técnicos
ev_json = |{{ "error": "{ lo_ex->get_longtext( ) }" }}|.  " ❌ Detalhes internos

" 2. Não validar input
" Sempre validar dados recebidos!

" 3. Hardcoded secrets
DATA lv_secret TYPE string VALUE 'senha123'.  " ❌ NUNCA!

" 4. Queries sem limite
SELECT * FROM huge_table INTO TABLE @lt_data.  " ❌ Timeout!

" 5. Não tratar erros HTTP
" Sempre usar status codes corretos
```

---

## 🔗 Próximos Passos

- **[OData](3_odata.md)** - Alternativa SAP para REST
- **[HTTP Client](4_http_client.md)** - Consumir APIs REST
- **[Web Services](5_web_services.md)** - SOAP vs REST

---

**Tags:** `#REST` `#API` `#HTTP` `#JSON` `#SICF` `#Integrações` `#ABAP`
