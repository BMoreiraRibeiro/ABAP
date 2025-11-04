---
tags:
  - ABAP
  - HTTP
  - REST
  - Integrações
  - API
---

# HTTP Client

## 📋 Visão Geral

**CL_HTTP_CLIENT** permite que programas ABAP façam **chamadas HTTP/HTTPS** para sistemas externos, consumindo **APIs REST**, **Web Services**, e outros recursos web.

**Casos de uso:**
- ✅ Consumir APIs REST externas
- ✅ Integração com serviços web
- ✅ Download de arquivos
- ✅ Webhooks e notificações
- ✅ Integração com cloud services

---

## 🚀 Criar HTTP Client

### Método CREATE_BY_URL

```abap
DATA: lo_http_client TYPE REF TO if_http_client,
      lv_url         TYPE string VALUE 'https://api.example.com/users'.

" Criar cliente HTTP
cl_http_client=>create_by_url(
  EXPORTING
    url                = lv_url
    ssl_id             = 'ANONYM'  " Certificado SSL
  IMPORTING
    client             = lo_http_client
  EXCEPTIONS
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
    OTHERS             = 4 ).

IF sy-subrc <> 0.
  MESSAGE 'Erro ao criar HTTP client' TYPE 'E'.
ENDIF.
```

### Método CREATE_BY_DESTINATION

**Usar destino RFC HTTP (SM59):**

```abap
DATA: lo_http_client TYPE REF TO if_http_client,
      lv_destination TYPE rfcdest VALUE 'Z_HTTP_DEST'.

" Criar via destino RFC
cl_http_client=>create_by_destination(
  EXPORTING
    destination        = lv_destination
  IMPORTING
    client             = lo_http_client
  EXCEPTIONS
    destination_not_found = 1
    internal_error        = 2
    OTHERS                = 3 ).

IF sy-subrc = 0.
  WRITE: / '✅ Cliente HTTP criado via destino'.
ENDIF.
```

**Configurar destino em SM59:**
1. **SM59** → Create
2. **RFC Destination:** `Z_HTTP_DEST`
3. **Connection Type:** G (HTTP Connection)
4. **Target Host:** `api.example.com`
5. **Path Prefix:** `/api/v1`

---

## 🌐 Métodos HTTP

### GET

**Ler dados:**

```abap
DATA: lo_http_client TYPE REF TO if_http_client,
      lv_response    TYPE string.

" Criar cliente
cl_http_client=>create_by_url(
  EXPORTING url    = 'https://jsonplaceholder.typicode.com/users'
  IMPORTING client = lo_http_client ).

" Configurar método GET
lo_http_client->request->set_method( if_http_request=>co_request_method_get ).

" Executar
lo_http_client->send(
  EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2 ).

IF sy-subrc = 0.
  " Receber resposta
  lo_http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2 ).
      
  IF sy-subrc = 0.
    " Obter body da resposta
    lv_response = lo_http_client->response->get_cdata( ).
    
    " Status code
    DATA(lv_code) = lo_http_client->response->get_status( )-code.
    
    WRITE: / |Status: { lv_code }|.
    WRITE: / |Response: { lv_response }|.
  ENDIF.
ENDIF.

" Fechar conexão
lo_http_client->close( ).
```

---

### POST

**Enviar dados:**

```abap
DATA: lo_http_client TYPE REF TO if_http_client,
      lv_json_request TYPE string,
      lv_json_response TYPE string.

" JSON para enviar
lv_json_request = '{ "name": "João Silva", "email": "joao@example.com" }'.

" Criar cliente
cl_http_client=>create_by_url(
  EXPORTING url    = 'https://api.example.com/users'
  IMPORTING client = lo_http_client ).

" Configurar POST
lo_http_client->request->set_method( if_http_request=>co_request_method_post ).

" Headers
lo_http_client->request->set_header_field(
  name  = 'Content-Type'
  value = 'application/json' ).

" Body
lo_http_client->request->set_cdata( lv_json_request ).

" Enviar
lo_http_client->send( ).
lo_http_client->receive( ).

" Resposta
lv_json_response = lo_http_client->response->get_cdata( ).
DATA(lv_status) = lo_http_client->response->get_status( )-code.

IF lv_status = 201.  " Created
  WRITE: / '✅ Usuário criado com sucesso'.
ELSE.
  WRITE: / |❌ Erro: { lv_status }|.
ENDIF.

lo_http_client->close( ).
```

---

### PUT

**Atualizar dados:**

```abap
DATA: lo_http_client TYPE REF TO if_http_client,
      lv_json TYPE string.

lv_json = '{ "name": "João Silva Updated", "email": "joao.new@example.com" }'.

cl_http_client=>create_by_url(
  EXPORTING url    = 'https://api.example.com/users/1'
  IMPORTING client = lo_http_client ).

" PUT
lo_http_client->request->set_method( if_http_request=>co_request_method_put ).
lo_http_client->request->set_header_field(
  name  = 'Content-Type'
  value = 'application/json' ).
lo_http_client->request->set_cdata( lv_json ).

lo_http_client->send( ).
lo_http_client->receive( ).

DATA(lv_status) = lo_http_client->response->get_status( )-code.
IF lv_status = 200.
  WRITE: / '✅ Usuário atualizado'.
ENDIF.

lo_http_client->close( ).
```

---

### DELETE

**Deletar dados:**

```abap
DATA: lo_http_client TYPE REF TO if_http_client.

cl_http_client=>create_by_url(
  EXPORTING url    = 'https://api.example.com/users/1'
  IMPORTING client = lo_http_client ).

" DELETE
lo_http_client->request->set_method( if_http_request=>co_request_method_delete ).

lo_http_client->send( ).
lo_http_client->receive( ).

DATA(lv_status) = lo_http_client->response->get_status( )-code.
IF lv_status = 204.  " No Content
  WRITE: / '✅ Usuário deletado'.
ENDIF.

lo_http_client->close( ).
```

---

## 🔐 Autenticação

### Basic Authentication

```abap
DATA: lo_http_client TYPE REF TO if_http_client,
      lv_username TYPE string VALUE 'admin',
      lv_password TYPE string VALUE 'password123'.

cl_http_client=>create_by_url(
  EXPORTING url    = 'https://api.example.com/data'
  IMPORTING client = lo_http_client ).

" Autenticação básica
lo_http_client->authenticate(
  username = lv_username
  password = lv_password ).

lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
lo_http_client->send( ).
lo_http_client->receive( ).

DATA(lv_response) = lo_http_client->response->get_cdata( ).
WRITE: / lv_response.

lo_http_client->close( ).
```

---

### Bearer Token (OAuth)

```abap
DATA: lo_http_client TYPE REF TO if_http_client,
      lv_token TYPE string VALUE 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...'.

cl_http_client=>create_by_url(
  EXPORTING url    = 'https://api.example.com/protected'
  IMPORTING client = lo_http_client ).

" Header de autorização
lo_http_client->request->set_header_field(
  name  = 'Authorization'
  value = |Bearer { lv_token }| ).

lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
lo_http_client->send( ).
lo_http_client->receive( ).

lo_http_client->close( ).
```

---

### API Key

```abap
DATA: lo_http_client TYPE REF TO if_http_client,
      lv_api_key TYPE string VALUE 'abc123xyz789'.

cl_http_client=>create_by_url(
  EXPORTING url    = 'https://api.example.com/data'
  IMPORTING client = lo_http_client ).

" API Key no header
lo_http_client->request->set_header_field(
  name  = 'X-API-Key'
  value = lv_api_key ).

" Ou na URL
DATA(lv_url) = |https://api.example.com/data?api_key={ lv_api_key }|.

lo_http_client->send( ).
lo_http_client->receive( ).

lo_http_client->close( ).
```

---

## 📤 Headers Customizados

### Adicionar Headers

```abap
" Content-Type
lo_http_client->request->set_header_field(
  name  = 'Content-Type'
  value = 'application/json' ).

" Accept
lo_http_client->request->set_header_field(
  name  = 'Accept'
  value = 'application/json' ).

" Custom headers
lo_http_client->request->set_header_field(
  name  = 'X-Custom-Header'
  value = 'CustomValue' ).

" User-Agent
lo_http_client->request->set_header_field(
  name  = 'User-Agent'
  value = 'SAP-ABAP-Client/1.0' ).
```

### Ler Headers da Resposta

```abap
" Após receive()
DATA(lv_content_type) = lo_http_client->response->get_header_field( 'Content-Type' ).
DATA(lv_server) = lo_http_client->response->get_header_field( 'Server' ).

WRITE: / |Content-Type: { lv_content_type }|.
WRITE: / |Server: { lv_server }|.

" Todos os headers
DATA: lt_headers TYPE tihttpnvp.
lo_http_client->response->get_header_fields( CHANGING fields = lt_headers ).

LOOP AT lt_headers INTO DATA(ls_header).
  WRITE: / |{ ls_header-name }: { ls_header-value }|.
ENDLOOP.
```

---

## 📊 Trabalhar com JSON

### /UI2/CL_JSON - Parse JSON

```abap
DATA: lv_json TYPE string,
      lo_json TYPE REF TO /ui2/cl_json.

" JSON recebido
lv_json = lo_http_client->response->get_cdata( ).

" Estrutura ABAP
TYPES: BEGIN OF ty_user,
         id    TYPE i,
         name  TYPE string,
         email TYPE string,
       END OF ty_user.

DATA: ls_user TYPE ty_user.

" Converter JSON → ABAP
/ui2/cl_json=>deserialize(
  EXPORTING
    json = lv_json
  CHANGING
    data = ls_user ).

WRITE: / |ID: { ls_user-id }|.
WRITE: / |Nome: { ls_user-name }|.
WRITE: / |Email: { ls_user-email }|.
```

### Criar JSON para Enviar

```abap
TYPES: BEGIN OF ty_product,
         name  TYPE string,
         price TYPE p DECIMALS 2,
         stock TYPE i,
       END OF ty_product.

DATA: ls_product TYPE ty_product,
      lv_json TYPE string.

ls_product-name  = 'Produto ABC'.
ls_product-price = '99.90'.
ls_product-stock = 50.

" Converter ABAP → JSON
lv_json = /ui2/cl_json=>serialize(
  data = ls_product
  compress = abap_false  " Formatado
  pretty_name = /ui2/cl_json=>pretty_mode-low_case ).  " camelCase

" Enviar JSON
lo_http_client->request->set_cdata( lv_json ).
```

---

## 🔧 Configurações Avançadas

### SSL/TLS

```abap
" Criar com SSL
cl_http_client=>create_by_url(
  EXPORTING
    url    = 'https://api.example.com'
    ssl_id = 'ANONYM'  " ou certificado específico
  IMPORTING
    client = lo_http_client ).

" Desabilitar verificação SSL (não recomendado em produção)
lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
```

**Configurar certificado SSL:**
1. **STRUST** - Trust Manager
2. Importar certificado
3. Usar SSL ID em `create_by_url`

---

### Timeout

```abap
" Timeout em segundos
lo_http_client->set_timeout(
  timeout = 30  " 30 segundos
).
```

---

### Proxy

```abap
" Configurar proxy
lo_http_client->get_proxy( )->set_proxy(
  proxy_host = 'proxy.empresa.com'
  proxy_port = '8080' ).

" Autenticação no proxy
lo_http_client->get_proxy( )->set_proxy_authentication(
  proxy_user = 'proxy_user'
  proxy_password = 'proxy_pass' ).
```

---

### Compression

```abap
" Aceitar resposta comprimida (gzip)
lo_http_client->request->set_header_field(
  name  = 'Accept-Encoding'
  value = 'gzip' ).
```

---

## 💡 Exemplos Completos

### Exemplo 1: Consultar API REST Pública

```abap
*&---------------------------------------------------------------------*
*& Report Z_HTTP_GET_USERS
*&---------------------------------------------------------------------*
REPORT z_http_get_users.

DATA: lo_http_client TYPE REF TO if_http_client,
      lv_json TYPE string.

START-OF-SELECTION.
  
  TRY.
      " API pública de teste
      cl_http_client=>create_by_url(
        EXPORTING url = 'https://jsonplaceholder.typicode.com/users'
        IMPORTING client = lo_http_client ).
      
      " GET
      lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
      
      " Enviar e receber
      lo_http_client->send( ).
      lo_http_client->receive( ).
      
      " Status
      DATA(ls_status) = lo_http_client->response->get_status( ).
      WRITE: / |Status: { ls_status-code } - { ls_status-reason }|, /.
      
      IF ls_status-code = 200.
        " JSON
        lv_json = lo_http_client->response->get_cdata( ).
        
        " Parse JSON
        TYPES: BEGIN OF ty_user,
                 id       TYPE i,
                 name     TYPE string,
                 username TYPE string,
                 email    TYPE string,
               END OF ty_user.
        
        DATA: lt_users TYPE TABLE OF ty_user.
        
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING data = lt_users ).
        
        WRITE: / |Usuários encontrados: { lines( lt_users ) }|, /.
        
        LOOP AT lt_users INTO DATA(ls_user).
          WRITE: / |{ ls_user-id } - { ls_user-name } ({ ls_user-email })|.
        ENDLOOP.
      ELSE.
        WRITE: / '❌ Erro ao buscar usuários'.
      ENDIF.
      
      " Fechar
      lo_http_client->close( ).
      
    CATCH cx_root INTO DATA(lo_ex).
      WRITE: / |Exceção: { lo_ex->get_text( ) }|.
  ENDTRY.
```

---

### Exemplo 2: POST com JSON

```abap
*&---------------------------------------------------------------------*
*& Report Z_HTTP_POST_DATA
*&---------------------------------------------------------------------*
REPORT z_http_post_data.

DATA: lo_http_client TYPE REF TO if_http_client,
      lv_request TYPE string,
      lv_response TYPE string.

START-OF-SELECTION.
  
  TRY.
      " Dados para enviar
      DATA: BEGIN OF ls_post,
              title  TYPE string VALUE 'Novo Post',
              body   TYPE string VALUE 'Conteúdo do post',
              userId TYPE i VALUE 1,
            END OF ls_post.
      
      " Converter para JSON
      lv_request = /ui2/cl_json=>serialize(
        data = ls_post
        compress = abap_false ).
      
      WRITE: / '═══ REQUEST ═══'.
      WRITE: / lv_request, /.
      
      " Criar cliente
      cl_http_client=>create_by_url(
        EXPORTING url = 'https://jsonplaceholder.typicode.com/posts'
        IMPORTING client = lo_http_client ).
      
      " Configurar POST
      lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
      lo_http_client->request->set_header_field(
        name  = 'Content-Type'
        value = 'application/json' ).
      lo_http_client->request->set_cdata( lv_request ).
      
      " Enviar
      lo_http_client->send( ).
      lo_http_client->receive( ).
      
      " Resposta
      DATA(lv_status) = lo_http_client->response->get_status( )-code.
      lv_response = lo_http_client->response->get_cdata( ).
      
      WRITE: / '═══ RESPONSE ═══'.
      WRITE: / |Status: { lv_status }|.
      WRITE: / lv_response, /.
      
      IF lv_status = 201.
        WRITE: / '✅ Post criado com sucesso!'.
      ELSE.
        WRITE: / '❌ Erro ao criar post'.
      ENDIF.
      
      lo_http_client->close( ).
      
    CATCH cx_root INTO DATA(lo_ex).
      WRITE: / |Erro: { lo_ex->get_text( ) }|.
  ENDTRY.
```

---

### Exemplo 3: API com Autenticação

```abap
*&---------------------------------------------------------------------*
*& Report Z_HTTP_AUTH_API
*&---------------------------------------------------------------------*
REPORT z_http_auth_api.

PARAMETERS: p_user TYPE string LOWER CASE DEFAULT 'api_user',
            p_pass TYPE string LOWER CASE DEFAULT 'api_pass',
            p_url  TYPE string LOWER CASE DEFAULT 'https://api.example.com/data'.

DATA: lo_http_client TYPE REF TO if_http_client.

START-OF-SELECTION.
  
  TRY.
      " Criar cliente
      cl_http_client=>create_by_url(
        EXPORTING
          url    = CONV string( p_url )
          ssl_id = 'ANONYM'
        IMPORTING
          client = lo_http_client ).
      
      " Basic Auth
      lo_http_client->authenticate(
        username = CONV string( p_user )
        password = CONV string( p_pass ) ).
      
      " Timeout
      lo_http_client->set_timeout( timeout = 60 ).
      
      " GET
      lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
      lo_http_client->request->set_header_field(
        name  = 'Accept'
        value = 'application/json' ).
      
      " Executar
      lo_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2 ).
      
      IF sy-subrc = 0.
        lo_http_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2 ).
        
        IF sy-subrc = 0.
          DATA(ls_status) = lo_http_client->response->get_status( ).
          
          CASE ls_status-code.
            WHEN 200.
              DATA(lv_data) = lo_http_client->response->get_cdata( ).
              WRITE: / '✅ Dados recebidos:', /.
              WRITE: / lv_data.
              
            WHEN 401.
              WRITE: / '❌ Não autorizado - verificar credenciais'.
              
            WHEN 404.
              WRITE: / '❌ Recurso não encontrado'.
              
            WHEN OTHERS.
              WRITE: / |❌ Erro { ls_status-code }: { ls_status-reason }|.
          ENDCASE.
        ELSE.
          WRITE: / '❌ Erro ao receber resposta'.
        ENDIF.
      ELSE.
        WRITE: / '❌ Erro ao enviar requisição'.
      ENDIF.
      
      lo_http_client->close( ).
      
    CATCH cx_root INTO DATA(lo_ex).
      WRITE: / |Exceção: { lo_ex->get_text( ) }|.
  ENDTRY.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre fechar conexão
TRY.
    " ... usar lo_http_client ...
  CATCH cx_root.
    " ... tratamento ...
ENDTRY.
lo_http_client->close( ).  " ✅ Sempre fechar

" 2. Tratar exceções
TRY.
    cl_http_client=>create_by_url(
      EXPORTING url = lv_url
      IMPORTING client = lo_http_client ).
  CATCH cx_root INTO DATA(lo_ex).
    WRITE: / lo_ex->get_text( ).  " ✅ Tratar erro
ENDTRY.

" 3. Verificar status code
DATA(lv_code) = lo_http_client->response->get_status( )-code.
IF lv_code <> 200.  " ✅ Verificar
  " Tratar erro
ENDIF.

" 4. Timeout adequado
lo_http_client->set_timeout( timeout = 30 ).  " ✅ Timeout

" 5. Usar destinos RFC para URLs recorrentes
cl_http_client=>create_by_destination(
  EXPORTING destination = 'Z_API_DEST'  " ✅ SM59
  IMPORTING client = lo_http_client ).
```

### ❌ Evitar

```abap
" 1. Não fechar conexão
lo_http_client->send( ).
lo_http_client->receive( ).
" ❌ Esquecer close()

" 2. Hardcoded credentials
DATA lv_password TYPE string VALUE 'senha123'.  " ❌ NUNCA!

" 3. Não tratar erros
lo_http_client->send( ).  " ❌ E se falhar?

" 4. Timeout muito alto
lo_http_client->set_timeout( timeout = 300 ).  " ❌ 5 minutos?

" 5. URLs hardcoded
DATA lv_url TYPE string VALUE 'http://prod-server.com'.  " ❌ Usar SM59
```

---

## 🔗 Próximos Passos

- **[OData](3_odata.md)** - Alternativa para APIs SAP
- **[REST API](7_rest_api.md)** - Criar APIs REST em ABAP
- **[Web Services](5_web_services.md)** - SOAP Web Services

---

**Tags:** `#HTTP` `#REST` `#API` `#Integrações` `#CL_HTTP_CLIENT` `#JSON`
