# 🔗 Integrações e RFCs

Exemplos de integração com **SAP e serviços externos**: BAPIs, RFCs, IDocs e chamadas HTTP/REST.

---

## 📖 O que vais aprender

- Chamar BAPIs (Business Application Programming Interfaces)
- Executar RFCs (Remote Function Calls)
- Trabalhar com IDocs (Intermediate Documents)
- Consumir APIs REST/HTTP externas
- Enviar e receber dados JSON/XML
- Integração com sistemas não-SAP
- OData services
- Web Services (SOAP)

---

## 🎯 Tipos de Integração

### 1️⃣ BAPIs
Funções SAP standard para operações de negócio.

**Exemplo:** Buscar lista de voos
```abap
CALL FUNCTION 'BAPI_FLIGHT_GETLIST'
  EXPORTING
    airline = 'LH'
  TABLES
    flight_list = DATA(lt_flights).

LOOP AT lt_flights INTO DATA(ls_flight).
  WRITE: / ls_flight-flightdate, ls_flight-airportfr.
ENDLOOP.
```

[Ver exemplo completo →](bapi_flight.md)

### 2️⃣ RFC (Remote Function Call)
Chamar funções em sistemas remotos.

**Exemplo:**
```abap
CALL FUNCTION 'Z_REMOTE_FUNCTION' 
  DESTINATION 'SISTEMA_REMOTO'
  EXPORTING
    iv_param = lv_value
  IMPORTING
    ev_result = lv_result.
```

### 3️⃣ HTTP / REST APIs
Consumir serviços web externos.

**Exemplo com CL_HTTP_CLIENT:**
```abap
DATA: lo_client TYPE REF TO if_http_client,
      lv_response TYPE string.

cl_http_client=>create_by_url(
  EXPORTING url = 'https://api.exemplo.com/dados'
  IMPORTING client = lo_client ).

lo_client->request->set_method( 'GET' ).
lo_client->send( ).
lo_client->receive( ).

lv_response = lo_client->response->get_cdata( ).
WRITE: / lv_response.
```

### 4️⃣ JSON / XML
Parse e criar estruturas de dados.

**JSON:**
```abap
DATA: lv_json TYPE string,
      lt_data TYPE TABLE OF sflight.

SELECT * FROM sflight INTO TABLE lt_data UP TO 10 ROWS.

" Serializar para JSON
lv_json = /ui2/cl_json=>serialize( data = lt_data ).
WRITE: / lv_json.

" Deserializar de JSON
/ui2/cl_json=>deserialize(
  EXPORTING json = lv_json
  CHANGING  data = lt_data ).
```

---

## 📚 Tutoriais e Exemplos

### BAPIs Comuns
- `BAPI_FLIGHT_GETLIST` — Buscar voos
- `BAPI_MATERIAL_GET_DETAIL` — Detalhes de material
- `BAPI_SALESORDER_CREATEFROMDAT2` — Criar ordem de venda
- `BAPI_CUSTOMER_GETLIST` — Listar clientes
- `BAPI_TRANSACTION_COMMIT` — Commit de transação

[Ver exemplo BAPI →](bapi_flight.md)

### Exercícios Práticos
- `ex01.md` → BAPI básica
- `ex02.md` → RFC para sistema remoto
- `ex03.md` → Consumir API REST
- `ex04.md` → Parse JSON
- `ex05.md` → Criar Web Service
- `ex06-ex10.md` → Integrações avançadas

---

## 🔧 Ferramentas Úteis

### Transações SAP
- **SE37** — Testar function modules e BAPIs
- **SM59** — Configurar destinos RFC
- **SPROXY** — Criar proxies para Web Services
- **SICF** — Configurar serviços HTTP
- **WE02** — Monitorizar IDocs

### Classes Úteis
- `CL_HTTP_CLIENT` — Cliente HTTP
- `/UI2/CL_JSON` — JSON serializer
- `CL_IXML` — Parser XML
- `CL_PROXY_CLIENT` — Web Service client

---

## 💡 Exemplo Completo: Integração REST

```abap
REPORT z_rest_integration.

DATA: lo_client TYPE REF TO if_http_client,
      lv_url TYPE string VALUE 'https://jsonplaceholder.typicode.com/posts/1',
      lv_json TYPE string.

TRY.
    " Criar cliente HTTP
    cl_http_client=>create_by_url(
      EXPORTING url = lv_url
      IMPORTING client = lo_client ).

    " Configurar método GET
    lo_client->request->set_method( 'GET' ).
    
    " Enviar request
    lo_client->send( ).
    
    " Receber response
    lo_client->receive( ).
    
    " Obter dados JSON
    lv_json = lo_client->response->get_cdata( ).
    
    " Parse JSON
    DATA: BEGIN OF ls_post,
            userid TYPE i,
            id     TYPE i,
            title  TYPE string,
            body   TYPE string,
          END OF ls_post.
    
    /ui2/cl_json=>deserialize(
      EXPORTING json = lv_json
      CHANGING  data = ls_post ).
    
    " Exibir
    WRITE: / 'Título:', ls_post-title,
         / 'Corpo:', ls_post-body.
    
  CATCH cx_root INTO DATA(lx_error).
    WRITE: / 'Erro:', lx_error->get_text( ).
ENDTRY.
```

---

## 🚨 Boas Práticas

### ✅ Fazer
- Sempre usar `TRY...CATCH` em integrações
- Validar dados antes de enviar
- Usar `BAPI_TRANSACTION_COMMIT` após BAPIs de atualização
- Configurar timeouts adequados
- Logar erros de integração

### ❌ Evitar
- Expor credenciais em código
- Não tratar erros de rede
- Ignorar mensagens de retorno de BAPIs
- Fazer integrações síncronas pesadas sem fallback

---

## 🔐 Segurança

- Use **SM59** para configurar autenticação RFC
- Credenciais devem estar em **tabelas seguras** (não hardcoded)
- Use **HTTPS** sempre que possível
- Valide **certificados SSL**
- Implemente **OAuth** quando disponível

---

## 🚀 Próximos Passos

1. Leia [BAPI Flight](bapi_flight.md)
2. Experimente os exercícios `ex01.md` a `ex10.md`
3. Configure uma integração REST real
4. Explore [Jobs](../jobs/index.md) para processar integrações em background
