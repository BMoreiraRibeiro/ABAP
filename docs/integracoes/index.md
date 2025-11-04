# 🔗 Integrações

Guia completo de **integração entre sistemas SAP** e **serviços externos**: RFC, BAPIs, OData, HTTP Client, Web Services, IDocs e APIs REST.

---

## 📖 Conteúdo

### 🌐 Protocolos e Tecnologias

1. **[RFC - Remote Function Call](1_rfc.md)**
   - Tipos de RFC (sRFC, aRFC, tRFC, qRFC, bgRFC)
   - Configuração SM59
   - RFC síncrono vs assíncrono
   - Chamadas para sistemas remotos
   - Segurança e performance

2. **[BAPI - Business Application Programming Interface](2_bapi.md)**
   - O que são BAPIs
   - BAPIs mais usados
   - COMMIT e ROLLBACK
   - Tratamento de erros (tabela RETURN)
   - Criar BAPIs customizados

3. **[OData Services](3_odata.md)**
   - OData V2 vs V4
   - Criar serviços OData (SEGW)
   - Consumir OData (SAPUI5/Fiori)
   - Query options ($filter, $select, $expand)
   - RAP e CDS Views

4. **[HTTP Client](4_http_client.md)**
   - CL_HTTP_CLIENT
   - Métodos GET, POST, PUT, DELETE
   - Autenticação (Basic, Bearer, API Key)
   - Headers e SSL
   - Trabalhar com JSON

5. **[Web Services SOAP](5_web_services.md)**
   - SOAP vs REST
   - Criar Web Service (SE80, SOAMANAGER)
   - Consumir WSDL externo (SPROXY)
   - WS-Security
   - Monitorização

6. **[IDocs - Intermediate Documents](6_idocs.md)**
   - Estrutura de IDocs
   - Configuração (SALE, WE20, WE21)
   - Criar IDocs Outbound
   - Processar IDocs Inbound
   - Monitorização (WE02, WE05)

7. **[REST API em ABAP](7_rest_api.md)**
   - Criar API REST (SICF)
   - Handler classes
   - JSON serialization
   - CORS e autenticação
   - Boas práticas REST

---

## 🎯 Quick Start

### Chamar BAPI

```abap
DATA: lt_return TYPE TABLE OF bapiret2.

CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
  EXPORTING
    material = 'MAT-001'
  TABLES
    return = lt_return.

" Verificar erros
READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  WRITE: / '❌ Erro ao buscar material'.
ELSE.
  WRITE: / '✅ Material encontrado'.
ENDIF.
```

### Consumir API REST

```abap
DATA: lo_http_client TYPE REF TO if_http_client.

cl_http_client=>create_by_url(
  EXPORTING url = 'https://api.example.com/users'
  IMPORTING client = lo_http_client ).

lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
lo_http_client->send( ).
lo_http_client->receive( ).

DATA(lv_json) = lo_http_client->response->get_cdata( ).
WRITE: / lv_json.

lo_http_client->close( ).
```

### Criar Ordem via BAPI

```abap
DATA: ls_header TYPE bapisdhd1,
      lt_items TYPE TABLE OF bapisditm,
      lt_return TYPE TABLE OF bapiret2,
      lv_order TYPE vbeln_va.

" Cabeçalho
ls_header-doc_type = 'TA'.
ls_header-sales_org = '1000'.
ls_header-distr_chan = '10'.
ls_header-division = '00'.

" Item
APPEND VALUE #(
  itm_number = '000010'
  material = 'MAT-001'
  target_qty = '10'
) TO lt_items.

" Criar ordem
CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
    order_header_in = ls_header
  IMPORTING
    salesdocument = lv_order
  TABLES
    return = lt_return
    order_items_in = lt_items.

" Verificar e commit
READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
  WRITE: / |✅ Ordem criada: { lv_order }|.
ENDIF.
```

---

## 📊 Comparação de Tecnologias

| Tecnologia | Tipo | Formato | Uso | Performance | Complexidade |
|------------|------|---------|-----|-------------|--------------|
| **RFC** | Sínc/Assínc | Binário | SAP ↔ SAP | ⚡ Rápido | 🟢 Baixa |
| **BAPI** | Síncrono | Binário | Processos de negócio | ⚡ Rápido | 🟢 Baixa |
| **OData** | Síncrono | JSON/XML | **Fiori, Apps Web** | ⚡ Rápido | 🟡 Média |
| **HTTP Client** | Síncrono | JSON/XML | APIs externas | ⚡ Rápido | 🟢 Baixa |
| **SOAP** | Síncrono | XML | Legacy, Sistemas externos | 🐌 Lento | 🔴 Alta |
| **IDoc** | Assíncrono | Proprietário | EDI, SAP ↔ SAP | 🟡 Médio | 🟡 Média |
| **REST** | Síncrono | JSON | **APIs modernas** | ⚡ Rápido | 🟢 Baixa |

---

## � Quando Usar Cada Tecnologia

### ✅ RFC
- Comunicação entre sistemas SAP
- Chamadas assíncronas (background)
- Processos que precisam de garantia de execução (tRFC)

### ✅ BAPI
- Operações de negócio padronizadas
- Criar/modificar dados mestres (clientes, materiais)
- Transações (ordens, faturas)

### ✅ OData
- **Aplicações Fiori/SAPUI5**
- Apps web modernas
- Mobile apps
- RESTful APIs com metadata

### ✅ HTTP Client
- Consumir APIs REST de terceiros
- Integração com cloud services
- Webhooks e notificações

### ✅ SOAP/Web Services
- Sistemas legados que exigem SOAP
- Contratos formais (WSDL)
- WS-Security necessário

### ✅ IDocs
- EDI (Electronic Data Interchange)
- Integração assíncrona com garantia de entrega
- Auditoria completa de mensagens

### ✅ REST API
- **Criar APIs customizadas**
- Expor dados SAP para sistemas externos
- Integrações modernas e leves
---

## 🔧 Ferramentas e Transactions

### Desenvolvimento
- **SE37** - Function Builder (testar BAPIs)
- **SE80** - Object Navigator (criar Web Services)
- **SEGW** - Gateway Service Builder (OData)
- **SICF** - HTTP Service Maintenance (REST API)
- **SPROXY** - Enterprise Service Builder

### Configuração
- **SM59** - RFC Destinations
- **WE20** - Partner Profiles (IDoc)
- **WE21** - Port Definition (IDoc)
- **SOAMANAGER** - Web Service Administration
- **SALE** - ALE Customizing

### Monitorização
- **WE02/WE05** - IDoc Display/List
- **SRT_UTIL** - Web Service Runtime
- **SXMB_MONI** - Integration Engine Monitoring
- **/IWFND/ERROR_LOG** - OData Error Log
- **ST22** - ABAP Dumps

---

## 🎓 Recursos de Aprendizagem

### Documentação Oficial SAP
- SAP Help Portal - BAPIs
- SAP Gateway Developer Guide
- SAP IDoc Documentation

### Tutoriais Práticos
- [RFC Configuration Guide](1_rfc.md#configuração-de-destinos-rfc-sm59)
- [BAPI Best Practices](2_bapi.md#boas-práticas)
- [OData Service Creation](3_odata.md#criar-odata-service-v2)
- [REST API Development](7_rest_api.md#criar-rest-api-provider)

---

## 🔒 Segurança

### Authorization Objects

**RFC:**
- `S_RFC` - RFC Authorization

**OData:**
- `/IWFND/RT_GW` - Gateway Runtime

**IDocs:**
- `S_IDOC_ALL` - IDoc Administration

### Boas Práticas

```abap
" ✅ Validar input
IF lv_input IS INITIAL.
  RAISE EXCEPTION TYPE cx_invalid_parameter.
ENDIF.

" ✅ Usar HTTPS
cl_http_client=>create_by_url(
  EXPORTING url = 'https://api.example.com'  " HTTPS!
  IMPORTING client = lo_client ).

" ✅ Não expor dados sensíveis
" Filtrar campos antes de enviar JSON

" ✅ Rate limiting
IF zcl_rate_limiter=>check_limit( sy-uname ) = abap_false.
  " Reject request
ENDIF.
```

---

## � Performance

### Otimizações

```abap
" ✅ Limitar dados
SELECT * FROM mara
  UP TO 100 ROWS  " Limite!
  INTO TABLE @DATA(lt_data).

" ✅ Usar índices
" WHERE clauses com campos indexados

" ✅ Paginação em APIs
" ?page=1&per_page=20

" ✅ Compressão
" SOAMANAGER → Enable compression

" ✅ Cache
" Cachear respostas de APIs externas
```

---

## 🔗 Próximos Passos

- **[SQL](../sql/index.md)** - Otimizar queries de integração
- **[Performance](../performance/index.md)** - Performance tuning
- **[Debug](../debug/index.md)** - Debugar integrações

---

**Tags:** `#Integrações` `#RFC` `#BAPI` `#OData` `#REST` `#HTTP` `#WebServices` `#IDoc` `#API`

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

1. Comece com **[RFC](1_rfc.md)** para entender chamadas remotas
2. Aprenda **[BAPIs](2_bapi.md)** para processos de negócio
3. Explore **[OData](3_odata.md)** para aplicações Fiori
4. Pratique com **[HTTP Client](4_http_client.md)** consumindo APIs externas
5. Crie sua própria **[REST API](7_rest_api.md)** em ABAP
