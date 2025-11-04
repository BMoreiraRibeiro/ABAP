---
tags:
  - ABAP
  - Web Services
  - SOAP
  - WSDL
  - Integrações
---

# Web Services SOAP

## 📋 Visão Geral

**Web Services SOAP** (Simple Object Access Protocol) permitem **integração** entre sistemas usando **XML** e protocolo **HTTP/HTTPS**.

**Características:**
- ✅ Baseado em XML
- ✅ Protocolo SOAP
- ✅ WSDL para definição
- ✅ Suporte a tipos complexos
- ✅ WS-Security para segurança
- ⚠️ Mais pesado que REST

---

## 🎯 SOAP vs REST

| Aspecto | SOAP | REST |
|---------|------|------|
| **Protocolo** | SOAP (XML) | HTTP |
| **Formato** | Apenas XML | JSON, XML, etc. |
| **Complexidade** | Alta | Baixa |
| **Performance** | Mais lento | Mais rápido |
| **Segurança** | WS-Security | HTTPS, OAuth |
| **Uso SAP** | Legacy, ERP | **Fiori, S/4HANA** |

---

## 🛠️ Criar Web Service (Provider)

### Transaction SE80

**Criar Function Module:**

```abap
FUNCTION z_ws_get_customer.
*"----------------------------------------------------------------------
*"*"Interface Local:
*"  IMPORTING
*"     VALUE(IV_KUNNR) TYPE  KUNNR
*"  EXPORTING
*"     VALUE(ES_CUSTOMER) TYPE  KNA1
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"  EXCEPTIONS
*"      CUSTOMER_NOT_FOUND
*"----------------------------------------------------------------------

  SELECT SINGLE * FROM kna1 INTO es_customer
    WHERE kunnr = iv_kunnr.
    
  IF sy-subrc = 0.
    ev_message = |Cliente { iv_kunnr } encontrado|.
  ELSE.
    ev_message = |Cliente { iv_kunnr } não existe|.
    RAISE customer_not_found.
  ENDIF.

ENDFUNCTION.
```

**Atributos da Function:**
- ☑ **Remote-Enabled Module** (SE37 → Attributes)

---

### Transaction SE37 - Enterprise Services

**Criar Web Service:**

1. **Utilities** → More Utilities → **Create Web Service**
2. **Service Definition:** `Z_CUSTOMER_WS`
3. **Endpoint:** `Z_CUSTOMER_EP`
4. **Profile:** `Default`
5. **Save & Activate**

---

### Transaction SOAMANAGER

**Configurar e ativar Web Service:**

1. **SOAMANAGER**
2. **Web Service Configuration**
3. **Search:** `Z_CUSTOMER_WS`
4. **Create Service**
5. **Binding:** `Z_CUSTOMER_BINDING`
6. **Configure:** HTTPS, autenticação
7. **Save**

**URL do WSDL:**
```
https://server:port/sap/bc/srt/wsdl/flv_10002A111AD1/bndg_url/sap/bc/srt/rfc/sap/z_customer_ws/100/z_customer_binding/z_customer_binding
```

---

## 🌐 Consumir Web Service (Consumer)

### Transaction SE80

**Criar Proxy Local:**

1. **SE80** → **Enterprise Services**
2. **Create** → **Service Consumer**
3. **WSDL:** URL ou upload arquivo
4. **Package:** ZPACKAGE
5. **Prefix:** ZCO_
6. **Generate**

---

### Usar Proxy Gerado

```abap
*&---------------------------------------------------------------------*
*& Report Z_CONSUME_WEBSERVICE
*&---------------------------------------------------------------------*
REPORT z_consume_webservice.

DATA: lo_proxy TYPE REF TO zco_customer_ws,
      ls_input TYPE zco_get_customer_request,
      ls_output TYPE zco_get_customer_response.

PARAMETERS: p_kunnr TYPE kunnr DEFAULT '0001000000'.

START-OF-SELECTION.
  
  TRY.
      " Criar proxy
      CREATE OBJECT lo_proxy
        EXPORTING
          logical_port_name = 'Z_CUSTOMER_LP'.  " SOAMANAGER
      
      " Input
      ls_input-iv_kunnr = p_kunnr.
      
      " Chamar Web Service
      CALL METHOD lo_proxy->get_customer
        EXPORTING
          input  = ls_input
        IMPORTING
          output = ls_output.
      
      " Resultado
      WRITE: / '═══ RESULTADO ═══', /.
      WRITE: / |Cliente: { ls_output-es_customer-kunnr }|.
      WRITE: / |Nome: { ls_output-es_customer-name1 }|.
      WRITE: / |Cidade: { ls_output-es_customer-ort01 }|.
      WRITE: / |Mensagem: { ls_output-ev_message }|, /.
      
    CATCH cx_ai_system_fault INTO DATA(lo_sys_fault).
      WRITE: / |Erro de sistema: { lo_sys_fault->get_text( ) }|.
      
    CATCH cx_ai_application_fault INTO DATA(lo_app_fault).
      WRITE: / |Erro de aplicação: { lo_app_fault->get_text( ) }|.
      
    CATCH cx_root INTO DATA(lo_ex).
      WRITE: / |Erro: { lo_ex->get_text( ) }|.
  ENDTRY.
```

---

### Configurar Logical Port (SOAMANAGER)

1. **SOAMANAGER**
2. **Single Service Administration**
3. **Search:** Proxy name
4. **Create Logical Port:** `Z_CUSTOMER_LP`
5. **URL:** URL do WSDL remoto
6. **Authentication:** Basic, SSL, etc.
7. **Save**

---

## 🔐 Segurança

### WS-Security

**Username Token:**

```xml
<wsse:Security>
  <wsse:UsernameToken>
    <wsse:Username>user</wsse:Username>
    <wsse:Password Type="PasswordText">pass</wsse:Password>
  </wsse:UsernameToken>
</wsse:Security>
```

**Configurar em SOAMANAGER:**
- Authentication: **User ID/Password**
- Transport Level Security: **HTTPS**

---

### Certificado SSL

**STRUST - Trust Manager:**

1. **STRUST**
2. **SSL Client (Anonymous)**
3. Import Certificate
4. Add to Certificate List

**SOAMANAGER:**
- Authentication: **SSL Client Certificate**
- Select Certificate

---

## 📊 SOAP Request/Response

### Request Example

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" 
                  xmlns:urn="urn:sap-com:document:sap:rfc:functions">
   <soapenv:Header/>
   <soapenv:Body>
      <urn:Z_WS_GET_CUSTOMER>
         <IV_KUNNR>0001000000</IV_KUNNR>
      </urn:Z_WS_GET_CUSTOMER>
   </soapenv:Body>
</soapenv:Envelope>
```

### Response Example

```xml
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
   <soapenv:Body>
      <n0:Z_WS_GET_CUSTOMER.Response xmlns:n0="urn:sap-com:document:sap:rfc:functions">
         <ES_CUSTOMER>
            <KUNNR>0001000000</KUNNR>
            <NAME1>Cliente Teste</NAME1>
            <ORT01>Lisboa</ORT01>
         </ES_CUSTOMER>
         <EV_MESSAGE>Cliente 0001000000 encontrado</EV_MESSAGE>
      </n0:Z_WS_GET_CUSTOMER.Response>
   </soapenv:Body>
</soapenv:Envelope>
```

---

## 💡 Exemplo Completo: Integração Externa

### Sistema Externo → SAP (Provider)

**Function Module:**

```abap
FUNCTION z_ws_create_sales_order.
*"----------------------------------------------------------------------
*"*"Interface Local:
*"  IMPORTING
*"     VALUE(IS_HEADER) TYPE  BAPISDHD1
*"  TABLES
*"      IT_ITEMS STRUCTURE  BAPISDITM
*"      ET_RETURN STRUCTURE  BAPIRET2
*"  EXPORTING
*"     VALUE(EV_ORDER_NUMBER) TYPE  VBELN_VA
*"----------------------------------------------------------------------

  " Criar ordem via BAPI
  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
    EXPORTING
      order_header_in = is_header
    IMPORTING
      salesdocument   = ev_order_number
    TABLES
      return          = et_return
      order_items_in  = it_items.
  
  " Commit
  READ TABLE et_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = 'X'.
  ENDIF.

ENDFUNCTION.
```

**Expor via SOAMANAGER** (passos acima).

---

### SAP → Sistema Externo (Consumer)

**Consumir Web Service de terceiros:**

```abap
*&---------------------------------------------------------------------*
*& Report Z_CALL_EXTERNAL_WS
*&---------------------------------------------------------------------*
REPORT z_call_external_ws.

DATA: lo_proxy TYPE REF TO zco_external_service,
      ls_request TYPE zco_weather_request,
      ls_response TYPE zco_weather_response.

PARAMETERS: p_city TYPE string DEFAULT 'Lisboa'.

START-OF-SELECTION.
  
  TRY.
      " Criar proxy (gerado a partir de WSDL externo)
      CREATE OBJECT lo_proxy
        EXPORTING
          logical_port_name = 'Z_WEATHER_LP'.
      
      " Request
      ls_request-city = p_city.
      
      " Chamar serviço externo
      CALL METHOD lo_proxy->get_weather
        EXPORTING
          input  = ls_request
        IMPORTING
          output = ls_response.
      
      " Resultado
      WRITE: / |Cidade: { p_city }|.
      WRITE: / |Temperatura: { ls_response-temperature }°C|.
      WRITE: / |Condição: { ls_response-condition }|.
      
    CATCH cx_root INTO DATA(lo_ex).
      WRITE: / |Erro: { lo_ex->get_text( ) }|.
  ENDTRY.
```

---

## 🔧 Transaction SPROXY

**Service Proxy** - alternativa a SE80 para criar proxies.

### Criar Proxy

1. **SPROXY**
2. **Outside-In** → External WSDL
3. Upload WSDL ou URL
4. **Generate**

### Vantagens

- ✅ Interface mais moderna
- ✅ Melhor para Enterprise Services
- ✅ Suporte a XSD complexos

---

## 🛠️ Monitorização

### Transaction SRT_UTIL

**Runtime Administration:**
- Ver logs de execução
- Mensagens SOAP
- Erros

### Transaction SXMB_MONI

**Integration Engine Monitoring:**
- Monitorar mensagens XML
- Ver payloads
- Reprocessar mensagens

### Transaction SICF

**HTTP Service Maintenance:**
- Ativar/desativar serviços
- Configurar handlers
- Ver estatísticas

---

## ⚡ Performance e Boas Práticas

### ✅ Fazer

```abap
" 1. Reutilizar instância do proxy
DATA: go_proxy TYPE REF TO zco_service.  " Global

FORM call_service.
  IF go_proxy IS NOT BOUND.
    CREATE OBJECT go_proxy
      EXPORTING logical_port_name = 'LP_NAME'.  " ✅ Criar uma vez
  ENDIF.
  
  " Usar go_proxy...
ENDFORM.

" 2. Tratar exceções específicas
TRY.
    CALL METHOD lo_proxy->method.
  CATCH cx_ai_system_fault.
    " Erro de comunicação
  CATCH cx_ai_application_fault.
    " Erro de negócio
  CATCH cx_root.
    " Outros erros
ENDTRY.

" 3. Timeout adequado
" Configurar em SOAMANAGER → Logical Port → Timeouts

" 4. Comprimir grandes payloads
" SOAMANAGER → Service Definition → Compression

" 5. Usar logical ports (não hardcode URLs)
CREATE OBJECT lo_proxy
  EXPORTING logical_port_name = 'Z_LP'.  " ✅ SM59
```

### ❌ Evitar

```abap
" 1. Criar proxy em cada chamada
FORM call_ws.
  CREATE OBJECT lo_proxy.  " ❌ Repetitivo!
  CALL METHOD lo_proxy->method.
ENDFORM.

" 2. Não tratar erros
CALL METHOD lo_proxy->method.  " ❌ E se falhar?

" 3. Hardcoded URLs
DATA lv_url TYPE string VALUE 'http://prod.com/ws'.  " ❌ Usar LP

" 4. Payloads muito grandes sem compressão
" Configurar compressão em SOAMANAGER

" 5. Sync chamadas para operações longas
CALL METHOD lo_proxy->long_operation.  " ❌ Timeout!
" Melhor: usar RFC assíncrono ou job
```

---

## 💡 Exemplo: Integração com Sistema Bancário

### Provider (SAP expõe)

**Function Module:**

```abap
FUNCTION z_ws_payment_status.
*"----------------------------------------------------------------------
*"*"Interface Local:
*"  IMPORTING
*"     VALUE(IV_PAYMENT_ID) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_STATUS) TYPE  STRING
*"     VALUE(EV_AMOUNT) TYPE  WRBTR
*"     VALUE(EV_CURRENCY) TYPE  WAERS
*"     VALUE(EV_DATE) TYPE  DATUM
*"----------------------------------------------------------------------

  " Buscar pagamento
  SELECT SINGLE
    status,
    amount,
    currency,
    payment_date
  FROM zpayments
  WHERE payment_id = @iv_payment_id
  INTO (@ev_status, @ev_amount, @ev_currency, @ev_date).
  
  IF sy-subrc <> 0.
    ev_status = 'NOT_FOUND'.
  ENDIF.

ENDFUNCTION.
```

**Expor via SOAMANAGER.**

---

### Consumer (SAP consome)

**Chamar API bancária:**

```abap
*&---------------------------------------------------------------------*
*& Report Z_BANK_API_CALL
*&---------------------------------------------------------------------*
REPORT z_bank_api_call.

DATA: lo_proxy TYPE REF TO zco_bank_api,
      ls_request TYPE zco_transfer_request,
      ls_response TYPE zco_transfer_response.

PARAMETERS: p_from TYPE string DEFAULT 'PT50123456789',
            p_to   TYPE string DEFAULT 'PT50987654321',
            p_amnt TYPE p DECIMALS 2 DEFAULT '1000.00'.

START-OF-SELECTION.
  
  TRY.
      " Criar proxy para API do banco
      CREATE OBJECT lo_proxy
        EXPORTING
          logical_port_name = 'Z_BANK_LP'.
      
      " Preparar transferência
      ls_request-from_account = p_from.
      ls_request-to_account   = p_to.
      ls_request-amount       = p_amnt.
      ls_request-currency     = 'EUR'.
      ls_request-description  = 'Pagamento SAP'.
      
      " Executar transferência
      CALL METHOD lo_proxy->transfer_funds
        EXPORTING
          input  = ls_request
        IMPORTING
          output = ls_response.
      
      " Resultado
      IF ls_response-status = 'SUCCESS'.
        WRITE: / '✅ Transferência realizada com sucesso!'.
        WRITE: / |ID Transação: { ls_response-transaction_id }|.
        WRITE: / |Timestamp: { ls_response-timestamp }|.
      ELSE.
        WRITE: / '❌ Erro na transferência:'.
        WRITE: / |Código: { ls_response-error_code }|.
        WRITE: / |Mensagem: { ls_response-error_message }|.
      ENDIF.
      
    CATCH cx_ai_system_fault INTO DATA(lo_sys_ex).
      WRITE: / |Erro de comunicação: { lo_sys_ex->get_text( ) }|.
      
    CATCH cx_ai_application_fault INTO DATA(lo_app_ex).
      WRITE: / |Erro do banco: { lo_app_ex->get_text( ) }|.
      
    CATCH cx_root INTO DATA(lo_ex).
      WRITE: / |Erro: { lo_ex->get_text( ) }|.
  ENDTRY.
```

---

## 🔧 Troubleshooting

### Erro: "Logical Port not found"

**Solução:**
1. **SOAMANAGER**
2. Criar Logical Port
3. Configurar URL e autenticação

---

### Erro: "HTTP 500 - Internal Server Error"

**Causas:**
- Erro no código do serviço
- Dados inválidos

**Solução:**
- **SRT_UTIL** → Ver logs
- **ST22** → Dumps
- Debugar function module

---

### Erro: "Connection timeout"

**Solução:**
1. **SOAMANAGER** → Logical Port
2. **Timeouts** tab
3. Aumentar timeout (ex: 60s)

---

### Erro: "Certificate validation failed"

**Solução:**
1. **STRUST**
2. Import certificado do servidor
3. Add to Certificate List

---

## 🔗 Alternativas Modernas

### REST vs SOAP

**Preferir REST quando:**
- ✅ Integração com sistemas modernos
- ✅ Aplicações mobile/web
- ✅ Performance crítica
- ✅ Simplicidade

**Usar SOAP quando:**
- ✅ Sistemas legados
- ✅ WS-Security necessário
- ✅ Contratos formais (WSDL)
- ✅ Transações distribuídas

### Migração para OData/REST

```abap
" SOAP (antigo)
CALL METHOD lo_proxy->get_customer
  EXPORTING input = ls_input
  IMPORTING output = ls_output.

" REST (moderno)
DATA(lo_http) = cl_http_client=>create_by_url( 'https://api.com/customers' ).
lo_http->request->set_method( if_http_request=>co_request_method_get ).
lo_http->send( ).
lo_http->receive( ).
DATA(lv_json) = lo_http->response->get_cdata( ).
```

---

## 🔗 Próximos Passos

- **[OData](3_odata.md)** - Alternativa RESTful moderna
- **[HTTP Client](4_http_client.md)** - Consumir APIs REST
- **[REST API](7_rest_api.md)** - Criar APIs REST em ABAP

---

**Tags:** `#WebServices` `#SOAP` `#WSDL` `#Integrações` `#SPROXY` `#SOAMANAGER`
