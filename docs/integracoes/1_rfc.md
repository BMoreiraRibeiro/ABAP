---
tags:
  - ABAP
  - RFC
  - Integrações
  - Remote Function Call
---

# RFC - Remote Function Call

## 📋 Visão Geral

**RFC** (Remote Function Call) permite que programas ABAP chamem funções em sistemas SAP remotos ou que sistemas externos chamem funções ABAP. É a base para comunicação entre sistemas SAP.

---

## 🎯 Tipos de RFC

### 1️⃣ sRFC - Synchronous RFC

Chamada **síncrona**: o programa espera pela resposta.

```abap
CALL FUNCTION 'Z_REMOTE_FUNCTION'
  DESTINATION 'SAP_SYSTEM'
  EXPORTING
    iv_param = lv_value
  IMPORTING
    ev_result = lv_result
  EXCEPTIONS
    system_failure = 1
    communication_failure = 2
    OTHERS = 3.

IF sy-subrc <> 0.
  MESSAGE 'Erro na chamada RFC' TYPE 'E'.
ENDIF.
```

**Características:**
- ✅ Resposta imediata
- ✅ Simples de usar
- ❌ Bloqueia execução
- ❌ Não recomendado para operações longas

---

### 2️⃣ aRFC - Asynchronous RFC

Chamada **assíncrona**: não espera pela resposta.

```abap
CALL FUNCTION 'Z_PROCESSO_LONGO'
  STARTING NEW TASK 'TASK1'
  DESTINATION 'SAP_SYSTEM'
  EXPORTING
    iv_param = lv_value
  EXCEPTIONS
    communication_failure = 1
    system_failure = 2
    OTHERS = 3.

" Continua execução sem esperar
WRITE: / 'Processamento iniciado em background'.
```

**Com callback:**
```abap
" Chamada assíncrona com callback
CALL FUNCTION 'Z_PROCESSO_LONGO'
  STARTING NEW TASK 'TASK1'
  DESTINATION 'SAP_SYSTEM'
  PERFORMING callback_form ON END OF TASK
  EXPORTING
    iv_param = lv_value.

WRITE: / 'Aguardando callback...'.

FORM callback_form USING pv_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_PROCESSO_LONGO'
    IMPORTING ev_result = DATA(lv_result).
    
  WRITE: / |Resultado recebido: { lv_result }|.
ENDFORM.
```

---

### 3️⃣ tRFC - Transactional RFC

RFC com **garantia de execução única**.

```abap
CALL FUNCTION 'Z_UPDATE_DATA'
  IN BACKGROUND TASK
  DESTINATION 'SAP_SYSTEM'
  EXPORTING
    iv_data = lv_data.

" Commit para enviar
COMMIT WORK.
```

**Características:**
- ✅ Executa exatamente uma vez
- ✅ Pode ser agendado
- ✅ Tolerante a falhas
- ⚠️ Assíncrono (sem retorno imediato)

---

### 4️⃣ qRFC - Queued RFC

RFC com **ordem garantida** de execução.

```abap
CALL FUNCTION 'Z_PROCESSO_SEQUENCIAL'
  IN BACKGROUND TASK
  DESTINATION 'SAP_SYSTEM'
  QUEUE 'QUEUE_NAME'
  EXPORTING
    iv_sequence = lv_seq.

COMMIT WORK.
```

**Uso:** Quando a ordem de execução é crítica.

---

### 5️⃣ bgRFC - Background RFC

RFC moderno para **processos em background**.

**Características:**
- Substituição do tRFC/qRFC
- Melhor monitorização
- Mais controle sobre execução

---

## 🔧 Configuração de Destinos RFC (SM59)

### Transaction SM59

**Caminho:** SM59 → Create

### Tipos de Conexão

| Tipo | Descrição | Uso |
|------|-----------|-----|
| **3** | ABAP Connection | Sistema SAP remoto |
| **H** | HTTP Connection | Servidor web |
| **G** | HTTP Connection to Ext. Server | Gateway |
| **T** | TCP/IP Connection | Sistemas não-SAP |

### Criar Destino RFC

**Exemplo: Conexão para sistema SAP remoto**

1. **SM59** → Create
2. **RFC Destination:** `Z_SYSTEM_QAS`
3. **Connection Type:** 3 (ABAP Connection)
4. **Technical Settings:**
   - Target Host: `sap-server.empresa.com`
   - System Number: `00`
   - Instance Number: `00`
5. **Logon & Security:**
   - Language: `PT`
   - Client: `100`
   - User: `RFC_USER`
   - Password: `******`
6. **Test Connection**
7. **Save**

---

## 💡 Exemplos Práticos

### Exemplo 1: Buscar Dados de Sistema Remoto

```abap
REPORT z_rfc_buscar_dados.

DATA: lt_clientes TYPE TABLE OF kna1,
      lv_dest TYPE rfcdest VALUE 'Z_SYSTEM_QAS'.

TRY.
    CALL FUNCTION 'Z_GET_CUSTOMERS'
      DESTINATION lv_dest
      IMPORTING
        et_customers = lt_clientes
      EXCEPTIONS
        system_failure = 1 MESSAGE DATA(lv_msg)
        communication_failure = 2 MESSAGE lv_msg
        OTHERS = 3.
        
    IF sy-subrc = 0.
      WRITE: / |{ lines( lt_clientes ) } clientes obtidos|.
      
      LOOP AT lt_clientes INTO DATA(ls_cliente) TO 10.
        WRITE: / ls_cliente-kunnr, ls_cliente-name1.
      ENDLOOP.
    ELSE.
      WRITE: / |Erro RFC: { lv_msg }|.
    ENDIF.
    
  CATCH cx_root INTO DATA(lo_ex).
    WRITE: / lo_ex->get_text( ).
ENDTRY.
```

---

### Exemplo 2: RFC Assíncrono com Múltiplas Tarefas

```abap
REPORT z_rfc_async_multi.

DATA: lt_tasks TYPE TABLE OF string,
      lv_count TYPE i.

" Iniciar múltiplas tarefas
DO 5 TIMES.
  DATA(lv_task) = |TASK_{ sy-index }|.
  APPEND lv_task TO lt_tasks.
  
  CALL FUNCTION 'Z_PROCESSO_LONGO'
    STARTING NEW TASK lv_task
    DESTINATION 'Z_SYSTEM_QAS'
    PERFORMING return_data ON END OF TASK
    EXPORTING
      iv_id = sy-index.
      
  WRITE: / |Tarefa { lv_task } iniciada|.
ENDDO.

" Aguardar todas as tarefas
WAIT UNTIL lv_count >= lines( lt_tasks ) UP TO 60 SECONDS.

WRITE: / |Todas as { lv_count } tarefas concluídas|.

FORM return_data USING pv_taskname.
  DATA: lv_result TYPE string.
  
  RECEIVE RESULTS FROM FUNCTION 'Z_PROCESSO_LONGO'
    IMPORTING ev_resultado = lv_result
    EXCEPTIONS
      communication_failure = 1
      system_failure = 2.
      
  IF sy-subrc = 0.
    ADD 1 TO lv_count.
    WRITE: / |{ pv_taskname }: { lv_result }|.
  ENDIF.
ENDFORM.
```

---

### Exemplo 3: tRFC para Atualização Garantida

```abap
REPORT z_trfc_update.

DATA: lt_orders TYPE TABLE OF vbak.

SELECT * FROM vbak INTO TABLE lt_orders UP TO 10 ROWS.

" Enviar dados para sistema remoto (tRFC)
CALL FUNCTION 'Z_UPDATE_ORDERS'
  IN BACKGROUND TASK
  DESTINATION 'Z_SYSTEM_PRD'
  TABLES
    it_orders = lt_orders.

" Commit para executar tRFC
COMMIT WORK.

IF sy-subrc = 0.
  WRITE: / 'Dados enviados via tRFC (execução garantida)'.
ELSE.
  WRITE: / 'Erro ao agendar tRFC'.
ENDIF.

" Verificar status: SM58
```

---

### Exemplo 4: RFC para Sistema Externo (Java/Python)

**Function Module RFC-enabled:**

```abap
FUNCTION z_get_flight_info.
*"----------------------------------------------------------------------
*"*"Interface Local:
*"  IMPORTING
*"     VALUE(IV_CARRID) TYPE  S_CARR_ID
*"     VALUE(IV_CONNID) TYPE  S_CONN_ID
*"  EXPORTING
*"     VALUE(ES_FLIGHT) TYPE  SFLIGHT
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  SELECT SINGLE * FROM sflight INTO es_flight
    WHERE carrid = iv_carrid
      AND connid = iv_connid.
      
  IF sy-subrc <> 0.
    RAISE not_found.
  ENDIF.

ENDFUNCTION.
```

**Ativar RFC:** SE37 → Attributes → ☑ Remote-Enabled Module

**Chamar de Python (usando pyrfc):**
```python
from pyrfc import Connection

conn = Connection(
    user='RFC_USER',
    passwd='password',
    ashost='sap-server.com',
    sysnr='00',
    client='100'
)

result = conn.call('Z_GET_FLIGHT_INFO', 
    IV_CARRID='LH',
    IV_CONNID='0400'
)

print(result['ES_FLIGHT'])
conn.close()
```

---

## 🛠️ Monitorização de RFC

### SM58 - Transactional RFC Monitor

Ver **tRFCs falhados** ou **pendentes**:

1. **SM58**
2. Ver lista de tRFCs
3. Reprocessar manualmente se necessário

### SM59 - RFC Destinations

- Listar todos os destinos
- Testar conexões
- Ver estatísticas

### ST22 - Dumps RFC

Analisar erros de RFC que geraram dump.

---

## 🔒 Segurança RFC

### Autenticação

**Usuário/Senha:**
```abap
" Configurado em SM59
User: RFC_USER
Password: ******
```

**Certificado:**
```
SM59 → Logon & Security → Use SSL/TLS
```

**Trusted RFC:**
```
SM59 → Logon & Security → Trusted System
```

### Autorização

**Perfil necessário:** `S_RFC`

```
Authorization Object: S_RFC
Activity: Execute (16)
Function Group: Z_FUNC_GROUP
```

---

## ⚡ Performance e Boas Práticas

### ✅ Fazer

```abap
" 1. Limitar quantidade de dados
CALL FUNCTION 'Z_GET_DATA'
  DESTINATION lv_dest
  EXPORTING
    iv_max_rows = 1000  " ✅ Limite
  IMPORTING
    et_data = lt_data.

" 2. Timeout adequado
CALL FUNCTION 'Z_LONG_PROCESS'
  DESTINATION lv_dest
  EXPORTING
    iv_param = lv_value
  EXCEPTIONS
    system_failure = 1
    communication_failure = 2
    resource_failure = 3.  " Timeout

" 3. Usar aRFC para operações longas
CALL FUNCTION 'Z_BATCH_PROCESS'
  STARTING NEW TASK 'ASYNC'
  DESTINATION lv_dest.

" 4. Pool de conexões (gerenciado pelo sistema)
" SM59 → Advanced Options → Connection Pooling
```

### ❌ Evitar

```abap
" 1. RFC síncrono para operações longas
CALL FUNCTION 'Z_PROCESSO_3_HORAS'
  DESTINATION lv_dest.  " ❌ Timeout!

" 2. Loops com RFC
LOOP AT lt_items INTO DATA(ls_item).
  CALL FUNCTION 'Z_PROCESS_ITEM'  " ❌ Muito lento!
    DESTINATION lv_dest
    EXPORTING iv_item = ls_item.
ENDLOOP.

" ✅ Melhor: enviar tabela completa
CALL FUNCTION 'Z_PROCESS_ALL_ITEMS'
  DESTINATION lv_dest
  TABLES it_items = lt_items.

" 3. Não tratar exceções
CALL FUNCTION 'Z_RFC_CALL'
  DESTINATION lv_dest.  " ❌ E se falhar?

" 4. Credenciais hardcoded
DATA lv_password TYPE string VALUE 'senha123'.  " ❌ NUNCA!
```

---

## 🔧 Troubleshooting

### Erro: "Destination not found"

**Solução:**
- Verificar SM59
- Destino existe?
- Nome correto?

### Erro: "Communication failure"

**Causas:**
- Rede down
- Servidor remoto offline
- Firewall bloqueando

**Solução:**
```abap
" Implementar retry
DATA lv_tentativas TYPE i VALUE 0.

DO 3 TIMES.
  lv_tentativas = lv_tentativas + 1.
  
  CALL FUNCTION 'Z_RFC_CALL'
    DESTINATION lv_dest
    EXCEPTIONS
      communication_failure = 1.
      
  IF sy-subrc = 0.
    EXIT.  " Sucesso
  ELSE.
    WAIT UP TO 2 SECONDS.  " Aguardar antes de tentar novamente
  ENDIF.
ENDDO.

IF sy-subrc <> 0.
  MESSAGE 'Falha após 3 tentativas' TYPE 'E'.
ENDIF.
```

### Erro: "Authorization failure"

**Solução:**
- Verificar usuário em SM59
- Verificar perfil S_RFC
- PFCG para ajustar autorizações

---

## 💡 Exemplo Completo: Sistema de Sincronização

```abap
*&---------------------------------------------------------------------*
*& Report Z_RFC_SYNC_CUSTOMERS
*&---------------------------------------------------------------------*
REPORT z_rfc_sync_customers.

PARAMETERS: p_dest TYPE rfcdest DEFAULT 'Z_SYSTEM_QAS'.

DATA: lt_customers TYPE TABLE OF kna1,
      lv_synced TYPE i,
      lv_errors TYPE i.

START-OF-SELECTION.
  
  WRITE: / '═══ SINCRONIZAÇÃO DE CLIENTES ═══', /.
  
  " 1. Buscar clientes do sistema local
  SELECT * FROM kna1 INTO TABLE lt_customers UP TO 100 ROWS
    WHERE land1 = 'PT'.
    
  WRITE: / |Clientes a sincronizar: { lines( lt_customers ) }|, /.
  
  " 2. Validar destino RFC
  DATA(lv_dest_exists) = abap_false.
  
  CALL FUNCTION 'RFC_PING'
    DESTINATION p_dest
    EXCEPTIONS
      communication_failure = 1
      system_failure = 2
      OTHERS = 3.
      
  IF sy-subrc = 0.
    WRITE: / |✅ Conexão RFC ativa: { p_dest }|.
    lv_dest_exists = abap_true.
  ELSE.
    WRITE: / |❌ Erro ao conectar: { p_dest }|.
    WRITE: / 'Abortando sincronização'.
    RETURN.
  ENDIF.
  
  SKIP.
  
  " 3. Sincronizar dados (tRFC para garantir execução)
  IF lv_dest_exists = abap_true.
    CALL FUNCTION 'Z_SYNC_CUSTOMERS'
      IN BACKGROUND TASK
      DESTINATION p_dest
      TABLES
        it_customers = lt_customers
      EXCEPTIONS
        communication_failure = 1
        system_failure = 2
        OTHERS = 3.
        
    IF sy-subrc = 0.
      COMMIT WORK.
      WRITE: / '✅ Sincronização agendada via tRFC'.
      WRITE: / 'Verificar status em SM58'.
    ELSE.
      WRITE: / '❌ Erro ao agendar sincronização'.
    ENDIF.
  ENDIF.
  
  SKIP.
  WRITE: / '═══ FIM ═══'.
```

---

## 🔗 Próximos Passos

- **[BAPI](2_bapi.md)** - BAPIs são RFCs especiais
- **[HTTP Client](4_http_client.md)** - Alternativa moderna a RFC
- **[IDocs](6_idocs.md)** - Outro método de integração

---

**Tags:** `#RFC` `#Integrações` `#RemoteFunctionCall` `#SM59` `#ABAP`
