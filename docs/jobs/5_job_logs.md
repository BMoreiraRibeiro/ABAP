---
tags:
  - ABAP
  - Jobs
  - Logs
  - Debugging
  - Troubleshooting
---

# Job Logs e Troubleshooting

## 📋 Visão Geral

**Job Logs** registram **mensagens**, **erros** e **outputs** da execução de jobs, essenciais para debugging e auditoria.

---

## 📄 Tipos de Logs

### 1️⃣ Job Log (Protocolo do Job)

**Contém:**
- ✅ Início e fim do job
- ✅ Programas executados
- ✅ Variantes usadas
- ✅ Mensagens do sistema
- ❌ Erros e exceções

**Acessar:**
```text
SM37 → Selecionar job → Job Log (F9)
```

---

### 2️⃣ Spool Output

**Contém:**
- Outputs de `WRITE`
- Listas geradas
- Relatórios impressos

**Acessar:**
```text
SM37 → Selecionar job → Spool list (Shift+F7)
```

---

### 3️⃣ Application Log

**Contém:**
- Logs customizados via `BAL_LOG_*`
- Estruturado para análise

**Acessar:**
```text
SLG1 → Object e Subobject do job
```

---

## 🔍 Analisar Job Log

### Mensagens Comuns

| Tipo | Mensagem | Significado |
|------|----------|-------------|
| ✅ | `Job started` | Job iniciou |
| ✅ | `Job finished` | Job concluído com sucesso |
| ⚠️ | `Job has been released` | Job liberado para execução |
| ❌ | `Job cancelled after system exception` | Dump/crash |
| ❌ | `Timeout during job execution` | Excedeu tempo máximo |
| ❌ | `No more storage space` | Memória insuficiente |

---

### Exemplo de Log

```text
14.01.2025 14:30:00 Job started
14.01.2025 14:30:01 Step 001 started (program Z_PROCESS_ORDERS, variant DAILY)
14.01.2025 14:32:15 1000 orders processed
14.01.2025 14:32:15 Step 001 ended
14.01.2025 14:32:15 Job finished
```

---

## 🛠️ Ler Logs Programaticamente

### Ler Job Log

```abap
REPORT z_read_job_log.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_MY_JOB',
      lv_job_number TYPE tbtcjob-jobcount VALUE '12345678',
      lt_log        TYPE TABLE OF tbtc7,
      ls_log        TYPE tbtc7.

CALL FUNCTION 'BP_JOBLOG_READ'
  EXPORTING
    jobname  = lv_job_name
    jobcount = lv_job_number
  TABLES
    joblog   = lt_log
  EXCEPTIONS
    OTHERS   = 1.

IF sy-subrc = 0.
  LOOP AT lt_log INTO ls_log.
    WRITE: / |{ ls_log-logdate } { ls_log-logtime } - { ls_log-logtext }|.
  ENDLOOP.
ELSE.
  WRITE: / '❌ Erro ao ler log'.
ENDIF.
```

---

### Verificar se Job Teve Erro

```abap
REPORT z_check_job_errors.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_MY_JOB',
      lv_job_number TYPE tbtcjob-jobcount VALUE '12345678',
      lt_log        TYPE TABLE OF tbtc7,
      ls_log        TYPE tbtc7,
      lv_has_error  TYPE abap_bool.

CALL FUNCTION 'BP_JOBLOG_READ'
  EXPORTING
    jobname = lv_job_name
    jobcount = lv_job_number
  TABLES
    joblog = lt_log.

LOOP AT lt_log INTO ls_log.
  " Verificar se contém palavras-chave de erro
  IF ls_log-logtext CS 'error' OR
     ls_log-logtext CS 'exception' OR
     ls_log-logtext CS 'cancelled'.
    lv_has_error = abap_true.
    WRITE: / |❌ ERRO: { ls_log-logtext }|.
  ENDIF.
ENDLOOP.

IF lv_has_error = abap_false.
  WRITE: / '✅ Job executou sem erros'.
ENDIF.
```

---

## 📧 Application Log (SLG1)

### Criar Application Log

```abap
REPORT z_job_with_applog.

DATA: lv_log_handle TYPE balloghndl,
      ls_log_header TYPE bal_s_log,
      ls_msg        TYPE bal_s_msg.

START-OF-SELECTION.
  
  " Criar cabeçalho do log
  ls_log_header-object    = 'ZJOB'.        " Objeto (criar em SLG0)
  ls_log_header-subobject = 'ORDERS'.      " Subobjeto
  ls_log_header-extnumber = 'Daily Run'.   " Identificador externo
  
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log_header
    IMPORTING
      e_log_handle = lv_log_handle.
  
  " Processar dados...
  DATA(lv_count) = 0.
  
  " Simular processamento
  DO 1000 TIMES.
    lv_count = lv_count + 1.
  ENDDO.
  
  " Adicionar mensagem de sucesso
  ls_msg-msgty = 'S'.  " Success
  ls_msg-msgid = '00'.
  ls_msg-msgno = '001'.
  ls_msg-msgv1 = |{ lv_count } ordens processadas|.
  
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = lv_log_handle
      i_s_msg      = ls_msg.
  
  " Salvar log no banco
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_log_handle = lv_log_handle.
  
  COMMIT WORK.
  
  WRITE: / '✅ Log salvo - Ver em SLG1'.
```

---

### Ler Application Log

```abap
REPORT z_read_application_log.

DATA: lt_log_header TYPE TABLE OF balhdr,
      ls_log_header TYPE balhdr,
      lv_log_handle TYPE balloghndl,
      lt_messages   TYPE TABLE OF bal_s_msg,
      ls_message    TYPE bal_s_msg.

" Buscar logs do objeto ZJOB
CALL FUNCTION 'BAL_DB_SEARCH'
  EXPORTING
    i_s_log_filter = VALUE balhdr( object = 'ZJOB' )
  IMPORTING
    e_t_log_header = lt_log_header.

LOOP AT lt_log_header INTO ls_log_header.
  
  " Carregar log
  CALL FUNCTION 'BAL_DB_LOAD'
    EXPORTING
      i_log_handle = ls_log_header-log_handle.
  
  " Ler mensagens
  CALL FUNCTION 'BAL_LOG_MSG_READ'
    EXPORTING
      i_log_handle = ls_log_header-log_handle
    IMPORTING
      e_t_msg      = lt_messages.
  
  WRITE: / |Log: { ls_log_header-extnumber }|.
  
  LOOP AT lt_messages INTO ls_message.
    WRITE: / |  { ls_message-msgty } - { ls_message-msgv1 }|.
  ENDLOOP.
  
ENDLOOP.
```

---

## 🐛 Debugging de Jobs

### Problema: Job com Erro

**Passos:**

1. **SM37 → Ver Job Log**
   ```text
   - Identificar mensagem de erro
   - Ver timestamp do erro
   ```

2. **ST22 → Verificar Dumps**
   ```text
   - Filtrar por usuário do job
   - Verificar data/hora do dump
   ```

3. **Re-executar em Foreground**
   ```abap
   " Executar programa diretamente (não via job)
   " Usar mesma variante
   " Debugar (breakpoints)
   ```

---

### Problema: Job Lento

**Passos:**

1. **SM37 → Ver Spool Output**
   ```text
   - Verificar quantidade de dados processados
   ```

2. **ST12 → Single Transaction Analysis**
   ```text
   - Analisar performance de SELECT
   - Identificar gargalos
   ```

3. **Otimizar código:**
   ```abap
   " Antes (lento)
   LOOP AT lt_orders INTO ls_order.
     SELECT SINGLE * FROM kna1 WHERE kunnr = ls_order-kunnr.
     " ...
   ENDLOOP.
   
   " Depois (rápido)
   SELECT * FROM kna1
     FOR ALL ENTRIES IN lt_orders
     WHERE kunnr = lt_orders-kunnr
     INTO TABLE lt_customers.
   ```

---

## 💡 Exemplos Práticos

### Exemplo 1: Job com Log Detalhado

```abap
*&---------------------------------------------------------------------*
*& Report Z_JOB_WITH_DETAILED_LOG
*&---------------------------------------------------------------------*
REPORT z_job_with_detailed_log.

DATA: lv_log_handle TYPE balloghndl,
      lt_orders     TYPE TABLE OF vbak,
      ls_order      TYPE vbak,
      lv_success    TYPE i,
      lv_errors     TYPE i.

START-OF-SELECTION.
  
  " Criar log
  PERFORM create_app_log CHANGING lv_log_handle.
  
  " Processar dados
  SELECT * FROM vbak UP TO 100 ROWS INTO TABLE lt_orders.
  
  LOOP AT lt_orders INTO ls_order.
    
    TRY.
        " Simular processamento
        IF ls_order-vbeln IS NOT INITIAL.
          lv_success = lv_success + 1.
          PERFORM log_message USING lv_log_handle 'S'
            |Ordem { ls_order-vbeln } processada|.
        ENDIF.
        
      CATCH cx_root INTO DATA(lx_error).
        lv_errors = lv_errors + 1.
        PERFORM log_message USING lv_log_handle 'E'
          |Erro ordem { ls_order-vbeln }: { lx_error->get_text( ) }|.
    ENDTRY.
    
  ENDLOOP.
  
  " Log final
  PERFORM log_message USING lv_log_handle 'I'
    |Total: { lines( lt_orders ) } | Sucesso: { lv_success } | Erros: { lv_errors }|.
  
  " Salvar log
  PERFORM save_app_log USING lv_log_handle.
  
  WRITE: / '✅ Processamento concluído - Ver log em SLG1'.

*&---------------------------------------------------------------------*
FORM create_app_log CHANGING cv_log_handle TYPE balloghndl.
  DATA: ls_log TYPE bal_s_log.
  
  ls_log-object    = 'ZJOB'.
  ls_log-subobject = 'ORDERS'.
  ls_log-extnumber = |Run { sy-datum } { sy-uzeit }|.
  
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING i_s_log = ls_log
    IMPORTING e_log_handle = cv_log_handle.
ENDFORM.

*&---------------------------------------------------------------------*
FORM log_message USING iv_log_handle TYPE balloghndl
                       iv_type        TYPE bapi_mtype
                       iv_message     TYPE string.
  DATA: ls_msg TYPE bal_s_msg.
  
  ls_msg-msgty = iv_type.
  ls_msg-msgid = '00'.
  ls_msg-msgno = '001'.
  ls_msg-msgv1 = iv_message.
  
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = iv_log_handle
      i_s_msg      = ls_msg.
ENDFORM.

*&---------------------------------------------------------------------*
FORM save_app_log USING iv_log_handle TYPE balloghndl.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING i_log_handle = iv_log_handle.
  COMMIT WORK.
ENDFORM.
```

---

### Exemplo 2: Monitor de Erros em Jobs

```abap
REPORT z_monitor_job_errors.

TYPES: BEGIN OF ty_error,
         jobname  TYPE tbtcjob-jobname,
         jobcount TYPE tbtcjob-jobcount,
         sdlstrtdt TYPE tbtcjob-sdlstrtdt,
         error    TYPE string,
       END OF ty_error.

DATA: lt_jobs   TYPE TABLE OF tbtcjob,
      ls_job    TYPE tbtcjob,
      lt_log    TYPE TABLE OF tbtc7,
      ls_log    TYPE tbtc7,
      lt_errors TYPE TABLE OF ty_error,
      ls_error  TYPE ty_error.

" Buscar jobs cancelados das últimas 24h
CALL FUNCTION 'BP_JOB_SELECT'
  EXPORTING
    from_date     = sy-datum - 1
    to_date       = sy-datum
    status_abort  = 'X'
  TABLES
    joblist       = lt_jobs.

LOOP AT lt_jobs INTO ls_job.
  
  " Ler log do job
  CALL FUNCTION 'BP_JOBLOG_READ'
    EXPORTING
      jobname = ls_job-jobname
      jobcount = ls_job-jobcount
    TABLES
      joblog = lt_log.
  
  " Buscar mensagens de erro
  LOOP AT lt_log INTO ls_log.
    IF ls_log-logtext CS 'error' OR ls_log-logtext CS 'exception'.
      ls_error-jobname   = ls_job-jobname.
      ls_error-jobcount  = ls_job-jobcount.
      ls_error-sdlstrtdt = ls_job-sdlstrtdt.
      ls_error-error     = ls_log-logtext.
      APPEND ls_error TO lt_errors.
      EXIT.  " Primeiro erro apenas
    ENDIF.
  ENDLOOP.
  
ENDLOOP.

" Exibir erros
IF lt_errors IS NOT INITIAL.
  WRITE: / '❌ Jobs com erro nas últimas 24h:'.
  WRITE: / ''.
  
  LOOP AT lt_errors INTO ls_error.
    WRITE: / |Job: { ls_error-jobname } ({ ls_error-jobcount })|.
    WRITE: / |Data: { ls_error-sdlstrtdt }|.
    WRITE: / |Erro: { ls_error-error }|.
    WRITE: / '---------------------------------------------------------'.
  ENDLOOP.
ELSE.
  WRITE: / '✅ Nenhum job com erro nas últimas 24h'.
ENDIF.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre logar eventos importantes
PERFORM log_message USING lv_log_handle 'I'
  |{ lv_count } registros processados|.

" 2. Logar erros com contexto
CATCH cx_root INTO DATA(lx_err).
  PERFORM log_message USING lv_log_handle 'E'
    |Erro ao processar { ls_data-key }: { lx_err->get_text( ) }|.

" 3. Usar Application Log para jobs complexos
" Estruturado, query fácil em SLG1

" 4. Incluir estatísticas finais
WRITE: / |Total: { lv_total } | OK: { lv_ok } | Erros: { lv_errors }|.

" 5. Manter logs por período limitado
" Configurar retenção em SLG2
```

### ❌ Evitar

```abap
" 1. Jobs sem logging
" ❌ Impossível troubleshooting

" 2. Mensagens genéricas
WRITE: / 'Erro'.  " ❌ Sem contexto

" 3. Logar tudo em WRITE
" ❌ Dificulta análise programática
" ✅ Usar Application Log

" 4. Ignorar warnings
" ⚠️ Podem indicar problemas futuros

" 5. Não limpar logs antigos
" ❌ Degrada performance do banco
```

---

## 🔗 Próximos Passos

- **[Criar Jobs](1_criar_jobs.md)** - Implementar logging desde criação
- **[Monitorização](4_monitorizacao.md)** - Verificar status em SM37
- **[Batch Processing](6_batch_processing.md)** - Logs em processamento batch

---

**Tags:** `#Logs` `#Debugging` `#Troubleshooting` `#SLG1` `#ApplicationLog`
