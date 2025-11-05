---
tags:
  - ABAP
  - Jobs
  - SM37
  - Monitorização
---

# Monitorização de Jobs (SM37)

## 📋 Visão Geral

**SM37** é a transação para **monitorar** jobs em background - verificar status, logs, erros e histórico.

---

## 🔍 Acessar SM37

1. **Executar transação:** `/nSM37`
2. **Filtros principais:**
   - **Job Name:** Nome do job (wildcards `*` permitidos)
   - **User:** Usuário que criou
   - **Start Date:** Período de execução
   - **Status:** Scheduled, Released, Ready, Active, Finished, Cancelled

---

## 📊 Status de Jobs

| Status | Significado | Ação |
|--------|-------------|------|
| **Scheduled** | Agendado, aguardando horário | ⏳ Aguardar |
| **Released** | Liberado para execução | 🟢 Normal |
| **Ready** | Pronto, aguardando server disponível | 🟡 Verificar carga |
| **Active** | Executando agora | ⚡ Em progresso |
| **Finished** | Concluído com sucesso | ✅ OK |
| **Cancelled** | Cancelado/Com erro | ❌ Ver log |

---

## 🛠️ Operações em SM37

### Ver Log do Job

1. Selecionar job
2. **Job → Job Log** (ou `F9`)

**Verificar:**
- ✅ Mensagens de sucesso
- ⚠️ Warnings
- ❌ Erros críticos

---

### Ver Spool de Output

1. Selecionar job
2. **Job → Spool list** (ou `Shift+F7`)

**Ver impressões/outputs:**
- Relatórios gerados
- Listas de dados
- Arquivos exportados

---

### Cancelar Job Ativo

```text
1. Selecionar job Active
2. Job → Cancel Active Job
3. Confirmar
```

⚠️ **Cuidado:** Cancela imediatamente, pode deixar dados inconsistentes!

---

### Repetir Job

```text
1. Selecionar job Finished
2. Job → Copy to reschedule
3. Ajustar agendamento se necessário
4. Salvar
```

---

### Deletar Job

```text
1. Selecionar job Scheduled ou Cancelled
2. Job → Delete
3. Confirmar
```

❌ **Não pode deletar jobs:** Active ou Released

---

## 📋 Filtros Úteis

### Jobs de Hoje

```text
Job Name: *
User: <seu usuário>
Start Date: <data atual>
Status: Finished + Cancelled
```

---

### Jobs Agendados Futuros

```text
Job Name: Z*
Status: Scheduled
Start Date: <hoje até futuro>
```

---

### Jobs com Erro (últimos 7 dias)

```text
Job Name: *
Status: Cancelled
Start Date: <-7 dias até hoje>
```

---

## 💡 Monitorização Programática

### Verificar Status de Job

```abap
REPORT z_check_job_status.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_MY_JOB',
      lv_job_number TYPE tbtcjob-jobcount VALUE '12345678',
      lt_jobs       TYPE TABLE OF tbtcjob,
      ls_job        TYPE tbtcjob.

CALL FUNCTION 'BP_JOB_SELECT'
  EXPORTING
    jobname  = lv_job_name
    jobcount = lv_job_number
  TABLES
    joblist  = lt_jobs.

READ TABLE lt_jobs INTO ls_job INDEX 1.

IF sy-subrc = 0.
  CASE ls_job-status.
    WHEN 'S'.  " Scheduled
      WRITE: / '⏳ Job agendado'.
    WHEN 'R'.  " Released/Ready
      WRITE: / '🟢 Job liberado'.
    WHEN 'A'.  " Active
      WRITE: / '⚡ Job executando'.
    WHEN 'F'.  " Finished
      WRITE: / '✅ Job concluído'.
    WHEN 'X'.  " Cancelled
      WRITE: / '❌ Job cancelado/erro'.
  ENDCASE.
ENDIF.
```

---

### Listar Jobs em Execução

```abap
REPORT z_list_active_jobs.

DATA: lt_jobs TYPE TABLE OF tbtcjob,
      ls_job  TYPE tbtcjob.

CALL FUNCTION 'BP_JOB_SELECT'
  EXPORTING
    status_active = 'X'
  TABLES
    joblist       = lt_jobs.

LOOP AT lt_jobs INTO ls_job.
  WRITE: / ls_job-jobname, ls_job-jobcount, ls_job-sdlstrtdt, ls_job-sdlstrttm.
ENDLOOP.

WRITE: / |Total: { lines( lt_jobs ) } jobs ativos|.
```

---

### Obter Log de Job

```abap
REPORT z_get_job_log.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_MY_JOB',
      lv_job_number TYPE tbtcjob-jobcount VALUE '12345678',
      lt_log        TYPE TABLE OF tbtc7,
      ls_log        TYPE tbtc7.

CALL FUNCTION 'BP_JOBLOG_READ'
  EXPORTING
    jobname  = lv_job_name
    jobcount = lv_job_number
  TABLES
    joblog   = lt_log.

LOOP AT lt_log INTO ls_log.
  WRITE: / ls_log-logtext.  " Mensagem do log
ENDLOOP.
```

---

### Cancelar Job Programaticamente

```abap
REPORT z_cancel_job.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_MY_JOB',
      lv_job_number TYPE tbtcjob-jobcount VALUE '12345678'.

CALL FUNCTION 'BP_JOB_ABORT'
  EXPORTING
    jobname  = lv_job_name
    jobcount = lv_job_number
  EXCEPTIONS
    OTHERS   = 1.

IF sy-subrc = 0.
  WRITE: / '✅ Job cancelado'.
ELSE.
  WRITE: / '❌ Erro ao cancelar job'.
ENDIF.
```

---

## 🔔 Monitorização Automática

### Criar Monitor de Jobs Críticos

```abap
*&---------------------------------------------------------------------*
*& Report Z_MONITOR_CRITICAL_JOBS
*& Verifica jobs críticos e envia alerta se houver falhas
*&---------------------------------------------------------------------*
REPORT z_monitor_critical_jobs.

TYPES: BEGIN OF ty_critical_job,
         jobname TYPE tbtcjob-jobname,
         descr   TYPE char50,
       END OF ty_critical_job.

DATA: lt_critical TYPE TABLE OF ty_critical_job,
      ls_critical TYPE ty_critical_job,
      lt_jobs     TYPE TABLE OF tbtcjob,
      ls_job      TYPE tbtcjob,
      lv_has_errors TYPE abap_bool.

" Definir jobs críticos a monitorar
lt_critical = VALUE #(
  ( jobname = 'Z_DAILY_BACKUP'    descr = 'Backup Diário' )
  ( jobname = 'Z_SYNC_CUSTOMERS'  descr = 'Sincronização Clientes' )
  ( jobname = 'Z_INVOICE_PROCESS' descr = 'Processamento Faturas' )
).

LOOP AT lt_critical INTO ls_critical.
  
  CLEAR lt_jobs.
  
  " Buscar execuções das últimas 24h
  CALL FUNCTION 'BP_JOB_SELECT'
    EXPORTING
      jobname       = ls_critical-jobname
      from_date     = sy-datum - 1
      to_date       = sy-datum
      status_finish = 'X'
      status_abort  = 'X'
    TABLES
      joblist       = lt_jobs.
  
  " Verificar se houve falhas
  READ TABLE lt_jobs TRANSPORTING NO FIELDS
    WITH KEY status = 'X'.  " Cancelled
  
  IF sy-subrc = 0.
    lv_has_errors = abap_true.
    WRITE: / |❌ ERRO: { ls_critical-descr } ({ ls_critical-jobname })|.
  ELSE.
    READ TABLE lt_jobs TRANSPORTING NO FIELDS
      WITH KEY status = 'F'.  " Finished
    
    IF sy-subrc = 0.
      WRITE: / |✅ OK: { ls_critical-descr }|.
    ELSE.
      WRITE: / |⚠️ AVISO: { ls_critical-descr } não executou nas últimas 24h|.
    ENDIF.
  ENDIF.
  
ENDLOOP.

IF lv_has_errors = abap_true.
  " Enviar e-mail de alerta (implementação omitida)
  WRITE: / '📧 Alerta enviado aos administradores'.
ENDIF.
```

---

## 📈 Análise de Performance

### Jobs Mais Longos

```abap
REPORT z_longest_jobs.

DATA: lt_jobs TYPE TABLE OF tbtcjob,
      ls_job  TYPE tbtcjob,
      lv_duration TYPE i.

CALL FUNCTION 'BP_JOB_SELECT'
  EXPORTING
    from_date     = sy-datum - 7
    to_date       = sy-datum
    status_finish = 'X'
  TABLES
    joblist       = lt_jobs.

LOOP AT lt_jobs INTO ls_job.
  
  " Calcular duração
  lv_duration = ls_job-enddate - ls_job-sdlstrtdt.
  lv_duration = lv_duration * 86400 +  " Dias em segundos
                ls_job-endtime - ls_job-sdlstrttm.
  
  WRITE: / ls_job-jobname,
           ls_job-jobcount,
           lv_duration, 'segundos'.
  
ENDLOOP.

" Ordenar por duração (implementação simplificada)
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Monitorar regularmente jobs críticos
" Criar job de monitorização que executa a cada hora

" 2. Analisar logs de jobs com erro
" Sempre verificar Job Log em SM37

" 3. Manter histórico limitado
" Configurar retenção de logs em SM36

" 4. Alertas automáticos
" Implementar notificações para falhas críticas

" 5. Documentar jobs importantes
" Manter lista de jobs críticos e responsáveis
```

### ❌ Evitar

```abap
" 1. Ignorar jobs cancelados
" ❌ Sempre investigar causas

" 2. Deletar jobs sem análise
" ❌ Verificar log antes de deletar

" 3. Cancelar jobs sem necessidade
" ❌ Pode causar inconsistências

" 4. Sobrecarregar com filtros amplos
" ❌ Usar filtros específicos em SM37

" 5. Não monitorar jobs periódicos
" ❌ Verificar execuções regulares
```

---

## 🔧 Troubleshooting

### Job Preso em "Ready"

**Causa:** Sem servidores background disponíveis

**Solução:**
1. SM50 - Verificar processos ativos
2. SM51 - Verificar servidores disponíveis
3. RZ04 - Configurar mais processos background
4. Ou aguardar liberação de recursos

---

### Job Cancelado sem Mensagem Clara

**Causa:** Dump, timeout, ou kill manual

**Solução:**
1. SM37 → Ver Job Log
2. ST22 → Verificar dumps do usuário do job
3. SM21 → System Log para eventos do sistema

---

### Job Não Inicia no Horário

**Causa:** Scheduled mas não Released

**Solução:**
```text
1. SM37 → Selecionar job
2. Verificar Status (deve ser Released, não Scheduled)
3. Job → Release para liberar manualmente
```

---

## 🔗 Próximos Passos

- **[Criar Jobs](1_criar_jobs.md)** - Criar novos jobs
- **[Agendamento](3_agendamento.md)** - Agendar execuções
- **[Job Logs](5_job_logs.md)** - Análise detalhada de logs

---

**Tags:** `#SM37` `#Monitorização` `#Jobs` `#Troubleshooting`
