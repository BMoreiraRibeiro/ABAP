---
tags:
  - ABAP
  - Jobs
  - Agendamento
  - Background
---

# Agendamento de Jobs

## 📋 Visão Geral

**Agendamento** permite executar jobs **automaticamente** em horários ou eventos específicos, sem intervenção manual.

---

## 🗓️ Tipos de Agendamento

### 1️⃣ Imediato

Executar **agora**:

```abap
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount  = lv_job_number
    jobname   = lv_job_name
    strtimmed = 'X'.  " ✅ Immediate
```

---

### 2️⃣ Data/Hora Específica

Executar em **momento futuro**:

```abap
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount  = lv_job_number
    jobname   = lv_job_name
    sdlstrtdt = '20250115'  " Data
    sdlstrttm = '143000'.   " Hora: 14:30
```

---

### 3️⃣ Periódico

#### Diário

```abap
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount       = lv_job_number
    jobname        = lv_job_name
    sdlstrtdt      = sy-datum
    sdlstrttm      = '060000'
    periodic_job   = 'X'
    periodic_param = VALUE tbtcjob(
      period    = 'D'  " Daily
      frequency = '1'  " Todos os dias
    ).
```

#### Semanal

```abap
periodic_param = VALUE tbtcjob(
  period    = 'W'  " Weekly
  frequency = '1'  " Toda semana
  weekdays  = VALUE #(  " Dias da semana
    ( day = 'MON' )
    ( day = 'WED' )
    ( day = 'FRI' )
  )
).
```

#### Mensal

```abap
periodic_param = VALUE tbtcjob(
  period    = 'M'  " Monthly
  frequency = '1'  " Todo mês
  monthdays = VALUE #(  " Dias do mês
    ( day = '01' )  " Dia 1
    ( day = '15' )  " Dia 15
  )
).
```

---

### 4️⃣ Após Outro Job

Job **dependente**:

```abap
DATA: lv_pred_job    TYPE tbtcjob-jobname VALUE 'JOB_PREDECESSOR',
      lv_pred_number TYPE tbtcjob-jobcount.

" Obter número do job predecessor (via SM37 ou código)
" ...

CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount      = lv_job_number
    jobname       = lv_job_name
    pred_jobname  = lv_pred_job
    pred_jobcount = lv_pred_number.
```

---

### 5️⃣ Por Evento

Job disparado por **evento SAP**:

```abap
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount = lv_job_number
    jobname  = lv_job_name
    event_id = 'SAP_TRIGGER_BACKUP'.
```

**Disparar evento:**
```abap
CALL FUNCTION 'BP_EVENT_RAISE'
  EXPORTING
    eventid              = 'SAP_TRIGGER_BACKUP'
  EXCEPTIONS
    bad_eventid          = 1
    eventid_does_not_exist = 2
    OTHERS               = 3.
```

---

## 📅 Configuração Avançada

### Data Final

Job periódico com **fim**:

```abap
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount       = lv_job_number
    jobname        = lv_job_name
    laststrtdt     = '20251231'  " ✅ Última execução
    periodic_job   = 'X'
    periodic_param = VALUE #( period = 'D' frequency = '1' ).
```

---

### Restrições de Tempo

**Executar apenas em horário específico:**

```abap
" SM36 → Start Condition → Period Values → Restrictions
" Executar apenas entre 00:00 e 06:00
```

---

### Modo de Operação

**Executar em modo específico do sistema:**

```abap
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount    = lv_job_number
    jobname     = lv_job_name
    op_mode_switch = 'X'
    op_mode_name   = 'BATCH_MODE'.
```

---

## 💡 Exemplos Práticos

### Exemplo 1: Backup Diário Noturno

```abap
*&---------------------------------------------------------------------*
*& Report Z_SCHEDULE_NIGHTLY_BACKUP
*&---------------------------------------------------------------------*
REPORT z_schedule_nightly_backup.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_NIGHTLY_BACKUP',
      lv_job_number TYPE tbtcjob-jobcount.

START-OF-SELECTION.
  
  " Abrir job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname  = lv_job_name
      jobclass = 'C'
    IMPORTING
      jobcount = lv_job_number.
  
  " Adicionar programa de backup
  SUBMIT z_backup_database
    VIA JOB lv_job_name NUMBER lv_job_number
    AND RETURN.
  
  " Agendar diariamente às 02:00
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount       = lv_job_number
      jobname        = lv_job_name
      sdlstrtdt      = sy-datum
      sdlstrttm      = '020000'  " 02:00 AM
      periodic_job   = 'X'
      periodic_param = VALUE #(
        period    = 'D'
        frequency = '1'
      ).
  
  WRITE: / '✅ Backup diário agendado para 02:00'.
```

---

### Exemplo 2: Relatório Semanal

```abap
REPORT z_schedule_weekly_report.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_WEEKLY_SALES',
      lv_job_number TYPE tbtcjob-jobcount.

START-OF-SELECTION.
  
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING jobname = lv_job_name
    IMPORTING jobcount = lv_job_number.
  
  SUBMIT z_sales_report
    USING SELECTION-SET 'WEEKLY'
    VIA JOB lv_job_name NUMBER lv_job_number
    AND RETURN.
  
  " Toda segunda-feira às 08:00
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount       = lv_job_number
      jobname        = lv_job_name
      sdlstrtdt      = sy-datum
      sdlstrttm      = '080000'
      periodic_job   = 'X'
      periodic_param = VALUE #(
        period    = 'W'
        frequency = '1'
        weekdays  = VALUE #( ( day = 'MON' ) )
      ).
  
  WRITE: / '✅ Relatório agendado para segundas às 08:00'.
```

---

### Exemplo 3: Processamento em Cadeia

```abap
REPORT z_chain_jobs.

DATA: lv_job1_name   TYPE tbtcjob-jobname VALUE 'Z_EXTRACT',
      lv_job1_number TYPE tbtcjob-jobcount,
      lv_job2_name   TYPE tbtcjob-jobname VALUE 'Z_PROCESS',
      lv_job2_number TYPE tbtcjob-jobcount.

START-OF-SELECTION.
  
  " ═══ JOB 1: EXTRAIR DADOS ═══
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING jobname = lv_job1_name
    IMPORTING jobcount = lv_job1_number.
  
  SUBMIT z_extract_data
    VIA JOB lv_job1_name NUMBER lv_job1_number
    AND RETURN.
  
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = lv_job1_number
      jobname   = lv_job1_name
      strtimmed = 'X'.
  
  WRITE: / |Job 1 criado: { lv_job1_number }|.
  
  " ═══ JOB 2: PROCESSAR (APÓS JOB 1) ═══
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING jobname = lv_job2_name
    IMPORTING jobcount = lv_job2_number.
  
  SUBMIT z_process_data
    VIA JOB lv_job2_name NUMBER lv_job2_number
    AND RETURN.
  
  " Executar após Job 1
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount      = lv_job2_number
      jobname       = lv_job2_name
      pred_jobname  = lv_job1_name
      pred_jobcount = lv_job1_number.
  
  WRITE: / '✅ Jobs em cadeia criados'.
  WRITE: / 'Job 2 executará após conclusão do Job 1'.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Jobs noturnos fora do horário de pico
sdlstrttm = '020000'  " ✅ 02:00 AM

" 2. Data final para jobs periódicos de teste
laststrtdt = '20250131'  " ✅ Termina em Jan/2025

" 3. Usar eventos para processos assíncronos
event_id = 'Z_CUSTOM_TRIGGER'  " ✅

" 4. Documentar dependências
*&---------------------------------------------------------------------*
*& Job: Z_PROCESS_ORDERS
*& Prerequisite: Z_EXTRACT_ORDERS deve completar primeiro
*&---------------------------------------------------------------------*

" 5. Class apropriada
jobclass = 'C'  " ✅ Baixa para não-urgentes
```

### ❌ Evitar

```abap
" 1. Jobs em horário de pico
sdlstrttm = '140000'  " ❌ 14:00 - horário comercial

" 2. Periódicos sem data final em produção
" periodic sem laststrtdt  " ❌ Pode executar indefinidamente

" 3. Dependências circulares
" Job A depende de Job B que depende de Job A  " ❌

" 4. Prioridade A para todos os jobs
jobclass = 'A'  " ❌ Reservar para críticos

" 5. Agendamentos muito frequentes
frequency = '1'  " A cada 1 minuto  " ❌ Sobrecarrega sistema
```

---

## 🔗 Próximos Passos

- **[Criar Jobs](1_criar_jobs.md)** - Criar jobs básicos
- **[Monitorização](4_monitorizacao.md)** - Monitorar jobs agendados (SM37)
- **[Job Logs](5_job_logs.md)** - Analisar logs de execução

---

**Tags:** `#Agendamento` `#Jobs` `#Background` `#SM36` `#Periódico`
