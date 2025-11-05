---
tags:
  - ABAP
  - Jobs
  - Background
  - SM36
---

# Criar Jobs - SM36

## 📋 Visão Geral

**Jobs em background** permitem executar programas ABAP sem interação do usuário, ideal para processos **longos**, **periódicos** ou **agendados**.

**Transaction SM36** - Define Background Jobs

---

## 🎯 Criar Job via SM36

### Passo a Passo

#### 1. Abrir SM36

**Transaction:** `SM36`

#### 2. Definir Job

**Campos obrigatórios:**
- **Job Name:** Nome do job (ex: `Z_PROCESS_INVOICES`)
- **Job Class:** Prioridade (A=Alta, B=Média, C=Baixa)

```
Job Name: Z_DAILY_REPORT
Job Class: C
```

#### 3. Adicionar Step

**Botão:** `Step`

**Tipos de step:**
- **ABAP Program** - Executar report ABAP
- **External Command** - Comando do sistema operacional
- **External Program** - Programa externo

**Exemplo - ABAP Program:**
```
Program Name: Z_SALES_REPORT
Variant: DAILY_SALES (opcional)
```

#### 4. Definir Agendamento

**Botão:** `Start Condition`

**Opções:**
- **Immediate** - Executar imediatamente
- **Date/Time** - Data e hora específica
- **After Job** - Após outro job
- **After Event** - Disparado por evento
- **At Operation Mode** - Em modo específico

**Exemplo - Agendamento Diário:**
```
Period Values: Daily
Start Date: 01.01.2025
Start Time: 06:00:00
```

#### 5. Salvar

**Botão:** `Save`

O job fica com status **Scheduled**.

---

## 💻 Criar Job Programaticamente

### Function Modules

**Sequência:**
1. `JOB_OPEN` - Abrir job
2. `SUBMIT` ou `JOB_SUBMIT` - Adicionar steps
3. `JOB_CLOSE` - Fechar e agendar

### Exemplo Completo

```abap
*&---------------------------------------------------------------------*
*& Report Z_CREATE_BACKGROUND_JOB
*&---------------------------------------------------------------------*
REPORT z_create_background_job.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_AUTO_BACKUP',
      lv_job_number TYPE tbtcjob-jobcount,
      lv_print_params TYPE pri_params.

START-OF-SELECTION.
  
  " ═══ 1. ABRIR JOB ═══
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_job_name
    IMPORTING
      jobcount         = lv_job_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  
  IF sy-subrc <> 0.
    MESSAGE 'Erro ao criar job' TYPE 'E'.
    RETURN.
  ENDIF.
  
  WRITE: / |✅ Job criado: { lv_job_name } ({ lv_job_number })|.
  
  " ═══ 2. ADICIONAR STEP (SUBMIT) ═══
  SUBMIT z_backup_data
    WITH p_date = sy-datum
    VIA JOB lv_job_name NUMBER lv_job_number
    AND RETURN.
  
  WRITE: / '✅ Step adicionado'.
  
  " ═══ 3. AGENDAR JOB ═══
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = lv_job_number
      jobname              = lv_job_name
      strtimmed            = 'X'  " Executar imediatamente
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      OTHERS               = 5.
  
  IF sy-subrc = 0.
    WRITE: / '✅ Job agendado com sucesso!'.
    WRITE: / 'Verificar em SM37'.
  ELSE.
    WRITE: / '❌ Erro ao agendar job'.
  ENDIF.
```

---

## 🗓️ Tipos de Agendamento

### Execução Imediata

```abap
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount  = lv_job_number
    jobname   = lv_job_name
    strtimmed = 'X'.  " Immediate
```

---

### Data e Hora Específica

```abap
DATA: lv_start_date TYPE btch0000-sdlstrtdt,
      lv_start_time TYPE btch0000-sdlstrttm.

lv_start_date = '20250115'.  " 15/01/2025
lv_start_time = '180000'.    " 18:00:00

CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount  = lv_job_number
    jobname   = lv_job_name
    sdlstrtdt = lv_start_date
    sdlstrttm = lv_start_time.
```

---

### Periódico (Diário)

```abap
DATA: ls_periodic TYPE btch0000.

" Configurar periodicidade
ls_periodic-period    = 'D'.  " Daily
ls_periodic-frequency = '1'.  " Todos os dias

CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount       = lv_job_number
    jobname        = lv_job_name
    sdlstrtdt      = sy-datum
    sdlstrttm      = '060000'
    periodic_job   = 'X'
    periodic_param = ls_periodic.
```

**Valores de period:**
- `D` - Daily (Diário)
- `W` - Weekly (Semanal)
- `M` - Monthly (Mensal)

---

### Após Outro Job

```abap
DATA: lv_predecessor TYPE tbtcjob-jobname VALUE 'JOB_ANTERIOR'.

CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount       = lv_job_number
    jobname        = lv_job_name
    pred_jobcount  = lv_pred_number
    pred_jobname   = lv_predecessor.
```

---

### Disparado por Evento

```abap
DATA: lv_event TYPE btceventid VALUE 'SAP_TRIGGER_JOB'.

CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount  = lv_job_number
    jobname   = lv_job_name
    event_id  = lv_event.
```

**Disparar evento:**
```abap
CALL FUNCTION 'BP_EVENT_RAISE'
  EXPORTING
    eventid = 'SAP_TRIGGER_JOB'.
```

---

## 📊 Job Classes

**Prioridade de execução:**

| Class | Prioridade | Uso |
|-------|-----------|-----|
| **A** | Alta | Jobs críticos, urgentes |
| **B** | Média | Jobs normais |
| **C** | Baixa | Jobs menos importantes |

**Definir class:**
```abap
CALL FUNCTION 'JOB_OPEN'
  EXPORTING
    jobname  = lv_job_name
    jobclass = 'A'.  " Alta prioridade
```

---

## 🔧 Múltiplos Steps

**Adicionar vários programas ao mesmo job:**

```abap
" Abrir job
CALL FUNCTION 'JOB_OPEN'
  EXPORTING jobname = 'Z_MULTI_STEP_JOB'
  IMPORTING jobcount = lv_job_number.

" Step 1: Extrair dados
SUBMIT z_extract_data
  VIA JOB 'Z_MULTI_STEP_JOB' NUMBER lv_job_number
  AND RETURN.

" Step 2: Processar dados
SUBMIT z_process_data
  VIA JOB 'Z_MULTI_STEP_JOB' NUMBER lv_job_number
  AND RETURN.

" Step 3: Enviar email
SUBMIT z_send_email
  VIA JOB 'Z_MULTI_STEP_JOB' NUMBER lv_job_number
  AND RETURN.

" Fechar job
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobcount  = lv_job_number
    jobname   = 'Z_MULTI_STEP_JOB'
    strtimmed = 'X'.
```

---

## 💡 Exemplo: Job Diário Automático

```abap
*&---------------------------------------------------------------------*
*& Report Z_SCHEDULE_DAILY_JOB
*&---------------------------------------------------------------------*
REPORT z_schedule_daily_job.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_DAILY_SALES',
      lv_job_number TYPE tbtcjob-jobcount.

PARAMETERS: p_time TYPE sy-uzeit DEFAULT '060000'.

START-OF-SELECTION.
  
  " Abrir job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname  = lv_job_name
      jobclass = 'C'
    IMPORTING
      jobcount = lv_job_number.
  
  " Adicionar programa
  SUBMIT z_sales_report
    WITH p_date = sy-datum
    WITH p_email = 'vendas@empresa.com'
    VIA JOB lv_job_name NUMBER lv_job_number
    AND RETURN.
  
  " Agendar diariamente
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount       = lv_job_number
      jobname        = lv_job_name
      sdlstrtdt      = sy-datum
      sdlstrttm      = p_time
      periodic_job   = 'X'
      periodic_param = VALUE #(
        period    = 'D'
        frequency = '1'
      ).
  
  IF sy-subrc = 0.
    WRITE: / |✅ Job agendado: { lv_job_name }|.
    WRITE: / |Execução diária às { p_time TIME = USER }|.
    WRITE: / 'Monitorizar em SM37'.
  ENDIF.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre verificar sy-subrc
CALL FUNCTION 'JOB_OPEN'
  IMPORTING jobcount = lv_job_number
  EXCEPTIONS
    cant_create_job = 1
    OTHERS = 2.

IF sy-subrc <> 0.  " ✅ Verificar
  MESSAGE 'Erro' TYPE 'E'.
ENDIF.

" 2. Nome descritivo para job
DATA lv_job_name TYPE tbtcjob-jobname VALUE 'Z_SALES_DAILY_RPT'.  " ✅

" 3. Usar variantes para parâmetros
SUBMIT z_report
  USING SELECTION-SET 'VARIANT_001'  " ✅ Variante
  VIA JOB lv_job_name NUMBER lv_job_number.

" 4. Definir prioridade apropriada
CALL FUNCTION 'JOB_OPEN'
  EXPORTING jobclass = 'C'.  " ✅ Baixa para jobs não-urgentes

" 5. Documentar jobs
*&---------------------------------------------------------------------*
*& Job: Z_DAILY_BACKUP
*& Descrição: Backup diário de dados de vendas
*& Frequência: Diário às 22:00
*& Owner: SAP_ADMIN
*&---------------------------------------------------------------------*
```

### ❌ Evitar

```abap
" 1. Não verificar erros
CALL FUNCTION 'JOB_OPEN'
  IMPORTING jobcount = lv_job_number.
" ❌ E se falhar?

" 2. Nome genérico
DATA lv_job_name TYPE tbtcjob-jobname VALUE 'JOB1'.  " ❌

" 3. Hardcoded parameters
SUBMIT z_report
  WITH p_date = '20250101'  " ❌ Data fixa!
  VIA JOB lv_job_name NUMBER lv_job_number.

" 4. Prioridade A para tudo
CALL FUNCTION 'JOB_OPEN'
  EXPORTING jobclass = 'A'.  " ❌ Só para críticos!

" 5. Não usar variantes
" Parâmetros hardcoded no código  " ❌
```

---

## 🔗 Próximos Passos

- **[Variantes](2_variantes.md)** - Criar e usar variantes de seleção
- **[Agendamento](3_agendamento.md)** - Opções avançadas de agendamento
- **[Monitorização](4_monitorizacao.md)** - SM37 e análise de jobs

---

**Tags:** `#Jobs` `#Background` `#SM36` `#ABAP` `#Agendamento`
