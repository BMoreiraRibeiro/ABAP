# ⏰ Jobs em Background

Agendamento e execução de **programas ABAP em background**: SUBMIT, background processing, monitorização e boas práticas.

---

## 📖 O que vais aprender

- Executar programas em background
- Usar SUBMIT para chamar reports
- Agendar jobs periódicos (SM36)
- Monitorizar jobs (SM37)
- Variantes de seleção
- Jobs com dependências
- Notificações e logs
- Tratamento de erros em background

---

## 🎯 Tipos de Execução

### 1️⃣ Execução Direta (Foreground)
Executar programa na sessão atual.

```abap
SUBMIT z_meu_report AND RETURN.
```

### 2️⃣ Execução em Background
Enviar para processar em segundo plano.

```abap
SUBMIT z_meu_report 
  VIA JOB 'MEU_JOB' 
  NUMBER lv_job_number
  AND RETURN.
```

[Ver exemplo completo →](submit.md)

### 3️⃣ Agendamento Periódico
Job automático (diário, semanal, mensal).

**Transação SM36:**
1. Definir nome do job
2. Adicionar step (programa)
3. Configurar periodicidade
4. Salvar e liberar

---

## 🛠️ Ferramentas

### Transações SAP

| Transação | Descrição |
|-----------|-----------|
| **SM36** | Criar e agendar jobs |
| **SM37** | Monitorizar jobs existentes |
| **SE38** | Criar variantes de seleção |
| **SM35** | Batch Input sessions |
| **SM13** | Update records |

---

## 💡 Exemplos Práticos

### [SUBMIT Básico](submit.md)
Como executar um report de outro programa.

```abap
SUBMIT z_report
  WITH p_carr = 'LH'
  WITH s_date IN lt_date_range
  VIA JOB 'PROCESSO_VOOS'
  AND RETURN.
```

### Criar Job Programaticamente

```abap
DATA: lv_job_name   TYPE btcjob VALUE 'Z_JOB_AUTOMATICO',
      lv_job_number TYPE btcjobcnt.

" Abrir job
CALL FUNCTION 'JOB_OPEN'
  EXPORTING
    jobname  = lv_job_name
  IMPORTING
    jobcount = lv_job_number.

" Adicionar step (programa)
SUBMIT z_meu_report
  WITH p_param = lv_value
  VIA JOB lv_job_name NUMBER lv_job_number
  AND RETURN.

" Fechar e agendar job
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobname  = lv_job_name
    jobcount = lv_job_number
    strtimmed = 'X'  " Iniciar imediatamente
  EXCEPTIONS
    OTHERS = 1.

IF sy-subrc = 0.
  WRITE: / |Job { lv_job_number } criado com sucesso|.
ENDIF.
```

### Job com Periodicidade

```abap
" Executar todos os dias às 2h da manhã
DATA: ls_start_date TYPE btch0000.

ls_start_date-sdlstrtdt = sy-datum.
ls_start_date-sdlstrttm = '020000'.  " 02:00:00

CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobname          = lv_job_name
    jobcount         = lv_job_number
    sdlstrtdt        = ls_start_date-sdlstrtdt
    sdlstrttm        = ls_start_date-sdlstrttm
    periodic_values  = 'X'  " Job periódico
    period_value     = '1'  " Valor
    period_unit      = 'D'  " Unidade (D=diário, W=semanal, M=mensal)
  EXCEPTIONS
    OTHERS = 1.
```

---

## 📚 Exercícios Práticos

### Exercícios Disponíveis
- `ex01.md` → SUBMIT básico
- `ex02.md` → Criar job programaticamente
- `ex03.md` → Job periódico com variantes

---

## 📊 Monitorização de Jobs

### Verificar Status do Job

```abap
DATA: lt_jobs TYPE TABLE OF tbtcjob.

CALL FUNCTION 'BP_JOBLIST_READ'
  EXPORTING
    job_select_param = VALUE btcselect( jobname = 'Z_MEU_JOB*' )
  TABLES
    job_list         = lt_jobs.

LOOP AT lt_jobs INTO DATA(ls_job).
  WRITE: / ls_job-jobname, ls_job-status.
ENDLOOP.
```

### Status possíveis:
- **R** — Running (em execução)
- **F** — Finished (concluído)
- **A** — Aborted (abortado)
- **S** — Scheduled (agendado)
- **Y** — Ready (pronto para executar)

---

## 🚨 Boas Práticas

### ✅ Fazer

1. **Usar variantes** para parâmetros complexos
2. **Agendar fora do horário de pico** (noite/fim de semana)
3. **Configurar notificações** em caso de erro
4. **Logar execução** do job (SLG1 ou tabela custom)
5. **Testar em foreground** antes de agendar
6. **Documentar periodicidade** e dependências

### ❌ Evitar

1. Jobs de longa duração sem commit work
2. Múltiplos jobs a processar os mesmos dados simultaneamente
3. Jobs sem tratamento de erros
4. Não monitorizar jobs críticos (SM37)
5. Esquecer variantes ao transportar jobs

---

## 🔔 Notificações

### Enviar Email em Caso de Erro

```abap
IF lv_error = abap_true.
  " Enviar email
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data = VALUE sodocchgi1( 
        obj_descr = 'Job falhou: Z_MEU_JOB' )
    " ... outros parâmetros
    TABLES
      receivers = VALUE somlreci1_tab( 
        ( receiver = 'user@empresa.com' rec_type = 'U' ) ).
ENDIF.
```

---

## 💾 Variantes de Seleção

### Criar Variante (SE38)
1. Executar programa
2. Preencher parâmetros
3. Menu: **Ir para → Variantes → Salvar como variante**
4. Dar nome à variante

### Usar Variante em SUBMIT

```abap
SUBMIT z_meu_report
  USING SELECTION-SET 'VARIANTE_PRODUCAO'
  VIA JOB 'PROCESSO_NOTURNO'
  AND RETURN.
```

---

## 🚀 Próximos Passos

1. Leia [SUBMIT](submit.md)
2. Experimente criar um job simples (SM36)
3. Pratique com `ex01.md` a `ex03.md`
4. Configure monitorização para jobs críticos
5. Explore [Integrações](../integracoes/index.md) para processar dados externos em background
