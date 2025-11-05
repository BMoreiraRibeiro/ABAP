# ⏰ Jobs em Background

Agendamento e execução de **programas ABAP em background**: criação de jobs, agendamento, monitorização e boas práticas.

---

## 📖 O que vais aprender

- Criar e agendar jobs em background
- Usar variantes de seleção
- Configurar agendamentos periódicos (diário, semanal, mensal)
- Monitorizar jobs (SM37)
- Analisar logs e troubleshooting
- Batch Input (BDC) para migração de dados
- Jobs com dependências
- Notificações e tratamento de erros

---

## 📚 Tópicos

### 1️⃣ [Criar Jobs](1_criar_jobs.md)
Como criar jobs via SM36 e programaticamente.

- Job básico com JOB_OPEN e JOB_CLOSE
- Adicionar programas com SUBMIT
- Classes de job (A, B, C)
- Jobs com múltiplos steps

### 2️⃣ [Variantes](2_variantes.md)
Usar variantes de seleção em jobs.

- Criar variantes via SE38
- Selection variables
- Variantes programáticas
- Atualizar e deletar variantes

### 3️⃣ [Agendamento](3_agendamento.md)
Opções avançadas de agendamento.

- Execução imediata, data/hora específica
- Jobs periódicos (diário, semanal, mensal)
- Jobs dependentes (após outro job)
- Jobs disparados por eventos

### 4️⃣ [Monitorização (SM37)](4_monitorizacao.md)
Monitorar e gerenciar jobs.

- Status de jobs (Scheduled, Active, Finished, Cancelled)
- Ver logs e spool
- Cancelar e repetir jobs
- Monitorização programática

### 5️⃣ [Job Logs](5_job_logs.md)
Análise de logs e troubleshooting.

- Job Log vs Spool vs Application Log
- Ler logs programaticamente
- Debugging de jobs com erro
- Boas práticas de logging

### 6️⃣ [Batch Processing](6_batch_processing.md)
Batch Input para migração de dados.

- CALL TRANSACTION vs Session Method
- Gravar transações com SHDB
- Processar grande volume de dados
- BDC em background jobs

---

## 🎯 Exemplo Rápido

### Criar Job Simples



```abap
DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_JOB_AUTOMATICO',
      lv_job_number TYPE tbtcjob-jobcount.

" 1. Abrir job
CALL FUNCTION 'JOB_OPEN'
  EXPORTING
    jobname  = lv_job_name
  IMPORTING
    jobcount = lv_job_number.

" 2. Adicionar programa
SUBMIT z_meu_report
  WITH p_param = lv_value
  VIA JOB lv_job_name NUMBER lv_job_number
  AND RETURN.

" 3. Agendar job (imediato)
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobname   = lv_job_name
    jobcount  = lv_job_number
    strtimmed = 'X'.  " Iniciar imediatamente

WRITE: / |✅ Job { lv_job_number } criado|.
```

---

## 🛠️ Ferramentas SAP

| Transação | Descrição |
|-----------|-----------|
| **SM36** | Criar e agendar jobs |
| **SM37** | Monitorizar jobs existentes |
| **SE38** | Criar variantes de seleção |
| **SM35** | Batch Input sessions |
| **SHDB** | Gravar Batch Input |
| **SLG1** | Application Log |
| **ST22** | Dumps de runtime |

---

## 📊 Monitorização (SM37)


```abap
DATA: lt_jobs TYPE TABLE OF tbtcjob.

CALL FUNCTION 'BP_JOB_SELECT'
  EXPORTING
    jobname       = 'Z_MEU_JOB*'
    status_finish = 'X'
  TABLES
    joblist       = lt_jobs.

LOOP AT lt_jobs INTO DATA(ls_job).
  WRITE: / ls_job-jobname, ls_job-status.
ENDLOOP.
```

**Status possíveis:**
- **S** — Scheduled (agendado)
- **R** — Released/Ready (pronto)
- **A** — Active (em execução)
- **F** — Finished (concluído)
- **X** — Cancelled (cancelado/erro)

---

## 🚨 Boas Práticas

### ✅ Fazer

```abap
" 1. Usar variantes para parâmetros
SUBMIT z_report USING SELECTION-SET 'VARIANTE_PROD'.

" 2. Agendar fora do horário de pico
sdlstrttm = '020000'  " ✅ 02:00 AM

" 3. Logar execução
PERFORM log_message USING 'Job iniciado'.

" 4. Testar em foreground primeiro
SUBMIT z_report AND RETURN.  " Teste antes de job

" 5. Monitorar jobs críticos
" Criar job de monitorização que verifica outros jobs
```

### ❌ Evitar

```abap
" 1. Jobs sem tratamento de erro
" ❌ Sempre verificar sy-subrc e mensagens

" 2. Processar tudo em foreground
" ❌ Usar jobs para grande volume

" 3. Jobs sem logging
" ❌ Impossível troubleshooting

" 4. Ignorar jobs cancelados em SM37
" ❌ Sempre investigar causas

" 5. Múltiplos jobs nos mesmos dados
" ❌ Pode causar deadlocks
```

---

## � Troubleshooting

### Job Cancelado

1. **SM37** → Selecionar job → **Job Log**
2. **ST22** → Verificar dumps
3. Re-executar em foreground com debug

### Job Lento

1. **SM37** → Ver tempo de execução
2. **ST12** → Analisar performance
3. Otimizar SELECTs e loops

### Job Não Inicia

1. Verificar status (deve ser **Released**)
2. **RZ04** → Verificar processos background disponíveis
3. Liberar manualmente se necessário

---

## � Próximos Passos

Explore os tópicos detalhados:

1. **[Criar Jobs](1_criar_jobs.md)** - Aprenda a criar jobs via SM36 e código
2. **[Variantes](2_variantes.md)** - Configure parâmetros com variantes
3. **[Agendamento](3_agendamento.md)** - Agende execuções periódicas
4. **[Monitorização](4_monitorizacao.md)** - Monitore jobs em SM37
5. **[Job Logs](5_job_logs.md)** - Analise logs e faça troubleshooting
6. **[Batch Processing](6_batch_processing.md)** - Migre dados com BDC

---

**Relacionado:**
- [Performance](../performance/index.md) - Otimize jobs lentos
- [Integrações](../integracoes/index.md) - Processe dados externos em background

---

**Tags:** `#Jobs` `#Background` `#SM36` `#SM37` `#Agendamento` `#BDC`