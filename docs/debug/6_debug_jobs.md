---
tags:
  - ABAP
  - Debug
  - Background Jobs
  - SM37
---

# Debug de Jobs em Background

## 📋 Visão Geral

Debugar **jobs em background** requer técnicas especiais, pois não têm interface visual e executam em processos separados.

---

## 🎯 Desafios

- ❌ Não há interface gráfica
- ❌ Executam em processo separado
- ❌ Podem executar em servidor diferente
- ❌ Podem executar fora do horário de trabalho
- ✅ Mas é possível debugar!

---

## 🔹 Método 1: JDBG (Recomendado)

### Comando Mágico

Adicionar `JDBG` na variante ou nos parâmetros:

1. **SM37** → Selecionar job
2. **Job → Change**
3. **Start → Immediate**
4. No campo **ABAP Program**:
   ```
   JDBG
   ```
5. Salvar e liberar job

### Quando Executar

O job **espera** pelo debugger quando inicia.

**Passos:**
1. Libertar job
2. **SM37** → Refresh até status "Active"
3. **SM50** → Selecionar processo do job
4. **Program/Mode → Debugging**
5. Debugger abre!

---

## 🔹 Método 2: Breakpoint de Utilizador

### Configurar Breakpoint

1. Abrir programa no SE38/SE80
2. Colocar breakpoint (F7) na linha desejada
3. Breakpoint deve ser **externo** (não session)

### Ativar Debug para Jobs

```
Menu: Utilities → Settings → ABAP Editor → Debugging
☑ Debugging with External Breakpoints
```

### Executar Job

1. Libertar job normalmente
2. Quando job atingir breakpoint, aparece notificação
3. Aceitar e debugger abre

**Nota:** Funciona apenas se job executar no mesmo servidor!

---

## 🔹 Método 3: SM37 Direct Debug

### Para Jobs Agendados

1. **SM37** → Encontrar job
2. Antes de libertar, marcar checkbox:
   ```
   ☑ Start with debugging
   ```
3. Libertar job
4. Job para na primeira linha executável

---

## 💡 Exemplo Prático

### Programa de Background

```abap
*&---------------------------------------------------------------------*
*& Report Z_JOB_EXEMPLO
*&---------------------------------------------------------------------*
REPORT z_job_exemplo.

PARAMETERS: p_days TYPE i DEFAULT 30.

DATA: lt_voos TYPE TABLE OF sflight,
      lv_count TYPE i.

START-OF-SELECTION.
  
  " 🔍 Breakpoint aqui para debug
  SELECT * FROM sflight INTO TABLE lt_voos
    WHERE fldate >= @( sy-datum - p_days ).
  
  lv_count = lines( lt_voos ).
  
  " 🔍 Breakpoint aqui também
  IF lv_count > 0.
    WRITE: / |Encontrados { lv_count } voos|.
  ELSE.
    WRITE: / 'Nenhum voo encontrado'.
  ENDIF.
```

### Agendar com Debug

**SM36:**
1. Nome do job: `Z_DEBUG_VOOS`
2. Programa: `Z_JOB_EXEMPLO`
3. Variante: (criar com p_days = 30)
4. Especificar campo adicional: `JDBG`
5. Start condition: Immediate
6. Salvar

**Debugar:**
1. **SM37** → Procurar `Z_DEBUG_VOOS`
2. Status: Scheduled → Released
3. Aguardar status: Active
4. **SM50** → Encontrar processo
5. **Program/Mode → Debugging**

---

## 🛠️ Troubleshooting

### Job Não Para no Breakpoint

**Causa 1:** Servidor diferente
- ✅ Solução: Usar JDBG

**Causa 2:** Breakpoint não é externo
- ✅ Solução: Settings → External Breakpoints

**Causa 3:** Job já executou
- ✅ Solução: Reagendar

### Não Consigo Aceitar Debug

**Causa:** Pop-up perdido ou fechado

**Solução:**
```
SM50 → Selecionar processo → Debugging (forçar)
```

### Job Fica Preso

**Causa:** Debug ativo mas não acedido

**Solução:**
```
SM50 → Processo → Cancel with Core (último recurso)
```

---

## 🔹 Debug de Job Periódico

### Cenário

Job executa de hora em hora, precisa debugar a próxima execução.

**Passos:**
1. **SM37** → Encontrar job periódico
2. Copiar job: **Job → Copy**
3. Modificar cópia:
   - Start: Immediate
   - Adicionar: JDBG
4. Libertar cópia
5. Debugar cópia (não afeta original)

---

## 💡 Boas Práticas

### ✅ Fazer

```abap
" 1. Log detalhado em jobs
WRITE: / |{ sy-datum } { sy-uzeit }: Iniciando processamento|.
WRITE: / |Parâmetros: p_days = { p_days }|.

" 2. Usar Application Log (BAL)
DATA: lo_log TYPE REF TO cl_application_log.
lo_log->add_message( 'Processando...' ).

" 3. Verificar variáveis de ambiente
IF sy-batch = 'X'.
  " Executando em background
  WRITE: / 'Modo batch ativo'.
ENDIF.

" 4. Tratamento de erros robusto
TRY.
    " Processar
  CATCH cx_root INTO DATA(lo_ex).
    WRITE: / lo_ex->get_text( ).
ENDTRY.
```

### ❌ Evitar

```abap
" 1. BREAK-POINT em jobs de produção
BREAK-POINT.  " ❌ Job fica preso!

" 2. Debug em horário de produção
" Pode afetar outros utilizadores

" 3. Job sem log
" Impossível saber o que aconteceu

" 4. Timeout longo em debug
" Job pode ser cancelado pelo sistema
```

---

## 📊 Monitorização de Jobs

### SM37 - Job Overview

```
- Status:
  ● Scheduled: Agendado
  ● Released: Libertado
  ● Active: Executando
  ● Finished: Concluído
  ● Cancelled: Cancelado
```

### Ver Spool/Log

1. **SM37** → Selecionar job
2. **Spool list** → Ver output
3. **Job log** → Ver mensagens sistema

### SM50 - Process Overview

Ver processos ativos em tempo real:
```
- Processo do job
- Tempo de execução
- Utilizador
- Status
```

---

## 🔹 Exemplo: Log de Aplicação

```abap
REPORT z_job_com_log.

DATA: lo_log TYPE REF TO cl_log.

START-OF-SELECTION.
  
  " Criar log
  TRY.
      cl_log=>create(
        EXPORTING
          object = 'ZJOB'
          subobject = 'PROC'
        RECEIVING
          log = lo_log
      ).
    CATCH cx_log.
      RETURN.
  ENDTRY.
  
  " Adicionar mensagens
  lo_log->add_msg( 'Iniciando processamento...' ).
  
  TRY.
      SELECT * FROM sflight INTO TABLE @DATA(lt_voos) UP TO 100 ROWS.
      
      lo_log->add_msg( |Processados { lines( lt_voos ) } registos| ).
      
    CATCH cx_sy_sql_error INTO DATA(lo_sql_ex).
      lo_log->add_msg(
        msgty = 'E'
        msgtext = lo_sql_ex->get_text( )
      ).
  ENDTRY.
  
  " Salvar log
  lo_log->save( ).
  
  " Ver log: SLG1
  WRITE: / 'Log salvo em SLG1 - Object: ZJOB'.
```

---

## 🔗 Transactions Úteis

- **SM36** - Agendar jobs
- **SM37** - Monitorizar jobs
- **SM50** - Process overview (debug ativo)
- **SLG1** - Application log viewer
- **SE38** - Executar programa (teste antes)

---

## 🔗 Próximos Passos

- **[Breakpoints](1_breakpoints.md)** - Usar breakpoints eficientemente
- **[ST22](4_st22.md)** - Analisar dumps de jobs
- **[SAT](5_sat.md)** - Medir performance de jobs

---

**Tags:** `#Debug` `#BackgroundJobs` `#SM37` `#JDBG` `#ABAP`
