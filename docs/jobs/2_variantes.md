---
tags:
  - ABAP
  - Variantes
  - Selection Screen
  - Jobs
---

# Variantes de Seleção

## 📋 Visão Geral

**Variantes** permitem salvar valores de **selection screen** para reutilização em execuções futuras ou jobs em background.

**Vantagens:**
- ✅ Reutilizar parâmetros
- ✅ Facilitar execução de jobs
- ✅ Padronizar processos
- ✅ Reduzir erros de digitação

---

## 🛠️ Criar Variante

### Via SE38

#### Passo a Passo

1. **SE38** → Nome do programa → **Execute**
2. Preencher **selection screen** com valores desejados
3. **Menu:** Goto → Variants → **Save as Variant**
4. **Nome da variante:** (ex: `DAILY_RUN`)
5. **Description:** "Execução diária"
6. **Save**

---

### Atributos de Campos

Ao salvar variante, pode configurar cada campo:

| Atributo | Descrição |
|----------|-----------|
| **Save field without values** | Campo vazio na variante |
| **Protect field** | Não permite alteração |
| **Hide field** | Campo invisível |
| **Required field** | Campo obrigatório |
| **Selection variable** | Valor dinâmico (data atual, etc.) |

---

## 💡 Selection Variables

**Valores dinâmicos** calculados em runtime:

### Variáveis de Data

| Variável | Descrição |
|----------|-----------|
| `D` | Data atual |
| `T` | Data atual - 1 dia |
| `M` | Primeiro dia do mês atual |
| `H` | Primeiro dia do mês anterior |

**Exemplo:**
```
Campo: S_DATE-LOW
Selection Variable: D (Data atual)
→ Sempre executará com data de hoje
```

### Variáveis de Tempo

| Variável | Descrição |
|----------|-----------|
| `S` | Hora atual |
| `I` | Hora atual - 1 hora |

---

## 🔧 Usar Variante em Job

### Via SM36

1. **SM36** → Create Job
2. **Step** → ABAP Program
3. **Program Name:** `Z_SALES_REPORT`
4. **Variant:** `DAILY_RUN` ✅
5. **Save**

---

### Programaticamente

```abap
SUBMIT z_sales_report
  USING SELECTION-SET 'DAILY_RUN'  " ✅ Usar variante
  VIA JOB lv_job_name NUMBER lv_job_number
  AND RETURN.
```

---

## 💻 Criar Variante Programaticamente

### Function Module: RS_VARIANT_CATALOG

```abap
*&---------------------------------------------------------------------*
*& Report Z_CREATE_VARIANT
*&---------------------------------------------------------------------*
REPORT z_create_variant.

DATA: lt_valutab TYPE TABLE OF rsparams,
      ls_valutab TYPE rsparams,
      lv_variant TYPE raldb-variant VALUE 'AUTO_VARIANT'.

START-OF-SELECTION.
  
  " Definir valores da variante
  ls_valutab-selname = 'P_DATE'.
  ls_valutab-kind    = 'P'.  " Parameter
  ls_valutab-sign    = 'I'.
  ls_valutab-option  = 'EQ'.
  ls_valutab-low     = sy-datum.
  APPEND ls_valutab TO lt_valutab.
  
  ls_valutab-selname = 'S_CARR'.
  ls_valutab-kind    = 'S'.  " Select-option
  ls_valutab-sign    = 'I'.
  ls_valutab-option  = 'EQ'.
  ls_valutab-low     = 'LH'.
  APPEND ls_valutab TO lt_valutab.
  
  " Criar variante
  CALL FUNCTION 'RS_CREATE_VARIANT'
    EXPORTING
      report               = 'Z_MY_REPORT'
      variant              = lv_variant
      vtext                = 'Variante Automática'
    TABLES
      valutab              = lt_valutab
    EXCEPTIONS
      variant_exists       = 1
      variant_locked       = 2
      OTHERS               = 3.
  
  IF sy-subrc = 0.
    WRITE: / |✅ Variante criada: { lv_variant }|.
  ELSE.
    WRITE: / '❌ Erro ao criar variante'.
  ENDIF.
```

---

## 🗂️ Estrutura de Variantes

### Tabela RSPARAMS

```abap
TYPES: BEGIN OF ty_variant,
  selname TYPE rsparams-selname,  " Nome do campo
  kind    TYPE rsparams-kind,     " P=Parameter, S=Select-option
  sign    TYPE rsparams-sign,     " I=Include, E=Exclude
  option  TYPE rsparams-option,   " EQ, NE, GT, LT, BT, CP
  low     TYPE rsparams-low,      " Valor baixo
  high    TYPE rsparams-high,     " Valor alto (para BT)
END OF ty_variant.
```

### Exemplo Completo

```abap
" Parameter (P_DATE)
APPEND VALUE #(
  selname = 'P_DATE'
  kind    = 'P'
  sign    = 'I'
  option  = 'EQ'
  low     = '20250101'
) TO lt_valutab.

" Select-option (S_CARR) - Range
APPEND VALUE #(
  selname = 'S_CARR'
  kind    = 'S'
  sign    = 'I'
  option  = 'BT'
  low     = 'AA'
  high    = 'LH'
) TO lt_valutab.

" Select-option (S_MATNR) - Multiple values
APPEND VALUE #(
  selname = 'S_MATNR'
  kind    = 'S'
  sign    = 'I'
  option  = 'EQ'
  low     = 'MAT001'
) TO lt_valutab.

APPEND VALUE #(
  selname = 'S_MATNR'
  kind    = 'S'
  sign    = 'I'
  option  = 'EQ'
  low     = 'MAT002'
) TO lt_valutab.
```

---

## 📊 Ler Variante Existente

### Function Module: RS_VARIANT_CONTENTS

```abap
DATA: lt_valutab TYPE TABLE OF rsparams,
      lv_variant TYPE raldb-variant VALUE 'DAILY_RUN'.

CALL FUNCTION 'RS_VARIANT_CONTENTS'
  EXPORTING
    report               = 'Z_SALES_REPORT'
    variant              = lv_variant
  TABLES
    valutab              = lt_valutab
  EXCEPTIONS
    variant_non_existent = 1
    variant_obsolete     = 2
    OTHERS               = 3.

IF sy-subrc = 0.
  LOOP AT lt_valutab INTO DATA(ls_val).
    WRITE: / ls_val-selname, ls_val-low.
  ENDLOOP.
ENDIF.
```

---

## 🔄 Atualizar Variante

```abap
" 1. Ler variante existente
CALL FUNCTION 'RS_VARIANT_CONTENTS'
  EXPORTING
    report  = 'Z_REPORT'
    variant = 'MY_VAR'
  TABLES
    valutab = lt_valutab.

" 2. Modificar valores
READ TABLE lt_valutab WITH KEY selname = 'P_DATE'
  ASSIGNING FIELD-SYMBOL(<fs_val>).
IF sy-subrc = 0.
  <fs_val>-low = sy-datum.  " Atualizar para data atual
ENDIF.

" 3. Deletar variante antiga
CALL FUNCTION 'RS_DELETE_VARIANT'
  EXPORTING
    report  = 'Z_REPORT'
    variant = 'MY_VAR'.

" 4. Criar variante atualizada
CALL FUNCTION 'RS_CREATE_VARIANT'
  EXPORTING
    report  = 'Z_REPORT'
    variant = 'MY_VAR'
  TABLES
    valutab = lt_valutab.
```

---

## 🗑️ Deletar Variante

```abap
CALL FUNCTION 'RS_DELETE_VARIANT'
  EXPORTING
    report               = 'Z_MY_REPORT'
    variant              = 'OLD_VARIANT'
  EXCEPTIONS
    variant_non_existent = 1
    variant_locked       = 2
    OTHERS               = 3.

IF sy-subrc = 0.
  WRITE: / '✅ Variante deletada'.
ENDIF.
```

---

## 💡 Exemplo Completo: Job com Variante Dinâmica

```abap
*&---------------------------------------------------------------------*
*& Report Z_JOB_WITH_DYNAMIC_VARIANT
*&---------------------------------------------------------------------*
REPORT z_job_with_dynamic_variant.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_MONTHLY_REPORT',
      lv_job_number TYPE tbtcjob-jobcount,
      lv_variant    TYPE raldb-variant VALUE 'MONTHLY',
      lt_valutab    TYPE TABLE OF rsparams.

START-OF-SELECTION.
  
  " ═══ 1. CRIAR VARIANTE COM DATAS DINÂMICAS ═══
  
  " Primeiro dia do mês atual
  DATA(lv_first_day) = CONV datum( |{ sy-datum(6) }01| ).
  
  " Último dia do mês atual
  DATA(lv_last_day) = lv_first_day.
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in  = lv_first_day
    IMPORTING
      last_day_of_month = lv_last_day.
  
  " Configurar range de datas
  APPEND VALUE #(
    selname = 'S_DATE'
    kind    = 'S'
    sign    = 'I'
    option  = 'BT'
    low     = lv_first_day
    high    = lv_last_day
  ) TO lt_valutab.
  
  " Deletar variante antiga (se existir)
  CALL FUNCTION 'RS_DELETE_VARIANT'
    EXPORTING
      report  = 'Z_SALES_MONTHLY'
      variant = lv_variant
    EXCEPTIONS
      OTHERS  = 0.
  
  " Criar nova variante
  CALL FUNCTION 'RS_CREATE_VARIANT'
    EXPORTING
      report  = 'Z_SALES_MONTHLY'
      variant = lv_variant
      vtext   = |Mês { sy-datum+4(2) }/{ sy-datum(4) }|
    TABLES
      valutab = lt_valutab.
  
  WRITE: / |✅ Variante criada: { lv_first_day } - { lv_last_day }|.
  
  " ═══ 2. CRIAR JOB COM VARIANTE ═══
  
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname  = lv_job_name
    IMPORTING
      jobcount = lv_job_number.
  
  " Submeter programa com variante
  SUBMIT z_sales_monthly
    USING SELECTION-SET lv_variant
    VIA JOB lv_job_name NUMBER lv_job_number
    AND RETURN.
  
  " Agendar para executar imediatamente
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = lv_job_number
      jobname   = lv_job_name
      strtimmed = 'X'.
  
  WRITE: / |✅ Job criado: { lv_job_name }|.
  WRITE: / 'Verificar em SM37'.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Nome descritivo para variante
DATA lv_variant TYPE raldb-variant VALUE 'DAILY_SALES_EUR'.  " ✅

" 2. Usar selection variables para datas
" No SE38 → Save Variant → Campo S_DATE → Selection Variable = 'D'

" 3. Proteger campos críticos
" Save Variant → Campo P_BUKRS → ☑ Protect field

" 4. Documentar variantes
*&---------------------------------------------------------------------*
*& Variante: MONTHLY_RUN
*& Descrição: Relatório mensal com range de todo o mês
*& Campos: S_DATE (BT - primeiro a último dia do mês)
*&---------------------------------------------------------------------*

" 5. Verificar se variante existe antes de usar
CALL FUNCTION 'RS_VARIANT_EXISTS'
  EXPORTING
    report  = 'Z_REPORT'
    variant = lv_variant
  EXCEPTIONS
    not_existent = 1.

IF sy-subrc = 0.  " ✅ Variante existe
  SUBMIT z_report USING SELECTION-SET lv_variant.
ENDIF.
```

### ❌ Evitar

```abap
" 1. Nome genérico
DATA lv_variant TYPE raldb-variant VALUE 'VAR1'.  " ❌

" 2. Hardcoded values em vez de variantes
SUBMIT z_report
  WITH s_date-low = '20250101'  " ❌ Usar variante!
  WITH s_date-high = '20251231'.

" 3. Não verificar se variante existe
SUBMIT z_report
  USING SELECTION-SET 'INEXISTENTE'  " ❌ Pode falhar!
  AND RETURN.

" 4. Variantes sem descrição
" Dificulta manutenção  " ❌

" 5. Misturar variante com parâmetros explícitos
SUBMIT z_report
  USING SELECTION-SET 'MY_VAR'
  WITH p_extra = lv_value.  " ❌ Confuso!
```

---

## 🔗 Próximos Passos

- **[Criar Jobs](1_criar_jobs.md)** - Usar variantes em jobs
- **[Agendamento](3_agendamento.md)** - Agendar jobs com variantes
- **[Monitorização](4_monitorizacao.md)** - Ver variantes usadas em SM37

---

**Tags:** `#Variantes` `#SelectionScreen` `#Jobs` `#ABAP` `#SE38`
