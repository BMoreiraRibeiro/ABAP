---
tags:
  - ABAP
  - Batch Processing
  - Batch Input
  - CALL TRANSACTION
  - BDC
---

# Processamento Batch (Batch Input)

## 📋 Visão Geral

**Batch Input (BDC)** permite **automatizar transações SAP** para processar grandes volumes de dados, simulando entrada manual de usuário.

---

## 🎯 Quando Usar

| Cenário | Solução |
|---------|---------|
| Migração de dados legados | ✅ Batch Input |
| Carga inicial de mestres | ✅ Batch Input |
| Processamento diário automático | ✅ Batch Input |
| Atualização em massa | ✅ Batch Input |
| Transação sem BAPI/FM | ✅ Batch Input |

---

## 🔧 Métodos de Batch Input

### 1️⃣ CALL TRANSACTION

**Rápido**, executa transação **diretamente** com dados.

**Vantagens:**
- ✅ Rápido
- ✅ Feedback imediato
- ✅ Permite commit/rollback por registro

**Desvantagens:**
- ❌ Sem log automático detalhado
- ❌ Precisa tratar erros manualmente

---

### 2️⃣ Session Method (SM35)

**Cria sessão** para processar **depois** em background.

**Vantagens:**
- ✅ Log completo em SM35
- ✅ Permite reprocessamento
- ✅ Processar grande volume

**Desvantagens:**
- ❌ Mais lento
- ❌ Processamento assíncrono

---

## 💡 CALL TRANSACTION

### Estrutura Básica

```abap
DATA: lt_bdcdata TYPE TABLE OF bdcdata,
      ls_bdcdata TYPE bdcdata,
      lt_messages TYPE TABLE OF bdcmsgcoll,
      ls_message TYPE bdcmsgcoll.

" Preencher BDC data
PERFORM bdc_dynpro   USING 'SAPMF02D' '0100'.
PERFORM bdc_field    USING 'RF02D-KUNNR' '1000'.
PERFORM bdc_field    USING 'RF02D-D0110' 'X'.
" ... mais campos

" Executar transação
CALL TRANSACTION 'XD02'
  USING lt_bdcdata
  MODE 'N'  " N=sem tela, A=todas, E=só erros
  UPDATE 'S'  " S=síncrono, A=assíncrono
  MESSAGES INTO lt_messages.

" Verificar mensagens
LOOP AT lt_messages INTO ls_message.
  IF ls_message-msgtyp = 'E' OR ls_message-msgtyp = 'A'.
    " Erro
  ENDIF.
ENDLOOP.
```

---

### Funções Auxiliares

```abap
*&---------------------------------------------------------------------*
*& Form BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING program TYPE bdcdata-program
                      dynpro  TYPE bdcdata-dynpro.
  
  CLEAR ls_bdcdata.
  ls_bdcdata-program  = program.
  ls_bdcdata-dynpro   = dynpro.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.
  
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BDC_FIELD
*&---------------------------------------------------------------------*
FORM bdc_field USING fnam TYPE bdcdata-fnam
                     fval TYPE bdcdata-fval.
  
  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = fnam.
  ls_bdcdata-fval = fval.
  APPEND ls_bdcdata TO lt_bdcdata.
  
ENDFORM.
```

---

### Exemplo Completo: Criar Cliente (XD01)

```abap
*&---------------------------------------------------------------------*
*& Report Z_BDC_CREATE_CUSTOMER
*&---------------------------------------------------------------------*
REPORT z_bdc_create_customer.

DATA: lt_bdcdata  TYPE TABLE OF bdcdata,
      ls_bdcdata  TYPE bdcdata,
      lt_messages TYPE TABLE OF bdcmsgcoll,
      ls_message  TYPE bdcmsgcoll.

PARAMETERS: p_kunnr TYPE kunnr,
            p_bukrs TYPE bukrs,
            p_name1 TYPE name1_gp,
            p_land1 TYPE land1_gp,
            p_ort01 TYPE ort01_gp.

START-OF-SELECTION.
  
  " ═══ Tela Inicial ═══
  PERFORM bdc_dynpro USING 'SAPMF02D' '0100'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'RF02D-KUNNR'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'RF02D-KUNNR' p_kunnr.
  PERFORM bdc_field  USING 'RF02D-BUKRS' p_bukrs.
  PERFORM bdc_field  USING 'RF02D-D0110' 'X'.  " Dados gerais
  
  " ═══ Dados Gerais ═══
  PERFORM bdc_dynpro USING 'SAPMF02D' '0110'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'KNA1-NAME1'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'KNA1-NAME1' p_name1.
  PERFORM bdc_field  USING 'KNA1-LAND1' p_land1.
  PERFORM bdc_field  USING 'KNA1-ORT01' p_ort01.
  
  " ═══ Salvar ═══
  PERFORM bdc_dynpro USING 'SAPMF02D' '0110'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=BU'.
  
  " ═══ Executar Transação ═══
  CALL TRANSACTION 'XD01'
    USING lt_bdcdata
    MODE 'N'
    UPDATE 'S'
    MESSAGES INTO lt_messages.
  
  " ═══ Verificar Resultado ═══
  READ TABLE lt_messages INTO ls_message
    WITH KEY msgtyp = 'S'.
  
  IF sy-subrc = 0.
    WRITE: / |✅ Cliente { p_kunnr } criado com sucesso|.
  ELSE.
    WRITE: / '❌ Erro ao criar cliente:'.
    LOOP AT lt_messages INTO ls_message
      WHERE msgtyp = 'E' OR msgtyp = 'A'.
      
      MESSAGE ID ls_message-msgid
              TYPE ls_message-msgtyp
              NUMBER ls_message-msgnr
              INTO DATA(lv_msg)
              WITH ls_message-msgv1
                   ls_message-msgv2
                   ls_message-msgv3
                   ls_message-msgv4.
      
      WRITE: / lv_msg.
    ENDLOOP.
  ENDIF.

*&---------------------------------------------------------------------*
*& Forms (BDC_DYNPRO e BDC_FIELD como acima)
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING program TYPE bdcdata-program
                      dynpro  TYPE bdcdata-dynpro.
  CLEAR ls_bdcdata.
  ls_bdcdata-program  = program.
  ls_bdcdata-dynpro   = dynpro.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.
ENDFORM.

FORM bdc_field USING fnam TYPE bdcdata-fnam
                     fval TYPE bdcdata-fval.
  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = fnam.
  ls_bdcdata-fval = fval.
  APPEND ls_bdcdata TO lt_bdcdata.
ENDFORM.
```

---

## 📦 Session Method (SM35)

### Criar Sessão BDC

```abap
REPORT z_bdc_session_create.

DATA: lv_session_name TYPE apqi-groupid VALUE 'Z_CUSTOMER_UPLOAD',
      lt_bdcdata      TYPE TABLE OF bdcdata,
      ls_bdcdata      TYPE bdcdata.

START-OF-SELECTION.
  
  " ═══ Abrir Sessão ═══
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client = sy-mandt
      group  = lv_session_name
      user   = sy-uname
      keep   = 'X'.  " Manter sessão após processamento
  
  " ═══ Loop pelos dados (ex: tabela interna) ═══
  DATA: lt_customers TYPE TABLE OF ty_customer.
  
  " Simular dados
  lt_customers = VALUE #(
    ( kunnr = '1001' name1 = 'Cliente A' land1 = 'BR' ort01 = 'São Paulo' )
    ( kunnr = '1002' name1 = 'Cliente B' land1 = 'BR' ort01 = 'Rio de Janeiro' )
  ).
  
  LOOP AT lt_customers INTO DATA(ls_customer).
    
    REFRESH lt_bdcdata.
    
    " Preencher BDC (similar ao exemplo CALL TRANSACTION)
    PERFORM bdc_dynpro USING 'SAPMF02D' '0100'.
    PERFORM bdc_field  USING 'RF02D-KUNNR' ls_customer-kunnr.
    " ... mais campos
    
    " Inserir na sessão
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode = 'XD01'
      TABLES
        dynprotab = lt_bdcdata.
    
  ENDLOOP.
  
  " ═══ Fechar Sessão ═══
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
  
  WRITE: / |✅ Sessão { lv_session_name } criada|.
  WRITE: / 'Processar em SM35'.
```

---

### Processar Sessão Programaticamente

```abap
REPORT z_bdc_process_session.

DATA: lv_session_name TYPE apqi-groupid VALUE 'Z_CUSTOMER_UPLOAD'.

START-OF-SELECTION.
  
  CALL FUNCTION 'BDC_PROCESS'
    EXPORTING
      group   = lv_session_name
      mode    = 'N'  " Sem tela
      update  = 'S'  " Síncrono
      ctumode = 'P'  " Process
    EXCEPTIONS
      OTHERS  = 1.
  
  IF sy-subrc = 0.
    WRITE: / '✅ Sessão processada'.
  ELSE.
    WRITE: / '❌ Erro ao processar sessão'.
  ENDIF.
```

---

## 🛠️ Gravar Transação (SHDB)

**SHDB** permite **gravar** uma transação para gerar código BDC automaticamente.

**Passos:**

1. **Executar SHDB**
2. **New Recording**
3. **Transaction Code:** (ex: XD01)
4. **Recording Name:** Z_XD01_RECORDING
5. **Executar transação normalmente** (preencher campos)
6. **Salvar**
7. **Program → Generate Program** → Código BDC gerado!

---

## 💡 Processamento em Background

### Job com Batch Input

```abap
REPORT z_job_batch_input.

DATA: lv_job_name   TYPE tbtcjob-jobname VALUE 'Z_BDC_CUSTOMERS',
      lv_job_number TYPE tbtcjob-jobcount.

START-OF-SELECTION.
  
  " Criar job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname  = lv_job_name
    IMPORTING
      jobcount = lv_job_number.
  
  " Executar programa BDC
  SUBMIT z_bdc_create_customers
    VIA JOB lv_job_name NUMBER lv_job_number
    WITH s_file = '/data/customers.txt'  " Parâmetro de arquivo
    AND RETURN.
  
  " Agendar imediatamente
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = lv_job_number
      jobname   = lv_job_name
      strtimmed = 'X'.
  
  WRITE: / |✅ Job { lv_job_number } agendado|.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre verificar mensagens
LOOP AT lt_messages INTO ls_message
  WHERE msgtyp = 'E' OR msgtyp = 'A' OR msgtyp = 'W'.
  " Processar erro
ENDLOOP.

" 2. Usar MODE 'N' em produção
MODE 'N'  " ✅ Sem tela, mais rápido

" 3. Commit por pacote (não todos de uma vez)
IF lv_counter MOD 100 = 0.
  COMMIT WORK.
ENDIF.

" 4. Logar erros detalhadamente
IF ls_message-msgtyp = 'E'.
  WRITE: / |Erro cliente { ls_customer-kunnr }: { lv_msg }|.
ENDIF.

" 5. Usar Session Method para grande volume
" Permite reprocessamento em caso de erro
```

### ❌ Evitar

```abap
" 1. MODE 'A' em produção
MODE 'A'  " ❌ Exibe todas as telas, muito lento

" 2. Ignorar mensagens de warning
" ⚠️ Podem indicar problemas

" 3. Processar tudo em foreground
" ❌ Trava interface
" ✅ Usar jobs em background

" 4. BDC sem tratamento de erro
CALL TRANSACTION 'XD01' USING lt_bdcdata.
" ❌ Sem MESSAGES INTO, impossível saber se funcionou

" 5. Não testar com MODE 'A' antes
" ✅ Testar com tela visível primeiro
```

---

## 🔧 Troubleshooting

### BDC Não Funciona

**Causa:** Campos ou telas mudaram

**Solução:**
1. Executar SHDB novamente
2. Re-gravar transação
3. Comparar código antigo com novo

---

### Mensagens de Erro Genéricas

**Causa:** Campos obrigatórios faltando

**Solução:**
```abap
" Executar com MODE 'E' (mostra só telas com erro)
MODE 'E'
```

---

### Performance Ruim

**Causa:** Muitas transações em sequência

**Solução:**
```abap
" Processar em pacotes
DATA: lv_counter TYPE i.

LOOP AT lt_customers INTO ls_customer.
  
  lv_counter = lv_counter + 1.
  
  " ... BDC ...
  
  " Commit a cada 100
  IF lv_counter MOD 100 = 0.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.  " Respiro para o sistema
  ENDIF.
  
ENDLOOP.
```

---

## 🔗 Próximos Passos

- **[Criar Jobs](1_criar_jobs.md)** - Executar BDC via jobs
- **[Agendamento](3_agendamento.md)** - Agendar processamentos batch
- **[Job Logs](5_job_logs.md)** - Logar resultados de BDC

---

**Tags:** `#BatchInput` `#BDC` `#CALL_TRANSACTION` `#SM35` `#SHDB` `#DataMigration`
