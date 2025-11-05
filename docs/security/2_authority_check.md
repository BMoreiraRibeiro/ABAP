---
tags:
  - ABAP
  - Security
  - AUTHORITY-CHECK
  - Authorization
---

# AUTHORITY-CHECK

## 📋 Visão Geral

**AUTHORITY-CHECK** é o comando ABAP para verificar se usuário atual tem autorização para executar uma operação.

---

## 🔧 Sintaxe Básica

```abap
AUTHORITY-CHECK OBJECT 'NOME_OBJETO'
  ID 'CAMPO1' FIELD valor1
  ID 'CAMPO2' FIELD valor2
  ID 'CAMPO3' DUMMY.  " Não verifica este campo

" Verificar resultado
IF sy-subrc <> 0.
  " Sem autorização
ENDIF.
```

---

## 📊 Valores de sy-subrc

| sy-subrc | Significado |
|----------|-------------|
| **0** | ✅ Tem autorização |
| **4** | ❌ Sem autorização |
| **8** | ⚠️ Dado mestre incompleto |
| **12** | ❌ Objeto não existe |
| **16** | ⚠️ Erro de sintaxe |
| **24** | ❌ Sem dado mestre |

---

## 💡 Exemplos Práticos

### Verificar Acesso a Transação

```abap
AUTHORITY-CHECK OBJECT 'S_TCODE'
  ID 'TCD' FIELD 'SE38'.

CASE sy-subrc.
  WHEN 0.
    MESSAGE 'Acesso permitido' TYPE 'S'.
  WHEN 4.
    MESSAGE 'Sem autorização para SE38' TYPE 'E'.
  WHEN 12.
    MESSAGE 'Objeto S_TCODE não configurado' TYPE 'E'.
ENDCASE.
```

---

### Verificar Acesso a Empresa

```abap
PARAMETERS: p_bukrs TYPE bukrs.

START-OF-SELECTION.
  
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD p_bukrs
    ID 'ACTVT' FIELD '03'.  " 03 = Visualizar
  
  IF sy-subrc <> 0.
    MESSAGE |Sem autorização para empresa { p_bukrs }| TYPE 'E'.
  ENDIF.
  
  " Continuar processamento...
  SELECT * FROM bkpf
    WHERE bukrs = @p_bukrs
    INTO TABLE @DATA(lt_docs).
```

---

### Verificar Múltiplos Campos

```abap
AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
  ID 'ACTVT' FIELD '02'         " 02 = Alterar
  ID 'WERKS' FIELD lv_planta
  ID 'LGORT' FIELD lv_deposito.

IF sy-subrc = 0.
  " Autorizado para alterar material nesta planta/depósito
  mo_material->update( ).
ELSE.
  MESSAGE 'Sem autorização para alterar material' TYPE 'E'.
ENDIF.
```

---

### DUMMY - Ignorar Campo

```abap
" Verificar apenas ACTVT, ignorar WERKS
AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
  ID 'ACTVT' FIELD '03'
  ID 'WERKS' DUMMY.  " Não verificado (permite qualquer planta)

IF sy-subrc = 0.
  " Pode visualizar materiais em qualquer planta
ENDIF.
```

---

## 🎯 Quando Verificar Autorizações?

### ✅ Sempre verificar antes de:

1. **Gravar/Alterar/Deletar dados**
   ```abap
   AUTHORITY-CHECK OBJECT 'Z_VENDAS'
     ID 'ACTVT' FIELD '02'.  " Alterar
   
   IF sy-subrc = 0.
     UPDATE vbak SET ...
   ENDIF.
   ```

2. **Executar transação**
   ```abap
   AUTHORITY-CHECK OBJECT 'S_TCODE'
     ID 'TCD' FIELD 'SM37'.
   ```

3. **Ler dados sensíveis**
   ```abap
   AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
     ID 'BUKRS' FIELD lv_company
     ID 'ACTVT' FIELD '03'.  " Visualizar
   
   IF sy-subrc = 0.
     SELECT * FROM bseg...
   ENDIF.
   ```

4. **Executar operações críticas**
   ```abap
   AUTHORITY-CHECK OBJECT 'S_DATASET'
     ID 'ACTVT' FIELD '34'  " Execute
     ID 'FILENAME' FIELD '/tmp/data.txt'.
   ```

---

## 💡 Exemplo Completo: Report Protegido

```abap
*&---------------------------------------------------------------------*
*& Report Z_SALES_REPORT
*& Relatório de vendas (protegido por autorizações)
*&---------------------------------------------------------------------*
REPORT z_sales_report.

PARAMETERS: p_vkorg TYPE vkorg,  " Org. vendas
            p_vtweg TYPE vtweg.  " Canal

START-OF-SELECTION.
  
  " ═══ Verificar autorização para org. vendas ═══
  AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
    ID 'VKORG' FIELD p_vkorg
    ID 'VTWEG' FIELD p_vtweg
    ID 'ACTVT' FIELD '03'.  " Visualizar
  
  CASE sy-subrc.
    WHEN 0.
      " OK - tem autorização
      
    WHEN 4.
      MESSAGE |Sem autorização para org. vendas { p_vkorg }| TYPE 'E'.
      RETURN.
      
    WHEN 12.
      MESSAGE 'Objeto V_VBAK_VKO não configurado' TYPE 'E'.
      RETURN.
      
    WHEN OTHERS.
      MESSAGE 'Erro ao verificar autorização' TYPE 'E'.
      RETURN.
  ENDCASE.
  
  " ═══ Processar apenas se autorizado ═══
  SELECT * FROM vbak
    WHERE vkorg = @p_vkorg
      AND vtweg = @p_vtweg
    INTO TABLE @DATA(lt_orders).
  
  LOOP AT lt_orders INTO DATA(ls_order).
    WRITE: / ls_order-vbeln, ls_order-kunnr, ls_order-netwr.
  ENDLOOP.
  
  WRITE: / |Total: { lines( lt_orders ) } ordens|.
```

---

## 🔐 Verificações Dinâmicas

### Verificar Lista de Valores

```abap
DATA: lt_empresas TYPE TABLE OF bukrs.

lt_empresas = VALUE #( ( '1000' ) ( '2000' ) ( '3000' ) ).

LOOP AT lt_empresas INTO DATA(lv_bukrs).
  
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD lv_bukrs
    ID 'ACTVT' FIELD '03'.
  
  IF sy-subrc = 0.
    " Processar empresa autorizada
    PERFORM process_company USING lv_bukrs.
  ELSE.
    WRITE: / |Empresa { lv_bukrs }: SEM AUTORIZAÇÃO|.
  ENDIF.
  
ENDLOOP.
```

---

### Verificar Antes de Loop

```abap
" ✅ Verificar ANTES de buscar dados
AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
  ID 'TABLE' FIELD 'MARA'
  ID 'ACTVT' FIELD '03'.

IF sy-subrc = 0.
  " Autorizado - buscar dados
  SELECT * FROM mara INTO TABLE @DATA(lt_materials).
  
  LOOP AT lt_materials INTO DATA(ls_material).
    WRITE: / ls_material-matnr.
  ENDLOOP.
ELSE.
  MESSAGE 'Sem autorização para tabela MARA' TYPE 'E'.
ENDIF.
```

---

## 🛡️ Proteger Transação Custom

### 1. Criar Objeto de Autorização (SU21)

```
Objeto: Z_MY_TCODE
Campos:
  - TCD (Código transação)
```

### 2. No Código da Transação

```abap
*&---------------------------------------------------------------------*
*& Transaction Z_MY_TRANS
*&---------------------------------------------------------------------*

INITIALIZATION.
  " Verificar autorização logo no início
  AUTHORITY-CHECK OBJECT 'Z_MY_TCODE'
    ID 'TCD' FIELD sy-tcode.  " sy-tcode = Z_MY_TRANS
  
  IF sy-subrc <> 0.
    MESSAGE 'Sem autorização para executar esta transação' TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.

START-OF-SELECTION.
  " ... lógica da transação ...
```

### 3. PFCG - Incluir em Role

```
Role: Z_USER_ROLE
Autorização:
  Z_MY_TCODE
    TCD: Z_MY_TRANS
```

---

## 📋 Logging de Tentativas

```abap
AUTHORITY-CHECK OBJECT 'S_TCODE'
  ID 'TCD' FIELD 'SE38'.

IF sy-subrc <> 0.
  " Logar tentativa falhada
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log = VALUE bal_s_log(
        object    = 'ZSECURITY'
        subobject = 'AUTH'
        aluser    = sy-uname
        aldate    = sy-datum
        altime    = sy-uzeit )
    IMPORTING
      e_log_handle = DATA(lv_log_handle).
  
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = lv_log_handle
      i_s_msg = VALUE bal_s_msg(
        msgty = 'E'
        msgid = 'ZZ'
        msgno = '001'
        msgv1 = |Tentativa negada: SE38 por { sy-uname }| ).
  
  CALL FUNCTION 'BAL_DB_SAVE'.
  COMMIT WORK.
  
  " Mensagem ao usuário
  MESSAGE 'Acesso negado. Incidente registrado.' TYPE 'E'.
ENDIF.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre verificar sy-subrc
AUTHORITY-CHECK OBJECT 'S_TCODE'
  ID 'TCD' FIELD 'SE38'.

IF sy-subrc <> 0.  " ✅ Verifica
  MESSAGE 'Sem autorização' TYPE 'E'.
ENDIF.

" 2. Mensagens claras
MESSAGE |Sem autorização para empresa { p_bukrs }| TYPE 'E'.  " ✅

" 3. Verificar ANTES de processar
AUTHORITY-CHECK ...  " ✅ Primeiro
IF sy-subrc = 0.
  " Processar
ENDIF.

" 4. Usar valores de atividade corretos
ID 'ACTVT' FIELD '01'.  " ✅ 01 = Criar (correto)
```

### ❌ Evitar

```abap
" 1. Ignorar sy-subrc
AUTHORITY-CHECK OBJECT 'S_TCODE'
  ID 'TCD' FIELD 'SE38'.
" ❌ Não verifica sy-subrc!

" 2. Processar e depois verificar
SELECT * FROM kna1...  " ❌ Já leu dados!
AUTHORITY-CHECK ...    " Tarde demais

" 3. Hardcoded bypass
IF sy-uname = 'ADMIN'.
  " ❌ Pula verificação
ELSE.
  AUTHORITY-CHECK ...
ENDIF.

" 4. Mensagens genéricas
MESSAGE 'Erro' TYPE 'E'.  " ❌ Não ajuda usuário
```

---

## 🔗 Próximos Passos

- **[Autorizações](1_autorizacoes.md)** - Conceitos básicos
- **[Roles e Perfis](3_roles_perfis.md)** - Gestão em PFCG
- **[Práticas Seguras](5_praticas_seguras.md)** - Segurança no código

---

**Tags:** `#AUTHORITY-CHECK` `#Security` `#Authorization` `#Access-Control`
