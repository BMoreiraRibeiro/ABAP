---
tags:
  - ABAP
  - BAPI
  - Integrações
  - Business API
---

# BAPI - Business Application Programming Interface

## 📋 Visão Geral

**BAPI** (Business Application Programming Interface) são métodos padronizados de Business Objects SAP que permitem **integração** com processos de negócio.

**Características:**
- ✅ RFC-enabled (podem ser chamados remotamente)
- ✅ Padronizados e documentados
- ✅ Validações de negócio incluídas
- ✅ Versionados e estáveis
- ✅ Suportados oficialmente pela SAP

---

## 🎯 Diferença: BAPI vs RFC vs Function Module

| Aspecto | Function Module | RFC | BAPI |
|---------|----------------|-----|------|
| **Propósito** | Qualquer lógica | Chamada remota | Processo de negócio |
| **Padronização** | Não | Não | Sim |
| **Documentação** | Opcional | Opcional | Obrigatória |
| **Validação** | Manual | Manual | Automática |
| **Estabilidade** | Pode mudar | Pode mudar | Versionado |
| **Uso Externo** | Não recomendado | Sim | **Sim (preferido)** |

---

## 🔍 Encontrar BAPIs

### Transaction BAPI

**Explorador de BAPIs:**

1. **BAPI** ou **BAPI Explorer**
2. Navegar pela árvore de Business Objects
3. Ver métodos disponíveis

**Exemplo de estrutura:**
```
📁 SalesOrder
  └─ 📄 BAPI_SALESORDER_CREATEFROMDAT2
  └─ 📄 BAPI_SALESORDER_CHANGE
  └─ 📄 BAPI_SALESORDER_GETLIST
  
📁 Material
  └─ 📄 BAPI_MATERIAL_SAVEDATA
  └─ 📄 BAPI_MATERIAL_GET_DETAIL
```

### SE37 - Function Builder

**Procurar BAPIs:**
- Padrão de nome: `BAPI_*`
- Exemplo: `BAPI_*CUSTOMER*`

---

## 📊 Estrutura Comum de BAPIs

### Parâmetros Típicos

```abap
CALL FUNCTION 'BAPI_EXEMPLO'
  EXPORTING
    iv_objeto_key    = lv_key           " Chave do objeto
  IMPORTING
    ev_resultado     = lv_resultado      " Resultado
  TABLES
    it_dados         = lt_dados          " Dados de entrada
    et_return        = lt_return         " Mensagens
  EXCEPTIONS
    error            = 1
    OTHERS           = 2.
```

### Tabela RETURN

**Todas as BAPIs** retornam mensagens via tabela `RETURN`:

```abap
TYPES: BEGIN OF ty_return,
         type   TYPE bapireturn-type,   " S/E/W/I/A
         id     TYPE bapireturn-id,      " Message Class
         number TYPE bapireturn-number,  " Message Number
         message TYPE bapireturn-message," Texto
       END OF ty_return.

DATA: lt_return TYPE TABLE OF bapiret2.
```

**Tipos de mensagem:**
- `S` - Success
- `E` - Error
- `W` - Warning
- `I` - Information
- `A` - Abort

---

## 💡 BAPIs Mais Usados

### 1️⃣ BAPI_SALESORDER_CREATEFROMDAT2

**Criar Ordem de Venda:**

```abap
DATA: ls_header    TYPE bapisdhd1,
      lt_items     TYPE TABLE OF bapisditm,
      lt_partners  TYPE TABLE OF bapiparnr,
      lt_return    TYPE TABLE OF bapiret2,
      lv_order     TYPE vbeln_va.

" Cabeçalho
ls_header-doc_type = 'TA'.
ls_header-sales_org = '1000'.
ls_header-distr_chan = '10'.
ls_header-division = '00'.

" Item
APPEND VALUE #(
  itm_number = '000010'
  material   = 'MAT-001'
  target_qty = '10'
  plant      = '1000'
) TO lt_items.

" Parceiro (Cliente)
APPEND VALUE #(
  partn_role = 'AG'  " Sold-to Party
  partn_numb = '1000'
) TO lt_partners.

" Criar ordem
CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
    order_header_in = ls_header
  IMPORTING
    salesdocument   = lv_order
  TABLES
    return          = lt_return
    order_items_in  = lt_items
    order_partners  = lt_partners.

" Verificar erros
READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  WRITE: / '❌ Erro ao criar ordem:'.
  LOOP AT lt_return INTO DATA(ls_msg) WHERE type = 'E'.
    WRITE: / ls_msg-message.
  ENDLOOP.
  
  " Desfazer
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  " Commit
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
      
  WRITE: / |✅ Ordem criada: { lv_order }|.
ENDIF.
```

---

### 2️⃣ BAPI_MATERIAL_GET_DETAIL

**Buscar Detalhes de Material:**

```abap
DATA: ls_material TYPE bapi_mara,
      lt_return   TYPE TABLE OF bapiret2.

CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
  EXPORTING
    material       = 'MAT-001'
  IMPORTING
    material_general_data = ls_material
  TABLES
    return         = lt_return.

IF ls_material IS NOT INITIAL.
  WRITE: / |Material: { ls_material-material }|.
  WRITE: / |Descrição: { ls_material-matl_desc }|.
  WRITE: / |Tipo: { ls_material-matl_type }|.
ELSE.
  WRITE: / '❌ Material não encontrado'.
ENDIF.
```

---

### 3️⃣ BAPI_GOODSMVT_CREATE

**Criar Movimento de Mercadorias:**

```abap
DATA: ls_header TYPE bapi2017_gm_head_01,
      ls_code   TYPE bapi2017_gm_code,
      lt_items  TYPE TABLE OF bapi2017_gm_item_create,
      lt_return TYPE TABLE OF bapiret2,
      lv_matdoc TYPE bapi2017_gm_head_ret-mat_doc.

" Código de movimento (MB01)
ls_code-gm_code = '01'.  " Goods Receipt

" Cabeçalho
ls_header-pstng_date = sy-datum.
ls_header-doc_date   = sy-datum.

" Item
APPEND VALUE #(
  material    = 'MAT-001'
  plant       = '1000'
  stge_loc    = '0001'
  move_type   = '501'      " GR sem pedido
  entry_qnt   = '100'
  entry_uom   = 'PC'
) TO lt_items.

" Criar movimento
CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
  EXPORTING
    goodsmvt_header  = ls_header
    goodsmvt_code    = ls_code
  IMPORTING
    materialdocument = lv_matdoc
  TABLES
    goodsmvt_item    = lt_items
    return           = lt_return.

" Verificar
READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  WRITE: / '❌ Erro ao criar movimento'.
ELSE.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING wait = 'X'.
  WRITE: / |✅ Material document: { lv_matdoc }|.
ENDIF.
```

---

### 4️⃣ BAPI_ACC_DOCUMENT_POST

**Lançar Documento Contábil:**

```abap
DATA: ls_header TYPE bapiache09,
      lt_gl     TYPE TABLE OF bapiacgl09,
      lt_curr   TYPE TABLE OF bapiaccr09,
      lt_return TYPE TABLE OF bapiret2,
      lv_docnr  TYPE bapi_acc_doc_header_ret.

" Cabeçalho
ls_header-username   = sy-uname.
ls_header-comp_code  = '1000'.
ls_header-doc_date   = sy-datum.
ls_header-pstng_date = sy-datum.
ls_header-doc_type   = 'SA'.
ls_header-ref_doc_no = 'REF-001'.

" Item GL - Débito
APPEND VALUE #(
  itemno_acc = '1'
  gl_account = '400000'
  comp_code  = '1000'
  pstng_date = sy-datum
  doc_type   = 'SA'
) TO lt_gl.

" Item GL - Crédito
APPEND VALUE #(
  itemno_acc = '2'
  gl_account = '500000'
  comp_code  = '1000'
  pstng_date = sy-datum
  doc_type   = 'SA'
) TO lt_gl.

" Valores
APPEND VALUE #(
  itemno_acc = '1'
  currency   = 'EUR'
  amt_doccur = '1000.00'
) TO lt_curr.

APPEND VALUE #(
  itemno_acc = '2'
  currency   = 'EUR'
  amt_doccur = '-1000.00'
) TO lt_curr.

" Lançar
CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
  EXPORTING
    documentheader = ls_header
  IMPORTING
    obj_key        = lv_docnr
  TABLES
    accountgl      = lt_gl
    currencyamount = lt_curr
    return         = lt_return.

" Verificar
READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  WRITE: / '❌ Erro ao lançar documento'.
ELSE.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING wait = 'X'.
  WRITE: / |✅ Documento contábil: { lv_docnr }|.
ENDIF.
```

---

### 5️⃣ BAPI_CUSTOMER_GETLIST

**Buscar Lista de Clientes:**

```abap
DATA: lt_customers TYPE TABLE OF bapi_customer_data,
      lt_return    TYPE TABLE OF bapiret2,
      ls_maxrows   TYPE bapi_maxrows.

ls_maxrows-maxrows = 100.

CALL FUNCTION 'BAPI_CUSTOMER_GETLIST'
  EXPORTING
    maxrows = ls_maxrows
  TABLES
    customer_list = lt_customers
    return        = lt_return.

WRITE: / |Clientes encontrados: { lines( lt_customers ) }|, /.

LOOP AT lt_customers INTO DATA(ls_customer) TO 20.
  WRITE: / |{ ls_customer-customer } - { ls_customer-name }|.
ENDLOOP.
```

---

## 🔄 Commit e Rollback

### BAPI_TRANSACTION_COMMIT

**SEMPRE** necessário após BAPIs de modificação:

```abap
" ✅ Forma correta
CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  " ... parâmetros ...
  
IF sy-subrc = 0.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.  " Aguardar commit
ENDIF.
```

**Parâmetro WAIT:**
- `wait = 'X'` - **Aguarda** commit terminar (recomendado)
- `wait = ' '` - Commit assíncrono

### BAPI_TRANSACTION_ROLLBACK

**Desfazer** alterações em caso de erro:

```abap
READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  " Erro encontrado - desfazer
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  MESSAGE 'Operação cancelada' TYPE 'E'.
ELSE.
  " Sucesso - confirmar
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING wait = 'X'.
ENDIF.
```

---

## 🛡️ Tratamento de Erros

### Análise de Mensagens RETURN

```abap
CLASS lcl_bapi_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_return
        IMPORTING it_return TYPE bapiret2_t
        RETURNING VALUE(rv_has_error) TYPE abap_bool,
        
      display_messages
        IMPORTING it_return TYPE bapiret2_t.
ENDCLASS.

CLASS lcl_bapi_helper IMPLEMENTATION.
  METHOD check_return.
    " Verifica se há erros
    READ TABLE it_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    rv_has_error = xsdbool( sy-subrc = 0 ).
    
    IF rv_has_error = abap_false.
      " Verificar aborts
      READ TABLE it_return WITH KEY type = 'A' TRANSPORTING NO FIELDS.
      rv_has_error = xsdbool( sy-subrc = 0 ).
    ENDIF.
  ENDMETHOD.
  
  METHOD display_messages.
    LOOP AT it_return INTO DATA(ls_msg).
      DATA(lv_icon) = SWITCH #( ls_msg-type
        WHEN 'E' THEN '❌'
        WHEN 'W' THEN '⚠️'
        WHEN 'S' THEN '✅'
        WHEN 'I' THEN 'ℹ️'
        ELSE '•' ).
        
      WRITE: / |{ lv_icon } { ls_msg-message }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" Uso
START-OF-SELECTION.
  DATA lt_return TYPE bapiret2_t.
  
  " ... chamar BAPI ...
  
  IF lcl_bapi_helper=>check_return( lt_return ) = abap_true.
    WRITE: / '═══ ERROS ═══', /.
    lcl_bapi_helper=>display_messages( lt_return ).
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    WRITE: / '✅ Sucesso!'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = 'X'.
  ENDIF.
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre verificar RETURN
CALL FUNCTION 'BAPI_XXX'
  TABLES return = lt_return.
  
READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.  " ✅ Verificar erros
  " Tratar erro
ENDIF.

" 2. SEMPRE fazer COMMIT após modificação
CALL FUNCTION 'BAPI_XXX_CREATE'
  " ... parâmetros ...
  
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'  " ✅ Commit
  EXPORTING wait = 'X'.

" 3. Usar WAIT = 'X' em commits
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING wait = 'X'.  " ✅ Aguardar

" 4. Rollback em caso de erro
IF lv_error = abap_true.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.  " ✅ Desfazer
ENDIF.

" 5. Documentar BAPIs usados
*&---------------------------------------------------------------------*
*& Usa BAPI_SALESORDER_CREATEFROMDAT2 para criar ordens
*& Requer autorização V_VBAK_AAT
*&---------------------------------------------------------------------*
```

### ❌ Evitar

```abap
" 1. Não verificar erros
CALL FUNCTION 'BAPI_XXX'
  TABLES return = lt_return.
" ❌ E se houve erro?

" 2. Esquecer COMMIT
CALL FUNCTION 'BAPI_XXX_CREATE'
  " ... criar algo ...
" ❌ Nada foi salvo!

" 3. COMMIT sem WAIT
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.  " ❌ Pode não ter terminado

" 4. Usar COMMIT WORK direto
COMMIT WORK.  " ❌ Usar BAPI_TRANSACTION_COMMIT

" 5. Misturar BAPI com UPDATE
CALL FUNCTION 'BAPI_XXX_CREATE'.
UPDATE dbtab SET field = value.  " ❌ Inconsistente!
```

---

## 🔧 Criar BAPI Customizado

### 1. Criar Function Module (SE37)

```abap
FUNCTION z_bapi_customer_create.
*"----------------------------------------------------------------------
*"*"Interface Local:
*"  IMPORTING
*"     VALUE(IS_CUSTOMER) TYPE  ZST_CUSTOMER
*"  EXPORTING
*"     VALUE(EV_KUNNR) TYPE  KUNNR
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  " Validações
  IF is_customer-name1 IS INITIAL.
    APPEND VALUE bapiret2(
      type    = 'E'
      id      = 'Z_MSG'
      number  = '001'
      message = 'Nome é obrigatório'
    ) TO return.
    RETURN.
  ENDIF.
  
  " Lógica de negócio
  TRY.
      " Gerar número
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'Z_KUNNR'
        IMPORTING
          number      = ev_kunnr.
          
      " Inserir cliente
      INSERT kna1 FROM VALUE #(
        kunnr = ev_kunnr
        name1 = is_customer-name1
        land1 = is_customer-land1
      ).
      
      IF sy-subrc = 0.
        APPEND VALUE bapiret2(
          type    = 'S'
          id      = 'Z_MSG'
          number  = '002'
          message = |Cliente { ev_kunnr } criado|
        ) TO return.
      ENDIF.
      
    CATCH cx_root INTO DATA(lo_ex).
      APPEND VALUE bapiret2(
        type    = 'E'
        message = lo_ex->get_text( )
      ) TO return.
  ENDTRY.

ENDFUNCTION.
```

### 2. Configurar como BAPI

1. **SE37** → Attributes
2. ☑ **Remote-Enabled Module**
3. **Processing Type:** Normal
4. Save & Activate

### 3. Criar Business Object (SWO1)

Opcional, mas recomendado para integração completa.

---

## 💡 Exemplo Completo: Criar Pedido de Compra

```abap
*&---------------------------------------------------------------------*
*& Report Z_BAPI_CREATE_PO
*&---------------------------------------------------------------------*
REPORT z_bapi_create_po.

DATA: ls_header  TYPE bapimepoheader,
      ls_headerx TYPE bapimepoheaderx,
      lt_items   TYPE TABLE OF bapimepoitem,
      lt_itemsx  TYPE TABLE OF bapimepoitemx,
      lt_return  TYPE TABLE OF bapiret2,
      lv_ponumber TYPE ebeln.

START-OF-SELECTION.
  
  " Cabeçalho do pedido
  ls_header-comp_code = '1000'.
  ls_header-doc_type  = 'NB'.
  ls_header-vendor    = '1000'.
  ls_header-purch_org = '1000'.
  ls_header-pur_group = '001'.
  
  " Flags de atualização
  ls_headerx-comp_code = 'X'.
  ls_headerx-doc_type  = 'X'.
  ls_headerx-vendor    = 'X'.
  ls_headerx-purch_org = 'X'.
  ls_headerx-pur_group = 'X'.
  
  " Item 1
  APPEND VALUE #(
    po_item   = '00010'
    material  = 'MAT-001'
    plant     = '1000'
    quantity  = '100'
    net_price = '10.50'
    price_unit = '1'
  ) TO lt_items.
  
  APPEND VALUE #(
    po_item   = '00010'
    material  = 'X'
    plant     = 'X'
    quantity  = 'X'
    net_price = 'X'
    price_unit = 'X'
  ) TO lt_itemsx.
  
  " Item 2
  APPEND VALUE #(
    po_item   = '00020'
    material  = 'MAT-002'
    plant     = '1000'
    quantity  = '50'
    net_price = '25.00'
    price_unit = '1'
  ) TO lt_items.
  
  APPEND VALUE #(
    po_item   = '00020'
    material  = 'X'
    plant     = 'X'
    quantity  = 'X'
    net_price = 'X'
    price_unit = 'X'
  ) TO lt_itemsx.
  
  " Criar pedido
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader  = ls_header
      poheaderx = ls_headerx
    IMPORTING
      exppurchaseorder = lv_ponumber
    TABLES
      return    = lt_return
      poitem    = lt_items
      poitemx   = lt_itemsx.
  
  " Verificar resultado
  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  
  IF sy-subrc = 0.
    " Erro
    WRITE: / '❌ Erro ao criar pedido:', /.
    LOOP AT lt_return INTO DATA(ls_msg) WHERE type CA 'EA'.
      WRITE: / ls_msg-message.
    ENDLOOP.
    
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    
  ELSE.
    " Sucesso
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = 'X'.
      
    WRITE: / '✅ Pedido criado com sucesso!'.
    WRITE: / |Número: { lv_ponumber }|.
    WRITE: / |Itens: { lines( lt_items ) }|, /.
    
    " Mensagens de sucesso
    LOOP AT lt_return INTO ls_msg WHERE type = 'S'.
      WRITE: / ls_msg-message.
    ENDLOOP.
  ENDIF.
```

---

## 🔗 Próximos Passos

- **[RFC](1_rfc.md)** - BAPIs são RFCs especiais
- **[OData](3_odata.md)** - Expor BAPIs via OData
- **[Web Services](5_web_services.md)** - Consumir BAPIs via SOAP

---

**Tags:** `#BAPI` `#Integrações` `#BusinessAPI` `#ABAP` `#RFC`
