---
tags:
  - ABAP
  - IDoc
  - EDI
  - Integrações
  - ALE
---

# IDocs - Intermediate Documents

## 📋 Visão Geral

**IDoc** (Intermediate Document) é o formato padrão SAP para **troca de dados** entre sistemas SAP ou entre SAP e sistemas externos via **EDI** (Electronic Data Interchange).

**Características:**
- ✅ Formato estruturado e padronizado
- ✅ Assíncrono
- ✅ Garantia de entrega
- ✅ Auditoria completa
- ✅ Suporte a EDI (EDIFACT, ANSI X12)
- ✅ Baseado em ALE (Application Link Enabling)

---

## 🎯 Conceitos Fundamentais

### Estrutura de um IDoc

```
┌─────────────────────────┐
│   CONTROL RECORD        │  ← Controle (EDIDC)
├─────────────────────────┤
│   DATA RECORDS          │  ← Dados (EDIDD)
│   - Segment 1           │
│   - Segment 2           │
│   - ...                 │
├─────────────────────────┤
│   STATUS RECORDS        │  ← Status (EDIDS)
└─────────────────────────┘
```

### Componentes

**1. IDoc Type (Tipo de IDoc)**
- Define estrutura dos segments
- Ex: `ORDERS05`, `DEBMAS06`, `MATMAS05`

**2. Message Type**
- Tipo de mensagem de negócio
- Ex: `ORDERS` (Ordem), `DEBMAS` (Cliente), `MATMAS` (Material)

**3. Segments**
- Blocos de dados
- Hierarquia pai-filho
- Ex: `E1EDK01` (Cabeçalho), `E1EDP01` (Item)

**4. Logical System**
- Identificador do sistema
- Configurado em **SALE**

**5. Partner Profile**
- Configuração de parceiro
- Transaction **WE20**

---

## 🔧 Configuração Básica

### 1. Logical System (SALE)

**Definir sistemas lógicos:**

1. **SALE** → Basic Settings → Logical Systems
2. **Define Logical System**
3. Nome: `SAPDEV100`, `SAPQAS100`, etc.
4. **Assign Logical System to Client**

---

### 2. Partner Profile (WE20)

**Configurar parceiro:**

1. **WE20**
2. **Create Partner Profile**
3. **Partner Number:** `VENDOR001` ou `CUSTOMER001`
4. **Partner Type:** `KU` (Cliente), `LI` (Fornecedor), `LS` (Sistema)
5. **Outbound Parameters:**
   - Message Type: `ORDERS`
   - IDoc Type: `ORDERS05`
   - Process Code: `ORDE`
6. **Inbound Parameters:**
   - Process Code: `ORDE`

---

### 3. Port Definition (WE21)

**Criar porta de comunicação:**

1. **WE21**
2. **Transactional RFC** → Create
3. **Port:** `SAPPORT`
4. **RFC Destination:** Destino em SM59
5. **Save**

**Tipos de Port:**
- **File Port** - Gravar IDoc em arquivo
- **Transactional RFC** - Enviar via RFC
- **ABAP/4 Connections** - Conexão direta
- **XML HTTP** - Via HTTP/XML

---

## 📤 Criar IDoc Outbound

### Exemplo: Enviar Ordem de Venda

```abap
*&---------------------------------------------------------------------*
*& Report Z_CREATE_IDOC_ORDERS
*&---------------------------------------------------------------------*
REPORT z_create_idoc_orders.

DATA: ls_control TYPE edidc,
      lt_data    TYPE TABLE OF edidd,
      ls_data    TYPE edidd,
      lv_docnum  TYPE edi_docnum.

" ═══ CONTROL RECORD ═══
ls_control-mestyp = 'ORDERS'.        " Message Type
ls_control-idoctp = 'ORDERS05'.      " IDoc Type
ls_control-rcvprt = 'LS'.            " Partner Type (Logical System)
ls_control-rcvprn = 'SAPQAS100'.     " Partner (Destination System)
ls_control-sndprt = 'LS'.            " Sender Type
ls_control-sndprn = 'SAPDEV100'.     " Sender
ls_control-direct = '2'.             " Direction (2=Outbound)

" ═══ DATA RECORDS ═══

" Segment: E1EDK01 (Cabeçalho da Ordem)
CLEAR ls_data.
ls_data-segnam = 'E1EDK01'.
ls_data-sdata  = '0000100000ORDRE1001'.  " Número da ordem
APPEND ls_data TO lt_data.

" Segment: E1EDKA1 (Parceiro - Cliente)
CLEAR ls_data.
ls_data-segnam = 'E1EDKA1'.
ls_data-sdata  = 'AG0001000000'.  " AG = Sold-to Party
APPEND ls_data TO lt_data.

" Segment: E1EDP01 (Item)
CLEAR ls_data.
ls_data-segnam = 'E1EDP01'.
ls_data-sdata  = '000010MAT-001    00000100PC'.  " Item 10, Material, Qtd
APPEND ls_data TO lt_data.

" ═══ CRIAR IDOC ═══
CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
  EXPORTING
    master_idoc_control = ls_control
  IMPORTING
    communication_idoc_control = ls_control
  TABLES
    communication_idoc_data = lt_data
  EXCEPTIONS
    error_in_idoc_control = 1
    error_writing_idoc_status = 2
    error_in_idoc_data = 3
    OTHERS = 4.

IF sy-subrc = 0.
  lv_docnum = ls_control-docnum.
  WRITE: / |✅ IDoc criado: { lv_docnum }|.
  WRITE: / 'Verificar: WE02 ou WE05'.
ELSE.
  WRITE: / '❌ Erro ao criar IDoc'.
ENDIF.
```

---

## 📥 Processar IDoc Inbound

### Function Module de Processamento

**Criar FM para processar IDoc recebido:**

```abap
FUNCTION z_idoc_input_orders.
*"----------------------------------------------------------------------
*"*"Interface Local:
*"  IMPORTING
*"     VALUE(INPUT_METHOD) TYPE  BDWFAP_PAR-INPUTMETHD
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"----------------------------------------------------------------------

  DATA: ls_control TYPE edidc,
        lt_data    TYPE TABLE OF edidd,
        ls_data    TYPE edidd,
        ls_status  TYPE bdidocstat.

  " Ler control record
  READ TABLE idoc_contrl INTO ls_control INDEX 1.
  
  " Ler dados
  lt_data = idoc_data[].
  
  " Processar segments
  LOOP AT lt_data INTO ls_data.
    CASE ls_data-segnam.
      WHEN 'E1EDK01'.
        " Processar cabeçalho
        DATA(lv_order) = ls_data-sdata+0(10).
        WRITE: / |Ordem: { lv_order }|.
        
      WHEN 'E1EDP01'.
        " Processar item
        DATA(lv_item) = ls_data-sdata+0(6).
        DATA(lv_material) = ls_data-sdata+6(18).
        WRITE: / |Item: { lv_item }, Material: { lv_material }|.
    ENDCASE.
  ENDLOOP.
  
  " ═══ LÓGICA DE NEGÓCIO ═══
  " Criar documento SAP (ordem, nota, etc.)
  
  TRY.
      " Exemplo: Criar ordem via BAPI
      " ...
      
      " Sucesso
      ls_status-docnum = ls_control-docnum.
      ls_status-status = '53'.  " Successfully posted
      ls_status-msgty  = 'S'.
      ls_status-msgv1  = 'Ordem criada com sucesso'.
      APPEND ls_status TO idoc_status.
      
    CATCH cx_root INTO DATA(lo_ex).
      " Erro
      ls_status-docnum = ls_control-docnum.
      ls_status-status = '51'.  " Error
      ls_status-msgty  = 'E'.
      ls_status-msgv1  = lo_ex->get_text( ).
      APPEND ls_status TO idoc_status.
  ENDTRY.

ENDFUNCTION.
```

---

### Registrar Process Code (WE42)

**Associar FM ao IDoc Type:**

1. **WE42** - Process Code
2. **Create** → Processing with function module
3. **Process Code:** `ZORD` (Inbound)
4. **Function Module:** `Z_IDOC_INPUT_ORDERS`
5. **IDoc Type:** `ORDERS05`
6. **Save**

---

## 🔍 Monitorização de IDocs

### WE02 - IDoc Display

**Ver IDoc específico:**

1. **WE02**
2. Número do IDoc
3. **Display**

**Informações:**
- Control Record
- Data Records (Segments)
- Status Records

---

### WE05 - IDoc List

**Buscar IDocs:**

1. **WE05**
2. **Filtros:**
   - Data de criação
   - Message Type
   - Status
   - Parceiro
3. **Execute**

**Status comuns:**
- `01` - IDoc gerado
- `03` - Enviado para partner
- `12` - Enviado ao port
- `30` - IDoc pronto para envio (outbound)
- `50` - IDoc adicionado (inbound)
- `51` - Erro na aplicação
- `53` - Postado com sucesso
- `64` - IDoc pronto para processamento

---

### WE09 - IDoc Search via Message Type

**Buscar por tipo de mensagem.**

---

### WE19 - Test Tool

**Testar processamento de IDocs:**

1. **WE19**
2. **Existing IDoc** ou **Create New**
3. **Edit Segments**
4. **Standard Inbound**
5. **Process**

---

## 💡 Exemplos Práticos

### Exemplo 1: Criar Cliente (DEBMAS)

```abap
REPORT z_create_idoc_customer.

DATA: ls_control TYPE edidc,
      lt_data    TYPE TABLE OF edidd,
      ls_data    TYPE edidd.

" Control
ls_control-mestyp = 'DEBMAS'.
ls_control-idoctp = 'DEBMAS06'.
ls_control-rcvprt = 'LS'.
ls_control-rcvprn = 'SAPQAS100'.
ls_control-sndprt = 'LS'.
ls_control-sndprn = 'SAPDEV100'.
ls_control-direct = '2'.

" E1KNA1M - Dados gerais do cliente
CLEAR ls_data.
ls_data-segnam = 'E1KNA1M'.
ls_data-sdata+0(10)  = '0001000099'.  " Número do cliente
ls_data-sdata+10(4)  = 'KUNA'.        " Account group
ls_data-sdata+14(35) = 'Cliente Teste SA'.  " Nome
ls_data-sdata+49(2)  = 'PT'.          " País
APPEND ls_data TO lt_data.

" E1KNVVM - Dados de vendas
CLEAR ls_data.
ls_data-segnam = 'E1KNVVM'.
ls_data-sdata+0(4)  = '1000'.  " Org. Vendas
ls_data-sdata+4(2)  = '10'.    " Canal
ls_data-sdata+6(2)  = '00'.    " Setor
APPEND ls_data TO lt_data.

" Criar IDoc
CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
  EXPORTING
    master_idoc_control = ls_control
  TABLES
    communication_idoc_data = lt_data.

IF sy-subrc = 0.
  WRITE: / |✅ IDoc Cliente criado: { ls_control-docnum }|.
ENDIF.
```

---

### Exemplo 2: Criar Material (MATMAS)

```abap
REPORT z_create_idoc_material.

DATA: ls_control TYPE edidc,
      lt_data    TYPE TABLE OF edidd,
      ls_data    TYPE edidd.

" Control
ls_control-mestyp = 'MATMAS'.
ls_control-idoctp = 'MATMAS05'.
ls_control-rcvprt = 'LS'.
ls_control-rcvprn = 'SAPQAS100'.
ls_control-direct = '2'.

" E1MARAM - Dados gerais
CLEAR ls_data.
ls_data-segnam = 'E1MARAM'.
ls_data-sdata+0(18) = 'MAT-TEST-001'.     " Material
ls_data-sdata+18(4) = 'FERT'.             " Tipo
ls_data-sdata+22(40) = 'Material de Teste'.  " Descrição
APPEND ls_data TO lt_data.

" E1MARCM - Dados de centro
CLEAR ls_data.
ls_data-segnam = 'E1MARCM'.
ls_data-sdata+0(4)  = '1000'.  " Centro
APPEND ls_data TO lt_data.

" Criar
CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
  EXPORTING
    master_idoc_control = ls_control
  TABLES
    communication_idoc_data = lt_data.

WRITE: / |IDoc: { ls_control-docnum }|.
```

---

### Exemplo 3: Processar IDoc de Arquivo

```abap
*&---------------------------------------------------------------------*
*& Report Z_PROCESS_IDOC_FILE
*&---------------------------------------------------------------------*
REPORT z_process_idoc_file.

PARAMETERS: p_file TYPE string DEFAULT 'C:\idocs\order.dat'.

DATA: lv_filename TYPE string,
      lt_idoc_data TYPE TABLE OF edidd,
      ls_idoc_control TYPE edidc.

lv_filename = p_file.

" Ler arquivo IDoc
CALL FUNCTION 'EDI_DATA_INCOMING'
  EXPORTING
    file_name = lv_filename
  IMPORTING
    idoc_control = ls_idoc_control
  TABLES
    idoc_data = lt_idoc_data
  EXCEPTIONS
    file_not_found = 1
    OTHERS = 2.

IF sy-subrc = 0.
  " Processar IDoc
  CALL FUNCTION 'IDOC_INBOUND_ASYNCHRONOUS'
    EXPORTING
      idoc_control = ls_idoc_control
    TABLES
      idoc_data = lt_idoc_data.
      
  WRITE: / |✅ IDoc processado: { ls_idoc_control-docnum }|.
ELSE.
  WRITE: / '❌ Erro ao ler arquivo'.
ENDIF.
```

---

## 🔧 Extensão de IDocs

### Criar Extension (WE30)

**Adicionar campos customizados:**

1. **WE30** - IDoc Type Development
2. **IDoc Type:** `ORDERS05`
3. **Create Extension**
4. **Extension:** `Z_ORDERS05_EXT`
5. **Add Segment:** `Z1EDP01` (cópia de E1EDP01 com campos extras)
6. **Activate**

---

### Usar Extension

```abap
" Control com extension
ls_control-idoctp = 'ORDERS05'.
ls_control-cimtyp = 'Z_ORDERS05_EXT'.  " Extension

" Segment customizado
CLEAR ls_data.
ls_data-segnam = 'Z1EDP01'.
ls_data-sdata  = '...[dados com campos extras]...'.
APPEND ls_data TO lt_data.
```

---

## 🔒 Segurança e Autorização

### Authorization Objects

**S_IDOCDEFT** - IDoc Definition
- Criar/modificar IDoc types

**S_IDOC_ALL** - IDoc Administration
- Processar IDocs
- Ver status

**S_ALE_OBJ** - ALE Distribution
- Enviar IDocs

---

## ⚡ Performance e Boas Práticas

### ✅ Fazer

```abap
" 1. Processar IDocs em background
SUBMIT rseinb00 WITH docnum = lv_docnum AND RETURN.

" 2. Usar Packet Processing para volume alto
" BD87 - Processar IDocs em massa

" 3. Monitorar regularmente
" WE05 - Verificar IDocs com erro (Status 51)

" 4. Implementar retry para erros temporários
IF ls_status-status = '51'.
  " Verificar se é erro temporário
  " Reprocessar: BD87
ENDIF.

" 5. Usar filter em partner profile
" WE20 → Inbound Parameters → Filter

" 6. Documentar estrutura de segments
*&---------------------------------------------------------------------*
*& Segment E1EDP01 - Item:
*&   Posição 0-6:   Número do item
*&   Posição 6-24:  Material
*&   Posição 24-39: Quantidade
*&---------------------------------------------------------------------*
```

### ❌ Evitar

```abap
" 1. Processar um a um quando há volume
LOOP AT lt_idocs INTO DATA(lv_idoc).
  " Processar individualmente  ❌ Lento!
ENDLOOP.
" Use BD87 para massa

" 2. Não tratar status de erro
" Sempre verificar WE05 para status 51

" 3. IDocs muito grandes
" Dividir em múltiplos IDocs menores

" 4. Não testar antes de produção
" Sempre usar WE19 para testes

" 5. Hardcoded partner profiles
DATA lv_partner TYPE edi_rcvprn VALUE 'SYSTEM'.  " ❌ Configurar em WE20
```

---

## 🔧 Troubleshooting

### IDoc com Status 51 (Error)

**Solução:**
1. **WE02** → Ver IDoc
2. **Status** tab → Ver mensagem de erro
3. Corrigir dados
4. **WE19** → Reprocessar ou **BD87**

---

### IDoc não processado (Status 64)

**Solução:**
1. **BD87** - Process Inbound IDocs
2. Selecionar IDocs
3. **Process**

---

### IDoc não enviado

**Solução:**
1. Verificar **WE20** - Partner Profile
2. Verificar **SM59** - RFC Destination
3. **WE21** - Port ativo?
4. **BD87** - Process Outbound IDocs

---

### Erro de autorização

**Solução:**
- **PFCG** → Adicionar S_IDOC_ALL
- **SU53** → Verificar autorização faltante

---

## 📊 Comparação: IDoc vs Outras Integrações

| Aspecto | IDoc | RFC | OData | Web Service |
|---------|------|-----|-------|-------------|
| **Tipo** | Assíncrono | Síncrono/Async | Síncrono | Síncrono |
| **Formato** | Proprietário | Binário | JSON/XML | XML (SOAP) |
| **Auditoria** | ✅ Completa | ❌ Limitada | ❌ Não | ⚠️ Depende |
| **Reprocessamento** | ✅ Sim | ❌ Não | ❌ Não | ❌ Não |
| **Performance** | ⚠️ Média | ✅ Rápido | ✅ Rápido | ⚠️ Média |
| **Uso SAP** | ✅ Tradicional | ✅ Comum | ✅ **Moderno** | ⚠️ Legacy |

---

## 🔗 Próximos Passos

- **[RFC](1_rfc.md)** - Alternativa síncrona
- **[OData](3_odata.md)** - Integração moderna
- **[REST API](7_rest_api.md)** - APIs customizadas

---

**Tags:** `#IDoc` `#EDI` `#ALE` `#Integrações` `#WE02` `#WE05` `#DEBMAS` `#MATMAS` `#ORDERS`
