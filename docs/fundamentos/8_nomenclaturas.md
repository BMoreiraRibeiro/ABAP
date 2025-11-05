---
tags:
  - ABAP
  - Fundamentos
  - Nomenclaturas
  - Convenções
  - Naming
---

# Nomenclaturas e Convenções ABAP

## 📋 Visão Geral

Seguir **convenções de nomenclatura** garante código legível, manutenível e alinhado com as **boas práticas SAP**. Esta página documenta os prefixos e padrões recomendados.

---

## 🔤 Regras Gerais

1. **Customer Namespace**: Use `Z` ou `Y` para objetos customizados
2. **Sem espaços**: Use `_` (underscore) para separar palavras
3. **Nomes descritivos**: Evite abreviações obscuras
4. **Case-insensitive**: ABAP não diferencia maiúsculas/minúsculas, mas use MAIÚSCULAS para constantes

---

## 📦 Objetos de Desenvolvimento

### Programas

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Report executável | `Z` ou `Y` | `ZVENDA_RELATORIO` | Programas tipo 1 |
| Include | `Z` ou `Y` | `ZINCLUDE_VENDAS` | Includes |
| Module Pool | `SAPMZ` | `SAPMZVENDA` | Programas tipo M |
| Function Group | `Z` ou `Y` | `ZVENDA` | Grupo de funções |

---

### Classes e Interfaces

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Classe global | `ZCL_` | `ZCL_VENDA_MANAGER` | Classes globais |
| Classe local | `LCL_` | `LCL_HELPER` | Classes locais em programas |
| Classe de teste | `LTCL_` | `LTCL_VENDA_TEST` | Classes de teste ABAP Unit |
| Interface global | `ZIF_` | `ZIF_CALCULADOR` | Interfaces globais |
| Interface local | `LIF_` | `LIF_HANDLER` | Interfaces locais |

**Exemplos:**
```abap
" ✅ Correto
CLASS zcl_customer_manager DEFINITION.
CLASS zcl_order_processor DEFINITION.

" ❌ Evitar
CLASS z_cust DEFINITION.        " Muito curto
CLASS CustomerManager DEFINITION.  " Sem prefixo Z
```

---

### Tabelas e Estruturas

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Tabela transparente | `Z` ou `Y` | `ZVENDA_HEADER` | Tabela de BD |
| Estrutura | `Z` ou `Y` | `ZSTR_VENDA` | Structure (sem tabela física) |
| Tabela customizing | `ZTC_` | `ZTC_CONFIG_VENDA` | Tabelas de configuração |
| Append Structure | `ZA_` | `ZA_MARA` | Extensão de tabela standard |

---

### Elementos de Dados e Domínios

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Elemento de dados | `ZE_` | `ZE_CUSTOMER_ID` | Data element |
| Domínio | `ZD_` | `ZD_STATUS` | Domain |
| Type Pool | `Z` | `ZTYPE_POOL` | Type pool (legacy) |

**Exemplo:**
```abap
" Domínio
ZD_STATUS: CHAR1 com value range (A, I, C)

" Elemento de dados
ZE_ORDER_STATUS → usa domínio ZD_STATUS

" Campo na tabela
ZORDER_HEADER-STATUS → tipo ZE_ORDER_STATUS
```

---

### Function Modules

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Function Module | `Z_` | `Z_CALC_PRICE` | FM customizado |
| RFC-enabled FM | `Z_RFC_` | `Z_RFC_GET_ORDERS` | Remote-enabled |

**Exemplo:**
```abap
FUNCTION z_calc_discount.
  IMPORTING iv_amount TYPE p.
  EXPORTING ev_discount TYPE p.
ENDFUNCTION.
```

---

## 🔢 Variáveis e Campos

### Prefixos de Variáveis (Notação Húngara)

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Local variable | `lv_` | `lv_customer_id` | Variável local |
| Structure | `ls_` | `ls_customer` | Estrutura local |
| Internal table | `lt_` | `lt_orders` | Tabela interna |
| Global variable | `gv_` | `gv_total_amount` | Variável global |
| Global structure | `gs_` | `gs_config` | Estrutura global |
| Global table | `gt_` | `gt_customers` | Tabela global |
| Parameter | `iv_`, `ev_`, `cv_` | `iv_input`, `ev_output` | Importing/Exporting/Changing |
| Field-symbol | `<fs_>` | `<fs_customer>` | Field-symbol |
| Reference | `lr_` | `lr_object` | Referência local |
| Constant | `lc_` / `gc_` | `lc_max_items` | Constante local/global |

**Exemplo completo:**
```abap
METHOD calculate_total.
  " Parâmetros
  DATA(lv_input) = iv_amount.      " Input parameter
  
  " Variáveis locais
  DATA: lv_total   TYPE p,          " Local variable
        lv_tax     TYPE p,
        ls_order   TYPE zorder,     " Local structure
        lt_items   TYPE TABLE OF zorder_item,  " Local table
        <fs_item>  TYPE zorder_item.           " Field-symbol
  
  " Constantes
  CONSTANTS: lc_tax_rate TYPE p VALUE '0.23'.
  
  " Referências
  DATA: lr_calculator TYPE REF TO zcl_calculator.
  
  " Global (atributo da classe)
  gv_counter = gv_counter + 1.
ENDMETHOD.
```

---

## 📝 Objetos de Dicionário

### Search Helps

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Search Help | `ZSH_` | `ZSH_CUSTOMERS` | F4 help |
| Collective SH | `ZSHC_` | `ZSHC_ORDERS` | Collective search help |

---

### Table Types

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Table Type | `ZTT_` | `ZTT_ORDERS` | Tipo tabela |
| Range Table Type | `ZRTT_` | `ZRTT_CUSTOMER` | RANGE table |

**Exemplo:**
```abap
" Definição
TYPES: ztt_orders TYPE TABLE OF zorder.

" Uso
DATA: lt_orders TYPE ztt_orders.
```

---

## 🎨 Objetos de UI

### Dynpros e Reports

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Selection Screen | - | `PARAMETERS: p_kunnr` | Tela de seleção |
| Parameter | `p_` | `p_customer_id` | PARAMETERS |
| Select-Option | `s_` | `s_date` | SELECT-OPTIONS |

**Exemplo:**
```abap
PARAMETERS: p_kunnr TYPE kunnr,
            p_date  TYPE datum.

SELECT-OPTIONS: s_vkorg FOR vbak-vkorg,
                s_datum FOR sy-datum.
```

---

### Smartforms e Adobe Forms

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Smartform | `Z_SF_` | `Z_SF_INVOICE` | Smartform |
| Adobe Form | `Z_AF_` | `Z_AF_INVOICE` | Adobe Form |
| Form Interface | `Z_FI_` | `Z_FI_INVOICE` | Interface do form |

---

## 🔐 Objetos de Autorização

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Authorization Object | `Z_` | `Z_VENDA` | Objeto de autorização |
| Authorization Class | `Z` | `ZVENDA` | Classe de autorização |

---

## 📊 Objetos de Mensagem

| Tipo | Prefixo | Exemplo | Descrição |
|------|---------|---------|-----------|
| Message Class | `Z` ou `Y` | `ZVENDA` | Classe de mensagem |

**Exemplo:**
```abap
MESSAGE e001(zvenda) WITH lv_customer.
" zvenda = classe de mensagem
" e001 = mensagem número 001, tipo erro
```

---

## 🎯 Boas Práticas

### ✅ Fazer

```abap
" Nomes descritivos
DATA: lv_customer_name TYPE string,
      lv_total_amount  TYPE p,
      lt_sales_orders  TYPE TABLE OF zvenda.

" Constantes em UPPER_CASE
CONSTANTS: lc_max_items TYPE i VALUE 1000,
           gc_tax_rate  TYPE p VALUE '0.23'.

" Classes com prefixo
CLASS zcl_order_processor DEFINITION.
CLASS zcl_customer_manager DEFINITION.

" Interfaces claras
INTERFACE zif_calculator.
INTERFACE zif_validator.

" Tabelas com namespace
ZVENDA_HEADER
ZVENDA_ITEM
ZTC_CONFIG_PRICING
```

### ❌ Evitar

```abap
" ❌ Nomes muito curtos
DATA: x TYPE i,
      tmp TYPE string.

" ❌ Sem prefixos
DATA: customer TYPE kunnr,
      amount TYPE p.

" ❌ Abreviações obscuras
DATA: lv_cst_nm TYPE string.  " customer name?

" ❌ Classes sem ZCL
CLASS customer_manager DEFINITION.

" ❌ Tabelas sem Z/Y
TABLE my_orders.
```

---

## 📚 Convenções por Contexto

### Em Classes Globais

```abap
CLASS zcl_order_processor DEFINITION PUBLIC.
  
  PUBLIC SECTION.
    " Atributos públicos (evitar)
    DATA: gv_public_var TYPE i READ-ONLY.
    
    " Métodos públicos
    METHODS: process_order
      IMPORTING iv_order_id TYPE zorder_id
      RETURNING VALUE(rv_success) TYPE abap_bool.
      
  PROTECTED SECTION.
    " Atributos protegidos
    DATA: gv_config TYPE zconfig.
    
  PRIVATE SECTION.
    " Atributos privados
    DATA: gv_counter TYPE i,
          gt_cache   TYPE TABLE OF zorder.
    
    " Métodos privados
    METHODS: validate_order
      IMPORTING iv_order_id TYPE zorder_id.
      
ENDCLASS.
```

### Em Function Modules

```abap
FUNCTION z_calculate_price.
  IMPORTING
    VALUE(iv_product_id) TYPE matnr
    VALUE(iv_quantity)   TYPE menge_d
  EXPORTING
    VALUE(ev_price)      TYPE netpr
  EXCEPTIONS
    product_not_found
    invalid_quantity.
```

---

## 🔗 Próximos Passos

- **[Orientação a Objetos Básica](7_OO_basica.md)** - Aplicar nomenclaturas em OO
- **[Classes e Objetos](../objetos/1_classes_objetos.md)** - Criar classes seguindo convenções
- **[Security](../security/index.md)** - Nomenclaturas de objetos de autorização

---

## 📖 Referências

- SAP Naming Conventions (SAP Help Portal)
- ABAP Code Inspector (SCI) - verifica nomenclaturas
- Clean ABAP Guidelines

---

**Tags:** `#nomenclaturas` `#naming` `#conventions` `#best-practices` `#zcl` `#zif`
