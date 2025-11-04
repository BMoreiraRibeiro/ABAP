---
tags:
  - ABAP
  - OData
  - Integrações
  - RESTful
  - Fiori
---

# OData Services

## 📋 Visão Geral

**OData** (Open Data Protocol) é um protocolo **RESTful** para consumir e expor dados via HTTP. É a base para aplicações **SAP Fiori**.

**Características:**
- ✅ Baseado em REST/HTTP
- ✅ Formato JSON ou XML
- ✅ Query options padronizadas
- ✅ CRUD completo (Create, Read, Update, Delete)
- ✅ Suporte a metadata
- ✅ Versionamento (V2/V4)

---

## 🎯 OData V2 vs V4

| Aspecto | OData V2 | OData V4 |
|---------|----------|----------|
| **Versão SAP** | NetWeaver 7.4+ | S/4HANA |
| **Sintaxe** | Mais simples | Mais poderosa |
| **Funções** | Limitadas | Actions/Functions |
| **Batch** | Sim | Sim (melhorado) |
| **Anotações** | Básicas | Avançadas (CDS) |
| **Uso** | Fiori clássico | **Fiori moderno** |

---

## 🛠️ Criar OData Service (V2)

### Transaction SEGW

**SAP Gateway Service Builder**

### Passo a Passo

#### 1. Criar Projeto

1. **SEGW** → Create Project
2. **Nome:** `Z_PRODUCTS_SRV`
3. **Descrição:** "Serviço de Produtos"
4. **Save**

#### 2. Definir Data Model

**Entity Type (Tabela):**

```
Nome: Product
Properties:
  - ProductId (Edm.String) [Key]
  - ProductName (Edm.String)
  - Category (Edm.String)
  - Price (Edm.Decimal)
  - Currency (Edm.String)
  - Stock (Edm.Int32)
```

**Criar:**
1. **Data Model** → Entity Types → Create
2. Adicionar properties
3. Marcar **ProductId** como key

#### 3. Criar Entity Set

**Entity Set (Coleção):**

```
Nome: ProductSet
Entity Type: Product
```

#### 4. Gerar Runtime Objects

1. **Generate** → Runtime Objects
2. Classe gerada: `ZCL_Z_PRODUCTS_SRV_DPC_EXT`

#### 5. Implementar Métodos

**Redefinir métodos na classe `_DPC_EXT`:**

##### GET (Read)

```abap
METHOD productset_get_entity.
  " Ler um produto específico
  DATA: ls_key TYPE zcl_z_products_srv_mpc=>ts_product.
  
  " Obter chave da URL
  io_tech_request_context->get_converted_keys(
    IMPORTING es_key_values = ls_key ).
  
  " Buscar produto
  SELECT SINGLE
    matnr AS product_id,
    maktx AS product_name,
    mtart AS category,
    netpr AS price,
    waers AS currency,
    labst AS stock
  FROM mara
  JOIN makt ON mara~matnr = makt~matnr
  JOIN mbew ON mara~matnr = mbew~matnr
  WHERE mara~matnr = @ls_key-product_id
    AND makt~spras = @sy-langu
  INTO CORRESPONDING FIELDS OF @er_entity.
  
  IF sy-subrc <> 0.
    " Erro 404 - Not Found
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid            = /iwbep/cx_mgw_busi_exception=>resource_not_found
        message_unlimited = |Produto { ls_key-product_id } não encontrado|.
  ENDIF.
ENDMETHOD.
```

##### GET_ENTITYSET (Query)

```abap
METHOD productset_get_entityset.
  " Ler múltiplos produtos
  DATA: lt_products TYPE zcl_z_products_srv_mpc=>tt_product,
        lv_top      TYPE i,
        lv_skip     TYPE i,
        lv_filter   TYPE string.
  
  " Query options
  lv_top   = io_tech_request_context->get_top( ).
  lv_skip  = io_tech_request_context->get_skip( ).
  lv_filter = io_tech_request_context->get_filter( )->get_filter_string( ).
  
  " Buscar produtos
  SELECT
    matnr AS product_id,
    maktx AS product_name,
    mtart AS category,
    netpr AS price,
    waers AS currency,
    labst AS stock
  FROM mara
  JOIN makt ON mara~matnr = makt~matnr
  JOIN mbew ON mara~matnr = mbew~matnr
  WHERE makt~spras = @sy-langu
  ORDER BY matnr
  INTO CORRESPONDING FIELDS OF TABLE @lt_products
  UP TO @lv_top ROWS
  OFFSET @lv_skip.
  
  " Aplicar filtros (se houver)
  IF lv_filter IS NOT INITIAL.
    " Processar filtro OData
    " Exemplo: $filter=Price gt 100
    " (simplificado - usar /IWBEP/CL_MGW_RUNTIME_PARSER)
  ENDIF.
  
  er_entityset = lt_products.
ENDMETHOD.
```

##### CREATE

```abap
METHOD productset_create_entity.
  " Criar novo produto
  DATA: ls_product TYPE zcl_z_products_srv_mpc=>ts_product,
        lv_matnr   TYPE matnr.
  
  " Ler dados do request
  io_data_provider->read_entry_data(
    IMPORTING es_data = ls_product ).
  
  " Validações
  IF ls_product-product_name IS INITIAL.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid = /iwbep/cx_mgw_busi_exception=>business_error
        message_unlimited = 'Nome do produto é obrigatório'.
  ENDIF.
  
  " Criar produto via BAPI
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata = VALUE bapimathead(
        material  = ls_product-product_id
        matl_type = ls_product-category
      )
    IMPORTING
      material = lv_matnr
    TABLES
      " ... parâmetros ...
      return = DATA(lt_return).
  
  " Verificar erros
  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_unlimited = 'Erro ao criar produto'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = 'X'.
      
    " Retornar produto criado
    er_entity = ls_product.
    er_entity-product_id = lv_matnr.
  ENDIF.
ENDMETHOD.
```

##### UPDATE

```abap
METHOD productset_update_entity.
  " Atualizar produto
  DATA: ls_product TYPE zcl_z_products_srv_mpc=>ts_product,
        ls_key     TYPE zcl_z_products_srv_mpc=>ts_product.
  
  " Obter chave
  io_tech_request_context->get_converted_keys(
    IMPORTING es_key_values = ls_key ).
  
  " Ler novos dados
  io_data_provider->read_entry_data(
    IMPORTING es_data = ls_product ).
  
  " Atualizar via BAPI ou direto
  UPDATE makt SET maktx = @ls_product-product_name
    WHERE matnr = @ls_key-product_id
      AND spras = @sy-langu.
  
  IF sy-subrc = 0.
    er_entity = ls_product.
  ELSE.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_unlimited = 'Erro ao atualizar produto'.
  ENDIF.
ENDMETHOD.
```

##### DELETE

```abap
METHOD productset_delete_entity.
  " Deletar produto
  DATA ls_key TYPE zcl_z_products_srv_mpc=>ts_product.
  
  " Obter chave
  io_tech_request_context->get_converted_keys(
    IMPORTING es_key_values = ls_key ).
  
  " Deletar produto (exemplo simplificado)
  DELETE FROM mara WHERE matnr = @ls_key-product_id.
  
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_unlimited = 'Erro ao deletar produto'.
  ENDIF.
ENDMETHOD.
```

#### 6. Registrar Serviço

1. **SEGW** → Service → Register
2. **System Alias:** LOCAL
3. **Package:** $TMP ou ZPACKAGE

#### 7. Ativar Serviço

**Transaction /IWFND/MAINT_SERVICE:**

1. Add Service
2. System: LOCAL
3. Technical Service Name: `Z_PRODUCTS_SRV`
4. **Add Selected Services**

---

## 🌐 Consumir OData Service

### Browser

**URL Base:**
```
http://server:port/sap/opu/odata/sap/Z_PRODUCTS_SRV/
```

**Metadata:**
```
http://server:port/sap/opu/odata/sap/Z_PRODUCTS_SRV/$metadata
```

**Entity Set:**
```
http://server:port/sap/opu/odata/sap/Z_PRODUCTS_SRV/ProductSet
```

**Entity específica:**
```
http://server:port/sap/opu/odata/sap/Z_PRODUCTS_SRV/ProductSet('MAT-001')
```

---

### Query Options

#### $filter

**Filtrar resultados:**

```
GET ProductSet?$filter=Price gt 100
GET ProductSet?$filter=Category eq 'FERT'
GET ProductSet?$filter=ProductName eq 'Produto A' and Price lt 50
```

**Operadores:**
- `eq` - Igual
- `ne` - Diferente
- `gt` - Maior que
- `ge` - Maior ou igual
- `lt` - Menor que
- `le` - Menor ou igual
- `and` - E lógico
- `or` - Ou lógico

#### $select

**Selecionar campos:**

```
GET ProductSet?$select=ProductId,ProductName,Price
```

#### $orderby

**Ordenar:**

```
GET ProductSet?$orderby=Price desc
GET ProductSet?$orderby=ProductName asc
```

#### $top e $skip

**Paginação:**

```
GET ProductSet?$top=10              # Primeiros 10
GET ProductSet?$top=10&$skip=20     # Itens 21-30
```

#### $expand

**Expandir navegação:**

```
GET ProductSet?$expand=Supplier
```

#### $count

**Contar registros:**

```
GET ProductSet/$count
GET ProductSet?$count=true
```

---

### JavaScript (SAPUI5/Fiori)

```javascript
// Criar modelo OData
var oModel = new sap.ui.model.odata.v2.ODataModel({
    serviceUrl: "/sap/opu/odata/sap/Z_PRODUCTS_SRV/"
});

// GET - Ler produtos
oModel.read("/ProductSet", {
    success: function(oData) {
        console.log(oData.results);
    },
    error: function(oError) {
        console.error(oError);
    }
});

// GET com filtros
oModel.read("/ProductSet", {
    filters: [
        new sap.ui.model.Filter("Price", "GT", 100),
        new sap.ui.model.Filter("Category", "EQ", "FERT")
    ],
    success: function(oData) {
        console.log(oData.results);
    }
});

// POST - Criar produto
oModel.create("/ProductSet", {
    ProductId: "MAT-999",
    ProductName: "Novo Produto",
    Category: "FERT",
    Price: "99.90",
    Currency: "EUR",
    Stock: 100
}, {
    success: function(oData) {
        console.log("Produto criado:", oData);
    },
    error: function(oError) {
        console.error(oError);
    }
});

// PUT - Atualizar produto
oModel.update("/ProductSet('MAT-001')", {
    ProductName: "Produto Atualizado",
    Price: "149.90"
}, {
    success: function() {
        console.log("Produto atualizado");
    }
});

// DELETE - Deletar produto
oModel.remove("/ProductSet('MAT-001')", {
    success: function() {
        console.log("Produto deletado");
    }
});
```

---

### Postman / cURL

#### GET

```bash
curl -X GET \
  "http://server:port/sap/opu/odata/sap/Z_PRODUCTS_SRV/ProductSet" \
  -H "Accept: application/json" \
  -u username:password
```

#### POST

```bash
curl -X POST \
  "http://server:port/sap/opu/odata/sap/Z_PRODUCTS_SRV/ProductSet" \
  -H "Content-Type: application/json" \
  -H "X-CSRF-Token: Fetch" \
  -u username:password \
  -d '{
    "ProductId": "MAT-999",
    "ProductName": "Novo Produto",
    "Price": "99.90",
    "Currency": "EUR"
  }'
```

**Nota:** Para POST/PUT/DELETE, primeiro fazer GET para obter **X-CSRF-Token**.

#### GET Token

```bash
curl -X GET \
  "http://server:port/sap/opu/odata/sap/Z_PRODUCTS_SRV/" \
  -H "X-CSRF-Token: Fetch" \
  -u username:password \
  -i
```

Retorna no header: `X-CSRF-Token: <token>`

---

## 📊 OData via CDS Views (RAP)

**ABAP RESTful Application Programming Model**

### Criar CDS View com OData

```abap
@AbapCatalog.sqlViewName: 'ZPRODUCTS_V'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Products'

@OData.publish: true  // ✅ Expor como OData automaticamente

define view Z_Products_CDS
  as select from mara
  inner join makt on mara.matnr = makt.matnr
{
  key mara.matnr as ProductId,
      makt.maktx as ProductName,
      mara.mtart as Category,
      mara.meins as Unit
}
where makt.spras = $session.system_language
```

**Ativação automática:** Gateway cria serviço OData automaticamente!

**URL:**
```
/sap/opu/odata/sap/Z_PRODUCTS_CDS_CDS/
```

---

### RAP - Behavior Definition

**Adicionar operações CRUD:**

```abap
managed implementation in class zbp_products unique;

define behavior for Z_Products_CDS alias Product
persistent table mara
lock master
{
  create;
  update;
  delete;
  
  field ( readonly ) ProductId;
  field ( mandatory ) ProductName;
  
  mapping for mara {
    ProductId = matnr;
    ProductName = maktx;
  }
}
```

---

## 🔒 Segurança OData

### Autenticação

**Tipos:**
1. **Basic Auth** - Usuário/Senha
2. **OAuth 2.0** - Token
3. **SAML** - SSO

### Autorização

**Authorization Object:** `/IWFND/RT_GW`

**Perfis necessários:**
- `/IWFND/RT_SERV_USER` - Usuário de serviço
- Autorizações específicas do objeto

### CSRF Protection

**OData POST/PUT/DELETE** requerem **X-CSRF-Token**.

**Obter token:**
```javascript
// SAPUI5
var oModel = new sap.ui.model.odata.v2.ODataModel({
    serviceUrl: "/sap/opu/odata/sap/Z_SERVICE/",
    tokenHandling: true  // Automático
});
```

---

## 🔧 Troubleshooting

### Gateway Client (/IWFND/GW_CLIENT)

**Testar serviço:**

1. **/IWFND/GW_CLIENT**
2. **Service:** `Z_PRODUCTS_SRV`
3. **Request:** `ProductSet`
4. **Execute**

### Error Log (/IWFND/ERROR_LOG)

Ver erros detalhados de execução.

### Trace (/IWFND/TRACES)

Ativar trace para debug:

1. **/IWFND/TRACES**
2. Create Trace
3. User: seu usuário
4. Executar request
5. Ver trace detalhado

---

## ⚡ Performance

### ✅ Boas Práticas

```abap
" 1. Usar $select para limitar campos
GET ProductSet?$select=ProductId,ProductName

" 2. Usar $top para limitar registros
GET ProductSet?$top=50

" 3. Implementar paginação server-side
METHOD productset_get_entityset.
  DATA(lv_top) = io_tech_request_context->get_top( ).
  DATA(lv_skip) = io_tech_request_context->get_skip( ).
  
  SELECT * FROM mara
    UP TO @lv_top ROWS
    OFFSET @lv_skip
    INTO TABLE @lt_data.
ENDMETHOD.

" 4. Usar CDS Views com anotações
@Analytics.query: true
@ObjectModel.representativeKey: 'ProductId'

" 5. Implementar filtros eficientes
" Converter filtros OData em WHERE clause
```

### ❌ Evitar

```abap
" 1. SELECT * sem limites
SELECT * FROM huge_table INTO TABLE @lt_data.  " ❌

" 2. Não implementar paginação
METHOD productset_get_entityset.
  SELECT * FROM mara INTO TABLE @lt_all.  " ❌ Todos os registros!
ENDMETHOD.

" 3. Processar filtros em ABAP em vez de SQL
SELECT * FROM mara INTO TABLE @lt_data.
LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs>).
  IF <fs>-price < 100.  " ❌ Filtrar no SELECT!
    DELETE lt_data.
  ENDIF.
ENDLOOP.
```

---

## 💡 Exemplo Completo: Fiori App

### Backend (SEGW Service)

**Já implementado acima:** `Z_PRODUCTS_SRV`

### Frontend (SAPUI5)

**manifest.json:**
```json
{
  "sap.app": {
    "id": "com.example.products",
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata/sap/Z_PRODUCTS_SRV/",
        "type": "OData",
        "settings": {
          "odataVersion": "2.0"
        }
      }
    }
  },
  "sap.ui5": {
    "models": {
      "": {
        "dataSource": "mainService",
        "preload": true
      }
    }
  }
}
```

**View (List.view.xml):**
```xml
<mvc:View
  controllerName="com.example.products.controller.List"
  xmlns:mvc="sap.ui.core.mvc"
  xmlns="sap.m">
  
  <Page title="Produtos">
    <content>
      <Table
        items="{
          path: '/ProductSet',
          sorter: { path: 'ProductName' }
        }">
        <columns>
          <Column><Text text="ID"/></Column>
          <Column><Text text="Nome"/></Column>
          <Column><Text text="Preço"/></Column>
        </columns>
        <items>
          <ColumnListItem>
            <cells>
              <Text text="{ProductId}"/>
              <Text text="{ProductName}"/>
              <ObjectNumber
                number="{Price}"
                unit="{Currency}"/>
            </cells>
          </ColumnListItem>
        </items>
      </Table>
    </content>
  </Page>
</mvc:View>
```

**Controller (List.controller.js):**
```javascript
sap.ui.define([
  "sap/ui/core/mvc/Controller",
  "sap/m/MessageToast"
], function (Controller, MessageToast) {
  "use strict";

  return Controller.extend("com.example.products.controller.List", {
    
    onInit: function () {
      var oModel = this.getView().getModel();
      
      // Refresh automático
      oModel.attachRequestCompleted(function() {
        MessageToast.show("Produtos carregados");
      });
    },
    
    onCreate: function () {
      var oModel = this.getView().getModel();
      
      oModel.create("/ProductSet", {
        ProductId: "MAT-NEW",
        ProductName: "Novo Produto",
        Price: "199.90",
        Currency: "EUR"
      }, {
        success: function () {
          MessageToast.show("Produto criado!");
          oModel.refresh();
        },
        error: function (oError) {
          MessageToast.show("Erro: " + oError.message);
        }
      });
    }
  });
});
```

---

## 🔗 Próximos Passos

- **[HTTP Client](4_http_client.md)** - Consumir OData de sistemas externos
- **[REST API](7_rest_api.md)** - Criar APIs RESTful customizadas
- **[Web Services](5_web_services.md)** - Alternativa SOAP ao OData

---

**Tags:** `#OData` `#RESTful` `#Fiori` `#Integrações` `#SAPUI5` `#SEGW` `#CDS`
