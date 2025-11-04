# CDS - Anotações e Metadata Extensions

## O que são Anotações?

Anotações são metadados que enriquecem as CDS Views com informações semânticas, comportamentais e de apresentação. São fundamentais para integração com frameworks como SAP Fiori, OData e Analytics.

### Sintaxe Básica

```abap
@AnnotationName: value
@AnnotationName.property: value
@AnnotationName: { property1: value1, property2: value2 }
```

## Categorias de Anotações

### 1. Anotações Técnicas

#### Access Control

```abap
// Verificar autorização
@AccessControl.authorizationCheck: #CHECK

// Não verificar (apenas desenvolvimento/demo)
@AccessControl.authorizationCheck: #NOT_REQUIRED

// Verificar apenas com privilégio específico
@AccessControl.authorizationCheck: #PRIVILEGED_ONLY
```

#### ABAP Catalog

```abap
// Buffering (cache)
@AbapCatalog.buffering.status: #ACTIVE
@AbapCatalog.buffering.type: #GENERIC
@AbapCatalog.buffering.numberOfKeyFields: 1

// Compiler
@AbapCatalog.compiler.compareFilter: true
```

### 2. Anotações de Apresentação

#### End User Text

```abap
@EndUserText.label: 'Customer Master Data'
@EndUserText.quickInfo: 'Complete customer information'

define view entity ZI_Customer
  as select from kna1
{
  key kunnr as CustomerId,
      
      @EndUserText.label: 'Customer Name'
      @EndUserText.quickInfo: 'Full name of the customer'
      name1 as CustomerName
}
```

#### UI Annotations

```abap
@UI.headerInfo: {
  typeName: 'Customer',
  typeNamePlural: 'Customers',
  title: { value: 'CustomerName' }
}

define view entity ZC_Customer
  as select from ZI_Customer
{
  @UI.lineItem: [{ position: 10, importance: #HIGH }]
  @UI.identification: [{ position: 10 }]
  key CustomerId,
  
  @UI.lineItem: [{ position: 20, importance: #HIGH }]
  @UI.identification: [{ position: 20 }]
  CustomerName,
  
  @UI.lineItem: [{ position: 30 }]
  Country
}
```

### 3. Anotações Semânticas

#### Currency e Amount

```abap
define view entity ZI_SalesOrder
  as select from vbak
{
  key vbeln as SalesOrderId,
      
      // Valor monetário
      @Semantics.amount.currencyCode: 'Currency'
      netwr as NetValue,
      
      // Código de moeda
      @Semantics.currencyCode: true
      waers as Currency
}
```

#### Quantity e Unit

```abap
define view entity ZI_Product
  as select from mara
{
  key matnr as ProductId,
      
      // Quantidade
      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      ntgew as NetWeight,
      
      // Unidade de medida
      @Semantics.unitOfMeasure: true
      meins as BaseUnit
}
```

#### Textos e Datas

```abap
define view entity ZI_Customer
  as select from kna1
{
  key kunnr as CustomerId,
      
      // Texto descritivo
      @Semantics.text: true
      name1 as CustomerName,
      
      // Nome comercial
      @Semantics.name.fullName: true
      name1 as FullName,
      
      // Endereço
      @Semantics.address.street: true
      stras as Street,
      
      @Semantics.address.city: true
      ort01 as City,
      
      @Semantics.address.zipCode: true
      pstlz as PostalCode
}
```

### 4. Anotações de Analytics

#### Analytics Annotations

```abap
@Analytics.dataCategory: #CUBE
@Analytics.dataExtraction.enabled: true

define view entity ZI_SalesAnalytics
  as select from vbak as SalesOrder
  
  association [0..1] to ZI_Customer as _Customer
    on $projection.CustomerId = _Customer.CustomerId
{
  // Dimensões
  @Analytics.dimension: true
  key SalesOrder.vbeln as SalesOrderId,
  
  @Analytics.dimension: true
  SalesOrder.kunnr as CustomerId,
  
  @Analytics.dimension: true
  SalesOrder.erdat as CreationDate,
  
  // Medidas
  @Analytics.measure: true
  @Aggregation.default: #SUM
  @Semantics.amount.currencyCode: 'Currency'
  SalesOrder.netwr as NetValue,
  
  @Semantics.currencyCode: true
  SalesOrder.waers as Currency,
  
  _Customer
}
```

## Metadata Extensions

Metadata Extensions permitem separar anotações de UI da definição da view, promovendo reutilização e manutenibilidade.

### Ativar Metadata Extension na View

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Data'
@Metadata.allowExtensions: true          // Habilitar extensões

define view entity ZI_Customer
  as select from kna1
{
  key kunnr as CustomerId,
      name1 as CustomerName,
      land1 as Country,
      ort01 as City
}
```

### Criar Metadata Extension

```abap
@Metadata.layer: #CUSTOMER

annotate view ZI_Customer with
{
  // Anotações de Header
  @UI.headerInfo: {
    typeName: 'Customer',
    typeNamePlural: 'Customers',
    title: { value: 'CustomerName' },
    description: { value: 'CustomerId' }
  }
  CustomerId;
  
  // Anotações de Campos
  @UI: {
    lineItem: [{ position: 10, importance: #HIGH }],
    identification: [{ position: 10 }],
    selectionField: [{ position: 10 }]
  }
  @EndUserText.label: 'Customer Number'
  CustomerId;
  
  @UI: {
    lineItem: [{ position: 20, importance: #HIGH }],
    identification: [{ position: 20 }],
    selectionField: [{ position: 20 }]
  }
  @EndUserText.label: 'Name'
  CustomerName;
  
  @UI: {
    lineItem: [{ position: 30 }],
    identification: [{ position: 30 }]
  }
  @EndUserText.label: 'Country'
  Country;
  
  @UI: {
    lineItem: [{ position: 40 }],
    identification: [{ position: 40 }]
  }
  @EndUserText.label: 'City'
  City;
}
```

### Layers de Metadata

```abap
// Diferentes camadas de extensão
@Metadata.layer: #CORE          // Nível mais baixo (SAP)
@Metadata.layer: #LOCALIZATION  // Localização
@Metadata.layer: #INDUSTRY      // Indústria específica
@Metadata.layer: #PARTNER       // Partner
@Metadata.layer: #CUSTOMER      // Cliente (mais alta prioridade)
```

## Anotações para OData

### Anotações OData Básicas

```abap
@OData.publish: true
@OData.entitySet.name: 'Customers'
@OData.entityType.name: 'Customer'

define view entity ZC_Customer
  as select from ZI_Customer
{
  @OData.property.name: 'CustomerID'
  key CustomerId,
  
  @OData.property.name: 'Name'
  CustomerName,
  
  @OData.property.name: 'CountryCode'
  Country
}
```

### Anotações de Navegação

```abap
define view entity ZC_SalesOrder
  as select from ZI_SalesOrder
{
  key SalesOrderId,
      CustomerId,
      
      // Exposição de associação para OData
      @OData.association.navigation: true
      _Customer,
      
      @OData.association.navigation: true
      _Items
}
```

## Exemplo Completo: View com Anotações Completas

### View Base

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order'
@Metadata.allowExtensions: true

define view entity ZI_SalesOrder
  as select from vbak as SalesOrder
  
  association [0..1] to ZI_Customer as _Customer
    on $projection.CustomerId = _Customer.CustomerId
  
  association [0..*] to ZI_SalesOrderItem as _Items
    on $projection.SalesOrderId = _Items.SalesOrderId
{
  key SalesOrder.vbeln as SalesOrderId,
      
      SalesOrder.erdat as CreationDate,
      
      SalesOrder.erzet as CreationTime,
      
      SalesOrder.ernam as CreatedBy,
      
      SalesOrder.kunnr as CustomerId,
      
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr as NetValue,
      
      @Semantics.currencyCode: true
      SalesOrder.waers as Currency,
      
      SalesOrder.vkorg as SalesOrganization,
      
      SalesOrder.vtweg as DistributionChannel,
      
      SalesOrder.spart as Division,
      
      // Associações
      _Customer,
      _Items
}
```

### Metadata Extension

```abap
@Metadata.layer: #CUSTOMER
@UI: {
  headerInfo: {
    typeName: 'Sales Order',
    typeNamePlural: 'Sales Orders',
    title: { value: 'SalesOrderId' },
    description: { value: 'CustomerName' }
  },
  presentationVariant: [{
    sortOrder: [{ by: 'CreationDate', direction: #DESC }],
    visualizations: [{type: #AS_LINEITEM}]
  }]
}

annotate view ZI_SalesOrder with
{
  @UI.facet: [
    {
      id: 'GeneralInfo',
      type: #COLLECTION,
      label: 'General Information',
      position: 10
    },
    {
      id: 'BasicData',
      parentId: 'GeneralInfo',
      type: #FIELDGROUP_REFERENCE,
      label: 'Basic Data',
      targetQualifier: 'BasicData',
      position: 10
    },
    {
      id: 'Amounts',
      parentId: 'GeneralInfo',
      type: #FIELDGROUP_REFERENCE,
      label: 'Amounts',
      targetQualifier: 'Amounts',
      position: 20
    },
    {
      id: 'Items',
      type: #LINEITEM_REFERENCE,
      label: 'Items',
      position: 20,
      targetElement: '_Items'
    }
  ]
  SalesOrderId;
  
  @UI: {
    lineItem: [{ position: 10, importance: #HIGH }],
    identification: [{ position: 10 }],
    selectionField: [{ position: 10 }]
  }
  @EndUserText.label: 'Sales Order'
  SalesOrderId;
  
  @UI: {
    lineItem: [{ position: 20, importance: #HIGH }],
    fieldGroup: [{ qualifier: 'BasicData', position: 10 }]
  }
  @EndUserText.label: 'Creation Date'
  CreationDate;
  
  @UI: {
    fieldGroup: [{ qualifier: 'BasicData', position: 20 }]
  }
  @EndUserText.label: 'Creation Time'
  CreationTime;
  
  @UI: {
    fieldGroup: [{ qualifier: 'BasicData', position: 30 }]
  }
  @EndUserText.label: 'Created By'
  CreatedBy;
  
  @UI: {
    lineItem: [{ position: 30, importance: #HIGH }],
    identification: [{ position: 20 }],
    selectionField: [{ position: 20 }]
  }
  @EndUserText.label: 'Customer'
  CustomerId;
  
  @UI: {
    lineItem: [{ position: 40, importance: #HIGH }],
    fieldGroup: [{ qualifier: 'Amounts', position: 10 }]
  }
  @EndUserText.label: 'Net Value'
  NetValue;
  
  @UI: {
    fieldGroup: [{ qualifier: 'Amounts', position: 20 }]
  }
  @EndUserText.label: 'Currency'
  Currency;
  
  @UI: {
    fieldGroup: [{ qualifier: 'BasicData', position: 40 }]
  }
  @EndUserText.label: 'Sales Organization'
  SalesOrganization;
}
```

## Anotações de Search

### Search Help

```abap
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.8

define view entity ZI_Customer
  as select from kna1
{
  @Search.defaultSearchElement: true
  @Search.ranking: #HIGH
  key kunnr as CustomerId,
  
  @Search.defaultSearchElement: true
  @Search.ranking: #HIGH
  name1 as CustomerName,
  
  @Search.defaultSearchElement: true
  @Search.ranking: #MEDIUM
  ort01 as City
}
```

## Anotações de Validação

### Value Help

```abap
define view entity ZI_SalesOrder
  as select from vbak
{
  key vbeln as SalesOrderId,
  
  @Consumption.valueHelpDefinition: [{
    entity: {
      name: 'ZI_Customer',
      element: 'CustomerId'
    }
  }]
  kunnr as CustomerId,
  
  @Consumption.valueHelpDefinition: [{
    entity: {
      name: 'ZI_Currency',
      element: 'CurrencyCode'
    }
  }]
  waers as Currency
}
```

### Filtros de Seleção

```abap
@Consumption.filter.mandatory: true
@Consumption.filter.selectionType: #SINGLE
@Consumption.filter.defaultValue: 'DE'

define view entity ZI_Customer
  as select from kna1
{
  key kunnr as CustomerId,
      name1 as CustomerName,
      land1 as Country
}
```

## Exercícios Práticos

### Exercício 1: Anotações Básicas
Adicione anotações semânticas completas a uma view de produtos:
- Quantidade com unidade
- Peso com unidade
- Textos descritivos

### Exercício 2: Metadata Extension
Crie uma Metadata Extension para uma view de pedidos com:
- Header Info
- Facets (abas)
- LineItem
- FieldGroups

### Exercício 3: Analytics
Configure uma view para analytics com dimensões e medidas.

---

!!! success "Próximos Passos"
    Na próxima página, aprenderemos sobre **Funções e Expressões em CDS**, incluindo cálculos, agregações e lógica condicional.