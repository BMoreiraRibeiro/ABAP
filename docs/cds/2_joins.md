# CDS - Associações e Joins

## Diferença entre Associações e Joins

### Joins (Clássicos)
- Executados imediatamente
- Todos os dados são carregados
- Definidos na cláusula `FROM`

### Associações (On-Demand)
- Executados apenas quando necessário
- Dados carregados sob demanda
- Reutilizáveis em outras views
- **Recomendado para CDS moderno**

## Associações em CDS

### Sintaxe Básica

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order with Customer'
define view entity ZI_SalesOrder
  as select from vbak as SalesOrder
  
  // Definição da associação
  association [0..1] to ZI_Customer as _Customer
    on $projection.CustomerId = _Customer.CustomerId
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.kunnr as CustomerId,
      SalesOrder.netwr as NetValue,
      
      // Exposição da associação
      _Customer
}
```

### Cardinalidades

```abap
// [min..max] ou [0..*]

association [1]      to ZI_Customer    as _Customer    // Exatamente 1
association [0..1]   to ZI_Address     as _Address     // Zero ou 1
association [1..*]   to ZI_Item        as _Items       // Um ou mais
association [0..*]   to ZI_Document    as _Documents   // Zero ou mais
```

## Consumindo Associações

### Em Consulta ABAP

```abap
SELECT FROM ZI_SalesOrder
  FIELDS SalesOrderId,
         CreationDate,
         CustomerId,
         \_Customer-CustomerName,           " Acesso via associação
         \_Customer-Country,
         \_Customer-City
  WHERE CreationDate >= @sy-datum
  INTO TABLE @DATA(lt_orders).

LOOP AT lt_orders INTO DATA(ls_order).
  WRITE: / ls_order-SalesOrderId,
           ls_order-CustomerName,
           ls_order-Country.
ENDLOOP.
```

### Em Outra CDS View

```abap
define view entity ZC_SalesOrderDisplay
  as select from ZI_SalesOrder
{
  key SalesOrderId,
      CreationDate,
      CustomerId,
      NetValue,
      
      // Projeção de campos da associação
      _Customer.CustomerName,
      _Customer.Country,
      _Customer.City,
      
      // Manter associação para uso posterior
      _Customer
}
```

## Path Expressions (Navegação em Cadeia)

### Múltiplos Níveis de Associação

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Item with Details'
define view entity ZI_SalesOrderItem
  as select from vbap as Item
  
  association [1] to ZI_SalesOrder as _SalesOrder
    on $projection.SalesOrderId = _SalesOrder.SalesOrderId
{
  key Item.vbeln as SalesOrderId,
  key Item.posnr as ItemNumber,
      Item.matnr as ProductId,
      Item.kwmeng as Quantity,
      
      // Navegação em cadeia: Item → Order → Customer
      _SalesOrder._Customer.CustomerName as CustomerName,
      _SalesOrder._Customer.Country as CustomerCountry,
      
      // Exposição das associações
      _SalesOrder
}
```

### Exemplo Completo de Path Expression

```abap
define view entity ZI_InvoiceItem
  as select from bseg as InvoiceItem
  
  association [1] to ZI_Invoice  as _Invoice
    on $projection.DocumentNumber = _Invoice.DocumentNumber
{
  key InvoiceItem.belnr as DocumentNumber,
  key InvoiceItem.buzei as LineItem,
      InvoiceItem.wrbtr as Amount,
      
      // Caminho: Item → Invoice → Customer → Country
      _Invoice._Customer._Country.CountryName as CountryName,
      _Invoice._Customer._Country.Region as Region,
      
      _Invoice
}
```

## Joins Explícitos

### Inner Join

```abap
define view entity ZI_CustomerOrders
  as select from kna1 as Customer
  
  inner join vbak as SalesOrder
    on Customer.kunnr = SalesOrder.kunnr
{
  key Customer.kunnr       as CustomerId,
      Customer.name1       as CustomerName,
      SalesOrder.vbeln     as SalesOrderId,
      SalesOrder.erdat     as OrderDate,
      SalesOrder.netwr     as NetValue
}
```

### Left Outer Join

```abap
define view entity ZI_CustomerWithOrders
  as select from kna1 as Customer
  
  left outer join vbak as SalesOrder
    on Customer.kunnr = SalesOrder.kunnr
{
  key Customer.kunnr       as CustomerId,
      Customer.name1       as CustomerName,
      SalesOrder.vbeln     as SalesOrderId,
      SalesOrder.erdat     as OrderDate,
      SalesOrder.netwr     as NetValue
}
```

!!! warning "Performance"
    Joins são executados imediatamente. Use associações sempre que possível para melhor performance, especialmente com HANA.

## Comparação: Join vs Associação

### Exemplo com Join

```abap
define view entity ZI_Order_WithJoin
  as select from vbak as SalesOrder
  
  inner join kna1 as Customer
    on SalesOrder.kunnr = Customer.kunnr
  
  inner join vbap as Item
    on SalesOrder.vbeln = Item.vbeln
{
  key SalesOrder.vbeln as SalesOrderId,
  key Item.posnr       as ItemNumber,
      Customer.name1   as CustomerName,
      Item.matnr       as ProductId
}
```

### Mesmo Exemplo com Associação (Recomendado)

```abap
define view entity ZI_Order_WithAssoc
  as select from vbak as SalesOrder
  
  association [0..1] to ZI_Customer as _Customer
    on $projection.CustomerId = _Customer.CustomerId
  
  association [0..*] to ZI_Item as _Items
    on $projection.SalesOrderId = _Items.SalesOrderId
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.kunnr as CustomerId,
      
      _Customer,
      _Items
}

// View de consumo que projeta os dados necessários
define view entity ZC_OrderDisplay
  as select from ZI_Order_WithAssoc
{
  key SalesOrderId,
      CustomerId,
      _Customer.CustomerName,
      _Items.ItemNumber,
      _Items.ProductId
}
```

## Associações com Filtros

### Filtro na Associação

```abap
define view entity ZI_SalesOrder
  as select from vbak as SalesOrder
  
  association [0..*] to vbap as _Items
    on  $projection.SalesOrderId = _Items.vbeln
    and _Items.abgru = ''                          // Apenas itens não rejeitados
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.kunnr as CustomerId,
      
      _Items
}
```

### Associação com Parâmetros

```abap
define view entity ZI_SalesOrderFiltered
  as select from vbak as SalesOrder
  
  association [0..*] to ZI_Item as _Items
    on  $projection.SalesOrderId = _Items.SalesOrderId
    and _Items.Quantity > 0
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.kunnr as CustomerId,
      SalesOrder.netwr as NetValue,
      
      _Items
}
```

## Associações para Textos

### View de Texto

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product Text'
define view entity ZI_ProductText
  as select from makt
{
  key matnr as ProductId,
  key spras as Language,
      maktx as ProductDescription
}
```

### View Principal com Associação de Texto

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product with Text'
define view entity ZI_Product
  as select from mara as Product
  
  association [0..*] to ZI_ProductText as _Text
    on $projection.ProductId = _Text.ProductId
{
  key Product.matnr as ProductId,
      Product.meins as BaseUnit,
      Product.ntgew as NetWeight,
      
      // Texto na língua de logon
      _Text[1: Language = $session.system_language].ProductDescription,
      
      _Text
}
```

## Exemplo Completo: Hierarquia de Views

### 1. View de Interface (Customer)

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Interface'
define view entity ZI_Customer
  as select from kna1
  
  association [0..1] to ZI_Country as _Country
    on $projection.Country = _Country.CountryCode
{
  key kunnr as CustomerId,
      name1 as CustomerName,
      land1 as Country,
      ort01 as City,
      
      _Country
}
```

### 2. View de Interface (Sales Order)

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Interface'
define view entity ZI_SalesOrder
  as select from vbak
  
  association [0..1] to ZI_Customer as _Customer
    on $projection.CustomerId = _Customer.CustomerId
  
  association [0..*] to ZI_SalesOrderItem as _Items
    on $projection.SalesOrderId = _Items.SalesOrderId
{
  key vbeln as SalesOrderId,
      erdat as CreationDate,
      kunnr as CustomerId,
      
      @Semantics.amount.currencyCode: 'Currency'
      netwr as NetValue,
      
      waers as Currency,
      
      _Customer,
      _Items
}
```

### 3. View de Consumo

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Consumption'
@Metadata.allowExtensions: true
define view entity ZC_SalesOrder
  as select from ZI_SalesOrder
{
  key SalesOrderId,
      CreationDate,
      CustomerId,
      
      _Customer.CustomerName,
      _Customer.Country,
      _Customer._Country.CountryName,
      
      NetValue,
      Currency,
      
      _Customer,
      _Items
}
```

## Exercícios Práticos

### Exercício 1: Associação Simples
Crie uma view ZI_Material com associação para ZI_MaterialText.

### Exercício 2: Path Expression
Crie uma view que mostre itens de pedido com o nome do país do cliente usando path expression.

### Exercício 3: Comparação
Implemente a mesma lógica usando:
1. Join explícito
2. Associação
Compare a performance com Data Preview.

---

!!! success "Próximos Passos"
    Na próxima página, exploraremos **Anotações e Metadata Extensions** para enriquecer as CDS Views com informações semânticas.