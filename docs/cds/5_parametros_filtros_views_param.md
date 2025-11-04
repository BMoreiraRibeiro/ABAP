# CDS - Parâmetros, Filtros e Views Parametrizadas

## Parâmetros em CDS Views

Parâmetros permitem criar views dinâmicas que filtram ou calculam dados baseados em valores fornecidos em runtime.

### Sintaxe Básica

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer by Country'
define view entity ZI_CustomerByCountry
  with parameters
    P_Country : land1
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      Customer.name1 as CustomerName,
      Customer.land1 as Country,
      Customer.ort01 as City
}
where
  Customer.land1 = $parameters.P_Country
```

### Consumindo View com Parâmetros

```abap
" Consulta ABAP
SELECT FROM ZI_CustomerByCountry( P_Country = 'DE' )
  FIELDS CustomerId,
         CustomerName,
         City
  INTO TABLE @DATA(lt_customers).

LOOP AT lt_customers INTO DATA(ls_customer).
  WRITE: / ls_customer-CustomerId,
           ls_customer-CustomerName,
           ls_customer-City.
ENDLOOP.
```

## Múltiplos Parâmetros

### View com Vários Parâmetros

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Orders by Date Range'
define view entity ZI_SalesOrderByDateRange
  with parameters
    P_DateFrom : abap.dats,
    P_DateTo   : abap.dats,
    P_Currency : waers
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.kunnr as CustomerId,
      
      @Semantics.currencyCode: true
  SalesOrder.waers as Currency,
  
  // Associação
  _Customer
}
where
      SalesOrder.erdat >= $parameters.P_DateFrom
  and SalesOrder.erdat <= $parameters.P_DateTo
  and SalesOrder.waers = $parameters.P_Currency
  and _Customer.Country = $parameters.P_Country
```

### Consumo do Dashboard

```abap
" ABAP Report
REPORT z_sales_dashboard.

PARAMETERS: p_from TYPE dats OBLIGATORY,
            p_to   TYPE dats OBLIGATORY,
            p_curr TYPE waers DEFAULT 'EUR',
            p_land TYPE land1 DEFAULT 'PT'.

START-OF-SELECTION.

  SELECT FROM ZI_SalesDashboardParam(
                P_DateFrom = @p_from,
                P_DateTo   = @p_to,
                P_Currency = @p_curr,
                P_Country  = @p_land )
    FIELDS Year,
           Quarter,
           CustomerSegment,
           NetValue,
           TotalValue
    INTO TABLE @DATA(lt_dashboard).

  " Processar resultados
  LOOP AT lt_dashboard INTO DATA(ls_line).
    WRITE: / ls_line-Year,
             ls_line-Quarter,
             ls_line-CustomerSegment,
             ls_line-NetValue,
             ls_line-TotalValue.
  ENDLOOP.
```

## Performance com Parâmetros

### Push-Down com Parâmetros

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Optimized Parametrized View'
define view entity ZI_OptimizedParamView
  with parameters
    P_SalesOrg : vbak.vkorg
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.netwr as NetValue
}
where
  SalesOrder.vkorg = $parameters.P_SalesOrg  -- Filtro no banco de dados
```

!!! tip "Performance"
    Parâmetros aplicados no WHERE são processados no banco de dados (push-down), melhorando drasticamente a performance.

## Exercícios Práticos

### Exercício 1: View Parametrizada Básica
Crie uma view de materiais filtrada por:
- Tipo de material (obrigatório)
- Data de criação (intervalo)

### Exercício 2: Window Functions
Crie uma view que mostre:
- Top 10 clientes por faturamento
- Ranking por país
- Número sequencial de pedidos por cliente

### Exercício 3: Dashboard Completo
Implemente um dashboard parametrizado com:
- Filtros de data (obrigatórios)
- Filtro de país (opcional)
- Agregações por mês e trimestre
- Categorização de clientes
- Window functions para ranking

### Exercício 4: Union de Dados
Combine dados de clientes (KNA1) e fornecedores (LFA1) em uma única view com:
- ID unificado
- Tipo de parceiro
- Endereço completo

---

## Resumo do Módulo CDS

### Conceitos Fundamentais
✅ CDS View Entities vs CDS Views  
✅ Anotações técnicas e semânticas  
✅ Nomenclatura e boas práticas  

### Associações e Joins
✅ Diferença entre associações e joins  
✅ Cardinalidades  
✅ Path expressions  
✅ Propagação de associações  

### Anotações e Metadata
✅ Anotações UI  
✅ Anotações Analytics  
✅ Metadata Extensions  
✅ Separation of Concerns  

### Funções e Expressões
✅ Funções de texto, data e matemáticas  
✅ CASE expressions  
✅ Agregações  
✅ Type casting  

### Parâmetros e Filtros
✅ Views parametrizadas  
✅ Session variables  
✅ Window functions  
✅ Subqueries e UNION  

---

## Recursos Adicionais

### Documentação SAP
- [CDS Development Guide](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/3fd8761d81f04a4eb44c7c5f62aadd46.html)
- [CDS Annotations Reference](https://help.sap.com/docs/SAP_S4HANA_ON-PREMISE/cc0c305d2fab47bd808adcad3ca7ee9d/630ce9b386b84e80bdfeeb87afcd1ded.html)

### Best Practices
- Use View Entities para novos desenvolvimentos
- Prefira associações sobre joins
- Implemente Metadata Extensions para UI
- Use parâmetros para views reutilizáveis
- Documente suas views com @EndUserText

### Próximos Passos
1. Praticar com exemplos reais
2. Integrar com SAP Fiori
3. Criar serviços OData
4. Implementar autorização (DCL)
5. Otimizar performance com HANA

!!! success "Conclusão"
    Parabéns! Você completou o tutorial completo de CDS. Continue praticando e explorando as capacidades avançadas do Core Data Services.Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr as NetValue,
      
      SalesOrder.waers as Currency
}
where
      SalesOrder.erdat >= $parameters.P_DateFrom
  and SalesOrder.erdat <= $parameters.P_DateTo
  and SalesOrder.waers = $parameters.P_Currency
```

### Uso em ABAP

```abap
DATA: lv_date_from TYPE dats,
      lv_date_to   TYPE dats.

lv_date_from = sy-datum - 30.
lv_date_to   = sy-datum.

SELECT FROM ZI_SalesOrderByDateRange(
              P_DateFrom = @lv_date_from,
              P_DateTo   = @lv_date_to,
              P_Currency = 'EUR' )
  FIELDS SalesOrderId,
         CreationDate,
         NetValue,
         Currency
  INTO TABLE @DATA(lt_orders).
```

## Parâmetros com Valores Padrão

### Definindo Valores Default

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Products with Optional Filter'
define view entity ZI_ProductsFiltered
  with parameters
    P_ProductType : mtart,
    @Environment.systemField: #SYSTEM_DATE
    P_ValidOn     : abap.dats
  as select from mara as Product
{
  key Product.matnr as ProductId,
      Product.mtart as ProductType,
      Product.ersda as CreationDate,
      Product.meins as BaseUnit
}
where
  Product.mtart = $parameters.P_ProductType
```

### System Fields como Parâmetros

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Current Language Texts'
define view entity ZI_ProductText
  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    P_Language : spras
  as select from makt
{
  key matnr as ProductId,
      spras as Language,
      maktx as ProductDescription
}
where
  spras = $parameters.P_Language
```

## Session Variables

### Variáveis de Sistema Disponíveis

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Orders with Session Info'
define view entity ZI_OrdersWithSessionInfo
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.kunnr as CustomerId,
      
      // Data atual do sistema
      $session.system_date as CurrentDate,
      
      // Língua de logon
      $session.system_language as SystemLanguage,
      
      // Cliente (mandante)
      $session.client as Client,
      
      // Usuário
      $session.user as CurrentUser,
      
      // Dias desde criação
      dats_days_between(
        SalesOrder.erdat,
        $session.system_date
      ) as DaysSinceCreation
}
```

### Filtro por Data Atual

```abap
define view entity ZI_CurrentMonthOrders
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.netwr as NetValue
}
where
      substring(SalesOrder.erdat, 1, 6) = 
      substring($session.system_date, 1, 6)
```

## Parâmetros em Associações

### Propagação de Parâmetros

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order with Items by Date'
define view entity ZI_SalesOrderFiltered
  with parameters
    P_DateFrom : abap.dats
  as select from vbak as SalesOrder
  
  association [0..*] to ZI_SalesOrderItem as _Items
    on $projection.SalesOrderId = _Items.SalesOrderId
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.kunnr as CustomerId,
      
      _Items
}
where
  SalesOrder.erdat >= $parameters.P_DateFrom
```

### View Consumidora com Parâmetros

```abap
define view entity ZC_SalesOrderWithDetails
  with parameters
    P_DateFrom : abap.dats,
    P_Country  : land1
  as select from ZI_SalesOrderFiltered(
                   P_DateFrom: $parameters.P_DateFrom
                 ) as SalesOrder
  
  association [0..1] to ZI_CustomerByCountry as _Customer
    on  $projection.CustomerId = _Customer.CustomerId
    and _Customer.P_Country = $parameters.P_Country
{
  key SalesOrder.SalesOrderId,
      SalesOrder.CreationDate,
      SalesOrder.CustomerId,
      
      _Customer.CustomerName,
      _Customer.Country,
      
      _Customer,
      _Items
}
```

## UNION e UNION ALL

### Combinar Múltiplas Fontes

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'All Business Partners'
define view entity ZI_AllBusinessPartners
  as select from kna1 as Customer
{
  key kunnr as PartnerId,
      name1 as PartnerName,
      'Customer' as PartnerType,
      land1 as Country
}

union all

select from lfa1 as Vendor
{
  key lifnr as PartnerId,
      name1 as PartnerName,
      'Vendor' as PartnerType,
      land1 as Country
}
```

### UNION com Transformação

```abap
define view entity ZI_CombinedAddresses
  as select from kna1 as Customer
{
  kunnr as PartnerId,
  concat('C-', kunnr) as PartnerCode,
  name1 as Name,
  stras as Street,
  ort01 as City,
  land1 as Country,
  'Customer' as Type
}

union

select from lfa1 as Vendor
{
  lifnr as PartnerId,
  concat('V-', lifnr) as PartnerCode,
  name1 as Name,
  stras as Street,
  ort01 as City,
  land1 as Country,
  'Vendor' as Type
}
```

## WHERE Clause Avançada

### Múltiplas Condições

```abap
define view entity ZI_ComplexFilter
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.kunnr as CustomerId,
      SalesOrder.netwr as NetValue,
      SalesOrder.waers as Currency
}
where
      SalesOrder.erdat >= '20240101'
  and (
        SalesOrder.netwr >= 10000
     or SalesOrder.kunnr in ( '0001000000', '0001000001' )
      )
  and SalesOrder.waers = 'EUR'
```

### Operadores IN e NOT IN

```abap
define view entity ZI_EuropeanCustomers
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      Customer.name1 as CustomerName,
      Customer.land1 as Country,
      Customer.ort01 as City
}
where
  Customer.land1 in ( 'DE', 'FR', 'ES', 'IT', 'PT', 'NL', 'BE' )
  and Customer.loevm <> 'X'  -- Não marcado para eliminação
```

### Operadores LIKE

```abap
define view entity ZI_CustomerNameSearch
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      Customer.name1 as CustomerName,
      Customer.ort01 as City
}
where
      Customer.name1 like '%GMBH%'
   or Customer.name1 like '%LTD%'
   or Customer.name1 like '%SA%'
```

## Subqueries (EXISTS e IN)

### EXISTS

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customers with Orders'
define view entity ZI_CustomersWithOrders
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      Customer.name1 as CustomerName,
      Customer.land1 as Country,
      Customer.ort01 as City
}
where exists (
  select from vbak as SalesOrder
  where SalesOrder.kunnr = Customer.kunnr
    and SalesOrder.erdat >= '20240101'
)
```

### NOT EXISTS

```abap
define view entity ZI_CustomersWithoutOrders
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      Customer.name1 as CustomerName,
      Customer.land1 as Country
}
where not exists (
  select from vbak as SalesOrder
  where SalesOrder.kunnr = Customer.kunnr
)
```

### IN com Subquery

```abap
define view entity ZI_HighValueCustomers
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      Customer.name1 as CustomerName,
      Customer.land1 as Country
}
where Customer.kunnr in (
  select SalesOrder.kunnr
  from vbak as SalesOrder
  where SalesOrder.netwr >= 100000
)
```

## Window Functions

### RANK e DENSE_RANK

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Ranking by Revenue'
define view entity ZI_CustomerRanking
  as select from vbak as SalesOrder
{
  key SalesOrder.kunnr as CustomerId,
      
      @Semantics.amount.currencyCode: 'Currency'
      sum(SalesOrder.netwr) as TotalRevenue,
      
      SalesOrder.waers as Currency,
      
      // Ranking (valores iguais recebem mesma posição, próximo é saltado)
      rank( ) over(
        partition by SalesOrder.waers
        order by sum(SalesOrder.netwr) descending
      ) as RevenueRank,
      
      // Dense Rank (sem saltar posições)
      dense_rank( ) over(
        partition by SalesOrder.waers
        order by sum(SalesOrder.netwr) descending
      ) as DenseRevenueRank
}
group by
  SalesOrder.kunnr,
  SalesOrder.waers
```

### ROW_NUMBER

```abap
define view entity ZI_OrdersWithRowNumber
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.kunnr as CustomerId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.netwr as NetValue,
      
      // Número sequencial
      row_number( ) over(
        partition by SalesOrder.kunnr
        order by SalesOrder.erdat descending
      ) as OrderSequence
}
```

### Filtrar por Window Function

```abap
define view entity ZI_LatestOrderPerCustomer
  as select from ZI_OrdersWithRowNumber
{
  key SalesOrderId,
      CustomerId,
      CreationDate,
      NetValue
}
where
  OrderSequence = 1  -- Apenas o pedido mais recente
```

## Parâmetros com Validação

### View com Filtro Obrigatório

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order - Mandatory Date'
define view entity ZI_SalesOrderMandatory
  with parameters
    @Consumption.filter.mandatory: true
    P_CreationDate : abap.dats
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.kunnr as CustomerId,
      SalesOrder.netwr as NetValue
}
where
  SalesOrder.erdat = $parameters.P_CreationDate
```

### Parâmetros com Lista de Valores

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Orders by Document Type'
define view entity ZI_OrdersByDocType
  with parameters
    @Consumption.valueHelpDefinition: [{
      entity: {
        name: 'I_SalesDocumentType',
        element: 'SalesDocumentType'
      }
    }]
    P_DocType : vbak.auart
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.auart as DocumentType,
      SalesOrder.erdat as CreationDate,
      SalesOrder.netwr as NetValue
}
where
  SalesOrder.auart = $parameters.P_DocType
```

## Exemplo Completo: Dashboard Parametrizado

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Dashboard - Parametrized'
@Analytics.dataCategory: #CUBE
define view entity ZI_SalesDashboardParam
  with parameters
    @Consumption.filter.mandatory: true
    @Consumption.filter.selectionType: #INTERVAL
    P_DateFrom : abap.dats,
    
    @Consumption.filter.mandatory: true
    @Consumption.filter.selectionType: #INTERVAL
    P_DateTo   : abap.dats,
    
    @Consumption.defaultValue: 'EUR'
    @Consumption.valueHelpDefinition: [{
      entity: {
        name: 'I_Currency',
        element: 'Currency'
      }
    }]
    P_Currency : waers,
    
    @Consumption.filter.selectionType: #SINGLE
    @Consumption.valueHelpDefinition: [{
      entity: {
        name: 'I_Country',
        element: 'Country'
      }
    }]
    P_Country  : land1
    
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
  
  @Analytics.dimension: true
  cast(substring(SalesOrder.erdat, 1, 6) as abap.char(6)) as YearMonth,
  
  @Analytics.dimension: true
  cast(substring(SalesOrder.erdat, 1, 4) as abap.numc(4)) as Year,
  
  @Analytics.dimension: true
  case cast(substring(SalesOrder.erdat, 5, 2) as abap.numc(2))
    when 1 then 'Q1' when 2 then 'Q1' when 3 then 'Q1'
    when 4 then 'Q2' when 5 then 'Q2' when 6 then 'Q2'
    when 7 then 'Q3' when 8 then 'Q3' when 9 then 'Q3'
    when 10 then 'Q4' when 11 then 'Q4' when 12 then 'Q4'
    else 'Unknown'
  end as Quarter,
  
  @Analytics.dimension: true
  _Customer.Country,
  
  @Analytics.dimension: true
  _Customer.City,
  
  @Analytics.dimension: true
  case
    when SalesOrder.netwr >= 100000 then 'VIP'
    when SalesOrder.netwr >= 50000 then 'Premium'
    when SalesOrder.netwr >= 10000 then 'Standard'
    else 'Basic'
  end as CustomerSegment,
  
  // Medidas
  @Analytics.measure: true
  @Aggregation.default: #SUM
  @Semantics.amount.currencyCode: 'Currency'
  SalesOrder.netwr as NetValue,
  
  @Analytics.measure: true
  @Aggregation.default: #SUM
  @Semantics.amount.currencyCode: 'Currency'
  SalesOrder.netwr * 0.23 as VATAmount,
  
  @Analytics.measure: true
  @Aggregation.default: #SUM
  @Semantics.amount.currencyCode: 'Currency'
  SalesOrder.netwr * 1.23 as TotalValue,
  
  @