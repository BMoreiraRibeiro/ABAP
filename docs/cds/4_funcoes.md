# CDS - Funções e Expressões

## Expressões Aritméticas

### Operações Básicas

```abap
define view entity ZI_ProductCalculations
  as select from mara as Product
{
  key Product.matnr as ProductId,
      
      // Adição
      Product.ntgew + Product.brgew as TotalWeight,
      
      // Subtração
      Product.brgew - Product.ntgew as TareWeight,
      
      // Multiplicação
      Product.ntgew * 1000 as WeightInGrams,
      
      // Divisão
      Product.ntgew / Product.volum as Density,
      
      // Parênteses para prioridade
      (Product.ntgew + Product.brgew) / 2 as AverageWeight
}
```

### Cálculos com Moeda/Quantidade

```abap
define view entity ZI_SalesOrderCalculations
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr as NetValue,
      
      // Cálculo de desconto (10%)
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr * 0.10 as DiscountAmount,
      
      // Valor com desconto
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr * 0.90 as NetAfterDiscount,
      
      // IVA (23%)
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr * 0.23 as VATAmount,
      
      // Total com IVA
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr * 1.23 as TotalWithVAT,
      
      @Semantics.currencyCode: true
      SalesOrder.waers as Currency
}
```

## Funções de Texto

### Concatenação

```abap
define view entity ZI_CustomerFullData
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      
      // Concatenação simples
      concat(Customer.name1, Customer.name2) as FullName,
      
      // Concatenação com separador
      concat_with_space(Customer.name1, Customer.name2, 1) as FullNameWithSpace,
      
      // Concatenação múltipla
      concat(concat(Customer.stras, ', '), Customer.ort01) as FullAddress,
      
      // Endereço completo formatado
      concat_with_space(
        concat_with_space(Customer.stras, Customer.ort01, 1),
        Customer.pstlz,
        1
      ) as CompleteAddress
}
```

### Manipulação de Strings

```abap
define view entity ZI_StringFunctions
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      
      // Maiúsculas
      upper(Customer.name1) as NameUpperCase,
      
      // Minúsculas
      lower(Customer.name1) as NameLowerCase,
      
      // Comprimento
      length(Customer.name1) as NameLength,
      
      // Substring (posição, comprimento)
      substring(Customer.name1, 1, 10) as NameFirst10Chars,
      
      // Left (primeiros N caracteres)
      left(Customer.name1, 5) as NameFirst5,
      
      // Right (últimos N caracteres)
      right(Customer.name1, 5) as NameLast5,
      
      // Trim (remover espaços)
      ltrim(Customer.name1, ' ') as NameLeftTrim,
      rtrim(Customer.name1, ' ') as NameRightTrim
}
```

### Replace e InStr

```abap
define view entity ZI_StringReplace
  as select from mara as Product
{
  key Product.matnr as ProductId,
      
      // Substituir caracteres
      replace(Product.matnr, '-', '_') as ProductIdUnderscore,
      
      // Posição de substring
      instr(Product.maktx, 'PREMIUM') as PremiumPosition,
      
      // Limpar caracteres especiais
      replace(
        replace(Product.maktx, '/', '-'),
        '\\', '-'
      ) as CleanDescription
}
```

## Funções de Data e Hora

### Operações com Datas

```abap
define view entity ZI_DateFunctions
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      
      // Data de criação
      SalesOrder.erdat as CreationDate,
      
      // Adicionar dias
      dats_add_days(SalesOrder.erdat, 30, 'INITIAL') as DueDatePlus30,
      
      // Adicionar meses
      dats_add_months(SalesOrder.erdat, 3, 'INITIAL') as DueDatePlus3Months,
      
      // Diferença em dias
      dats_days_between(SalesOrder.erdat, $session.system_date) as DaysSinceCreation,
      
      // Ano, mês, dia
      cast(substring(SalesOrder.erdat, 1, 4) as abap.numc(4)) as Year,
      cast(substring(SalesOrder.erdat, 5, 2) as abap.numc(2)) as Month,
      cast(substring(SalesOrder.erdat, 7, 2) as abap.numc(2)) as Day
}
```

### Timestamp

```abap
define view entity ZI_TimestampFunctions
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      
      // Data e hora atuais
      $session.system_date as CurrentDate,
      cast($session.system_date as abap.dats) as CurrentDateCast,
      
      // Timestamp
      tstmp_current_utctimestamp() as CurrentTimestamp,
      
      // Adicionar segundos ao timestamp
      tstmp_add_seconds(
        tstmp_current_utctimestamp(),
        cast(3600 as abap.dec(15,0)),
        'INITIAL'
      ) as TimestampPlus1Hour
}
```

## Expressões CASE

### CASE Simples

```abap
define view entity ZI_CustomerCategory
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      Customer.name1 as CustomerName,
      Customer.land1 as Country,
      
      // Categorização por país
      case Customer.land1
        when 'DE' then 'Germany'
        when 'FR' then 'France'
        when 'ES' then 'Spain'
        when 'PT' then 'Portugal'
        else 'Other'
      end as CountryName,
      
      // Região
      case Customer.land1
        when 'DE' then 'Central Europe'
        when 'FR' then 'Western Europe'
        when 'ES' then 'Southern Europe'
        when 'PT' then 'Southern Europe'
        when 'IT' then 'Southern Europe'
        else 'Other Region'
      end as Region
}
```

### CASE com Condições

```abap
define view entity ZI_SalesOrderStatus
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr as NetValue,
      SalesOrder.waers as Currency,
      
      // Status baseado em valor
      case
        when SalesOrder.netwr >= 100000 then 'VIP'
        when SalesOrder.netwr >= 50000 then 'High Value'
        when SalesOrder.netwr >= 10000 then 'Medium Value'
        else 'Standard'
      end as OrderCategory,
      
      // Prioridade
      case
        when SalesOrder.netwr >= 100000 then 1
        when SalesOrder.netwr >= 50000 then 2
        when SalesOrder.netwr >= 10000 then 3
        else 4
      end as Priority,
      
      // Dias desde criação
      case
        when dats_days_between(SalesOrder.erdat, $session.system_date) > 365
          then 'Old'
        when dats_days_between(SalesOrder.erdat, $session.system_date) > 180
          then 'Medium'
        when dats_days_between(SalesOrder.erdat, $session.system_date) > 30
          then 'Recent'
        else 'New'
      end as OrderAge
}
```

## Funções de Conversão

### CAST (Conversão de Tipo)

```abap
define view entity ZI_TypeConversions
  as select from vbak as SalesOrder
{
  key SalesOrder.vbeln as SalesOrderId,
      
      // String para número
      cast(SalesOrder.vbeln as abap.int4) as OrderNumber,
      
      // Número para decimal
      cast(SalesOrder.netwr as abap.dec(15,2)) as NetValueDecimal,
      
      // String para data
      cast(SalesOrder.erdat as abap.dats) as CreationDateFormatted,
      
      // Conversão de moeda (tipo)
      cast(SalesOrder.netwr as abap.curr(15,2)) as NetValueCurrency
}
```

### Conversão de Unidades

```abap
define view entity ZI_UnitConversions
  as select from mara as Product
{
  key Product.matnr as ProductId,
      
      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      Product.ntgew as NetWeight,
      
      // Conversão para gramas (assumindo KG)
      @Semantics.quantity.unitOfMeasure: 'WeightUnitGrams'
      Product.ntgew * 1000 as NetWeightGrams,
      
      'G' as WeightUnitGrams,
      
      @Semantics.unitOfMeasure: true
      Product.meins as BaseUnit,
      
      // Volume em litros para mililitros
      @Semantics.quantity.unitOfMeasure: 'VolumeUnitML'
      Product.volum * 1000 as VolumeML,
      
      'ML' as VolumeUnitML
}
```

## Funções de Agregação

### Agregações Básicas

```abap
define view entity ZI_CustomerOrderStats
  as select from vbak as SalesOrder
{
  key SalesOrder.kunnr as CustomerId,
      
      // Contagem
      count(*) as TotalOrders,
      
      // Soma
      @Semantics.amount.currencyCode: 'Currency'
      sum(SalesOrder.netwr) as TotalRevenue,
      
      // Média
      @Semantics.amount.currencyCode: 'Currency'
      avg(SalesOrder.netwr as abap.dec(15,2)) as AverageOrderValue,
      
      // Mínimo
      @Semantics.amount.currencyCode: 'Currency'
      min(SalesOrder.netwr) as MinOrderValue,
      
      // Máximo
      @Semantics.amount.currencyCode: 'Currency'
      max(SalesOrder.netwr) as MaxOrderValue,
      
      SalesOrder.waers as Currency
}
group by
  SalesOrder.kunnr,
  SalesOrder.waers
```

### Agregações com HAVING

```abap
define view entity ZI_HighValueCustomers
  as select from vbak as SalesOrder
{
  key SalesOrder.kunnr as CustomerId,
      
      count(*) as OrderCount,
      
      @Semantics.amount.currencyCode: 'Currency'
      sum(SalesOrder.netwr) as TotalRevenue,
      
      SalesOrder.waers as Currency
}
group by
  SalesOrder.kunnr,
  SalesOrder.waers
having
  sum(SalesOrder.netwr) > 100000
```

## Funções Matemáticas

### Funções Avançadas

```abap
define view entity ZI_MathFunctions
  as select from mara as Product
{
  key Product.matnr as ProductId,
      
      // Valor absoluto
      abs(Product.ntgew - Product.brgew) as WeightDifference,
      
      // Arredondamento
      round(Product.ntgew, 2) as NetWeightRounded,
      
      // Teto (arredondar para cima)
      ceil(Product.volum) as VolumeCeiling,
      
      // Chão (arredondar para baixo)
      floor(Product.volum) as VolumeFloor,
      
      // Divisão com resto
      division(cast(Product.ntgew as abap.int4), 10, 2) as WeightDivided,
      
      // Módulo
      mod(cast(Product.ntgew as abap.int4), 10) as WeightModulo
}
```

## COALESCE e NULLIF

### Tratamento de Valores Nulos

```abap
define view entity ZI_NullHandling
  as select from kna1 as Customer
{
  key Customer.kunnr as CustomerId,
      
      // Usar valor padrão se nulo
      coalesce(Customer.name2, Customer.name1) as DisplayName,
      
      coalesce(Customer.telf1, 'No Phone') as PhoneNumber,
      
      // Retornar null se valores são iguais
      nullif(Customer.name1, Customer.name2) as Name1IfDifferent,
      
      // Combinação
      coalesce(
        nullif(Customer.name2, ''),
        Customer.name1,
        'Unknown'
      ) as BestName
}
```

## Exemplo Completo: Dashboard de Vendas

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Dashboard'
define view entity ZI_SalesDashboard
  as select from vbak as SalesOrder
  
  association [0..1] to ZI_Customer as _Customer
    on $projection.CustomerId = _Customer.CustomerId
{
  key SalesOrder.vbeln as SalesOrderId,
      
      SalesOrder.kunnr as CustomerId,
      
      SalesOrder.erdat as CreationDate,
      
      // Ano e Mês
      cast(substring(SalesOrder.erdat, 1, 4) as abap.numc(4)) as Year,
      cast(substring(SalesOrder.erdat, 5, 2) as abap.numc(2)) as Month,
      
      // Quarter
      case cast(substring(SalesOrder.erdat, 5, 2) as abap.numc(2))
        when 1 then 'Q1'
        when 2 then 'Q1'
        when 3 then 'Q1'
        when 4 then 'Q2'
        when 5 then 'Q2'
        when 6 then 'Q2'
        when 7 then 'Q3'
        when 8 then 'Q3'
        when 9 then 'Q3'
        when 10 then 'Q4'
        when 11 then 'Q4'
        when 12 then 'Q4'
        else 'Unknown'
      end as Quarter,
      
      // Valores
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr as NetValue,
      
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr * 0.23 as VATAmount,
      
      @Semantics.amount.currencyCode: 'Currency'
      SalesOrder.netwr * 1.23 as TotalValue,
      
      SalesOrder.waers as Currency,
      
      // Categorias
      case
        when SalesOrder.netwr >= 100000 then 'VIP'
        when SalesOrder.netwr >= 50000 then 'Premium'
        when SalesOrder.netwr >= 10000 then 'Standard'
        else 'Basic'
      end as CustomerSegment,
      
      // Dias desde criação
      dats_days_between(SalesOrder.erdat, $session.system_date) as DaysSinceCreation,
      
      // Status
      case
        when dats_days_between(SalesOrder.erdat, $session.system_date) <= 7
          then 'New'
        when dats_days_between(SalesOrder.erdat, $session.system_date) <= 30
          then 'Recent'
        when dats_days_between(SalesOrder.erdat, $session.system_date) <= 90
          then 'Active'
        else 'Old'
      end as OrderStatus,
      
      // Nome do cliente via associação
      _Customer.CustomerName,
      _Customer.Country,
      
      _Customer
}
```

## Exercícios Práticos

### Exercício 1: Cálculos Complexos
Crie uma view que calcule:
- Margem de lucro (diferença entre preço de venda e custo)
- Percentagem de desconto
- Valor com IVA de 23%

### Exercício 2: Manipulação de Textos
Crie uma view que:
- Concatene nome e sobrenome
- Formate endereço completo
- Extraia código postal dos primeiros 4 dígitos

### Exercício 3: Agregações
Crie uma view agregada com:
- Total de vendas por cliente
- Média de valor de pedido
- Número de pedidos por mês

---

!!! success "Próximos Passos"
    Na próxima página, exploraremos **Parâmetros, Filtros e Views Parametrizadas**, incluindo como criar views dinâmicas e reutilizáveis.