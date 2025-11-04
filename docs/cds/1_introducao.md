# CDS - Introdução e Conceitos Fundamentais

## O que são Core Data Services (CDS)?

Core Data Services (CDS) é uma linguagem de definição de dados semântica que permite definir modelos de dados diretamente no dicionário ABAP. CDS representa uma evolução significativa no desenvolvimento SAP, permitindo modelagem de dados mais próxima da camada de base de dados com melhor performance.

### Principais Vantagens

- **Performance**: Processamento push-down para HANA
- **Reutilização**: Views podem ser consumidas por outros objetos
- **Anotações**: Metadados semânticos integrados
- **Modularidade**: Composição de views complexas
- **Integração**: Suporte nativo para OData, Fiori, Analytics

## Tipos de CDS Views

### 1. CDS View Entity (Sintaxe Moderna)

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer View'
define view entity ZI_Customer
  as select from kna1
{
  key kunnr as CustomerId,
      name1 as CustomerName,
      land1 as Country,
      ort01 as City,
      @Semantics.amount.currencyCode: 'Currency'
      ktokd as AccountGroup
}
```

### 2. CDS View (Sintaxe Legada)

```abap
@AbapCatalog.sqlViewName: 'ZVCUSTOMER'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer View'
define view Z_Customer
  as select from kna1
{
  key kunnr as CustomerId,
      name1 as CustomerName,
      land1 as Country,
      ort01 as City
}
```

!!! tip "Recomendação"
    Use **View Entities** para novos desenvolvimentos. É a sintaxe moderna e recomendada pela SAP.

## Estrutura de uma CDS View

### Componentes Essenciais

```abap
// 1. Anotações (Metadata)
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order View'
@Metadata.allowExtensions: true

// 2. Definição
define view entity ZI_SalesOrder
  
  // 3. Cláusula AS SELECT
  as select from vbak as SalesOrder
  
  // 4. Associações (Joins reutilizáveis)
  association [0..*] to ZI_SalesOrderItem as _Item
    on $projection.SalesOrderId = _Item.SalesOrderId
  
{
  // 5. Lista de Campos
  key SalesOrder.vbeln as SalesOrderId,
      SalesOrder.erdat as CreationDate,
      SalesOrder.ernam as CreatedBy,
      SalesOrder.kunnr as CustomerId,
      
      // 6. Exposição de Associações
      _Item
}
```

## Anotações Fundamentais

### Anotações Técnicas

```abap
// Controle de autorização
@AccessControl.authorizationCheck: #CHECK
@AccessControl.authorizationCheck: #NOT_REQUIRED

// Label para usuário final
@EndUserText.label: 'Descrição da View'

// Permitir extensões via Metadata Extension
@Metadata.allowExtensions: true

// Ignorar validações (apenas desenvolvimento)
@AbapCatalog.compiler.compareFilter: true
```

### Anotações Semânticas

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
      meins as BaseUnit,
      
      // Moeda
      @Semantics.currencyCode: true
      waers as Currency,
      
      // Texto
      @Semantics.text: true
      maktx as ProductDescription
}
```

## Criação de CDS View no ADT

### Passo a Passo

1. **Criar objeto CDS**
   - Right-click no package → New → Other ABAP Repository Object
   - Core Data Services → Data Definition
   - Nome: `ZI_CUSTOMER`

2. **Template básico**
```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Data'
define view entity ZI_Customer
  as select from kna1
{
  key kunnr as CustomerId,
      name1 as Name,
      land1 as Country
}
```

3. **Ativar** (Ctrl+F3)

4. **Testar** (F8 - Data Preview)

## Nomenclatura e Convenções

### Prefixos Recomendados

| Prefixo | Tipo | Descrição | Exemplo |
|---------|------|-----------|---------|
| `ZI_` | Interface View | View básica, representa entidade | `ZI_Customer` |
| `ZC_` | Consumption View | View para consumo (Fiori, OData) | `ZC_Customer` |
| `ZP_` | Projection View | Projeção de campos específicos | `ZP_CustomerBasic` |
| `ZR_` | Restricted View | View com filtros | `ZR_CustomerGermany` |

### Nomenclatura de Campos

```abap
define view entity ZI_SalesOrder
  as select from vbak
{
  // Use nomes descritivos em CamelCase
  key vbeln as SalesOrderId,        // ✓ Bom
      erdat as CreationDate,         // ✓ Bom
      kunnr as CustomerId,           // ✓ Bom
      
  // Evite abreviações obscuras
  // key vbeln as SO_ID,             // ✗ Evitar
  // erdat as CrDt,                  // ✗ Evitar
}
```

## Exemplo Completo: View de Cliente com Endereço

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer with Address'
define view entity ZI_CustomerAddress
  as select from kna1 as Customer
  
  association [0..1] to ZI_Country as _Country
    on $projection.Country = _Country.CountryCode
{
  key Customer.kunnr as CustomerId,
      
      @EndUserText.label: 'Customer Name'
      Customer.name1 as CustomerName,
      
      @EndUserText.label: 'Street'
      Customer.stras as Street,
      
      @EndUserText.label: 'City'
      Customer.ort01 as City,
      
      @EndUserText.label: 'Postal Code'
      Customer.pstlz as PostalCode,
      
      @EndUserText.label: 'Country'
      Customer.land1 as Country,
      
      // Exposição da associação
      _Country
}
```

## Exercícios Práticos

### Exercício 1: View Básica de Produtos
Crie uma CDS View que mostre produtos da tabela MARA:
- Número do material (MATNR)
- Descrição (MAKTX da MAKT)
- Unidade base (MEINS)
- Peso líquido (NTGEW)

### Exercício 2: View com Anotações
Adicione anotações semânticas apropriadas à view do Exercício 1.

### Exercício 3: View de Pedidos
Crie uma view da tabela VBAK mostrando:
- Número do pedido
- Data de criação
- Cliente
- Tipo de documento

---

!!! success "Próximos Passos"
    Na próxima página, aprenderemos sobre **Associações e Joins em CDS**, incluindo como criar relacionamentos entre views e otimizar consultas.