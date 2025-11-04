# 🔷 CDS e AMDP

Esta secção apresenta **Core Data Services (CDS)** e **ABAP Managed Database Procedures (AMDP)** — tecnologias modernas para acesso e processamento de dados em SAP.

---

## 📖 O que vais aprender

- Core Data Services (CDS Views)
- Annotations e metadata
- Associations e relacionamentos
- CDS com parâmetros
- AMDP (procedimentos em SQLScript)
- Performance com pushdown para HANA
- Consumir CDS em programas ABAP
- Diferenças entre CDS e views clássicas

---

## 🎯 O que são CDS Views?

CDS Views são **views semânticas** definidas em linguagem DDL (Data Definition Language) que:

- Substituem views clássicas (SE11)
- Suportam annotations (metadados)
- Permitem associations (navegação entre entidades)
- Otimizam performance (pushdown para DB)
- Integram-se nativamente com Fiori/OData

---

## 💡 Exemplo de CDS View

### CDS Simples

```sql
@AbapCatalog.sqlViewName: 'ZV_FLIGHTS'
@EndUserText.label: 'Voos e Companhias'
@AccessControl.authorizationCheck: #CHECK

define view Z_CDS_Flights as
  select from sflight
  association [0..1] to scarr as _Carrier 
    on $projection.carrid = _Carrier.carrid
{
  key carrid,
  key connid,
  key fldate,
      price,
      currency,
      
      // Expor associação
      _Carrier
}
```

### Consumir em ABAP

```abap
SELECT * FROM z_cds_flights
  INTO TABLE @DATA(lt_flights)
  UP TO 10 ROWS.

LOOP AT lt_flights INTO DATA(ls_flight).
  WRITE: / ls_flight-carrid, 
         ls_flight-connid, 
         ls_flight-price.
ENDLOOP.
```

---

## 🔗 Associations

Definir relacionamentos entre entidades sem fazer JOINs explícitos.

```sql
define view Z_CDS_Orders as
  select from vbak
  association [0..*] to vbap as _Items
    on $projection.vbeln = _Items.vbeln
  association [0..1] to kna1 as _Customer
    on $projection.kunnr = _Customer.kunnr
{
  key vbeln,
      kunnr,
      vkorg,
      vtweg,
      
      // Expor associações
      _Items,
      _Customer
}
```

**Usar em ABAP:**
```abap
SELECT * FROM z_cds_orders
  INTO TABLE @DATA(lt_orders).

LOOP AT lt_orders INTO DATA(ls_order).
  " Navegar pela associação
  SELECT * FROM @ls_order-_Items AS items
    INTO TABLE @DATA(lt_items).
  
  WRITE: / 'Ordem:', ls_order-vbeln, 
         'Itens:', lines( lt_items ).
ENDLOOP.
```

---

## 🎨 Annotations Comuns

| Annotation | Descrição |
|------------|-----------|
| `@AbapCatalog.sqlViewName` | Nome da view SQL gerada |
| `@EndUserText.label` | Descrição da view |
| `@AccessControl.authorizationCheck` | Controlo de acesso |
| `@Analytics.dataCategory` | Tipo de dados (dimensão/facto) |
| `@ObjectModel.usageType` | Uso previsto |
| `@VDM.viewType` | Tipo de view no VDM |

---

## 🔢 CDS com Parâmetros

```sql
@AbapCatalog.sqlViewName: 'ZV_FLIGHTS_PARAM'
@EndUserText.label: 'Voos por Companhia'

define view Z_CDS_Flights_Param
  with parameters
    p_carrid : s_carr_id
as
  select from sflight
{
  key carrid,
  key connid,
  key fldate,
      price
}
where carrid = :p_carrid
```

**Usar em ABAP:**
```abap
SELECT * FROM z_cds_flights_param( p_carrid = 'LH' )
  INTO TABLE @DATA(lt_flights).
```

---

## ⚡ AMDP (ABAP Managed Database Procedures)

Executar **SQLScript nativo** diretamente na base de dados (HANA).

### Exemplo AMDP

```abap
CLASS zcl_amdp_example DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    
    CLASS-METHODS get_top_flights
      IMPORTING VALUE(iv_carrid) TYPE s_carr_id
                VALUE(iv_limit)  TYPE i
      EXPORTING VALUE(et_result) TYPE tt_sflight.
ENDCLASS.

CLASS zcl_amdp_example IMPLEMENTATION.
  METHOD get_top_flights BY DATABASE PROCEDURE 
    FOR HDB LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING sflight.
    
    et_result = SELECT carrid, connid, fldate, price
                FROM sflight
                WHERE carrid = :iv_carrid
                ORDER BY price DESC
                LIMIT :iv_limit;
  ENDMETHOD.
ENDCLASS.
```

**Usar:**
```abap
DATA: lt_flights TYPE tt_sflight.

zcl_amdp_example=>get_top_flights(
  EXPORTING
    iv_carrid = 'LH'
    iv_limit  = 10
  IMPORTING
    et_result = lt_flights ).
```

---

## 📊 CDS vs. Views Clássicas

| Característica | CDS View | View Clássica (SE11) |
|----------------|----------|---------------------|
| Linguagem | DDL (SQL-like) | ABAP Dictionary |
| Associations | ✅ Sim | ❌ Não |
| Annotations | ✅ Sim | ❌ Não |
| Parâmetros | ✅ Sim | ❌ Não |
| Performance HANA | ✅ Otimizado | ⚠️ Limitado |
| OData/Fiori | ✅ Nativo | ⚠️ Requer adapter |

---

## 🚀 Boas Práticas

### ✅ Fazer

1. **Naming convention**: Z_CDS_ ou Z_I_ (interface) / Z_C_ (consumption)
2. **Usar associations** em vez de JOINs repetidos
3. **Annotations completas** para metadados
4. **Testar performance** com SE16 / Data Preview
5. **Documentar** propósito da view

### ❌ Evitar

1. Lógica complexa em CDS (usar AMDP)
2. Muitas camadas de views aninhadas
3. SELECTs sem WHERE em CDS com dados grandes
4. Esquecer `@AccessControl.authorizationCheck`

---

## 🛠️ Ferramentas Recomendadas

- **Eclipse ADT (ABAP Development Tools)**: Criar e editar CDS Views
- **Data Preview (F8)**: Testar CDS Views no Eclipse
- **Transaction SE16**: Ver dados da SQL view gerada
- **Transaction SE11**: Dicionário de dados (limitado para CDS)

---

## 🔗 Próximos Passos

1. Consulte a secção [CDS Views](../cds/index.md) para tutoriais detalhados
2. Instale **ABAP Development Tools (ADT)** no Eclipse
3. Crie uma CDS simples
4. Experimente associations
5. Teste AMDP se tiver HANA
6. Explore [SQL](../sql/index.md) para otimizar queries

---

**Tags:** `#CDS` `#AMDP` `#HANA` `#Performance` `#Modern-ABAP`
