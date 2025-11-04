# CDS Views (Core Data Services)

## 📋 Visão Geral

**CDS Views** são views semânticas definidas em linguagem DDL (Data Definition Language) que substituem as views clássicas do SAP. Oferecem funcionalidades avançadas como associations, annotations e otimização de performance.

---

## 🎯 O que são CDS Views?

CDS (Core Data Services) é uma tecnologia SAP moderna que permite:

- ✅ Criar views com sintaxe SQL moderna
- ✅ Definir relacionamentos entre entidades (associations)
- ✅ Adicionar metadados através de annotations
- ✅ Otimizar performance com pushdown para base de dados
- ✅ Integração nativa com Fiori e OData

---

## 📚 Conteúdos

Esta secção está organizada nos seguintes tópicos:

1. **[Introdução](1_introducao.md)** - Conceitos básicos e primeira CDS View
2. **[JOINs e Associações](2_joins.md)** - Como relacionar dados em CDS
3. **[Anotações e Metadata](3_anotacoes_metadata.md)** - Configurar comportamento das views
4. **[Funções e Expressões](4_funcoes.md)** - Cálculos e manipulação de dados
5. **[Parâmetros e Views Parametrizadas](5_parametros_filtros_views_param.md)** - CDS dinâmicas

---

## 🚀 Exemplo Rápido

### CDS View Básica

```sql
@AbapCatalog.sqlViewName: 'ZV_CLIENTES'
@EndUserText.label: 'Clientes Ativos'
@AccessControl.authorizationCheck: #CHECK

define view Z_CDS_Clientes as
  select from kna1
{
  key kunnr as Cliente,
      name1 as Nome,
      ort01 as Cidade,
      land1 as Pais
}
where loevm = ''  -- Não marcado para eliminação
```

### Consumir em ABAP

```abap
SELECT * FROM z_cds_clientes
  INTO TABLE @DATA(lt_clientes)
  UP TO 100 ROWS.

LOOP AT lt_clientes INTO DATA(ls_cliente).
  WRITE: / ls_cliente-cliente, ls_cliente-nome.
ENDLOOP.
```

---

## 🔗 Próximos Passos

1. Comece pela [Introdução](1_introducao.md) para entender os conceitos básicos
2. Instale o **ABAP Development Tools (ADT)** no Eclipse para criar CDS Views
3. Pratique com os exemplos em cada secção

---

## 📊 CDS vs. Views Clássicas

| Característica | CDS View | View Clássica (SE11) |
|----------------|----------|---------------------|
| **Sintaxe** | DDL (SQL-like) | ABAP Dictionary |
| **Associations** | ✅ Sim | ❌ Não |
| **Annotations** | ✅ Sim | ❌ Não |
| **Parâmetros** | ✅ Sim | ❌ Não |
| **Performance HANA** | ✅ Otimizado | ⚠️ Limitado |
| **OData/Fiori** | ✅ Nativo | ⚠️ Requer adapter |

---

**Tags:** `#CDS` `#Views` `#DDL` `#HANA` `#Performance`
