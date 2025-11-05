---
tags:
  - ABAP
  - Security
  - S_DEVELOP
  - Development Authorization
---

# Objeto S_DEVELOP

## 📋 Visão Geral

**S_DEVELOP** controla acesso a **objetos de desenvolvimento** (programas, classes, tabelas, etc.) em SAP.

---

## 🔑 Campos do S_DEVELOP

```
S_DEVELOP - Development/Correction
  Campos:
    - DEVCLASS (Pacote)
    - OBJTYPE (Tipo de objeto)
    - OBJNAME (Nome do objeto)
    - P_GROUP (Grupo de permissões)
    - ACTVT (Atividade)
```

---

## 📊 Tipos de Objeto (OBJTYPE)

| OBJTYPE | Descrição |
|---------|-----------|
| **PROG** | Programas ABAP |
| **CLAS** | Classes ABAP |
| **FUGR** | Grupos de funções |
| **TABL** | Tabelas/Estruturas |
| **DTEL** | Elementos de dados |
| **DOMA** | Domínios |
| **TRAN** | Transações |
| **DEVC** | Pacotes |
| **DEBUG** | Debugging |

---

## 🎯 Atividades (ACTVT)

| ACTVT | Descrição |
|-------|-----------|
| **01** | Criar |
| **02** | Alterar |
| **03** | Visualizar |
| **06** | Deletar |
| **70** | Ler (SELECT) |

---

## 💡 Exemplos Práticos

### Criar Programa Z*

```abap
" No SE38/SE80
AUTHORITY-CHECK OBJECT 'S_DEVELOP'
  ID 'DEVCLASS' FIELD '$TMP'
  ID 'OBJTYPE'  FIELD 'PROG'
  ID 'OBJNAME'  FIELD 'Z*'
  ID 'P_GROUP'  FIELD '*'
  ID 'ACTVT'    FIELD '01'.  " Criar

IF sy-subrc <> 0.
  MESSAGE 'Sem autorização para criar programas' TYPE 'E'.
ENDIF.
```

---

### Alterar Tabela Custom

```abap
AUTHORITY-CHECK OBJECT 'S_DEVELOP'
  ID 'DEVCLASS' FIELD 'ZPACKAGE'
  ID 'OBJTYPE'  FIELD 'TABL'
  ID 'OBJNAME'  FIELD 'Z*'
  ID 'P_GROUP'  FIELD '*'
  ID 'ACTVT'    FIELD '02'.  " Alterar

IF sy-subrc <> 0.
  MESSAGE 'Sem autorização para alterar tabelas' TYPE 'E'.
ENDIF.
```

---

### Debug em Produção

```abap
AUTHORITY-CHECK OBJECT 'S_DEVELOP'
  ID 'DEVCLASS' FIELD '*'
  ID 'OBJTYPE'  FIELD 'DEBUG'
  ID 'OBJNAME'  FIELD '*'
  ID 'P_GROUP'  FIELD '*'
  ID 'ACTVT'    FIELD '03'.  " Executar debug

IF sy-subrc <> 0.
  MESSAGE 'Debug não permitido em produção' TYPE 'E'.
  LEAVE PROGRAM.
ENDIF.
```

---

## 🛡️ Proteger Objetos Custom

### Role para Desenvolvedor Junior

```
Role: Z_DEV_JUNIOR

S_DEVELOP:
  DEVCLASS: Z*, $TMP
  OBJTYPE:  PROG, CLAS, FUGR
  OBJNAME:  Z*
  P_GROUP:  *
  ACTVT:    01, 02, 03 (Criar, Alterar, Visualizar)
```

### Role para Desenvolvedor Senior

```
Role: Z_DEV_SENIOR

S_DEVELOP:
  DEVCLASS: Z*, $TMP, SAP*
  OBJTYPE:  * (Todos)
  OBJNAME:  Z*, Y*
  P_GROUP:  *
  ACTVT:    01, 02, 03, 06 (+ Deletar)
```

---

## 🚨 Debug em Produção

**Restringir severamente!**

```
Role: Z_PROD_SUPPORT

S_DEVELOP:
  OBJTYPE:  DEBUG
  ACTVT:    03

+ Condições:
  - Apenas emergency user
  - Logging de sessões debug
  - Aprovação obrigatória
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```
1. Separar por ambiente
   DEV: S_DEVELOP com ACTVT 01, 02, 03, 06
   QAS: S_DEVELOP apenas 03 (visualizar)
   PRD: SEM S_DEVELOP (exceto suporte)

2. Limitar por namespace
   OBJNAME: Z*, Y* (apenas custom)

3. Debug controlado
   Apenas emergency user em produção

4. Logging
   Registrar criação/alteração de objetos
```

### ❌ Evitar

```
1. S_DEVELOP com * em produção  ❌
2. Debug liberado para todos  ❌
3. Deletar objetos SAP standard  ❌
4. Não auditar mudanças  ❌
```

---

## 🔗 Próximos Passos

- **[Autorizações](1_autorizacoes.md)** - Conceitos
- **[Práticas Seguras](5_praticas_seguras.md)** - Segurança no código

---

**Tags:** `#S_DEVELOP` `#Development-Security` `#Debug-Authorization`
