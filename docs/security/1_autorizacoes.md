---
tags:
  - ABAP
  - Security
  - Authorization
  - SAP Security
---

# Autorizações SAP

## 📋 Visão Geral

O sistema de **autorizações SAP** controla **quem** pode fazer **o quê**, protegendo dados e funcionalidades críticas.

---

## 🔑 Conceitos Principais

### Objeto de Autorização
Define **o quê** pode ser protegido.

**Exemplo:** `S_TCODE` (acesso a transações)

---

### Campo de Autorização
Define **como** é protegido.

**Exemplo:** `TCD` (código da transação dentro de S_TCODE)

---

### Valor de Atividade (ACTVT)
Tipo de operação.

| Código | Atividade |
|--------|-----------|
| `01` | Criar (Create) |
| `02` | Alterar (Change) |
| `03` | Visualizar (Display) |
| `06` | Deletar (Delete) |
| `16` | Executar (Execute) |

---

## 🛠️ Transações Principais

| Transação | Descrição |
|-----------|-----------|
| **SU01** | Manutenção de usuários |
| **SU21** | Manutenção de objetos de autorização |
| **SU24** | Verificar dados de autorização |
| **PFCG** | Manutenção de roles |
| **SU53** | Avaliar verificação de autorização |
| **ST01** | System trace (autoriza ções) |

---

## 📊 Objetos de Autorização Comuns

### Desenvolvimento

```
S_DEVELOP - Objetos de desenvolvimento
  Campos:
    - DEVCLASS (Pacote)
    - OBJTYPE (Tipo: PROG, CLAS, TABL...)
    - OBJNAME (Nome do objeto)
    - ACTVT (Atividade: 01, 02, 03...)
```

### Transações

```
S_TCODE - Execução de transações
  Campos:
    - TCD (Código da transação)
```

### Tabelas

```
S_TABU_NAM - Acesso a tabelas
  Campos:
    - TABLE (Nome da tabela)
    - ACTVT (Atividade)
```

### Documentos Financeiros

```
F_BKPF_BUK - Documentos contabilísticos
  Campos:
    - BUKRS (Empresa)
    - ACTVT (Atividade)
```

### Ordens de Venda

```
V_VBAK_VKO - Ordens de venda
  Campos:
    - VKORG (Org. vendas)
    - VTWEG (Canal distribuição)
    - ACTVT (Atividade)
```

---

## 💡 Consultar Objetos (SU21)

1. **Executar:** `SU21`
2. **Classe:** (ex: BC_A - Application-specific)
3. **Ver objetos** disponíveis
4. **Duplo-clique** para ver campos

---

## 🔍 Verificar Autorizações do Usuário

### SU01 - Dados do Usuário

```
1. SU01
2. Inserir username
3. Aba "Roles"
4. Ver roles atribuídos
```

### SU53 - Análise de Erro

Executar **imediatamente após** erro de autorização:

```
1. Erro: "Sem autorização para transação SE38"
2. Executar: SU53
3. Ver:
   - Objeto faltando (S_TCODE)
   - Valor verificado (TCD = SE38)
   - Resultado (Retorna: 4 - Sem autorização)
```

---

## 🎯 Hierarquia de Autorizações

```
Usuário (SU01)
  └── Roles (PFCG)
        └── Perfil de Autorização
              └── Objeto de Autorização (SU21)
                    └── Campo de Autorização
                          └── Valor
```

**Exemplo:**
```
Usuário: BMOREIRA
  └── Role: Z_VENDEDOR
        └── Perfil: Z_VENDEDOR_P
              └── Objeto: V_VBAK_VKO
                    ├── VKORG: 1000
                    ├── VTWEG: *
                    └── ACTVT: 01, 03 (Criar, Visualizar)
```

---

## 🔐 Princípio do Menor Privilégio

Dar **apenas** as autorizações **necessárias** para o trabalho.

### ✅ Correto

```
Vendedor:
  - V_VBAK_VKO (Ordens de venda): 01, 03  ✅ Criar e visualizar
  - V_VBAP_VKO (Itens): 01, 03
```

### ❌ Incorreto

```
Vendedor:
  - S_TCODE: * (TODAS as transações!)  ❌ PERIGOSO!
  - F_BKPF_BUK: * (Documentos financeiros)  ❌ Não precisa!
```

---

## 💡 Exemplo: Criar Objeto Custom

### 1. SU21 - Criar Objeto

```
SU21 → Create
Objeto: Z_VENDAS
Classe: BC_A (Application)

Campos:
  - ACTVT (Atividade): 01, 02, 03
  - VKORG (Org. vendas)
  - SPART (Setor atividade)
```

### 2. Usar em Código

```abap
AUTHORITY-CHECK OBJECT 'Z_VENDAS'
  ID 'ACTVT' FIELD '02'  " Alterar
  ID 'VKORG' FIELD '1000'
  ID 'SPART' FIELD '01'.

IF sy-subrc <> 0.
  MESSAGE 'Sem autorização para alterar vendas org 1000' TYPE 'E'.
ENDIF.
```

### 3. PFCG - Incluir em Role

```
PFCG → Criar role Z_VENDEDOR_SP
Aba "Authorizations"
  Adicionar objeto Z_VENDAS
    ACTVT: 01, 02, 03
    VKORG: 1000
    SPART: 01
Gerar perfil
Atribuir a usuários
```

---

## 🎓 Trace de Autorizações (ST01)

Para descobrir que autorizações um programa precisa:

```
1. ST01 → Ativar trace
   Marcar: "Authorization Check"
   
2. Executar programa/transação

3. ST01 → Desativar trace

4. Analisar resultados:
   - Objetos verificados
   - Valores testados
   - Resultado (OK / Fail)
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```
1. Usar objetos standard quando possível
   - S_TCODE para transações
   - S_TABU_NAM para tabelas

2. Criar objetos Z* para lógica custom
   - Z_VENDAS, Z_COMPRAS, etc.

3. Documentar requisitos de autorização
   - README do programa: "Requer S_TCODE e Z_VENDAS"

4. Testar com usuário restrito
   - Não testar sempre como admin!

5. Logging de falhas
   - Logar tentativas de acesso não autorizado
```

### ❌ Evitar

```
1. Dar SAP_ALL a todos  ❌
   - Apenas emergency user

2. Wildcards demais  ❌
   - TCD: * (todas transações)

3. Não validar autorizações  ❌
   - Confiar apenas em UI

4. Hardcoded bypass  ❌
   - IF sy-uname = 'ADMIN'. (sem auth check)

5. Não revisar roles periodicamente  ❌
   - Remover acessos desnecessários
```

---

## 🔗 Próximos Passos

- **[AUTHORITY-CHECK](2_authority_check.md)** - Implementar verificações
- **[Roles e Perfis](3_roles_perfis.md)** - Gestão em PFCG
- **[S_DEVELOP](4_s_develop.md)** - Proteger desenvolvimento

---

**Tags:** `#Authorization` `#Security` `#SU21` `#PFCG` `#SU53`
