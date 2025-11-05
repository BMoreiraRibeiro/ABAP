---
tags:
  - ABAP
  - Security
  - PFCG
  - Roles
  - Profiles
---

# Roles e Perfis (PFCG)

## 📋 Visão Geral

**Roles** agrupam autorizações que são atribuídas a usuários. **PFCG** é a transação para criar e manter roles.

---

## 🎯 Conceitos

### Role (Função)
Conjunto de autorizações para uma função de negócio.

**Exemplo:** `Z_VENDEDOR` - Permite criar/visualizar ordens de venda

### Perfil de Autorização
Gerado automaticamente pelo role, contém autorizações técnicas.

### Single Role vs Composite Role
- **Single:** Autorizações diretas
- **Composite:** Agrupa vários single roles

---

## 🛠️ Criar Role (PFCG)

### Passo 1: Criar Role

```
1. Executar: PFCG
2. Role: Z_VENDEDOR
3. Descrição: "Vendedor - Criar e visualizar ordens"
4. Salvar
```

### Passo 2: Adicionar Transações

```
Aba "Menu"
  → Insert Transaction
    - VA01 (Criar ordem)
    - VA03 (Visualizar ordem)
  → Salvar
```

### Passo 3: Gerar Autorizações

```
Aba "Authorizations"
  → Change Authorization Data
  → Generate (botão)
  
Sistema adiciona automaticamente:
  - S_TCODE (para VA01, VA03)
  - V_VBAK_VKO (ordens de venda)
```

### Passo 4: Editar Valores

```
Expandir V_VBAK_VKO:
  ACTVT: 01, 03 (Criar, Visualizar)
  VKORG: 1000 (Org. vendas específica)
  VTWEG: * (Todos canais)
  SPART: * (Todos setores)
  
Salvar e Gerar
```

### Passo 5: Atribuir a Usuários

```
Aba "User"
  → User Comparison
  → Adicionar: BMOREIRA, USER01, USER02
  → Complete Comparison
```

---

## 💡 Exemplo Completo

### Role para Vendas

```
Role: Z_SALES_CLERK
Descrição: Auxiliar de Vendas

Menu:
  ├── VA01 - Criar ordem
  ├── VA02 - Alterar ordem
  ├── VA03 - Visualizar ordem
  └── VL01N - Criar remessa

Autorizações:
  ├── S_TCODE
  │     TCD: VA01, VA02, VA03, VL01N
  │
  ├── V_VBAK_VKO (Ordens)
  │     ACTVT: 01, 02, 03
  │     VKORG: 1000
  │     VTWEG: 10, 20
  │     SPART: *
  │
  └── V_LIKP_VKO (Remessas)
        ACTVT: 01, 03
        VKORG: 1000
        VSTEL: *
```

---

## 🏗️ Composite Role

Agregar vários single roles:

```
Composite: Z_SALES_MANAGER
Descrição: Gestor de Vendas

Contém:
  ├── Z_SALES_CLERK (Single)
  ├── Z_SALES_REPORTS (Single)
  └── Z_SALES_APPROVAL (Single)
```

**Criar:**
```
PFCG → Role Type: Composite Role
  → Add Roles:
     - Z_SALES_CLERK
     - Z_SALES_REPORTS
     - Z_SALES_APPROVAL
  → Salvar
```

---

## 🔐 Objetos Custom em Roles

### 1. Criar Objeto (SU21)

```
Z_VENDAS
  ACTVT: 01, 02, 03
  VKORG
  SPART
```

### 2. Adicionar Manual em PFCG

```
PFCG → Role Z_VENDEDOR
  → Aba "Authorizations"
  → Manually (botão)
  → Object Class: BC_A
  → Objeto: Z_VENDAS
  → Editar valores:
       ACTVT: 01, 03
       VKORG: 1000
       SPART: 01
  → Salvar e Gerar
```

---

## 🎓 Transportar Roles

```
PFCG → Role Z_VENDEDOR
  → Utilities → Transport
  → Ordem de transporte: DEVK9##### 
  → Salvar
```

⚠️ **Atenção:** Usuários NÃO são transportados! Devem ser atribuídos manualmente em cada sistema.

---

## ⚡ Boas Práticas

### ✅ Fazer

```
1. Naming convention clara
   - Z_<AREA>_<FUNCTION>
   - Z_SALES_CLERK, Z_FI_ACCOUNTANT

2. Documentar descrição
   - "Vendedor - Criar e visualizar ordens org 1000"

3. Princípio do menor privilégio
   - Apenas autorizações necessárias

4. Single roles específicos
   - Um role = uma função

5. Composite para roles complexos
   - Gestor = Clerk + Reports + Approval
```

### ❌ Evitar

```
1. SAP_ALL para usuários normais  ❌
2. Wildcards demais (* em tudo)  ❌
3. Roles sem documentação  ❌
4. Copiar SAP standard (S_*) e modificar  ❌
5. Não revisar roles periodicamente  ❌
```

---

## 🔗 Próximos Passos

- **[Autorizações](1_autorizacoes.md)** - Conceitos básicos
- **[AUTHORITY-CHECK](2_authority_check.md)** - Verificações no código
- **[S_DEVELOP](4_s_develop.md)** - Proteger desenvolvimento

---

**Tags:** `#PFCG` `#Roles` `#Profiles` `#Authorization-Management`
