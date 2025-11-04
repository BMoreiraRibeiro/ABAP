---
tags:
  - ABAP
  - Debug
  - Debugger
  - Ferramentas
---

# Debugger Clássico vs Novo

## 📋 Visão Geral

O SAP oferece dois debuggers: o **Clássico** (antigo) e o **Novo** (moderno). Ambos têm vantagens e desvantagens dependendo da situação.

---

## 🎯 Comparação Rápida

| Aspecto | Debugger Clássico | Novo Debugger |
|---------|-------------------|---------------|
| **Interface** | Janelas separadas | Interface integrada com abas |
| **Performance** | Mais rápido | Mais lento |
| **Visualização** | Simples | Rica e visual |
| **Objetos OO** | Limitado | Excelente |
| **Tabelas** | Básico | Navegação avançada |
| **Atalho** | `/hc` | `/hs` ou padrão |

---

## 🔹 Debugger Clássico

### Ativar
```abap
/hc  " Forçar debugger clássico
```

### Vantagens
- ✅ **Mais rápido** em sistemas lentos
- ✅ Familiar para desenvolvedores antigos
- ✅ Menos consumo de memória
- ✅ Melhor para debug rápido

### Desvantagens
- ❌ Interface antiga
- ❌ Limitado para objetos
- ❌ Visualização de tabelas básica

### Layout

```
┌─────────────────────────────────────┐
│  Código Fonte                       │
├─────────────────────────────────────┤
│  Fields (Variáveis)                 │
├─────────────────────────────────────┤
│  Tables (Tabelas Internas)          │
└─────────────────────────────────────┘
```

### Atalhos Principais

| Atalho | Ação |
|--------|------|
| **F5** | Step into (entrar em método/função) |
| **F6** | Step over (executar linha) |
| **F7** | Return (sair do método atual) |
| **F8** | Continue (até próximo breakpoint) |
| **Ctrl+F** | Procurar campo |
| **Shift+F2** | Definir variável |

---

## 🔹 Novo Debugger

### Ativar
```abap
/hs  " Forçar novo debugger
```

Ou configurar como padrão:
**Utilities → Settings → ABAP Editor → Debugging → New Debugger**

### Vantagens
- ✅ Interface moderna com abas
- ✅ **Excelente para objetos OO**
- ✅ Navegação avançada em tabelas
- ✅ Múltiplas views simultâneas
- ✅ Melhor visualização de estruturas

### Desvantagens
- ❌ Mais lento em sistemas antigos
- ❌ Mais complexo para iniciantes
- ❌ Consome mais memória

### Layout com Abas

```
┌──────────────────────────────────────────────┐
│ Source Code | Variables | Breakpoints | ...  │
├──────────────────────────────────────────────┤
│                                              │
│  [Aba ativa com conteúdo]                   │
│                                              │
└──────────────────────────────────────────────┘
```

### Abas Disponíveis

1. **Source Code** - Código fonte
2. **Variables** - Variáveis locais e globais
3. **Breakpoints** - Gestão de breakpoints
4. **Watchpoints** - Gestão de watchpoints
5. **Call Stack** - Pilha de chamadas
6. **Tables** - Visualização de tabelas internas
7. **Objects** - Navegação em objetos OO
8. **Settings** - Configurações do debugger

---

## ⚡ Atalhos Comuns (Ambos Debuggers)

### Navegação

| Atalho | Ação | Descrição |
|--------|------|-----------|
| **F5** | Step Into | Entra em método/função |
| **F6** | Step Over | Executa linha sem entrar |
| **F7** | Return | Volta ao chamador |
| **F8** | Continue | Até próximo breakpoint |
| **Shift+F8** | Continue to cursor | Até linha do cursor |

### Breakpoints

| Atalho | Ação |
|--------|------|
| **Shift+F7** | Criar breakpoint |
| **Shift+F12** | Criar watchpoint |
| **Ctrl+Shift+F7** | Ver breakpoints |

### Visualização

| Atalho | Ação |
|--------|------|
| **Ctrl+F** | Procurar |
| **Shift+F2** | Alterar valor de variável |
| **Ctrl+F10** | Ver tabela |

---

## 💡 Casos de Uso

### Quando Usar Debugger Clássico

```abap
" ✅ Debug rápido de código procedural
REPORT z_simples.

DATA: lv_total TYPE i.

DO 10 TIMES.
  lv_total = lv_total + sy-index.  " Simples, não precisa features avançadas
ENDLOOP.
```

### Quando Usar Novo Debugger

```abap
" ✅ Debug de código OO complexo
CLASS lcl_processador DEFINITION.
  PUBLIC SECTION.
    METHODS: processar IMPORTING io_dados TYPE REF TO lcl_dados.
ENDCLASS.

" Novo debugger é melhor para navegar em objetos
DATA(lo_proc) = NEW lcl_processador( ).
DATA(lo_dados) = NEW lcl_dados( ).
lo_proc->processar( lo_dados ).
```

---

## 🔍 Funcionalidades Avançadas

### Novo Debugger: Navegação em Objetos

```abap
CLASS lcl_cliente DEFINITION.
  PUBLIC SECTION.
    DATA: mv_nome TYPE string,
          mo_endereco TYPE REF TO lcl_endereco.
ENDCLASS.

" No novo debugger:
" - Expandir objeto
" - Ver atributos e referências
" - Navegar em objetos aninhados
DATA(lo_cliente) = NEW lcl_cliente( ).
```

**Visualização:**
```
📦 lo_cliente (LCL_CLIENTE)
  └─ 📄 mv_nome: "João Silva"
  └─ 📦 mo_endereco (LCL_ENDERECO)
      └─ 📄 mv_rua: "Rua A"
      └─ 📄 mv_cidade: "Lisboa"
```

### Novo Debugger: Tabelas Internas

```abap
DATA: lt_clientes TYPE TABLE OF kna1.

SELECT * FROM kna1 INTO TABLE lt_clientes UP TO 1000 ROWS.

" No novo debugger:
" - Ver tabela em grid
" - Ordenar colunas
" - Filtrar dados
" - Exportar para Excel
```

---

## 🛠️ Personalização

### Configurar Debugger Padrão

**Caminho:**
```
SAP GUI → Utilities → Settings → ABAP Editor → Debugging
```

**Opções:**
- ☑️ New ABAP Debugger
- ☐ Classic ABAP Debugger

### Layout do Novo Debugger

**Personalizar abas:**
1. Clicar com botão direito na área de abas
2. **Configure Layout**
3. Arrastar/adicionar/remover abas

**Layouts predefinidos:**
- Standard
- Desktop 1, 2, 3
- Analysis
- Custom (criar o seu)

---

## 🎓 Comparação Prática

### Exemplo: Debug de Loop

**Debugger Clássico:**
```
1. Ver código
2. Ver variáveis numa janela separada
3. Alternar entre janelas
```

**Novo Debugger:**
```
1. Código e variáveis lado a lado em abas
2. Ver call stack simultaneamente
3. Tudo numa interface integrada
```

### Exemplo: Debug de Objeto

**Debugger Clássico:**
```
lo_objeto->mv_atributo  " Precisa digitar caminho completo
```

**Novo Debugger:**
```
📦 lo_objeto (expandir visualmente)
  └─ 📄 mv_atributo: valor
  └─ 📦 mo_outro_objeto
      └─ ...
```

---

## 💡 Dicas e Truques

### Forçar Debugger Específico

```abap
" No código (temporário)
BREAK-POINT.  " Usa debugger padrão

" Na transação
/hc  " Clássico
/hs  " Novo
/h   " Padrão configurado
```

### Alternar Durante Debug

**No debugger:**
```
Settings → Switch to Classic/New Debugger
```

### Desempenho Melhorado

**Novo Debugger:**
- Desativar abas não usadas
- Limitar visualização de tabelas
- Configuração: Settings → Maximum Table Size (padrão: 1000)

**Clássico:**
- Já otimizado por natureza
- Melhor para sistemas lentos

---

## 🔗 Comandos Especiais

### Debugger Clássico

```
/h    - Ativar debug
/hdel - Apagar breakpoints
/hx   - Desativar debug
```

### Novo Debugger

```
/hs   - Forçar novo debugger
/hc   - Forçar clássico
/hx   - Sair do debug
```

---

## 🎯 Boas Práticas

### ✅ Fazer

```abap
" 1. Usar novo debugger para OO
CLASS lcl_complexo DEFINITION.
  " ... código OO complexo
ENDCLASS.

" 2. Usar clássico para scripts rápidos
REPORT z_quick_fix.
" Debug rápido

" 3. Personalizar layout
" Salvar layouts diferentes para diferentes tarefas

" 4. Aprender atalhos
" F5, F6, F7, F8 - essenciais!
```

### ❌ Evitar

```abap
" 1. Debugger errado para a tarefa
" Clássico para debug OO complexo ❌
" Novo para sistema muito lento ❌

" 2. Não configurar layout
" Perder tempo procurando informação

" 3. Ignorar call stack
" Fundamental para entender fluxo

" 4. Não usar watchpoints
" Rastrear mudanças manualmente
```

---

## 🔗 Próximos Passos

- **[ST22](4_st22.md)** - Analisar dumps
- **[SAT](5_sat.md)** - Runtime Analysis
- **[Breakpoints](1_breakpoints.md)** - Usar breakpoints eficientemente

---

**Tags:** `#Debug` `#Debugger` `#Ferramentas` `#ABAP` `#Desenvolvimento`
