---
tags:
  - ABAP
  - Debug
  - Watchpoints
  - Monitorização
---

# Watchpoints em ABAP

## 📋 Visão Geral

**Watchpoints** monitorizam variáveis e param a execução quando o valor da variável **muda** ou atinge uma **condição específica**. São essenciais para rastrear mudanças inesperadas em dados.

---

## 🎯 O que são Watchpoints?

Diferentemente dos breakpoints (que param em linhas de código), watchpoints param quando:
- Uma variável **muda de valor**
- Uma variável **atinge um valor específico**
- Um campo de estrutura/tabela é **modificado**

---

## 🔹 Como Criar Watchpoints

### Método 1: Durante o Debug

1. Entrar em modo debug (/h ou F7)
2. Selecionar variável na aba "Fields"
3. Clicar com botão direito → **Create Watchpoint**
4. Ou usar atalho: **Shift+F12**

### Método 2: Antes do Debug

1. No editor, posicionar cursor na variável
2. Menu: **Utilities → Breakpoints → Create Watchpoint**

---

## 🔹 Tipos de Watchpoints

### 1️⃣ Watchpoint Simples (Change)

Para quando variável muda de valor.

```abap
REPORT z_watchpoint_demo.

DATA: lv_contador TYPE i VALUE 0.

START-OF-SELECTION.
  
  " 👁️ Criar watchpoint em lv_contador
  
  DO 10 TIMES.
    lv_contador = lv_contador + 1.  " ⛔ Para aqui a cada mudança
  ENDLOOP.
  
  WRITE: / |Contador final: { lv_contador }|.
```

**Quando usa:**
- Descobrir onde uma variável é modificada
- Rastrear mudanças inesperadas
- Debug de loops complexos

---

### 2️⃣ Watchpoint com Condição

Para apenas quando variável atinge valor específico.

```abap
REPORT z_watchpoint_condicional.

DATA: lv_preco TYPE p DECIMALS 2.

START-OF-SELECTION.
  
  SELECT price FROM sflight INTO lv_preco UP TO 100 ROWS.
    " 👁️ Watchpoint: lv_preco > 1000
    " ⛔ Para apenas quando preço > 1000
    
    WRITE: / lv_preco.
  ENDSELECT.
```

**Condições possíveis:**
```abap
lv_valor > 100          " Maior que
lv_status = 'E'         " Igual a
lv_contador >= 50       " Maior ou igual
ls_data-campo IS INITIAL " Inicial
```

---

### 3️⃣ Watchpoint em Estrutura

Monitora campo específico de estrutura.

```abap
REPORT z_watchpoint_estrutura.

DATA: BEGIN OF ls_cliente,
        id    TYPE i,
        nome  TYPE string,
        ativo TYPE abap_bool,
      END OF ls_cliente.

START-OF-SELECTION.
  
  ls_cliente-id = 1.
  ls_cliente-nome = 'João'.
  
  " 👁️ Watchpoint em ls_cliente-ativo
  ls_cliente-ativo = abap_true.  " ⛔ Para aqui
  
  WRITE: / ls_cliente-nome.
```

---

### 4️⃣ Watchpoint em Tabela Interna

Monitora mudanças em tabela interna.

```abap
REPORT z_watchpoint_tabela.

DATA: lt_clientes TYPE TABLE OF kna1.

START-OF-SELECTION.
  
  " 👁️ Watchpoint em lt_clientes
  " Condição: lines( lt_clientes ) > 100
  
  SELECT * FROM kna1 INTO TABLE lt_clientes UP TO 1000 ROWS.
  " ⛔ Para quando tabela tiver > 100 linhas
  
  WRITE: / |Total: { lines( lt_clientes ) }|.
```

---

### 5️⃣ Watchpoint Global

Monitora variável em qualquer programa da call stack.

**Como criar:**
1. Debugger → Settings → Watchpoints
2. Selecionar "Global Watchpoint"
3. Inserir nome da variável

**Útil para:**
- Rastrear variáveis em chamadas de função
- Debug de código que não pode ser modificado
- Análise de framework SAP

---

## 💡 Exemplos Práticos

### Exemplo 1: Rastrear Mudança Inesperada

```abap
REPORT z_debug_mudanca.

DATA: lv_total TYPE p DECIMALS 2 VALUE '100.00'.

START-OF-SELECTION.
  
  " 👁️ Watchpoint em lv_total
  " Objetivo: descobrir onde valor está sendo zerado
  
  PERFORM calcular_desconto CHANGING lv_total.
  PERFORM aplicar_taxa CHANGING lv_total.
  PERFORM processar_pagamento CHANGING lv_total.
  
  " lv_total está 0 mas deveria ter valor!
  WRITE: / |Total: { lv_total }|.

FORM calcular_desconto CHANGING cv_valor TYPE p.
  cv_valor = cv_valor * '0.9'.
ENDFORM.

FORM aplicar_taxa CHANGING cv_valor TYPE p.
  cv_valor = cv_valor + 10.
ENDFORM.

FORM processar_pagamento CHANGING cv_valor TYPE p.
  cv_valor = 0.  " ⛔ Watchpoint para aqui!
ENDFORM.
```

### Exemplo 2: Debug de Loop com Acumulador

```abap
REPORT z_debug_acumulador.

DATA: lt_valores TYPE TABLE OF i,
      lv_soma TYPE i.

START-OF-SELECTION.
  
  lt_valores = VALUE #( ( 10 ) ( 20 ) ( 30 ) ( -100 ) ( 40 ) ).
  
  " 👁️ Watchpoint em lv_soma
  " Condição: lv_soma < 0
  
  LOOP AT lt_valores INTO DATA(lv_valor).
    lv_soma = lv_soma + lv_valor.  " ⛔ Para quando soma fica negativa
  ENDLOOP.
  
  WRITE: / |Soma: { lv_soma }|.
```

### Exemplo 3: Monitorar System Field

```abap
REPORT z_debug_sysubrc.

DATA: lt_dados TYPE TABLE OF kna1.

START-OF-SELECTION.
  
  " 👁️ Watchpoint em sy-subrc
  " Condição: sy-subrc <> 0
  
  SELECT * FROM kna1 INTO TABLE lt_dados WHERE kunnr = '9999999999'.
  " ⛔ Para se SELECT falhar
  
  IF sy-subrc = 0.
    WRITE: / |Encontrados: { lines( lt_dados ) }|.
  ELSE.
    WRITE: / 'Nenhum dado encontrado'.
  ENDIF.
```

### Exemplo 4: Debug de Objeto OO

```abap
REPORT z_debug_objeto.

CLASS lcl_conta DEFINITION.
  PUBLIC SECTION.
    METHODS: depositar IMPORTING iv_valor TYPE p DECIMALS 2.
    DATA: mv_saldo TYPE p DECIMALS 2 READ-ONLY.
ENDCLASS.

CLASS lcl_conta IMPLEMENTATION.
  METHOD depositar.
    mv_saldo = mv_saldo + iv_valor.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  
  DATA(lo_conta) = NEW lcl_conta( ).
  
  " 👁️ Watchpoint em lo_conta->mv_saldo
  " Condição: lo_conta->mv_saldo > 1000
  
  lo_conta->depositar( 100 ).
  lo_conta->depositar( 200 ).
  lo_conta->depositar( 800 ).  " ⛔ Para aqui (saldo = 1100)
  
  WRITE: / |Saldo: { lo_conta->mv_saldo }|.
```

---

## 🛠️ Gestão de Watchpoints

### Ver Watchpoints Ativos

**No Debugger:**
- Menu: Settings → Watchpoints
- Ou: Aba "Watchpoints"

### Desativar Watchpoint

- Clicar com botão direito → Deactivate
- Mantém configuração mas não para execução

### Apagar Watchpoint

- Clicar com botão direito → Delete
- Ou: Shift+F12 na variável novamente

### Apagar Todos Watchpoints

**No Debugger:**
```
Settings → Watchpoints → Delete All
```

---

## 🔍 Watchpoint vs Breakpoint

| Aspecto | Breakpoint | Watchpoint |
|---------|------------|------------|
| **Para em** | Linha de código | Mudança de variável |
| **Usado para** | Verificar fluxo | Rastrear dados |
| **Performance** | Rápido | Mais lento |
| **Condição** | Local no código | Valor da variável |
| **Melhor para** | Lógica | Estado de dados |

**Quando usar cada um:**

```abap
" ✅ Use BREAKPOINT quando quer parar numa linha específica
IF lv_valor > 100.
  BREAK-POINT.  " Parar sempre que entrar no IF
  PERFORM processar.
ENDIF.

" ✅ Use WATCHPOINT quando quer saber QUANDO variável muda
" 👁️ Watchpoint em lv_status
" Descobrir onde lv_status está sendo modificado
```

---

## ⚡ Performance de Watchpoints

### Impacto

Watchpoints têm **overhead significativo** porque verificam a variável após cada statement.

**Exemplo de impacto:**
```abap
" Sem watchpoint: 1 segundo
" Com watchpoint em lv_contador: 10 segundos

DO 1000000 TIMES.
  lv_contador = lv_contador + 1.
ENDLOOP.
```

### Otimizações

```abap
" ❌ Evitar watchpoint em loops gigantes sem condição
" 👁️ Watchpoint em lv_contador (sem condição)
DO 1000000 TIMES.
  lv_contador = lv_contador + 1.  " Para 1 milhão de vezes!
ENDLOOP.

" ✅ Usar com condição
" 👁️ Watchpoint em lv_contador
" Condição: lv_contador = 500000
DO 1000000 TIMES.
  lv_contador = lv_contador + 1.  " Para apenas 1 vez
ENDLOOP.

" ✅ Ou usar breakpoint condicional
DO 1000000 TIMES.
  lv_contador = lv_contador + 1.
  " Breakpoint aqui, condição: sy-index = 500000
ENDLOOP.
```

---

## 💡 Casos de Uso Avançados

### 1. Rastrear Corrupção de Dados

```abap
" Problema: campo mv_id está sendo zerado inesperadamente
CLASS lcl_produto DEFINITION.
  PUBLIC SECTION.
    DATA: mv_id TYPE i.
    METHODS: processar.
ENDCLASS.

" 👁️ Watchpoint em lo_prod->mv_id
" Condição: lo_prod->mv_id = 0
" Descobre exatamente onde o ID é zerado
```

### 2. Debug de Tabelas Grandes

```abap
DATA: lt_clientes TYPE TABLE OF kna1.

" 👁️ Watchpoint em sy-tabix
" Condição: sy-tabix = 5000
" Para na linha 5000 do loop sem parar 4999 vezes antes

SELECT * FROM kna1 INTO TABLE lt_clientes.

LOOP AT lt_clientes INTO DATA(ls_cliente).
  " Processar
ENDLOOP.
```

### 3. Monitorar Exceções

```abap
DATA: lv_erro TYPE string.

" 👁️ Watchpoint em lv_erro
" Condição: lv_erro IS NOT INITIAL

TRY.
    " Código complexo
  CATCH cx_root INTO DATA(lo_ex).
    lv_erro = lo_ex->get_text( ).  " ⛔ Para aqui
ENDTRY.
```

---

## 🎓 Boas Práticas

### ✅ Fazer

```abap
" 1. Usar condições para reduzir overhead
" 👁️ Watchpoint: lv_contador = 1000
" Em vez de parar a cada mudança

" 2. Watchpoint em campos específicos, não estruturas inteiras
" 👁️ ls_data-status (específico)
" Em vez de ls_data (toda estrutura)

" 3. Remover watchpoints quando não necessários
" Performance!

" 4. Documentar o que está a investigar
" Comentar: "Investigando porque mv_total fica negativo"
```

### ❌ Evitar

```abap
" 1. Watchpoint sem condição em loops grandes
DO 1000000 TIMES.
  lv_var = sy-index.  " ❌ Para 1 milhão de vezes!
ENDLOOP.

" 2. Múltiplos watchpoints simultâneos
" ❌ Impacto de performance exponencial

" 3. Watchpoint em system fields muito usados
" 👁️ sy-subrc  " ❌ Muda constantemente!
" 👁️ sy-tabix  " ❌ Muda a cada iteração!

" 4. Esquecer watchpoints ativos
" Verificar sempre depois de debug
```

---

## 🔧 Troubleshooting

### Watchpoint Não Para

**Possíveis causas:**
1. Variável não está mudando
2. Condição nunca é verdadeira
3. Watchpoint desativado
4. Variável é local e saiu do escopo

**Solução:**
```abap
" Adicionar WRITE para confirmar mudança
lv_valor = lv_valor + 10.
WRITE: / |Valor mudou para: { lv_valor }|.
```

### Performance Muito Lenta

**Causa:** Watchpoint em loop sem condição

**Solução:**
```abap
" Adicionar condição ao watchpoint
" Ou converter para breakpoint condicional
```

### Watchpoint em Objeto Não Funciona

**Causa:** Referência do objeto mudou

**Solução:**
```abap
" Watchpoint na classe/atributo, não na referência
" 👁️ lo_obj->mv_campo ✅
" Em vez de lo_obj ❌
```

---

## 🔗 Atalhos Úteis

| Atalho | Ação |
|--------|------|
| **Shift+F12** | Criar/Remover watchpoint |
| **Ctrl+Shift+F12** | Ver todos watchpoints |
| **F8** | Continuar até próximo watchpoint |

---

## 🔗 Próximos Passos

- **[Debugger](3_debugger.md)** - Navegar eficientemente no debugger
- **[Breakpoints](1_breakpoints.md)** - Complementar com breakpoints
- **[ST22](4_st22.md)** - Analisar dumps quando watchpoint encontra problemas

---

**Tags:** `#Debug` `#Watchpoints` `#Monitorização` `#ABAP` `#Troubleshooting`
