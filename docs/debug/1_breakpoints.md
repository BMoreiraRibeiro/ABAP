---
tags:
  - ABAP
  - Debug
  - Breakpoints
  - Desenvolvimento
---

# Breakpoints em ABAP

## 📋 Visão Geral

**Breakpoints** são pontos de paragem que permitem pausar a execução do programa para analisar o estado das variáveis, lógica e fluxo de execução.

---

## 🎯 Tipos de Breakpoints

### 1️⃣ Breakpoint Estático (BREAK-POINT)

Inserido diretamente no código fonte.

```abap
REPORT z_breakpoint_demo.

DATA: lv_contador TYPE i,
      lv_total TYPE i.

START-OF-SELECTION.
  
  DO 10 TIMES.
    lv_contador = sy-index.
    
    BREAK-POINT.  " ⛔ Execução para aqui
    
    lv_total = lv_total + lv_contador.
  ENDLOOP.
  
  WRITE: / |Total: { lv_total }|.
```

**Características:**
- ✅ Permanente até ser removido do código
- ✅ Funciona para todos os utilizadores
- ❌ Deve ser removido antes de transportar
- ❌ Ativado mesmo em produção (perigoso!)

**Variante para Utilizador Específico:**
```abap
" Apenas ativa para o seu utilizador
BREAK sy-uname.

" Apenas para utilizador específico
BREAK 'JSILVA'.
```

---

### 2️⃣ Breakpoint Dinâmico (Session)

Definido na GUI sem alterar código.

**Como criar:**
1. Abrir programa no editor (SE38/SE80)
2. Clicar na linha desejada
3. Pressionar **F7** ou clicar no início da linha
4. Aparece um ⛔ vermelho

**Características:**
- ✅ Não altera código
- ✅ Apenas para a sua sessão
- ❌ Perdido ao fechar sessão
- ✅ Ideal para desenvolvimento

**Atalhos:**
- **F7** - Criar/remover breakpoint
- **Shift+F7** - Desativar breakpoint temporariamente

---

### 3️⃣ Breakpoint Condicional

Para apenas quando uma condição é verdadeira.

**Como criar:**
1. Criar breakpoint dinâmico (F7)
2. No debugger, clicar com botão direito no breakpoint
3. Selecionar "Condition"
4. Inserir condição

**Exemplos de condições:**

```abap
" Parar quando contador for maior que 100
lv_contador > 100

" Parar quando campo específico tiver valor
ls_cliente-nome = 'João Silva'

" Parar quando tabela tiver mais de 1000 linhas
lines( lt_dados ) > 1000

" Condição complexa
lv_valor > 1000 AND ls_data-mes = '12'
```

**Exemplo prático:**

```abap
SELECT * FROM sflight INTO TABLE @DATA(lt_voos).

LOOP AT lt_voos INTO DATA(ls_voo).
  " Breakpoint aqui com condição: ls_voo-price > 1000
  
  WRITE: / |{ ls_voo-carrid } { ls_voo-connid } - { ls_voo-price }|.
ENDLOOP.
```

---

### 4️⃣ Breakpoint em Statement

Para quando um statement específico é executado.

**Como criar:**
1. Menu: Breakpoints → Create Statement Breakpoint
2. Inserir o statement (ex: `SELECT`, `CALL FUNCTION`)

**Exemplos:**

```abap
" Parar em todos os SELECTs
Breakpoint em: SELECT

" Parar em chamadas de função específica
Breakpoint em: CALL FUNCTION 'BAPI_CUSTOMER_GET'

" Parar em métodos de classe
Breakpoint em: CALL METHOD
```

---

### 5️⃣ Breakpoint HTTP (Debug Web)

Para debugar aplicações web (BSP, WebDynpro, Fiori).

**Como ativar:**
1. Executar `/h` no SAP GUI
2. Abrir aplicação web no browser
3. Execução para no primeiro statement ABAP

**Ou usar o Debugger URL:**
```
?sap-client=100&sap-user=USUARIO&sap-password=SENHA&SAP-DEBUGGER=X
```

---

### 6️⃣ Breakpoint em Exceção

Para quando uma exceção é lançada.

**Como criar:**
1. Menu Debugger: Settings → User-Specific Settings
2. Aba "Exceptions"
3. Selecionar classes de exceção

**Exemplo:**
```abap
TRY.
    DATA(lv_result) = 10 / 0.  " ⛔ Para aqui em CX_SY_ZERODIVIDE
  CATCH cx_sy_zerodivide.
    WRITE: / 'Divisão por zero!'.
ENDTRY.
```

---

### 7️⃣ Breakpoint em System Field

Para quando um system field muda.

**Exemplo:**
- Breakpoint quando `sy-subrc <> 0`
- Breakpoint quando `sy-tabix = 50`

---

## 🛠️ Gestão de Breakpoints

### Ver Todos os Breakpoints

**Transaction:** `/h` → Settings → Display/Delete Breakpoints

Ou menu: **Utilities → Settings → ABAP Debugger → Breakpoints**

### Desativar Temporariamente

- Clicar com botão direito → Deactivate
- Ícone muda para ⭕ (vazio)

### Apagar Breakpoints

**Apagar todos:**
```abap
" No debugger
/hdel
```

**Apagar um específico:**
- F7 na linha do breakpoint

---

## 💡 Exemplos Práticos

### Exemplo 1: Debug de LOOP com Condição

```abap
REPORT z_debug_loop.

DATA: lt_clientes TYPE TABLE OF kna1.

SELECT * FROM kna1 INTO TABLE lt_clientes UP TO 1000 ROWS.

LOOP AT lt_clientes INTO DATA(ls_cliente).
  " ⛔ Breakpoint condicional aqui
  " Condição: ls_cliente-land1 = 'PT'
  
  IF ls_cliente-land1 = 'PT'.
    WRITE: / ls_cliente-name1.
  ENDIF.
ENDLOOP.
```

### Exemplo 2: Debug de SELECT com Performance

```abap
REPORT z_debug_select.

DATA: lt_voos TYPE TABLE OF sflight.

" ⛔ Breakpoint antes do SELECT
SELECT * FROM sflight INTO TABLE lt_voos
  WHERE carrid = 'LH'
    AND connid = '0400'.

" ⛔ Breakpoint depois do SELECT para ver resultado
WRITE: / |Registos encontrados: { lines( lt_voos ) }|.
```

### Exemplo 3: Debug de Chamada de Função

```abap
REPORT z_debug_function.

DATA: lv_customer TYPE kna1-kunnr VALUE '0000001000'.

" ⛔ Breakpoint antes da chamada
CALL FUNCTION 'BAPI_CUSTOMER_GETDETAIL'
  EXPORTING
    customerno = lv_customer
  EXCEPTIONS
    OTHERS     = 1.

" ⛔ Breakpoint depois para ver sy-subrc
IF sy-subrc <> 0.
  WRITE: / 'Erro ao buscar cliente'.
ENDIF.
```

### Exemplo 4: Debug com Variáveis Inline

```abap
REPORT z_debug_inline.

SELECT * FROM sflight INTO TABLE @DATA(lt_voos) UP TO 10 ROWS.

" ⛔ Breakpoint aqui - lt_voos já está populada
DATA(lv_total) = REDUCE i( INIT sum = 0
                            FOR wa IN lt_voos
                            NEXT sum = sum + wa-seatsocc ).

WRITE: / |Total de lugares ocupados: { lv_total }|.
```

---

## 🎓 Boas Práticas

### ✅ Fazer

```abap
" 1. Usar breakpoints condicionais em LOOPs grandes
LOOP AT lt_tabela INTO DATA(ls_linha).
  " Condição: sy-tabix = 500
  " Evita parar 499 vezes!
ENDLOOP.

" 2. Usar BREAK sy-uname em desenvolvimento
BREAK sy-uname.  " Só para você

" 3. Comentar breakpoints estáticos
" BREAK-POINT.  " TODO: Remover antes de transportar

" 4. Usar breakpoints de statement para análise
" Em vez de colocar 50 breakpoints, usar breakpoint em SELECT
```

### ❌ Evitar

```abap
" 1. BREAK-POINT em produção
BREAK-POINT.  " ❌ NUNCA em código produtivo!

" 2. Breakpoints sem condição em LOOPs grandes
LOOP AT lt_tabela INTO DATA(ls_linha).  " 1.000.000 de linhas
  BREAK-POINT.  " ❌ Vai parar 1 milhão de vezes!
ENDLOOP.

" 3. Esquecer breakpoints dinâmicos ativos
" Verificar sempre: Utilities → Settings → Display Breakpoints

" 4. Usar breakpoint quando watchpoint seria melhor
" Se quer saber QUANDO um valor muda, use watchpoint!
```

---

## 🔍 Troubleshooting

### Breakpoint Não Para

**Possíveis causas:**
1. Código não é executado (lógica condicional)
2. Breakpoint desativado
3. Debugger desligado
4. Código em RFC/Background job (precisa configuração especial)

**Solução:**
```abap
" Adicionar WRITE antes do breakpoint para confirmar execução
WRITE: / 'Antes do breakpoint'.
BREAK-POINT.
```

### Breakpoint Desaparece

**Causa:** Breakpoints dinâmicos são perdidos ao:
- Fechar sessão
- Recompilar programa
- Mudar de utilizador

**Solução:** Usar breakpoints estáticos temporários ou anotar as linhas.

### Performance Degradada

**Causa:** Muitos breakpoints ativos

**Solução:**
```abap
" Limpar todos
/hdel
```

---

## 🔗 Atalhos Úteis

| Atalho | Ação |
|--------|------|
| **F7** | Criar/Remover breakpoint |
| **Shift+F7** | Desativar breakpoint |
| **/h** | Ativar debugger |
| **/hdel** | Apagar todos breakpoints |
| **Ctrl+Shift+F7** | Ver todos breakpoints |

---

## 🔗 Próximos Passos

- **[Watchpoints](2_watchpoints.md)** - Monitorizar mudanças em variáveis
- **[Debugger](3_debugger.md)** - Navegar no debugger
- **[ST22](4_st22.md)** - Analisar dumps

---

**Tags:** `#Debug` `#Breakpoints` `#ABAP` `#Desenvolvimento` `#Troubleshooting`
