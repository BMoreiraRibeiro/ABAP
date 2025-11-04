# 🐞 Debug e Análise

Ferramentas e técnicas para **depurar programas ABAP**, usar breakpoints e analisar o comportamento em tempo de execução.

---

## 📖 O que vais aprender

- Ativar e usar o debugger clássico e novo
- Tipos de breakpoints (estáticos, dinâmicos, condicionais)
- Análise de variáveis e tabelas internas
- Watchpoints para detetar mudanças em campos
- Call Stack e navegação entre chamadas
- Debugging de jobs em background
- Análise de dumps (ST22)
- Runtime Analysis (SAT, SE30)

---

## 🎯 Ordem de Aprendizagem

### 1️⃣ [Breakpoints](1_breakpoints.md)
Tipos de breakpoints e como usá-los eficientemente.

**Tópicos:**
- Breakpoint estático vs dinâmico
- Breakpoints condicionais
- Breakpoints HTTP e de exceção
- Gestão de breakpoints

### 2️⃣ [Watchpoints](2_watchpoints.md)
Monitorizar mudanças em variáveis durante a execução.

**Tópicos:**
- Criar watchpoints
- Watchpoints com condições
- Watchpoints em estruturas e tabelas
- Performance de watchpoints

### 3️⃣ [Debugger Clássico vs Novo](3_debugger.md)
Comparação e uso dos dois debuggers disponíveis.

**Tópicos:**
- Diferenças entre debuggers
- Atalhos de teclado
- Quando usar cada um
- Personalização do layout

### 4️⃣ [Análise de Dumps (ST22)](4_st22.md)
Interpretar e resolver erros em runtime.

**Tópicos:**
- Estrutura de um dump
- Erros comuns e soluções
- Call stack
- Análise de variáveis no erro

### 5️⃣ [Runtime Analysis (SAT)](5_sat.md)
Medir e otimizar performance de programas.

**Tópicos:**
- Criar medições
- Interpretar resultados
- Identificar gargalos
- Comparar antes/depois

### 6️⃣ [Debug de Jobs em Background](6_debug_jobs.md)
Técnicas para debugar processos em background.

**Tópicos:**
- Método JDBG
- Breakpoints externos
- SM37 e SM50
- Logs de aplicação

---

## 🛠️ Ferramentas Principais

### Debugger
- `/h` → Ativar debugger
- `/hc` → Debugger clássico
- `/hs` → Novo debugger
- `/hdel` → Apagar breakpoints

### Atalhos Essenciais
- **F5** → Step into (entrar em método)
- **F6** → Step over (executar linha)
- **F7** → Return (voltar ao chamador)
- **F8** → Continue (até próximo breakpoint)
- **Shift+F12** → Criar watchpoint

### Transactions
- **ST22** → Análise de dumps
- **SAT (SE30)** → Runtime Analysis
- **ST05** → SQL Trace
- **SM37** → Background Jobs
- **SM50** → Process Overview

---

## 💡 Exemplo Rápido

```abap
REPORT z_debug_demo.

DATA: lt_voos TYPE TABLE OF sflight,
      lv_soma TYPE p DECIMALS 2.

START-OF-SELECTION.
  
  " ⛔ Breakpoint aqui (F7)
  SELECT * FROM sflight INTO TABLE lt_voos UP TO 100 ROWS.
  
  LOOP AT lt_voos INTO DATA(ls_voo).
    " 👁️ Watchpoint em lv_soma
    lv_soma = lv_soma + ls_voo-price.
    
    " ⛔ Breakpoint condicional: ls_voo-price > 1000
    IF ls_voo-price > 1000.
      WRITE: / |Voo caro: { ls_voo-carrid } { ls_voo-connid }|.
    ENDIF.
  ENDLOOP.
  
  WRITE: / |Soma total: { lv_soma }|.
```

---

## 🚨 Boas Práticas

### ✅ Fazer
- Remover `BREAK-POINT` antes de transportar código
- Usar breakpoints condicionais para LOOPs grandes
- Analisar ST22 para entender erros
- Medir com SAT antes de otimizar
- Usar JDBG para debug de jobs

### ❌ Evitar
- Deixar `BREAK-POINT` em código produtivo
- Debugar sem ter hipóteses sobre o problema
- Ignorar o call stack
- Otimizar sem medir
- Debug em produção sem autorização

---

## 🔗 Próximos Passos

1. Comece por [Breakpoints](1_breakpoints.md)
2. Aprenda [Watchpoints](2_watchpoints.md) para casos avançados
3. Domine o [Debugger](3_debugger.md) para navegar eficientemente
4. Use [ST22](4_st22.md) quando encontrar erros
5. Otimize com [SAT](5_sat.md) após identificar problemas
6. Explore [Performance](../performance/index.md) para técnicas de otimização

---

**Tags:** `#Debug` `#Breakpoint` `#Watchpoint` `#ST22` `#SAT` `#Performance`
