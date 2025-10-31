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

## 🎯 Tipos de Debug

### 1️⃣ Breakpoint Estático
Inserir `BREAK-POINT` diretamente no código.

**Exemplo:**
```abap
DATA lv_valor TYPE i VALUE 10.
BREAK-POINT.  " ← Execução para aqui
lv_valor = lv_valor * 2.
WRITE: / lv_valor.
```

### 2️⃣ Breakpoint Dinâmico
Definir na GUI sem alterar código (F7 ou duplo clique na linha).

### 3️⃣ Breakpoint Condicional
Para apenas quando uma condição é verdadeira.

```abap
" No debugger: adicionar condição ao breakpoint
" Exemplo: lv_contador > 100
```

### 4️⃣ Watchpoint
Monitorizar mudanças em variáveis específicas.

---

## 🛠️ Ferramentas de Debug

### Debugger Clássico
- `/h` antes de executar o programa
- F5 → Step into (entrar em método)
- F6 → Step over (executar linha)
- F7 → Return (voltar ao chamador)
- F8 → Continue (até próximo breakpoint)

### Novo Debugger
- Mais visual e com abas
- Melhor para análise de objetos
- Ativado por padrão em SAP GUI recentes

### Análise de Dumps (ST22)
- Ver erros em runtime
- Call stack completo
- Valores de variáveis no momento do erro

### Runtime Analysis (SAT)
- Medir performance do código
- Identificar gargalos
- Ver tempo gasto por método/função

---

## 📚 Tutoriais

### [Breakpoints](breakpoint.md)
Guia completo sobre tipos e uso de breakpoints.

### Exercícios Práticos
- `ex01.md` → Debug básico
- `ex02.md` → Watchpoints
- `ex03.md` → Debug em LOOPs
- `ex04.md` → Análise de dumps
- `ex05.md` → Runtime analysis

---

## 💡 Exemplo Prático

```abap
REPORT z_debug_demo.

DATA: lt_voos TYPE TABLE OF sflight,
      lv_soma TYPE p DECIMALS 2.

START-OF-SELECTION.
  
  " ✅ Colocar breakpoint aqui (F7)
  SELECT * FROM sflight INTO TABLE lt_voos UP TO 100 ROWS.
  
  LOOP AT lt_voos INTO DATA(ls_voo).
    lv_soma = lv_soma + ls_voo-price.
    
    " ✅ Breakpoint condicional: ls_voo-price > 1000
    IF ls_voo-price > 1000.
      WRITE: / |Voo caro: { ls_voo-carrid } { ls_voo-connid }|.
    ENDIF.
  ENDLOOP.
  
  WRITE: / |Soma total: { lv_soma }|.
```

### Como debugar este código:

1. Executar com `/h` ou F7 na primeira linha
2. Usar F6 para ir linha a linha
3. Inspecionar `lt_voos` na aba "Variáveis"
4. Adicionar watchpoint em `lv_soma`
5. Usar F8 para continuar até ao próximo breakpoint

---

## 🚨 Boas Práticas

### ✅ Fazer
- Remover `BREAK-POINT` antes de transportar código
- Usar breakpoints condicionais para LOOPs grandes
- Usar watchpoints para rastrear mudanças inesperadas
- Analisar ST22 para entender erros

### ❌ Evitar
- Deixar `BREAK-POINT` em código produtivo
- Debugar sem ter hipóteses sobre o problema
- Ignorar o call stack

---

## 🔗 Próximos Passos

1. Leia [Breakpoints](breakpoint.md)
2. Pratique com programas simples
3. Experimente os exercícios `ex01.md` a `ex05.md`
4. Aprenda [Performance](../performance/index.md) para otimizar código
