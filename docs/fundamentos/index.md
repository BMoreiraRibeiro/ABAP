---
tags:
  - ABAP
  - Fundamentos
  - Iniciante
---

# 🧩 Fundamentos ABAP

Esta secção introduz os **conceitos fundamentais da linguagem ABAP** — a base essencial de todo o desenvolvimento SAP.

---

## 📖 O que vais aprender

- Tipos de dados primitivos e complexos
- Declaração e uso de variáveis e constantes
- Trabalhar com tabelas internas (arrays ABAP)
- Estruturas e tipos customizados
- Condições e operadores lógicos
- Loops e iterações
- Expressões modernas ABAP (7.40+)
- Orientação a objetos básica

---

## 🎯 Ordem de Aprendizagem

### 1️⃣ [Variáveis e Tipos de Dados](1_variaveis_tipo_dados.md)
Tipos elementares (`i`, `p`, `c`, `string`, `d`, `t`), declaração de variáveis e constantes.

### 2️⃣ [Estruturas](2_estruturas.md)
Agrupar campos relacionados numa única entidade com `TYPES BEGIN OF` e `STRUCTURES`.

### 3️⃣ [Condições e Lógica](3_condicoes_logica.md)
`IF`, `CASE`, operadores lógicos e comparações.

### 4️⃣ [Loops](4_loops.md)
`LOOP AT`, `DO`, `WHILE` e como iterar sobre dados.

### 5️⃣ [Expressões](5_expressoes.md)
Expressões modernas ABAP: `VALUE`, `CORRESPONDING`, `FILTER`, operador `|...|`.

### 6️⃣ [Tabelas Internas](6_tabelas_internas.md)
Como criar, manipular e iterar sobre coleções de dados em memória.

### 7️⃣ [Orientação a Objetos Básica](7_OO_basica.md)
Classes, objetos, métodos, atributos e encapsulamento.

---

## 💡 Exemplo Rápido

```abap
REPORT z_fundamentos_demo.

" 1. Declarar variáveis
DATA: lv_nome  TYPE string VALUE 'Bruno',
      lv_idade TYPE i VALUE 25.

" 2. Estrutura
TYPES: BEGIN OF ty_pessoa,
         nome  TYPE string,
         idade TYPE i,
       END OF ty_pessoa.

" 3. Tabela interna
DATA lt_pessoas TYPE TABLE OF ty_pessoa.

" 4. Adicionar dados (expressão moderna)
lt_pessoas = VALUE #(
  ( nome = 'Bruno' idade = 25 )
  ( nome = 'Ana'   idade = 30 )
  ( nome = 'João'  idade = 28 )
).

" 5. Iterar com inline declaration
LOOP AT lt_pessoas INTO DATA(ls_pessoa).
  WRITE: / |{ ls_pessoa-nome } tem { ls_pessoa-idade } anos|.
ENDLOOP.

" 6. Condição
IF lv_idade >= 18.
  WRITE: / 'É maior de idade'.
ENDIF.
```

---

## 🚀 Próximos Passos

1. Comece por [Variáveis e Tipos de Dados](1_variaveis_tipo_dados.md)
2. Siga a ordem numérica acima (1→7)
3. Execute os exemplos no seu sistema SAP
4. Depois avance para [SQL ABAP](../sql/index.md)
