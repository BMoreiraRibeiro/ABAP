# 🧩 Fundamentos ABAP

Esta secção introduz os **conceitos fundamentais da linguagem ABAP** — a base essencial de todo o desenvolvimento SAP.

---

## 📖 O que vais aprender

- Tipos de dados primitivos e complexos
- Declaração e uso de variáveis e constantes
- Trabalhar com tabelas internas (arrays ABAP)
- Field-symbols e referências de dados
- Estruturas e tipos customizados
- Criar ecrãs de seleção básicos
- Tratamento de exceções com TRY/CATCH

---

## 🎯 Ordem de Aprendizagem

### 1️⃣ [Tipos de Dados](tipos_de_dados.md)
Tipos elementares: `i`, `p`, `c`, `string`, `d`, `t` e como usá-los.

### 2️⃣ [Variáveis e Constantes](variaveis_constantes.md)
Declaração com `DATA`, `CONSTANTS` e boas práticas de nomenclatura.

### 3️⃣ [Tabelas Internas](tabelas_internas.md)
Como criar, manipular e iterar sobre coleções de dados em memória.

### 4️⃣ [Estruturas](estruturas.md)
Agrupar campos relacionados numa única entidade.

### 5️⃣ [Field-Symbols e Referências](field_symbols_refs.md)
Apontadores e referências dinâmicas para dados.

### 6️⃣ [Ecrãs de Seleção](select_screen_basico.md)
Criar interfaces simples para entrada de parâmetros.

### 7️⃣ [Tratamento de Exceções](excecoes_try_catch.md)
Gerir erros de forma estruturada com `TRY...CATCH`.

### 8️⃣ [Tipos Customizados](tipos_customizados.md)
Definir tipos próprios localmente ou globalmente.

---

## 💡 Exemplo Rápido

```abap
REPORT z_fundamentos_demo.

" Declarar variáveis
DATA: lv_nome  TYPE string VALUE 'Bruno',
      lv_idade TYPE i VALUE 25.

" Tabela interna
TYPES: BEGIN OF ty_pessoa,
         nome  TYPE string,
         idade TYPE i,
       END OF ty_pessoa.

DATA lt_pessoas TYPE TABLE OF ty_pessoa.

" Adicionar dados
APPEND VALUE #( nome = lv_nome idade = lv_idade ) TO lt_pessoas.
APPEND VALUE #( nome = 'Ana' idade = 30 ) TO lt_pessoas.

" Iterar
LOOP AT lt_pessoas INTO DATA(ls_pessoa).
  WRITE: / |{ ls_pessoa-nome } tem { ls_pessoa-idade } anos|.
ENDLOOP.
```

---

## 🚀 Próximos Passos

1. Comece por [Tipos de Dados](tipos_de_dados.md)
2. Siga a ordem numérica acima
3. Execute os exemplos no seu sistema SAP
4. Depois avance para [SQL ABAP](../sql/index.md)
