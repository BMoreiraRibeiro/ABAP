# Estruturas em ABAP

As estruturas agrupam múltiplos campos de tipos diferentes sob um único nome.

---

### 🔹 Exemplo

```abap
TYPES: BEGIN OF ty_aluno,
         nome TYPE string,
         idade TYPE i,
         curso TYPE string,
       END OF ty_aluno.

DATA ls_aluno TYPE ty_aluno.
ls_aluno-nome = 'Cristiano'.
ls_aluno-idade = 21.
ls_aluno-curso = 'Engenharia'.

WRITE: / ls_aluno-nome, ls_aluno-curso.
```

---

💡 Estruturas são essenciais para tabelas internas (linhas são do tipo de estrutura).  
