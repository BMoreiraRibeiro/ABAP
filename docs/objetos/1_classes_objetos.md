---
tags:
  - ABAP
  - OO
  - Classes
  - Objetos
  - Programação
---

# Classes e Objetos em ABAP

A Programação Orientada a Objetos (OOP) permite criar código mais modular, reutilizável e fácil de manter.

---

## 🔹 O que é uma Classe?

Uma **classe** é um modelo (template) que define:
- **Atributos** (dados/variáveis)
- **Métodos** (comportamentos/funções)

Um **objeto** é uma instância (exemplar) de uma classe.

---

## 🔹 Estrutura de uma Classe

```abap
CLASS lcl_pessoa DEFINITION.
  PUBLIC SECTION.
    " Atributos públicos
    DATA: mv_nome  TYPE string,
          mv_idade TYPE i.
    
    " Métodos públicos
    METHODS: 
      set_nome IMPORTING iv_nome TYPE string,
      get_nome RETURNING VALUE(rv_nome) TYPE string,
      apresentar.
      
  PRIVATE SECTION.
    " Atributos privados
    DATA: mv_saldo TYPE p DECIMALS 2.
    
    " Métodos privados
    METHODS: calcular_bonus RETURNING VALUE(rv_bonus) TYPE p DECIMALS 2.
ENDCLASS.

CLASS lcl_pessoa IMPLEMENTATION.
  METHOD set_nome.
    mv_nome = iv_nome.
  ENDMETHOD.
  
  METHOD get_nome.
    rv_nome = mv_nome.
  ENDMETHOD.
  
  METHOD apresentar.
    WRITE: / |Olá, eu sou { mv_nome } e tenho { mv_idade } anos.|.
  ENDMETHOD.
  
  METHOD calcular_bonus.
    rv_bonus = mv_saldo * '0.10'.
  ENDMETHOD.
ENDCLASS.
```

---

## 🔹 Criar e Usar Objetos

```abap
REPORT z_oo_exemplo.

" 1. Definir a classe (acima)

START-OF-SELECTION.

  " 2. Declarar referência ao objeto
  DATA: lo_pessoa TYPE REF TO lcl_pessoa.
  
  " 3. Criar instância (objeto)
  CREATE OBJECT lo_pessoa.
  
  " 4. Usar o objeto
  lo_pessoa->set_nome( 'Bruno' ).
  lo_pessoa->mv_idade = 25.
  lo_pessoa->apresentar( ).
  
  " 5. Verificar se objeto existe
  IF lo_pessoa IS BOUND.
    WRITE: / 'Objeto existe e está ativo'.
  ENDIF.
```

**Saída:**
```
Olá, eu sou Bruno e tenho 25 anos.
Objeto existe e está ativo
```

---

## 🔹 Sintaxe Moderna: NEW

```abap
" ✅ Sintaxe moderna (ABAP 7.40+)
DATA(lo_pessoa) = NEW lcl_pessoa( ).
lo_pessoa->set_nome( 'Ana' ).
lo_pessoa->mv_idade = 30.

" Inline com uso direto
NEW lcl_pessoa( )->set_nome( 'João' )->apresentar( ).
```

---

## 🔹 Visibilidade de Seções

| Seção | Acesso | Uso |
|-------|--------|-----|
| **PUBLIC** | Dentro e fora da classe | Interface pública |
| **PROTECTED** | Classe e subclasses | Herança |
| **PRIVATE** | Apenas dentro da classe | Implementação interna |

```abap
CLASS lcl_conta DEFINITION.
  PUBLIC SECTION.
    METHODS: 
      depositar IMPORTING iv_valor TYPE p DECIMALS 2,
      get_saldo RETURNING VALUE(rv_saldo) TYPE p DECIMALS 2.
      
  PROTECTED SECTION.
    DATA: mv_saldo TYPE p DECIMALS 2.
    
  PRIVATE SECTION.
    DATA: mv_senha TYPE string.
    METHODS: validar_senha IMPORTING iv_senha TYPE string
                           RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.

CLASS lcl_conta IMPLEMENTATION.
  METHOD depositar.
    mv_saldo = mv_saldo + iv_valor.
  ENDMETHOD.
  
  METHOD get_saldo.
    rv_saldo = mv_saldo.
  ENDMETHOD.
  
  METHOD validar_senha.
    rv_ok = COND #( WHEN iv_senha = mv_senha THEN abap_true
                    ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.
```

---

## 🔹 Classes Locais vs Globais

### Classes Locais

Definidas **dentro** de um programa (report, function group, etc.).

```abap
REPORT z_classe_local.

" Definição local
CLASS lcl_calculadora DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: somar IMPORTING iv_a TYPE i
                                   iv_b TYPE i
                         RETURNING VALUE(rv_soma) TYPE i.
ENDCLASS.

CLASS lcl_calculadora IMPLEMENTATION.
  METHOD somar.
    rv_soma = iv_a + iv_b.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lv_resultado) = lcl_calculadora=>somar( iv_a = 10 iv_b = 20 ).
  WRITE: / lv_resultado.
```

### Classes Globais

Criadas na **SE24** ou **SE80**, reutilizáveis em todo o sistema.

```abap
" Usar classe global ZCL_UTILS
DATA(lo_utils) = NEW zcl_utils( ).
lo_utils->metodo_publico( ).
```

---

## 🔹 Métodos de Instância vs Métodos Estáticos

### Métodos de Instância

Requerem um objeto (`lo_obj->metodo()`):

```abap
CLASS lcl_contador DEFINITION.
  PUBLIC SECTION.
    DATA: mv_contador TYPE i.
    METHODS: incrementar.
ENDCLASS.

CLASS lcl_contador IMPLEMENTATION.
  METHOD incrementar.
    mv_contador = mv_contador + 1.
  ENDMETHOD.
ENDCLASS.

" Uso
DATA(lo_cont1) = NEW lcl_contador( ).
lo_cont1->incrementar( ).
lo_cont1->incrementar( ).
WRITE: / lo_cont1->mv_contador.  " 2

DATA(lo_cont2) = NEW lcl_contador( ).
lo_cont2->incrementar( ).
WRITE: / lo_cont2->mv_contador.  " 1
```

### Métodos Estáticos (CLASS-METHODS)

Não requerem objeto (`lcl_classe=>metodo()`):

```abap
CLASS lcl_utils DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: 
      maiuscula IMPORTING iv_texto TYPE string
                RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.
  METHOD maiuscula.
    rv_result = to_upper( iv_texto ).
  ENDMETHOD.
ENDCLASS.

" Uso (sem criar objeto)
DATA(lv_texto) = lcl_utils=>maiuscula( 'olá mundo' ).
WRITE: / lv_texto.  " OLÁ MUNDO
```

---

## 🔹 Exemplo Completo: Sistema de Biblioteca

```abap
*&---------------------------------------------------------------------*
*& Report Z_OO_BIBLIOTECA
*&---------------------------------------------------------------------*
REPORT z_oo_biblioteca.

" ============ DEFINIÇÕES ============
CLASS lcl_livro DEFINITION.
  PUBLIC SECTION.
    DATA: mv_titulo TYPE string,
          mv_autor  TYPE string,
          mv_isbn   TYPE string,
          mv_disponivel TYPE abap_bool.
          
    METHODS: 
      constructor IMPORTING iv_titulo TYPE string
                            iv_autor  TYPE string
                            iv_isbn   TYPE string,
      emprestar,
      devolver,
      get_info RETURNING VALUE(rv_info) TYPE string.
ENDCLASS.

CLASS lcl_biblioteca DEFINITION.
  PUBLIC SECTION.
    METHODS: 
      adicionar_livro IMPORTING io_livro TYPE REF TO lcl_livro,
      listar_livros,
      buscar_por_isbn IMPORTING iv_isbn TYPE string
                      RETURNING VALUE(ro_livro) TYPE REF TO lcl_livro.
      
  PRIVATE SECTION.
    DATA: mt_livros TYPE TABLE OF REF TO lcl_livro.
ENDCLASS.

" ============ IMPLEMENTAÇÕES ============
CLASS lcl_livro IMPLEMENTATION.
  METHOD constructor.
    mv_titulo = iv_titulo.
    mv_autor = iv_autor.
    mv_isbn = iv_isbn.
    mv_disponivel = abap_true.
  ENDMETHOD.
  
  METHOD emprestar.
    IF mv_disponivel = abap_true.
      mv_disponivel = abap_false.
      WRITE: / |Livro "{ mv_titulo }" emprestado com sucesso.|.
    ELSE.
      WRITE: / |Livro "{ mv_titulo }" já está emprestado.|.
    ENDIF.
  ENDMETHOD.
  
  METHOD devolver.
    mv_disponivel = abap_true.
    WRITE: / |Livro "{ mv_titulo }" devolvido.|.
  ENDMETHOD.
  
  METHOD get_info.
    DATA(lv_status) = COND string( WHEN mv_disponivel = abap_true 
                                   THEN 'Disponível' 
                                   ELSE 'Emprestado' ).
    rv_info = |{ mv_titulo } - { mv_autor } ({ lv_status })|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_biblioteca IMPLEMENTATION.
  METHOD adicionar_livro.
    APPEND io_livro TO mt_livros.
    WRITE: / |Livro adicionado: { io_livro->mv_titulo }|.
  ENDMETHOD.
  
  METHOD listar_livros.
    WRITE: / 'LIVROS NA BIBLIOTECA:', /.
    ULINE.
    LOOP AT mt_livros INTO DATA(lo_livro).
      WRITE: / lo_livro->get_info( ).
    ENDLOOP.
  ENDMETHOD.
  
  METHOD buscar_por_isbn.
    LOOP AT mt_livros INTO DATA(lo_livro).
      IF lo_livro->mv_isbn = iv_isbn.
        ro_livro = lo_livro.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" ============ PROGRAMA PRINCIPAL ============
START-OF-SELECTION.

  " Criar biblioteca
  DATA(lo_bib) = NEW lcl_biblioteca( ).
  
  " Adicionar livros
  lo_bib->adicionar_livro( NEW lcl_livro( 
    iv_titulo = 'Clean Code'
    iv_autor  = 'Robert Martin'
    iv_isbn   = '978-0132350884'
  ) ).
  
  lo_bib->adicionar_livro( NEW lcl_livro( 
    iv_titulo = 'Design Patterns'
    iv_autor  = 'Gang of Four'
    iv_isbn   = '978-0201633610'
  ) ).
  
  SKIP.
  
  " Listar livros
  lo_bib->listar_livros( ).
  
  SKIP.
  
  " Emprestar livro
  DATA(lo_livro) = lo_bib->buscar_por_isbn( '978-0132350884' ).
  IF lo_livro IS BOUND.
    lo_livro->emprestar( ).
  ENDIF.
  
  SKIP.
  
  " Listar novamente
  lo_bib->listar_livros( ).
```

---

## 💡 Boas Práticas

| ✅ Fazer | ❌ Evitar |
|---------|-----------|
| Usar nomes descritivos | Abreviaturas confusas |
| Métodos pequenos e focados | Métodos gigantes |
| Encapsular dados (private) | Tudo público |
| Usar NEW quando possível | CREATE OBJECT desnecessário |
| Verificar IS BOUND | Usar objetos sem verificar |

---

## 🚀 Próximo Passo

Aprenda sobre [Atributos e Métodos](2_atributos_metodos.md) em detalhe.
