---
tags:
  - ABAP
  - OO
  - Herança
  - Polimorfismo
  - Programação
---

# Herança em ABAP

## 📋 Visão Geral

**Herança** permite criar uma nova classe (subclasse) baseada numa classe existente (superclasse), reutilizando e estendendo funcionalidades.

---

## 🎯 Conceitos Fundamentais

- **Superclasse**: Classe base (pai)
- **Subclasse**: Classe derivada (filho) que herda da superclasse
- **INHERITING FROM**: Palavra-chave para herdar
- **Redefinição**: Alterar comportamento de métodos herdados
- **SUPER**: Referência à superclasse

---

## 🔹 Sintaxe Básica

```abap
CLASS lcl_animal DEFINITION.
  PUBLIC SECTION.
    DATA: mv_nome TYPE string.
    
    METHODS:
      constructor IMPORTING iv_nome TYPE string,
      fazer_som,
      mover.
      
  PROTECTED SECTION.
    DATA: mv_energia TYPE i VALUE 100.
ENDCLASS.

CLASS lcl_animal IMPLEMENTATION.
  METHOD constructor.
    mv_nome = iv_nome.
  ENDMETHOD.
  
  METHOD fazer_som.
    WRITE: / |{ mv_nome } faz um som genérico|.
  ENDMETHOD.
  
  METHOD mover.
    mv_energia = mv_energia - 10.
    WRITE: / |{ mv_nome } moveu-se. Energia: { mv_energia }|.
  ENDMETHOD.
ENDCLASS.

" ========== SUBCLASSE ==========
CLASS lcl_cao DEFINITION INHERITING FROM lcl_animal.
  PUBLIC SECTION.
    METHODS:
      fazer_som REDEFINITION,
      abanar_cauda.
ENDCLASS.

CLASS lcl_cao IMPLEMENTATION.
  METHOD fazer_som.
    WRITE: / |{ mv_nome } faz: Au au!|.
  ENDMETHOD.
  
  METHOD abanar_cauda.
    WRITE: / |{ mv_nome } está a abanar a cauda!|.
  ENDMETHOD.
ENDCLASS.
```

**Uso:**

```abap
START-OF-SELECTION.
  DATA(lo_cao) = NEW lcl_cao( iv_nome = 'Rex' ).
  lo_cao->fazer_som( ).      " Au au! (redefinido)
  lo_cao->mover( ).          " Herdado da superclasse
  lo_cao->abanar_cauda( ).   " Método próprio
```

---

## 🔹 Níveis de Acesso em Herança

| Seção | Superclasse | Subclasse | Fora |
|-------|-------------|-----------|------|
| **PUBLIC** | ✅ | ✅ | ✅ |
| **PROTECTED** | ✅ | ✅ | ❌ |
| **PRIVATE** | ✅ | ❌ | ❌ |

```abap
CLASS lcl_veiculo DEFINITION.
  PUBLIC SECTION.
    DATA: mv_marca TYPE string.  " Acessível em todo o lado
    
  PROTECTED SECTION.
    DATA: mv_velocidade TYPE i.  " Acessível na classe e subclasses
    METHODS: acelerar.
    
  PRIVATE SECTION.
    DATA: mv_num_chassis TYPE string.  " Apenas na própria classe
    METHODS: validar_chassis.
ENDCLASS.

CLASS lcl_carro DEFINITION INHERITING FROM lcl_veiculo.
  PUBLIC SECTION.
    METHODS:
      acelerar REDEFINITION,
      buzinar.
ENDCLASS.

CLASS lcl_carro IMPLEMENTATION.
  METHOD acelerar.
    mv_velocidade = mv_velocidade + 10.  " ✅ PROTECTED - OK
    " mv_num_chassis = '123'.  " ❌ PRIVATE - ERRO!
  ENDMETHOD.
  
  METHOD buzinar.
    WRITE: / 'Biiiiip!'.
  ENDMETHOD.
ENDCLASS.
```

---

## 🔹 Usar SUPER para Chamar Método da Superclasse

```abap
CLASS lcl_pessoa DEFINITION.
  PUBLIC SECTION.
    DATA: mv_nome TYPE string.
    
    METHODS:
      constructor IMPORTING iv_nome TYPE string,
      apresentar.
ENDCLASS.

CLASS lcl_pessoa IMPLEMENTATION.
  METHOD constructor.
    mv_nome = iv_nome.
  ENDMETHOD.
  
  METHOD apresentar.
    WRITE: / |Olá, eu sou { mv_nome }|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_funcionario DEFINITION INHERITING FROM lcl_pessoa.
  PUBLIC SECTION.
    DATA: mv_cargo TYPE string,
          mv_salario TYPE p DECIMALS 2.
    
    METHODS:
      constructor IMPORTING iv_nome TYPE string
                            iv_cargo TYPE string
                            iv_salario TYPE p DECIMALS 2,
      apresentar REDEFINITION.
ENDCLASS.

CLASS lcl_funcionario IMPLEMENTATION.
  METHOD constructor.
    " Chamar construtor da superclasse
    super->constructor( iv_nome ).
    
    " Inicializar atributos próprios
    mv_cargo = iv_cargo.
    mv_salario = iv_salario.
  ENDMETHOD.
  
  METHOD apresentar.
    " Chamar método da superclasse
    super->apresentar( ).
    
    " Adicionar informação extra
    WRITE: / |Trabalho como { mv_cargo } e ganho { mv_salario }€|.
  ENDMETHOD.
ENDCLASS.

" Uso
START-OF-SELECTION.
  DATA(lo_func) = NEW lcl_funcionario(
    iv_nome = 'João'
    iv_cargo = 'Programador'
    iv_salario = '2500.00'
  ).
  
  lo_func->apresentar( ).
  " Saída:
  " Olá, eu sou João
  " Trabalho como Programador e ganho 2500.00€
```

---

## 🔹 Classes Abstratas e Métodos Abstratos

```abap
CLASS lcl_forma DEFINITION ABSTRACT.
  PUBLIC SECTION.
    DATA: mv_cor TYPE string.
    
    METHODS:
      constructor IMPORTING iv_cor TYPE string,
      calcular_area ABSTRACT
        RETURNING VALUE(rv_area) TYPE p DECIMALS 2,
      calcular_perimetro ABSTRACT
        RETURNING VALUE(rv_perimetro) TYPE p DECIMALS 2,
      desenhar.
ENDCLASS.

CLASS lcl_forma IMPLEMENTATION.
  METHOD constructor.
    mv_cor = iv_cor.
  ENDMETHOD.
  
  METHOD desenhar.
    WRITE: / |Desenhando forma de cor { mv_cor }|.
  ENDMETHOD.
ENDCLASS.

" Subclasse: Círculo
CLASS lcl_circulo DEFINITION INHERITING FROM lcl_forma.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_cor TYPE string
                            iv_raio TYPE p DECIMALS 2,
      calcular_area REDEFINITION,
      calcular_perimetro REDEFINITION.
      
  PRIVATE SECTION.
    DATA: mv_raio TYPE p DECIMALS 2.
    CONSTANTS: mc_pi TYPE p DECIMALS 5 VALUE '3.14159'.
ENDCLASS.

CLASS lcl_circulo IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_cor ).
    mv_raio = iv_raio.
  ENDMETHOD.
  
  METHOD calcular_area.
    rv_area = mc_pi * mv_raio * mv_raio.
  ENDMETHOD.
  
  METHOD calcular_perimetro.
    rv_perimetro = 2 * mc_pi * mv_raio.
  ENDMETHOD.
ENDCLASS.

" Subclasse: Retângulo
CLASS lcl_retangulo DEFINITION INHERITING FROM lcl_forma.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_cor TYPE string
                            iv_largura TYPE p DECIMALS 2
                            iv_altura TYPE p DECIMALS 2,
      calcular_area REDEFINITION,
      calcular_perimetro REDEFINITION.
      
  PRIVATE SECTION.
    DATA: mv_largura TYPE p DECIMALS 2,
          mv_altura TYPE p DECIMALS 2.
ENDCLASS.

CLASS lcl_retangulo IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_cor ).
    mv_largura = iv_largura.
    mv_altura = iv_altura.
  ENDMETHOD.
  
  METHOD calcular_area.
    rv_area = mv_largura * mv_altura.
  ENDMETHOD.
  
  METHOD calcular_perimetro.
    rv_perimetro = 2 * ( mv_largura + mv_altura ).
  ENDMETHOD.
ENDCLASS.

" Uso
START-OF-SELECTION.
  " DATA(lo_forma) = NEW lcl_forma( 'azul' ).  " ❌ ERRO: classe abstrata!
  
  DATA(lo_circulo) = NEW lcl_circulo(
    iv_cor = 'vermelho'
    iv_raio = '5.0'
  ).
  
  WRITE: / 'Área do círculo:', lo_circulo->calcular_area( ).
  WRITE: / 'Perímetro:', lo_circulo->calcular_perimetro( ).
  
  DATA(lo_ret) = NEW lcl_retangulo(
    iv_cor = 'azul'
    iv_largura = '10.0'
    iv_altura = '5.0'
  ).
  
  WRITE: / 'Área do retângulo:', lo_ret->calcular_area( ).
```

---

## 🔹 Classes Finais (FINAL)

```abap
" Classe que NÃO pode ser herdada
CLASS lcl_configuracao DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA: sv_ambiente TYPE string.
    CLASS-METHODS: inicializar.
ENDCLASS.

CLASS lcl_configuracao IMPLEMENTATION.
  METHOD inicializar.
    sv_ambiente = 'PRODUÇÃO'.
  ENDMETHOD.
ENDCLASS.

" ❌ ERRO: não se pode herdar de classe FINAL
" CLASS lcl_config_extendida DEFINITION INHERITING FROM lcl_configuracao.
" ENDCLASS.
```

**Métodos Finais:**

```abap
CLASS lcl_base DEFINITION.
  PUBLIC SECTION.
    METHODS:
      metodo_normal,
      metodo_final FINAL.
ENDCLASS.

CLASS lcl_base IMPLEMENTATION.
  METHOD metodo_normal.
    WRITE: / 'Método normal'.
  ENDMETHOD.
  
  METHOD metodo_final.
    WRITE: / 'Método final - não pode ser redefinido'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_derivada DEFINITION INHERITING FROM lcl_base.
  PUBLIC SECTION.
    METHODS:
      metodo_normal REDEFINITION.
      " metodo_final REDEFINITION.  " ❌ ERRO: método é FINAL
ENDCLASS.

CLASS lcl_derivada IMPLEMENTATION.
  METHOD metodo_normal.
    WRITE: / 'Método redefinido'.
  ENDMETHOD.
ENDCLASS.
```

---

## 💡 Exemplo Completo: Sistema de Funcionários

```abap
*&---------------------------------------------------------------------*
*& Report Z_OO_HERANCA_FUNCIONARIOS
*&---------------------------------------------------------------------*
REPORT z_oo_heranca_funcionarios.

" ========== CLASSE BASE ==========
CLASS lcl_funcionario DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_nome TYPE string
                            iv_id TYPE i,
      calcular_salario ABSTRACT
        RETURNING VALUE(rv_salario) TYPE p DECIMALS 2,
      apresentar,
      get_info RETURNING VALUE(rv_info) TYPE string.
      
  PROTECTED SECTION.
    DATA: mv_nome TYPE string,
          mv_id TYPE i,
          mv_anos_servico TYPE i VALUE 0.
          
    METHODS:
      calcular_bonus 
        RETURNING VALUE(rv_bonus) TYPE p DECIMALS 2.
ENDCLASS.

CLASS lcl_funcionario IMPLEMENTATION.
  METHOD constructor.
    mv_nome = iv_nome.
    mv_id = iv_id.
  ENDMETHOD.
  
  METHOD apresentar.
    WRITE: / |ID: { mv_id }, Nome: { mv_nome }|.
    WRITE: / |Salário: { calcular_salario( ) }€|.
  ENDMETHOD.
  
  METHOD get_info.
    rv_info = |{ mv_id } - { mv_nome }|.
  ENDMETHOD.
  
  METHOD calcular_bonus.
    " Bónus de 100€ por ano de serviço
    rv_bonus = mv_anos_servico * 100.
  ENDMETHOD.
ENDCLASS.

" ========== SUBCLASSE: PROGRAMADOR ==========
CLASS lcl_programador DEFINITION INHERITING FROM lcl_funcionario.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_nome TYPE string
                            iv_id TYPE i
                            iv_linguagem TYPE string
                            iv_salario_base TYPE p DECIMALS 2,
      calcular_salario REDEFINITION,
      apresentar REDEFINITION.
      
  PRIVATE SECTION.
    DATA: mv_linguagem TYPE string,
          mv_salario_base TYPE p DECIMALS 2,
          mv_projetos_completos TYPE i VALUE 0.
ENDCLASS.

CLASS lcl_programador IMPLEMENTATION.
  METHOD constructor.
    super->constructor( 
      iv_nome = iv_nome
      iv_id = iv_id
    ).
    mv_linguagem = iv_linguagem.
    mv_salario_base = iv_salario_base.
  ENDMETHOD.
  
  METHOD calcular_salario.
    DATA(lv_bonus_projetos) = mv_projetos_completos * 200.
    rv_salario = mv_salario_base + calcular_bonus( ) + lv_bonus_projetos.
  ENDMETHOD.
  
  METHOD apresentar.
    super->apresentar( ).
    WRITE: / |Linguagem: { mv_linguagem }|.
    WRITE: / |Projetos completos: { mv_projetos_completos }|.
  ENDMETHOD.
ENDCLASS.

" ========== SUBCLASSE: GESTOR ==========
CLASS lcl_gestor DEFINITION INHERITING FROM lcl_funcionario.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_nome TYPE string
                            iv_id TYPE i
                            iv_departamento TYPE string
                            iv_salario_base TYPE p DECIMALS 2,
      calcular_salario REDEFINITION,
      apresentar REDEFINITION,
      adicionar_subordinado IMPORTING io_func TYPE REF TO lcl_funcionario.
      
  PRIVATE SECTION.
    DATA: mv_departamento TYPE string,
          mv_salario_base TYPE p DECIMALS 2,
          mt_subordinados TYPE TABLE OF REF TO lcl_funcionario.
ENDCLASS.

CLASS lcl_gestor IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      iv_nome = iv_nome
      iv_id = iv_id
    ).
    mv_departamento = iv_departamento.
    mv_salario_base = iv_salario_base.
  ENDMETHOD.
  
  METHOD calcular_salario.
    " Gestor ganha bónus por cada subordinado
    DATA(lv_bonus_equipa) = lines( mt_subordinados ) * 300.
    rv_salario = mv_salario_base + calcular_bonus( ) + lv_bonus_equipa.
  ENDMETHOD.
  
  METHOD apresentar.
    super->apresentar( ).
    WRITE: / |Departamento: { mv_departamento }|.
    WRITE: / |Equipa: { lines( mt_subordinados ) } pessoas|.
  ENDMETHOD.
  
  METHOD adicionar_subordinado.
    APPEND io_func TO mt_subordinados.
  ENDMETHOD.
ENDCLASS.

" ========== SUBCLASSE: ESTAGIÁRIO ==========
CLASS lcl_estagiario DEFINITION INHERITING FROM lcl_funcionario.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_nome TYPE string
                            iv_id TYPE i
                            iv_universidade TYPE string,
      calcular_salario REDEFINITION,
      apresentar REDEFINITION.
      
  PRIVATE SECTION.
    DATA: mv_universidade TYPE string.
    CONSTANTS: mc_salario_fixo TYPE p DECIMALS 2 VALUE '800.00'.
ENDCLASS.

CLASS lcl_estagiario IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      iv_nome = iv_nome
      iv_id = iv_id
    ).
    mv_universidade = iv_universidade.
  ENDMETHOD.
  
  METHOD calcular_salario.
    rv_salario = mc_salario_fixo.  " Salário fixo
  ENDMETHOD.
  
  METHOD apresentar.
    super->apresentar( ).
    WRITE: / |Universidade: { mv_universidade }|.
  ENDMETHOD.
ENDCLASS.

" ========== PROGRAMA PRINCIPAL ==========
START-OF-SELECTION.

  " Criar funcionários
  DATA(lo_prog1) = NEW lcl_programador(
    iv_nome = 'João Silva'
    iv_id = 1
    iv_linguagem = 'ABAP'
    iv_salario_base = '2500.00'
  ).
  
  DATA(lo_prog2) = NEW lcl_programador(
    iv_nome = 'Maria Santos'
    iv_id = 2
    iv_linguagem = 'Java'
    iv_salario_base = '2800.00'
  ).
  
  DATA(lo_gestor) = NEW lcl_gestor(
    iv_nome = 'Carlos Mendes'
    iv_id = 3
    iv_departamento = 'TI'
    iv_salario_base = '4000.00'
  ).
  
  DATA(lo_estagiario) = NEW lcl_estagiario(
    iv_nome = 'Ana Costa'
    iv_id = 4
    iv_universidade = 'Universidade de Lisboa'
  ).
  
  " Gestor adiciona subordinados
  lo_gestor->adicionar_subordinado( lo_prog1 ).
  lo_gestor->adicionar_subordinado( lo_prog2 ).
  lo_gestor->adicionar_subordinado( lo_estagiario ).
  
  " Apresentar todos
  WRITE: / '========== PROGRAMADOR 1 =========='.
  lo_prog1->apresentar( ).
  SKIP.
  
  WRITE: / '========== PROGRAMADOR 2 =========='.
  lo_prog2->apresentar( ).
  SKIP.
  
  WRITE: / '========== GESTOR =========='.
  lo_gestor->apresentar( ).
  SKIP.
  
  WRITE: / '========== ESTAGIÁRIO =========='.
  lo_estagiario->apresentar( ).
  
  " Polimorfismo: lista de funcionários
  SKIP 2.
  WRITE: / '========== TODOS OS FUNCIONÁRIOS =========='.
  DATA: lt_todos TYPE TABLE OF REF TO lcl_funcionario.
  
  APPEND lo_prog1 TO lt_todos.
  APPEND lo_prog2 TO lt_todos.
  APPEND lo_gestor TO lt_todos.
  APPEND lo_estagiario TO lt_todos.
  
  DATA(lv_total_salarios) = CONV p( 0 DECIMALS 2 ).
  
  LOOP AT lt_todos INTO DATA(lo_func).
    WRITE: / lo_func->get_info( ), '-', lo_func->calcular_salario( ), '€'.
    lv_total_salarios = lv_total_salarios + lo_func->calcular_salario( ).
  ENDLOOP.
  
  SKIP.
  WRITE: / |Total de salários: { lv_total_salarios }€|.
```

---

## 🎯 Boas Práticas

### ✅ Fazer

```abap
" 1. Usar PROTECTED para dados que subclasses precisam
PROTECTED SECTION.
  DATA: mv_dados_compartilhados TYPE string.

" 2. Chamar super->constructor em subclasses
METHOD constructor.
  super->constructor( iv_param ).
  " Inicialização própria
ENDMETHOD.

" 3. Classes abstratas para conceitos genéricos
CLASS lcl_veiculo DEFINITION ABSTRACT.
  METHODS calcular_consumo ABSTRACT.
ENDCLASS.

" 4. FINAL para evitar herança indesejada
CLASS lcl_utilitario DEFINITION FINAL.
ENDCLASS.
```

### ❌ Evitar

```abap
" 1. Herança muito profunda (mais de 3-4 níveis)
" Classe -> SubClasse -> SubSubClasse -> SubSubSubClasse  " ❌

" 2. Tornar tudo PRIVATE (dificulta herança)
PRIVATE SECTION.
  DATA: mv_tudo_privado TYPE i.  " ❌ Use PROTECTED

" 3. Herança apenas para reutilizar código
" Use composição em vez de herança quando apropriado

" 4. Redefinir sem chamar super quando necessário
METHOD metodo REDEFINITION.
  " Ignora lógica importante da superclasse  " ❌
  " super->metodo( ).  " ✅ Chame quando faz sentido
ENDMETHOD.
```

---

## 🔗 Próximos Passos

- **[Polimorfismo](6_polimorfismo.md)** - Casting e tratamento polimórfico
- **[Interfaces](4_interfaces.md)** - Alternativa à herança múltipla
- **[Factory Pattern](7_factory_pattern.md)** - Criar objetos através de herança

---

**Tags:** `#OO` `#Herança` `#INHERITING` `#ABSTRACT` `#FINAL` `#ABAP`
