# Construtores

## 📋 Visão Geral

O **construtor** é um método especial executado automaticamente quando um objeto é criado. É usado para inicializar o objeto com valores.

---

## 🎯 Método CONSTRUCTOR

```abap
CLASS lcl_pessoa DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_nome TYPE string
                                   iv_idade TYPE i.
    DATA: mv_nome TYPE string,
          mv_idade TYPE i.
ENDCLASS.

CLASS lcl_pessoa IMPLEMENTATION.
  METHOD constructor.
    mv_nome = iv_nome.
    mv_idade = iv_idade.
    WRITE: / |Objeto criado: { mv_nome }, { mv_idade } anos|.
  ENDMETHOD.
ENDCLASS.

" Usar
DATA(lo_pessoa) = NEW lcl_pessoa( iv_nome = 'João' iv_idade = 30 ).
```

---

## 🔧 Construtor com Valores Padrão

```abap
CLASS lcl_produto DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor 
      IMPORTING iv_nome TYPE string
                iv_preco TYPE p DECIMALS 2 DEFAULT '0.00'
                iv_stock TYPE i DEFAULT 0.
                
  PRIVATE SECTION.
    DATA: mv_nome TYPE string,
          mv_preco TYPE p DECIMALS 2,
          mv_stock TYPE i.
ENDCLASS.

CLASS lcl_produto IMPLEMENTATION.
  METHOD constructor.
    mv_nome = iv_nome.
    mv_preco = iv_preco.
    mv_stock = iv_stock.
  ENDMETHOD.
ENDCLASS.

" Uso com valores padrão
DATA(lo_prod1) = NEW lcl_produto( iv_nome = 'Caneta' ).
DATA(lo_prod2) = NEW lcl_produto( 
  iv_nome = 'Caderno' 
  iv_preco = '5.50'
  iv_stock = 100 
).
```

---

## 🏭 Construtor Estático (CLASS_CONSTRUCTOR)

Executado **uma única vez** quando a classe é usada pela primeira vez.

```abap
CLASS lcl_config DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: class_constructor.
    CLASS-DATA: sv_ambiente TYPE string,
                sv_versao TYPE string.
ENDCLASS.

CLASS lcl_config IMPLEMENTATION.
  METHOD class_constructor.
    sv_ambiente = 'PRODUÇÃO'.
    sv_versao = '1.0.0'.
    WRITE: / 'Configuração inicial carregada'.
  ENDMETHOD.
ENDCLASS.

" Ao usar a classe pela primeira vez, class_constructor é chamado
WRITE: / lcl_config=>sv_ambiente.  " Trigger: class_constructor executado
WRITE: / lcl_config=>sv_versao.
```

---

## 💡 Padrão Factory com Construtor Privado

```abap
CLASS lcl_singleton DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance 
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_singleton.
    METHODS: fazer_algo.
    
  PRIVATE SECTION.
    CLASS-DATA: so_instance TYPE REF TO lcl_singleton.
    METHODS: constructor.
ENDCLASS.

CLASS lcl_singleton IMPLEMENTATION.
  METHOD constructor.
    WRITE: / 'Singleton criado'.
  ENDMETHOD.
  
  METHOD get_instance.
    IF so_instance IS NOT BOUND.
      CREATE OBJECT so_instance.
    ENDIF.
    ro_instance = so_instance.
  ENDMETHOD.
  
  METHOD fazer_algo.
    WRITE: / 'Executando ação do singleton'.
  ENDMETHOD.
ENDCLASS.

" Uso
DATA(lo_obj1) = lcl_singleton=>get_instance( ).
DATA(lo_obj2) = lcl_singleton=>get_instance( ).
" lo_obj1 e lo_obj2 apontam para o MESMO objeto
```

---

## 🎯 Validação no Construtor

```abap
CLASS lcl_conta DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor 
      IMPORTING iv_numero TYPE string
                iv_saldo_inicial TYPE p DECIMALS 2
      RAISING cx_parameter_invalid.
      
  PRIVATE SECTION.
    DATA: mv_numero TYPE string,
          mv_saldo TYPE p DECIMALS 2.
ENDCLASS.

CLASS lcl_conta IMPLEMENTATION.
  METHOD constructor.
    " Validações
    IF iv_numero IS INITIAL.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
    
    IF iv_saldo_inicial < 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
    
    " Inicialização
    mv_numero = iv_numero.
    mv_saldo = iv_saldo_inicial.
  ENDMETHOD.
ENDCLASS.

" Uso com tratamento de erro
TRY.
    DATA(lo_conta) = NEW lcl_conta( 
      iv_numero = '12345'
      iv_saldo_inicial = '1000.00'
    ).
  CATCH cx_parameter_invalid.
    WRITE: / 'Erro ao criar conta: parâmetros inválidos'.
ENDTRY.
```

---

## 🔗 Próximos Passos

- **[Interfaces](4_interfaces.md)** - Definir contratos
- **[Herança](5_heranca.md)** - Reutilizar código
- **[Singleton Pattern](8_singleton.md)** - Padrão de design

---

**Tags:** `#OO` `#Constructor` `#Singleton` `#Factory` `#ABAP`
