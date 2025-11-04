# Atributos e Métodos

## 📋 Visão Geral

Atributos são as **variáveis/dados** de uma classe, e métodos são as **funções/comportamentos**. Este capítulo explora em detalhe como defini-los e usá-los.

---

## 🎯 Atributos (DATA)

### Atributos de Instância

Cada objeto tem a sua própria cópia do atributo.

```abap
CLASS lcl_carro DEFINITION.
  PUBLIC SECTION.
    DATA: mv_marca  TYPE string,
          mv_modelo TYPE string,
          mv_ano    TYPE i,
          mv_km     TYPE i.
ENDCLASS.

" Cada carro tem os seus próprios valores
DATA(lo_carro1) = NEW lcl_carro( ).
lo_carro1->mv_marca = 'BMW'.
lo_carro1->mv_km = 50000.

DATA(lo_carro2) = NEW lcl_carro( ).
lo_carro2->mv_marca = 'Audi'.
lo_carro2->mv_km = 30000.

" lo_carro1 e lo_carro2 são independentes
```

### Atributos Estáticos (CLASS-DATA)

Partilhados por **todos** os objetos da classe.

```abap
CLASS lcl_contador DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: sv_total_objetos TYPE i.  " Partilhado
    DATA: mv_id TYPE i.                    " Individual
    
    METHODS: constructor.
ENDCLASS.

CLASS lcl_contador IMPLEMENTATION.
  METHOD constructor.
    sv_total_objetos = sv_total_objetos + 1.
    mv_id = sv_total_objetos.
  ENDMETHOD.
ENDCLASS.

" Usar
DATA(lo_obj1) = NEW lcl_contador( ).  " sv_total_objetos = 1
DATA(lo_obj2) = NEW lcl_contador( ).  " sv_total_objetos = 2
DATA(lo_obj3) = NEW lcl_contador( ).  " sv_total_objetos = 3

WRITE: / 'Total de objetos:', lcl_contador=>sv_total_objetos.  " 3
WRITE: / 'ID do objeto 2:', lo_obj2->mv_id.                     " 2
```

### Atributos Read-Only

```abap
CLASS lcl_produto DEFINITION.
  PUBLIC SECTION.
    METHODS: 
      get_preco RETURNING VALUE(rv_preco) TYPE p DECIMALS 2,
      set_preco IMPORTING iv_preco TYPE p DECIMALS 2.
      
  PRIVATE SECTION.
    DATA: mv_preco TYPE p DECIMALS 2.  " Privado = read-only de fora
ENDCLASS.

CLASS lcl_produto IMPLEMENTATION.
  METHOD get_preco.
    rv_preco = mv_preco.
  ENDMETHOD.
  
  METHOD set_preco.
    IF iv_preco > 0.
      mv_preco = iv_preco.
    ELSE.
      MESSAGE 'Preço deve ser positivo' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Uso
DATA(lo_prod) = NEW lcl_produto( ).
lo_prod->set_preco( '99.99' ).
DATA(lv_preco) = lo_prod->get_preco( ).
" lo_prod->mv_preco = '50.00'.  " ❌ ERRO: atributo privado
```

---

## 🔧 Métodos (METHODS)

### Métodos de Instância

Operam sobre dados do objeto.

```abap
CLASS lcl_calculadora DEFINITION.
  PUBLIC SECTION.
    DATA: mv_valor TYPE i.
    
    METHODS:
      adicionar IMPORTING iv_num TYPE i,
      subtrair  IMPORTING iv_num TYPE i,
      get_resultado RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS lcl_calculadora IMPLEMENTATION.
  METHOD adicionar.
    mv_valor = mv_valor + iv_num.
  ENDMETHOD.
  
  METHOD subtrair.
    mv_valor = mv_valor - iv_num.
  ENDMETHOD.
  
  METHOD get_resultado.
    rv_result = mv_valor.
  ENDMETHOD.
ENDCLASS.

" Uso
DATA(lo_calc) = NEW lcl_calculadora( ).
lo_calc->adicionar( 10 ).
lo_calc->adicionar( 5 ).
lo_calc->subtrair( 3 ).
WRITE: / 'Resultado:', lo_calc->get_resultado( ).  " 12
```

### Métodos Estáticos (CLASS-METHODS)

Não precisam de objeto, trabalham apenas com dados estáticos.

```abap
CLASS lcl_utils DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      converter_celsius_fahrenheit 
        IMPORTING iv_celsius TYPE p DECIMALS 2
        RETURNING VALUE(rv_fahrenheit) TYPE p DECIMALS 2,
      validar_email
        IMPORTING iv_email TYPE string
        RETURNING VALUE(rv_valido) TYPE abap_bool.
ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.
  METHOD converter_celsius_fahrenheit.
    rv_fahrenheit = ( iv_celsius * '1.8' ) + 32.
  ENDMETHOD.
  
  METHOD validar_email.
    rv_valido = COND #( WHEN iv_email CS '@' AND iv_email CS '.'
                        THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.

" Uso (sem criar objeto)
DATA(lv_temp) = lcl_utils=>converter_celsius_fahrenheit( '25.0' ).  " 77.0
DATA(lv_ok) = lcl_utils=>validar_email( 'user@exemplo.com' ).      " X
```

---

## 📥 Parâmetros de Métodos

### IMPORTING

Recebe valores de entrada.

```abap
METHODS: calcular_desconto
  IMPORTING iv_preco TYPE p DECIMALS 2
            iv_desconto_percentual TYPE p DECIMALS 2
  RETURNING VALUE(rv_preco_final) TYPE p DECIMALS 2.

METHOD calcular_desconto.
  DATA(lv_desconto) = iv_preco * iv_desconto_percentual / 100.
  rv_preco_final = iv_preco - lv_desconto.
ENDMETHOD.
```

### EXPORTING

Retorna valores (menos comum, use RETURNING quando possível).

```abap
METHODS: dividir
  IMPORTING iv_dividendo TYPE i
            iv_divisor TYPE i
  EXPORTING ev_quociente TYPE i
            ev_resto TYPE i.

METHOD dividir.
  ev_quociente = iv_dividendo DIV iv_divisor.
  ev_resto = iv_dividendo MOD iv_divisor.
ENDMETHOD.

" Uso
DATA: lv_quociente TYPE i,
      lv_resto TYPE i.

lo_obj->dividir(
  EXPORTING iv_dividendo = 17
            iv_divisor = 5
  IMPORTING ev_quociente = lv_quociente
            ev_resto = lv_resto ).

WRITE: / lv_quociente, lv_resto.  " 3 2
```

### RETURNING

Retorna um único valor (recomendado).

```abap
METHODS: potencia
  IMPORTING iv_base TYPE i
            iv_expoente TYPE i
  RETURNING VALUE(rv_resultado) TYPE i.

METHOD potencia.
  rv_resultado = iv_base ** iv_expoente.
ENDMETHOD.

" Uso inline
DATA(lv_result) = lo_obj->potencia( iv_base = 2 iv_expoente = 10 ).
WRITE: / lv_result.  " 1024
```

### CHANGING

Modifica parâmetro existente.

```abap
METHODS: incrementar_contador
  CHANGING cv_contador TYPE i.

METHOD incrementar_contador.
  cv_contador = cv_contador + 1.
ENDMETHOD.

" Uso
DATA lv_contador TYPE i VALUE 5.
lo_obj->incrementar_contador( CHANGING cv_contador = lv_contador ).
WRITE: / lv_contador.  " 6
```

---

## 🔄 Method Chaining

Encadear chamadas de métodos.

```abap
CLASS lcl_builder DEFINITION.
  PUBLIC SECTION.
    METHODS:
      set_nome IMPORTING iv_nome TYPE string
               RETURNING VALUE(ro_self) TYPE REF TO lcl_builder,
      set_idade IMPORTING iv_idade TYPE i
                RETURNING VALUE(ro_self) TYPE REF TO lcl_builder,
      build RETURNING VALUE(rv_info) TYPE string.
      
  PRIVATE SECTION.
    DATA: mv_nome TYPE string,
          mv_idade TYPE i.
ENDCLASS.

CLASS lcl_builder IMPLEMENTATION.
  METHOD set_nome.
    mv_nome = iv_nome.
    ro_self = me.  " Retorna referência para si mesmo
  ENDMETHOD.
  
  METHOD set_idade.
    mv_idade = iv_idade.
    ro_self = me.
  ENDMETHOD.
  
  METHOD build.
    rv_info = |{ mv_nome } tem { mv_idade } anos|.
  ENDMETHOD.
ENDCLASS.

" Uso: method chaining
DATA(lv_info) = NEW lcl_builder( )->set_nome( 'João' )
                                   ->set_idade( 30 )
                                   ->build( ).
WRITE: / lv_info.  " João tem 30 anos
```

---

## 💡 Exemplo Completo: Classe Conta Bancária

```abap
CLASS lcl_conta_bancaria DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: sv_total_contas TYPE i.
    
    METHODS:
      constructor IMPORTING iv_titular TYPE string
                            iv_saldo_inicial TYPE p DECIMALS 2 DEFAULT 0,
      depositar IMPORTING iv_valor TYPE p DECIMALS 2,
      levantar IMPORTING iv_valor TYPE p DECIMALS 2
               RETURNING VALUE(rv_sucesso) TYPE abap_bool,
      transferir IMPORTING iv_valor TYPE p DECIMALS 2
                           io_destino TYPE REF TO lcl_conta_bancaria
                 RETURNING VALUE(rv_sucesso) TYPE abap_bool,
      get_saldo RETURNING VALUE(rv_saldo) TYPE p DECIMALS 2,
      get_extrato RETURNING VALUE(rv_extrato) TYPE string.
      
  PRIVATE SECTION.
    DATA: mv_numero TYPE string,
          mv_titular TYPE string,
          mv_saldo TYPE p DECIMALS 2,
          mt_transacoes TYPE TABLE OF string.
          
    METHODS: 
      gerar_numero_conta RETURNING VALUE(rv_numero) TYPE string,
      registrar_transacao IMPORTING iv_descricao TYPE string.
ENDCLASS.

CLASS lcl_conta_bancaria IMPLEMENTATION.
  METHOD constructor.
    sv_total_contas = sv_total_contas + 1.
    mv_numero = gerar_numero_conta( ).
    mv_titular = iv_titular.
    mv_saldo = iv_saldo_inicial.
    
    registrar_transacao( |Conta aberta com saldo inicial: { iv_saldo_inicial }| ).
  ENDMETHOD.
  
  METHOD gerar_numero_conta.
    rv_numero = |{ sy-datum }{ sy-uzeit }{ sv_total_contas }|.
  ENDMETHOD.
  
  METHOD depositar.
    IF iv_valor > 0.
      mv_saldo = mv_saldo + iv_valor.
      registrar_transacao( |Depósito: +{ iv_valor }| ).
    ENDIF.
  ENDMETHOD.
  
  METHOD levantar.
    IF iv_valor > 0 AND iv_valor <= mv_saldo.
      mv_saldo = mv_saldo - iv_valor.
      registrar_transacao( |Levantamento: -{ iv_valor }| ).
      rv_sucesso = abap_true.
    ELSE.
      rv_sucesso = abap_false.
    ENDIF.
  ENDMETHOD.
  
  METHOD transferir.
    IF iv_valor > 0 AND iv_valor <= mv_saldo.
      mv_saldo = mv_saldo - iv_valor.
      io_destino->depositar( iv_valor ).
      registrar_transacao( |Transferência enviada: -{ iv_valor } para { io_destino->mv_titular }| ).
      rv_sucesso = abap_true.
    ELSE.
      rv_sucesso = abap_false.
    ENDIF.
  ENDMETHOD.
  
  METHOD get_saldo.
    rv_saldo = mv_saldo.
  ENDMETHOD.
  
  METHOD get_extrato.
    rv_extrato = concat_lines_of( table = mt_transacoes sep = cl_abap_char_utilities=>newline ).
  ENDMETHOD.
  
  METHOD registrar_transacao.
    APPEND |{ sy-datum } { sy-uzeit }: { iv_descricao }| TO mt_transacoes.
  ENDMETHOD.
ENDCLASS.

" Programa de teste
START-OF-SELECTION.
  
  DATA(lo_conta1) = NEW lcl_conta_bancaria( 
    iv_titular = 'João Silva'
    iv_saldo_inicial = '1000.00'
  ).
  
  DATA(lo_conta2) = NEW lcl_conta_bancaria(
    iv_titular = 'Maria Santos'
    iv_saldo_inicial = '500.00'
  ).
  
  " Operações
  lo_conta1->depositar( '200.00' ).
  lo_conta1->levantar( '150.00' ).
  lo_conta1->transferir( iv_valor = '300.00' io_destino = lo_conta2 ).
  
  " Exibir resultados
  WRITE: / 'Saldo conta 1:', lo_conta1->get_saldo( ).
  WRITE: / 'Saldo conta 2:', lo_conta2->get_saldo( ).
  SKIP.
  WRITE: / 'Extrato conta 1:', /, lo_conta1->get_extrato( ).
```

---

## 🎯 Boas Práticas

### ✅ Fazer

```abap
" 1. Atributos privados com getters/setters
PRIVATE SECTION.
  DATA mv_preco TYPE p DECIMALS 2.
PUBLIC SECTION.
  METHODS get_preco RETURNING VALUE(rv_preco) TYPE p DECIMALS 2.
  METHODS set_preco IMPORTING iv_preco TYPE p DECIMALS 2.

" 2. Métodos pequenos e focados
METHOD calcular_total.
  rv_total = calcular_subtotal( ) + calcular_impostos( ).
ENDMETHOD.

" 3. Usar RETURNING em vez de EXPORTING
METHODS calcular RETURNING VALUE(rv_result) TYPE i.  " ✅
" Em vez de:
" METHODS calcular EXPORTING ev_result TYPE i.  " ❌

" 4. Validação em setters
METHOD set_idade.
  IF iv_idade >= 0 AND iv_idade <= 150.
    mv_idade = iv_idade.
  ELSE.
    RAISE EXCEPTION TYPE cx_parameter_invalid.
  ENDIF.
ENDMETHOD.
```

### ❌ Evitar

```abap
" 1. Atributos públicos sem controlo
PUBLIC SECTION.
  DATA mv_saldo TYPE p DECIMALS 2.  " ❌ Qualquer um pode alterar

" 2. Métodos gigantes
METHOD processar_tudo.
  " 500 linhas de código  " ❌
ENDMETHOD.

" 3. Nomes não descritivos
METHODS x IMPORTING iv_y TYPE i.  " ❌

" 4. Modificar parâmetros IMPORTING
METHOD processar.
  iv_parametro = iv_parametro + 1.  " ❌ Use CHANGING
ENDMETHOD.
```

---

## 🔗 Próximos Passos

- **[Construtores](3_construtores.md)** - Inicialização de objetos
- **[Interfaces](4_interfaces.md)** - Contratos entre classes
- **[Herança](5_heranca.md)** - Reutilização de código

---

**Tags:** `#OO` `#Atributos` `#Métodos` `#Encapsulamento` `#ABAP`
