# Variáveis e Tipos de Dados

## Declaração de Variáveis

### Sintaxe Moderna (Inline)

```abap
" Declaração inline com DATA()
DATA(lv_nome) = 'João Silva'.
DATA(lv_idade) = 25.
DATA(lv_salario) = CONV decfloat34( '2500.50' ).

" Declaração inline em loops
LOOP AT lt_clientes INTO DATA(ls_cliente).
  WRITE: / ls_cliente-nome.
ENDLOOP.

" Declaração inline em SELECT
SELECT SINGLE * FROM scarr INTO @DATA(ls_carrier) WHERE carrid = 'LH'.
```

### Declaração Tradicional

```abap
DATA: lv_nome    TYPE string,
      lv_idade   TYPE i,
      lv_salario TYPE p DECIMALS 2.

lv_nome = 'Maria Santos'.
lv_idade = 30.
lv_salario = '3000.00'.
```

## Tipos de Dados Elementares

### Numéricos

```abap
" Inteiros
DATA(lv_int) = 100.                    " TYPE i (4 bytes)
DATA(lv_int8) = CONV int8( 1000000 ).  " TYPE int8 (8 bytes)

" Decimais
DATA(lv_packed) = CONV p( '123.45' ).  " Packed number
DATA lv_preco TYPE p DECIMALS 2 VALUE '99.99'.

" Float
DATA(lv_float) = CONV f( '3.14159' ).
DATA(lv_decfloat) = CONV decfloat34( '3.141592653589793' ).
```

### Texto

```abap
" String (comprimento dinâmico)
DATA(lv_texto) = 'Isto é uma string'.
DATA(lv_concatenado) = |Cliente: { lv_nome }, Idade: { lv_idade }|.

" Caracteres (comprimento fixo)
DATA lv_code TYPE c LENGTH 10 VALUE 'ABC123'.
DATA lv_char TYPE char20 VALUE 'Texto fixo'.

" Texto longo
DATA lv_texto_longo TYPE string.
lv_texto_longo = 'Lorem ipsum dolor sit amet, ' &&
                 'consectetur adipiscing elit.'.
```

### Data e Hora

```abap
" Data atual
DATA(lv_data) = sy-datum.              " YYYYMMDD
DATA(lv_hora) = sy-uzeit.              " HHMMSS
DATA(lv_timestamp) = CONV timestamp( utclong_current( ) ).

" Tipos específicos
DATA lv_datum TYPE d VALUE '20250101'. " Data
DATA lv_uzeit TYPE t VALUE '143000'.   " Hora
```

### Outros Tipos

```abap
" Hexadecimal
DATA lv_hex TYPE x LENGTH 4.
lv_hex = '0F1E2D3C'.

" Booleano (ABAP usa ABAP_BOOL ou CHAR1)
DATA(lv_ativo) = abap_true.            " 'X'
DATA(lv_inativo) = abap_false.         " ' '

IF lv_ativo = abap_true.
  " Lógica quando ativo
ENDIF.
```

## Conversões de Tipo

### CONV e CAST

```abap
" CONV - Conversão de tipo elementar
DATA(lv_string_num) = '12345'.
DATA(lv_numero) = CONV i( lv_string_num ).

DATA(lv_preco_string) = CONV string( lv_preco ).

" CAST - Conversão de objetos
DATA(lo_obj) = CAST cl_abap_typedescr( 
  cl_abap_typedescr=>describe_by_data( lv_nome ) 
).
```

### Conversões Implícitas

```abap
DATA lv_char10 TYPE c LENGTH 10.
DATA lv_string TYPE string.

lv_char10 = 'TESTE'.
lv_string = lv_char10.  " Conversão automática

" Atenção: pode haver perda de informação
DATA lv_num TYPE i VALUE 123456789.
DATA lv_char TYPE c LENGTH 5.
lv_char = lv_num.  " Trunca para '12345'
```

## Constantes

```abap
" Constantes da classe
CLASS lcl_calculator DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      gc_pi    TYPE p DECIMALS 14 VALUE '3.14159265358979',
      gc_taxa  TYPE p DECIMALS 2 VALUE '1.23',
      gc_max   TYPE i VALUE 1000.
      
    CONSTANTS gc_separador TYPE c LENGTH 1 VALUE '-'.
ENDCLASS.

" Uso de constantes
DATA(lv_area) = gc_pi * lv_raio * lv_raio.
```

## Tipos Locais

```abap
TYPES: BEGIN OF ty_cliente,
         id      TYPE i,
         nome    TYPE string,
         email   TYPE string,
         ativo   TYPE abap_bool,
         criado  TYPE timestamp,
       END OF ty_cliente.

DATA ls_cliente TYPE ty_cliente.
ls_cliente = VALUE #( 
  id     = 1
  nome   = 'Ana Costa'
  email  = 'ana@example.com'
  ativo  = abap_true
  criado = utclong_current( )
).

" Tipos de tabela
TYPES ty_t_clientes TYPE STANDARD TABLE OF ty_cliente WITH DEFAULT KEY.
DATA lt_clientes TYPE ty_t_clientes.
```

## VALUE e Inicialização

```abap
" VALUE com estruturas
DATA(ls_produto) = VALUE ty_produto(
  id        = 100
  nome      = 'Laptop'
  preco     = '999.99'
  stock     = 50
  categoria = 'Informática'
).

" CLEAR vs. VALUE #()
CLEAR ls_produto.                    " Limpa valores
ls_produto = VALUE #( ).             " Sintaxe moderna

" Inicialização de tabelas
DATA(lt_numeros) = VALUE ty_t_numeros( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).
```

## Âmbito de Variáveis

```abap
CLASS lcl_exemplo DEFINITION.
  PUBLIC SECTION.
    DATA mv_publico TYPE string.           " Acessível externamente
    
  PROTECTED SECTION.
    DATA mv_protegido TYPE string.         " Acessível em subclasses
    
  PRIVATE SECTION.
    DATA mv_privado TYPE string.           " Apenas nesta classe
    
    METHODS processar.
ENDCLASS.

CLASS lcl_exemplo IMPLEMENTATION.
  METHOD processar.
    " Variáveis locais do método
    DATA(lv_temp) = 'Temporário'.
    
    " Acesso a atributos
    mv_privado = 'Valor privado'.
    me->mv_publico = 'Valor público'.  " me-> é opcional
  ENDMETHOD.
ENDCLASS.
```

## Dicas de Boas Práticas

!!! tip "Nomenclatura"
    - Use prefixos claros: `lv_` para variáveis locais, `gv_` para globais
    - `ls_` para estruturas, `lt_` para tabelas internas
    - `lo_` para objetos, `lr_` para referências
    - `gc_` para constantes globais, `lc_` para constantes locais

!!! warning "Performance"
    - Evite conversões desnecessárias em loops
    - Use tipos adequados (não declare `TYPE p DECIMALS 10` se só precisa de 2)
    - Prefira `STRING` para textos dinâmicos, `C` para fixos

!!! example "Sintaxe Moderna"
    ```abap
    " ❌ Evitar
    DATA lv_resultado TYPE string.
    lv_resultado = 'Valor: ' && lv_valor.
    
    " ✅ Preferir
    DATA(lv_resultado) = |Valor: { lv_valor }|.
    ```

!!! info "Declaração Inline"
    - Use `DATA()` sempre que o tipo seja óbvio pelo contexto
    - Torna o código mais conciso e legível
    - Disponível desde ABAP 7.40

---

**Tags:** #Fundamentos #Variáveis #Modern-ABAP