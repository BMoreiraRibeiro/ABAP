# Expressões

## String Templates

### Sintaxe Básica

```abap
DATA(lv_nome) = 'João Silva'.
DATA(lv_idade) = 30.

" Template simples
DATA(lv_msg) = |Olá, { lv_nome }!|.

" Múltiplas variáveis
DATA(lv_info) = |Nome: { lv_nome }, Idade: { lv_idade } anos|.

" Com expressões
DATA(lv_calc) = |O dobro da idade é { lv_idade * 2 }|.
```

### Formatação em Templates

```abap
DATA(lv_valor) = '1234.567'.
DATA(lv_data) = sy-datum.

" Números com formatação
DATA(lv_fmt1) = |Valor: { lv_valor NUMBER = USER }|.  " Formato utilizador
DATA(lv_fmt2) = |Preço: { lv_valor CURRENCY = 'EUR' }|.

" Datas
DATA(lv_data_fmt) = |Data: { lv_data DATE = USER }|.
DATA(lv_data_iso) = |Data ISO: { lv_data DATE = ISO }|.

" Alinhamento (ALIGN LEFT/CENTER/RIGHT)
DATA(lv_align) = |{ 'Texto' WIDTH = 20 ALIGN = RIGHT }|.
DATA(lv_pad) = |{ lv_idade WIDTH = 5 ALIGN = RIGHT PAD = '0' }|.  " 00030

" Maiúsculas/Minúsculas
DATA(lv_upper) = |{ lv_nome CASE = UPPER }|.  " JOÃO SILVA
DATA(lv_lower) = |{ lv_nome CASE = LOWER }|.  " joão silva
```

### Templates Complexos

```abap
" Com condições inline
DATA(lv_status) = |Cliente { COND #( WHEN lv_ativo = abap_true 
                                      THEN 'ativo' 
                                      ELSE 'inativo' ) }|.

" Aninhado
DATA(lv_relatorio) = |Resumo: { |Total: { lv_total }| }|.

" Com chamada de método
DATA(lv_result) = |Resultado: { lo_objeto->get_valor( ) }|.

" Multilinha (com \n ou caracteres especiais)
DATA(lv_email) = |Olá { lv_nome },{ cl_abap_char_utilities=>newline }|
              && |{ cl_abap_char_utilities=>newline }|
              && |Obrigado pela sua encomenda.|.
```

## Operadores Aritméticos

### Operações Básicas

```abap
DATA(lv_a) = 10.
DATA(lv_b) = 3.

" Adição, subtração, multiplicação, divisão
DATA(lv_soma) = lv_a + lv_b.      " 13
DATA(lv_sub) = lv_a - lv_b.       " 7
DATA(lv_mult) = lv_a * lv_b.      " 30
DATA(lv_div) = lv_a / lv_b.       " 3 (divisão inteira)

" Divisão com decimais
DATA(lv_div_dec) = CONV decfloat34( lv_a ) / lv_b.  " 3.333...

" Módulo (resto da divisão)
DATA(lv_resto) = lv_a MOD lv_b.   " 1

" Potência
DATA(lv_pot) = lv_a ** 2.         " 100

" Operações combinadas
DATA(lv_result) = ( lv_a + lv_b ) * 2 - 5.
```

### Operações com Atribuição

```abap
DATA lv_contador TYPE i VALUE 10.

" Incremento e decremento
lv_contador = lv_contador + 1.
lv_contador = lv_contador - 1.

" Multiplicação/divisão com atribuição
lv_contador = lv_contador * 2.
lv_contador = lv_contador / 2.

" ADD, SUBTRACT, MULTIPLY, DIVIDE (forma tradicional)
ADD 5 TO lv_contador.
SUBTRACT 3 FROM lv_contador.
MULTIPLY lv_contador BY 2.
DIVIDE lv_contador BY 4.
```

## Operações com Strings

### Concatenação

```abap
DATA(lv_primeiro) = 'João'.
DATA(lv_ultimo) = 'Silva'.

" Operador && (mantém espaços)
DATA(lv_nome1) = lv_primeiro && ' ' && lv_ultimo.

" CONCATENATE (remove espaços trailing por defeito)
DATA lv_nome2 TYPE string.
CONCATENATE lv_primeiro lv_ultimo INTO lv_nome2 SEPARATED BY space.

" String template (recomendado)
DATA(lv_nome3) = |{ lv_primeiro } { lv_ultimo }|.
```

### Manipulação de Strings

```abap
DATA(lv_texto) = '  ABAP Programming  '.

" Comprimento
DATA(lv_len) = strlen( lv_texto ).

" Remover espaços
DATA(lv_trim) = condense( lv_texto ).  " Remove espaços extra
lv_texto = shift_left( val = lv_texto ).   " Remove espaços à esquerda
lv_texto = shift_right( val = lv_texto ).  " Remove espaços à direita

" Substituir
DATA(lv_novo) = replace( val = lv_texto sub = 'ABAP' with = 'SAP' ).

" Substring
DATA(lv_sub) = substring( val = lv_texto off = 2 len = 4 ).

" Maiúsculas/Minúsculas
DATA(lv_upper) = to_upper( lv_texto ).
DATA(lv_lower) = to_lower( lv_texto ).

" Dividir string
SPLIT lv_texto AT space INTO DATA(lv_part1) DATA(lv_part2).

" Encontrar
DATA(lv_pos) = find( val = lv_texto sub = 'Programming' ).
IF lv_pos >= 0.
  WRITE: / |Encontrado na posição { lv_pos }|.
ENDIF.
```

### Match e Expressões Regulares

```abap
DATA(lv_email) = 'utilizador@example.com'.

" Verificar padrão
IF matches( val = lv_email regex = '^\w+@\w+\.\w+$' ).
  WRITE: / 'Email válido'.
ENDIF.

" Extrair com regex
DATA(lv_dominio) = replace( val = lv_email regex = '^.*@' with = '' ).

" Contar ocorrências
DATA(lv_count) = count( val = 'teste teste teste' sub = 'teste' ).
```

## Operações com Tabelas Internas

### Leitura de Linhas

```abap
" Acesso direto por índice []
DATA(ls_primeiro) = lt_clientes[ 1 ].  " Primeira linha

" Com DEFAULT (evita dump se não existir)
DATA(ls_cliente) = VALUE #( lt_clientes[ id = 100 ] DEFAULT ls_default ).

" Acesso a campo específico
DATA(lv_nome) = lt_clientes[ 1 ]-nome.

" Opcional com ?
TRY.
    DATA(ls_opt) = lt_clientes[ id = 999 ].
  CATCH cx_sy_itab_line_not_found.
    " Linha não encontrada
ENDTRY.
```

### Funções de Tabela

```abap
" Número de linhas
DATA(lv_count) = lines( lt_clientes ).

" Existe linha?
IF line_exists( lt_clientes[ id = 100 ] ).
  WRITE: / 'Cliente existe'.
ENDIF.

" Índice da linha
DATA(lv_idx) = line_index( lt_clientes[ nome = 'João' ] ).
```

### FILTER

```abap
" Filtrar tabela
DATA(lt_ativos) = FILTER #( lt_clientes WHERE ativo = abap_true ).

" Com múltiplas condições
DATA(lt_filtrado) = FILTER #( lt_produtos 
  WHERE categoria = 'ELETRO' 
    AND preco > 100 
    AND stock > 0 
).

" EXCEPT - excluir registos
DATA(lt_inativos) = FILTER #( lt_clientes EXCEPT WHERE ativo = abap_true ).
```

## VALUE Constructor

### Criar Estruturas

```abap
" Estrutura simples
DATA(ls_pessoa) = VALUE ty_pessoa(
  id    = 1
  nome  = 'Ana'
  idade = 25
).

" Com BASE (manter valores existentes)
ls_pessoa = VALUE #( BASE ls_pessoa
  idade = 26
).

" Estrutura aninhada
DATA(ls_pedido) = VALUE ty_pedido(
  numero = '001'
  cliente = VALUE #(
    id   = 100
    nome = 'Cliente XYZ'
  )
  itens = VALUE #(
    ( produto = 'A' qtd = 5 )
    ( produto = 'B' qtd = 3 )
  )
).
```

### Criar Tabelas

```abap
" Tabela com valores diretos
DATA(lt_numeros) = VALUE ty_t_int( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

" Tabela de estruturas
DATA(lt_clientes) = VALUE ty_t_clientes(
  ( id = 1 nome = 'João'  ativo = abap_true )
  ( id = 2 nome = 'Maria' ativo = abap_true )
  ( id = 3 nome = 'Pedro' ativo = abap_false )
).

" Com BASE (adicionar a existente)
lt_clientes = VALUE #( BASE lt_clientes
  ( id = 4 nome = 'Ana' ativo = abap_true )
).

" Com LINES OF (juntar tabelas)
DATA(lt_todos) = VALUE ty_t_clientes(
  ( id = 1 nome = 'A' )
  LINES OF lt_clientes
  ( id = 999 nome = 'Z' )
).
```

## CORRESPONDING

### Entre Estruturas

```abap
" Copiar campos com mesmo nome
DATA(ls_destino) = CORRESPONDING ty_destino( ls_origem ).

" Com BASE
ls_destino = CORRESPONDING #( BASE ls_destino ls_origem ).

" Com MAPPING (mapear campos diferentes)
ls_destino = CORRESPONDING #( ls_origem 
  MAPPING campo_dest = campo_orig
          nome_novo  = nome_antigo
).

" EXCEPT (excluir campos)
ls_destino = CORRESPONDING #( ls_origem EXCEPT campo1 campo2 ).
```

### Entre Tabelas

```abap
" Converter tabela completa
DATA(lt_destino) = CORRESPONDING ty_t_destino( lt_origem ).

" Manter registos + adicionar
lt_destino = CORRESPONDING #( BASE lt_destino lt_origem ).

" Com transformação inline
DATA(lt_resumo) = CORRESPONDING ty_t_resumo( lt_vendas
  MAPPING id_resumo = id_venda
          valor_total = valor
).
```

## CONV (Conversão de Tipos)

```abap
" Converter tipos básicos
DATA(lv_string) = '12345'.
DATA(lv_int) = CONV i( lv_string ).

DATA(lv_decimal) = CONV decfloat34( '3.14159' ).
DATA(lv_packed) = CONV p( lv_decimal ).

" Em expressões
DATA(lv_resultado) = CONV decfloat34( lv_valor1 ) / CONV decfloat34( lv_valor2 ).

" Com data/hora
DATA(lv_timestamp) = CONV timestamp( utclong_current( ) ).
```

## CAST (Conversão de Objetos)

```abap
" Cast de referências
DATA lo_generic TYPE REF TO object.
DATA(lo_specific) = CAST cl_my_class( lo_generic ).

" Cast seguro com TRY
TRY.
    DATA(lo_cliente) = CAST cl_cliente( lo_objeto ).
    lo_cliente->processar( ).
  CATCH cx_sy_move_cast_error.
    " Cast falhou
ENDTRY.

" Verificar tipo antes
IF lo_objeto IS INSTANCE OF cl_cliente.
  DATA(lo_cli) = CAST cl_cliente( lo_objeto ).
ENDIF.
```

## REF (Criar Referência)

```abap
" Criar referência a variável
DATA lv_valor TYPE i VALUE 100.
DATA(lr_valor) = REF #( lv_valor ).

" Modificar via referência
lr_valor->* = 200.  " lv_valor agora é 200

" Referência a estrutura
DATA ls_cliente TYPE ty_cliente.
DATA(lr_cliente) = REF #( ls_cliente ).
lr_cliente->nome = 'João'.

" Criar nova instância
DATA(lr_nova) = NEW ty_cliente( ).
```

## Dicas de Boas Práticas

!!! tip "String Templates"
    - Use sempre string templates em vez de CONCATENATE
    - Mais legível e menos propenso a erros
    - Performance equivalente ou melhor

!!! warning "Cuidado com Conversões"
    ```abap
    " ❌ Pode perder precisão
    DATA(lv_result) = 10 / 3.  " Resultado: 3 (inteiro)
    
    " ✅ Converter para decimal primeiro
    DATA(lv_result) = CONV decfloat34( 10 ) / 3.  " 3.333...
    
    " ❌ Acesso direto pode causar dump
    DATA(ls_cliente) = lt_clientes[ 999 ].
    
    " ✅ Com tratamento de erro
    TRY.
        DATA(ls_cliente) = lt_clientes[ 999 ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
    ```

!!! example "Expressões Modernas"
    ```abap
    " ❌ Forma antiga
    DATA lv_resultado TYPE string.
    IF lv_idade >= 18.
      lv_resultado = 'Adulto'.
    ELSE.
      lv_resultado = 'Menor'.
    ENDIF.
    CONCATENATE 'Status:' lv_resultado INTO lv_resultado SEPARATED BY space.
    
    " ✅ Forma moderna
    DATA(lv_resultado) = |Status: { COND #( WHEN lv_idade >= 18 
                                             THEN 'Adulto' 
                                             ELSE 'Menor' ) }|.
    ```

!!! info "Performance de Expressões"
    - VALUE, CORRESPONDING, FILTER são otimizados pelo compilador
    - String templates são tão rápidos quanto CONCATENATE
    - Use inline declarations para reduzir código
    - REDUCE é eficiente para agregações

---

**Tags:** #Fundamentos #Expressões #Modern-ABAP #Performance