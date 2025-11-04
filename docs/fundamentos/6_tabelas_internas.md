# Tabelas Internas

## Tipos de Tabelas

### STANDARD TABLE (mais comum)

```abap
" Declaração de tipo
TYPES ty_t_clientes TYPE STANDARD TABLE OF ty_cliente WITH DEFAULT KEY.

" Declaração inline
DATA lt_produtos TYPE STANDARD TABLE OF ty_produto WITH EMPTY KEY.

" Com chave específica
DATA lt_pedidos TYPE STANDARD TABLE OF ty_pedido
  WITH NON-UNIQUE KEY numero.

" Múltiplas chaves
DATA lt_vendas TYPE STANDARD TABLE OF ty_venda
  WITH NON-UNIQUE KEY pais ano mes
  WITH UNIQUE SORTED KEY by_id COMPONENTS id.
```

### SORTED TABLE

```abap
" Sempre ordenada pela chave
DATA lt_clientes TYPE SORTED TABLE OF ty_cliente
  WITH UNIQUE KEY id.

" Chave não única
DATA lt_produtos TYPE SORTED TABLE OF ty_produto
  WITH NON-UNIQUE KEY categoria preco.

" Acesso binário automático (mais rápido)
```

### HASHED TABLE

```abap
" Acesso hash (muito rápido para leitura)
DATA lt_cache TYPE HASHED TABLE OF ty_cache
  WITH UNIQUE KEY id.

" Ideal para lookups frequentes
DATA lt_config TYPE HASHED TABLE OF ty_config
  WITH UNIQUE KEY chave.

" Não permite índice numérico
" Não pode ser ordenada
```

## Inicialização e Preenchimento

### VALUE Constructor

```abap
" Tabela vazia
DATA(lt_empty) = VALUE ty_t_produtos( ).

" Com valores iniciais
DATA(lt_numeros) = VALUE ty_t_int( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

" Tabela de estruturas
DATA(lt_clientes) = VALUE ty_t_clientes(
  ( id = 1 nome = 'João Silva'   cidade = 'Lisboa' )
  ( id = 2 nome = 'Maria Santos' cidade = 'Porto' )
  ( id = 3 nome = 'Pedro Costa'  cidade = 'Braga' )
).

" Com FOR (loop inline)
DATA(lt_quadrados) = VALUE ty_t_int(
  FOR i = 1 THEN i + 1 WHILE i <= 10
  ( i * i )
).
```

### APPEND

```abap
" Adicionar estrutura
DATA ls_cliente TYPE ty_cliente.
ls_cliente = VALUE #( id = 1 nome = 'Ana' ).
APPEND ls_cliente TO lt_clientes.

" Inline
APPEND VALUE #( id = 2 nome = 'Bruno' ) TO lt_clientes.

" Múltiplas linhas
APPEND LINES OF lt_novos TO lt_clientes.

" Com FROM/TO
APPEND LINES OF lt_origem FROM 5 TO 10 TO lt_destino.
```

### INSERT

```abap
" Inserir em posição específica
DATA(ls_novo) = VALUE ty_cliente( id = 99 nome = 'Novo Cliente' ).
INSERT ls_novo INTO lt_clientes INDEX 1.

" Inserir inline
INSERT VALUE #( id = 100 nome = 'Outro' ) INTO lt_clientes INDEX 3.

" Inserir tabela em posição
INSERT LINES OF lt_novos INTO lt_clientes INDEX 5.

" Em sorted/hashed table (insere na posição correcta)
INSERT ls_cliente INTO TABLE lt_clientes_sorted.
```

## Leitura de Dados

### READ TABLE

```abap
" Por índice
READ TABLE lt_clientes INDEX 1 INTO DATA(ls_cliente).
IF sy-subrc = 0.
  WRITE: / ls_cliente-nome.
ENDIF.

" Por chave
READ TABLE lt_clientes INTO ls_cliente WITH KEY id = 100.

" Com ASSIGNING (mais eficiente)
READ TABLE lt_produtos ASSIGNING FIELD-SYMBOL(<ls_prod>) WITH KEY id = 200.
IF sy-subrc = 0.
  <ls_prod>-preco = <ls_prod>-preco * '1.1'.
ENDIF.

" Com REFERENCE
READ TABLE lt_clientes REFERENCE INTO DATA(lr_cliente) WITH KEY id = 50.

" Busca binária (em sorted table)
READ TABLE lt_clientes_sorted INTO ls_cliente 
  WITH KEY id = 100 BINARY SEARCH.
```

### Acesso Direto por []

```abap
" Por índice (pode causar dump se não existir)
DATA(ls_primeiro) = lt_clientes[ 1 ].

" Por chave
DATA(ls_cliente) = lt_clientes[ id = 100 ].

" Com KEY secundária
DATA(ls_venda) = lt_vendas[ KEY by_date data = lv_data ].

" Acesso a campo específico
DATA(lv_nome) = lt_clientes[ 1 ]-nome.

" Com DEFAULT (evita dump)
DATA(ls_safe) = VALUE #( lt_clientes[ id = 999 ] 
                          DEFAULT VALUE #( id = 0 nome = 'Não encontrado' ) ).

" OPTIONAL - retorna referência ou null
TRY.
    DATA(ls_opt) = lt_clientes[ id = 999 ].
  CATCH cx_sy_itab_line_not_found.
    " Tratar ausência
ENDTRY.
```

## Modificação de Dados

### MODIFY

```abap
" Modificar linha específica
DATA(ls_update) = VALUE ty_cliente( id = 1 nome = 'Nome Atualizado' ).
MODIFY TABLE lt_clientes FROM ls_update.

" Modificar por índice
MODIFY lt_clientes INDEX 1 FROM ls_update.

" Modificar dentro de loop
LOOP AT lt_produtos ASSIGNING FIELD-SYMBOL(<ls_prod>).
  <ls_prod>-preco = <ls_prod>-preco * '1.1'.
ENDLOOP.

" MODIFY dentro de loop (menos eficiente)
LOOP AT lt_clientes INTO DATA(ls_cli).
  ls_cli-ultima_atualizacao = sy-datum.
  MODIFY lt_clientes FROM ls_cli.
ENDLOOP.

" Inline com TRANSPORTING
MODIFY lt_produtos FROM VALUE #( id = 100 stock = 50 )
  TRANSPORTING stock WHERE id = 100.
```

### DELETE

```abap
" Deletar por índice
DELETE lt_clientes INDEX 1.

" Deletar por chave
DELETE TABLE lt_clientes WITH TABLE KEY id = 100.

" Deletar com WHERE
DELETE lt_produtos WHERE stock = 0.

" Deletar dentro de loop
LOOP AT lt_clientes INTO DATA(ls_cli).
  IF ls_cli-ativo = abap_false.
    DELETE lt_clientes.  " Deleta linha atual
  ENDIF.
ENDLOOP.

" Deletar linhas adjacentes
DELETE lt_clientes FROM 5 TO 10.

" Limpar tabela completa
CLEAR lt_clientes.
" ou
lt_clientes = VALUE #( ).
" ou
REFRESH lt_clientes.
```

## Ordenação

### SORT

```abap
" Ordenação simples (ascendente)
SORT lt_clientes BY nome.

" Descendente
SORT lt_clientes BY nome DESCENDING.

" Múltiplos campos
SORT lt_vendas BY pais ASCENDING ano DESCENDING mes ASCENDING.

" Caso insensitivo
SORT lt_produtos BY descricao AS TEXT.

" Estável (mantém ordem original para iguais)
SORT lt_dados BY campo STABLE.
```

## Filtros e Transformações

### FILTER

```abap
" Filtrar com WHERE
DATA(lt_ativos) = FILTER #( lt_clientes WHERE ativo = abap_true ).

" Múltiplas condições
DATA(lt_premium) = FILTER #( lt_clientes 
  WHERE tipo = 'VIP' 
    AND saldo > 10000 
    AND ativo = abap_true 
).

" EXCEPT - inverter filtro
DATA(lt_inativos) = FILTER #( lt_clientes EXCEPT WHERE ativo = abap_true ).

" Com chave secundária
DATA(lt_filtrado) = FILTER #( lt_vendas 
  USING KEY by_pais 
  WHERE pais = 'PT' 
).
```

### REDUCE (Agregações)

```abap
" Somar valores
DATA(lv_total) = REDUCE decfloat34(
  INIT sum = 0
  FOR ls_venda IN lt_vendas
  NEXT sum = sum + ls_venda-valor
).

" Contar com condição
DATA(lv_count_ativos) = REDUCE i(
  INIT cnt = 0
  FOR ls_cliente IN lt_clientes
  WHERE ( ativo = abap_true )
  NEXT cnt = cnt + 1
).

" Encontrar máximo
DATA(lv_max_preco) = REDUCE decfloat34(
  INIT max = 0
  FOR ls_produto IN lt_produtos
  NEXT max = COND #( WHEN ls_produto-preco > max 
                     THEN ls_produto-preco 
                     ELSE max )
).

" Concatenar strings
DATA(lv_nomes) = REDUCE string(
  INIT texto = ''
  FOR ls_cliente IN lt_clientes
  NEXT texto = texto && ls_cliente-nome && ', '
).
```

### CORRESPONDING

```abap
" Converter entre tabelas de tipos diferentes
DATA(lt_destino) = CORRESPONDING ty_t_destino( lt_origem ).

" Com BASE (manter existentes)
lt_destino = CORRESPONDING #( BASE lt_destino lt_origem ).

" Com MAPPING
DATA(lt_resumo) = CORRESPONDING ty_t_resumo( lt_vendas
  MAPPING id_resumo = id_venda
          total = valor
).

" EXCEPT campos
DATA(lt_parcial) = CORRESPONDING ty_t_parcial( lt_completo
  EXCEPT campo1 campo2
).
```

## Operações com Conjuntos

### Comparação de Tabelas

```abap
" Verificar se iguais
IF lt_tabela1 = lt_tabela2.
  WRITE: / 'Tabelas idênticas'.
ENDIF.

" Verificar se vazia
IF lt_clientes IS INITIAL.
  WRITE: / 'Sem clientes'.
ENDIF.

IF lt_clientes IS NOT INITIAL.
  " Processar
ENDIF.
```

### Funções de Tabela

```abap
" Número de linhas
DATA(lv_count) = lines( lt_clientes ).

" Verificar existência
IF line_exists( lt_clientes[ id = 100 ] ).
  WRITE: / 'Cliente existe'.
ENDIF.

" Obter índice
DATA(lv_index) = line_index( lt_clientes[ nome = 'João' ] ).
IF lv_index > 0.
  WRITE: / |Encontrado no índice { lv_index }|.
ENDIF.
```

## GROUP BY (Agrupamento)

```abap
" Agrupar e processar
LOOP AT lt_vendas INTO DATA(ls_venda)
  GROUP BY ( pais = ls_venda-pais 
             ano  = ls_venda-ano )
  ASSIGNING FIELD-SYMBOL(<group>).
  
  WRITE: / |País: { <group>-pais }, Ano: { <group>-ano }|.
  
  " Processar membros do grupo
  DATA(lv_total) = REDUCE decfloat34(
    INIT sum = 0
    FOR ls_item IN GROUP <group>
    NEXT sum = sum + ls_item-valor
  ).
  
  WRITE: / |Total: { lv_total }|.
  
ENDLOOP.

" Criar tabela agrupada
DATA(lt_agrupado) = VALUE ty_t_resumo(
  FOR GROUPS <group> OF ls_venda IN lt_vendas
  GROUP BY ( pais = ls_venda-pais )
  ( pais  = <group>-pais
    total = REDUCE decfloat34(
              INIT sum = 0
              FOR ls IN GROUP <group>
              NEXT sum = sum + ls-valor
            )
    count = REDUCE i(
              INIT cnt = 0
              FOR ls IN GROUP <group>
              NEXT cnt = cnt + 1
            )
  )
).
```

## Tabelas Aninhadas

```abap
TYPES: BEGIN OF ty_cabecalho,
         numero TYPE string,
         data   TYPE datum,
         itens  TYPE ty_t_itens,
       END OF ty_cabecalho.

DATA(lt_pedidos) = VALUE ty_t_cabecalho(
  ( numero = 'P001'
    data   = sy-datum
    itens  = VALUE #(
      ( produto = 'A' qtd = 5 preco = '10.00' )
      ( produto = 'B' qtd = 3 preco = '20.00' )
    )
  )
  ( numero = 'P002'
    data   = sy-datum
    itens  = VALUE #(
      ( produto = 'C' qtd = 10 preco = '5.00' )
    )
  )
).

" Processar tabelas aninhadas
LOOP AT lt_pedidos INTO DATA(ls_pedido).
  WRITE: / |Pedido: { ls_pedido-numero }|.
  
  LOOP AT ls_pedido-itens INTO DATA(ls_item).
    WRITE: /5 |Produto: { ls_item-produto }, Qtd: { ls_item-qtd }|.
  ENDLOOP.
ENDLOOP.
```

## Dicas de Boas Práticas

!!! tip "Escolha do Tipo de Tabela"
    - **STANDARD**: Uso geral, permite duplicados, acesso por índice
    - **SORTED**: Quando precisa de ordem, busca binária automática
    - **HASHED**: Lookups muito frequentes, grandes volumes

!!! warning "Performance"
    ```abap
    " ❌ Evitar - leitura linear em loop
    LOOP AT lt_pedidos INTO DATA(ls_ped).
      READ TABLE lt_clientes INTO DATA(ls_cli) 
        WITH KEY id = ls_ped-cliente_id.
    ENDLOOP.
    
    " ✅ Preferir - usar HASHED ou SORTED
    DATA lt_clientes TYPE HASHED TABLE OF ty_cliente
      WITH UNIQUE KEY id.
    
    " ✅ Ou criar índice secundário
    DATA lt_clientes TYPE STANDARD TABLE OF ty_cliente
      WITH NON-UNIQUE KEY id
      WITH UNIQUE HASHED KEY by_id COMPONENTS id.
    ```

!!! example "Sintaxe Moderna"
    ```abap
    " ❌ Forma antiga
    DATA lt_resultado TYPE ty_t_clientes.
    DATA ls_cliente TYPE ty_cliente.
    
    LOOP AT lt_clientes INTO ls_cliente WHERE ativo = abap_true.
      APPEND ls_cliente TO lt_resultado.
    ENDLOOP.
    
    " ✅ Forma moderna
    DATA(lt_resultado) = FILTER #( lt_clientes WHERE ativo = abap_true ).
    ```

!!! info "Declaração de Chaves"
    ```abap
    " ❌ Evitar DEFAULT KEY (ambíguo)
    DATA lt_data TYPE STANDARD TABLE OF ty_struct WITH DEFAULT KEY.
    
    " ✅ Ser explícito
    DATA lt_data TYPE STANDARD TABLE OF ty_struct WITH EMPTY KEY.
    " ou
    DATA lt_data TYPE STANDARD TABLE OF ty_struct 
      WITH NON-UNIQUE KEY campo1 campo2.
    ```

---

**Tags:** #Fundamentos #Tabelas-Internas #Modern-ABAP #Performance