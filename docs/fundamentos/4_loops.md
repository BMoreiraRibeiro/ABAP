# Loops

## LOOP AT (Tabelas Internas)

### Sintaxe Moderna com Declaração Inline

```abap
" Declaração inline do work area
LOOP AT lt_clientes INTO DATA(ls_cliente).
  WRITE: / ls_cliente-nome, ls_cliente-cidade.
ENDLOOP.

" Com field-symbol (mais eficiente, não copia dados)
LOOP AT lt_produtos ASSIGNING FIELD-SYMBOL(<ls_produto>).
  <ls_produto>-preco = <ls_produto>-preco * '1.1'.  " Aumenta 10%
ENDLOOP.

" Com referência (para estruturas grandes)
LOOP AT lt_documentos REFERENCE INTO DATA(lr_doc).
  WRITE: / lr_doc->numero, lr_doc->descricao.
ENDLOOP.
```

### Sintaxe Tradicional

```abap
DATA ls_cliente TYPE ty_cliente.

LOOP AT lt_clientes INTO ls_cliente.
  WRITE: / ls_cliente-nome.
ENDLOOP.

" Com field-symbol declarado
FIELD-SYMBOLS <ls_produto> TYPE ty_produto.

LOOP AT lt_produtos ASSIGNING <ls_produto>.
  <ls_produto>-stock = <ls_produto>-stock - 1.
ENDLOOP.
```

## Loop com Condições WHERE

```abap
" Filtrar durante o loop
LOOP AT lt_clientes INTO DATA(ls_cliente)
  WHERE pais = 'PT' AND ativo = abap_true.
  
  WRITE: / ls_cliente-nome.
ENDLOOP.

" Múltiplas condições
LOOP AT lt_produtos ASSIGNING FIELD-SYMBOL(<ls_prod>)
  WHERE categoria = 'ELETRO' 
    AND preco > 100
    AND stock > 0.
  
  " Processar produtos
ENDLOOP.

" Com OR
LOOP AT lt_pedidos INTO DATA(ls_pedido)
  WHERE status = 'A' OR status = 'P'.
  
  WRITE: / ls_pedido-numero.
ENDLOOP.
```

## Loop com FROM/TO

```abap
" Processar linhas específicas
LOOP AT lt_dados INTO DATA(ls_dado) FROM 10 TO 20.
  WRITE: / ls_dado-campo.
ENDLOOP.

" Apenas primeiros N registos
LOOP AT lt_clientes INTO DATA(ls_cliente) TO 100.
  " Processa apenas primeiros 100
ENDLOOP.
```

## Controlo de Fluxo no Loop

### CONTINUE e EXIT

```abap
LOOP AT lt_produtos INTO DATA(ls_produto).
  
  " Saltar este registo
  IF ls_produto-tipo = 'OBSOLETO'.
    CONTINUE.  " Vai para próxima iteração
  ENDIF.
  
  " Parar loop completamente
  IF ls_produto-categoria = 'DESCONTINUADO'.
    EXIT.  " Sai do loop
  ENDIF.
  
  " Processar produto
  WRITE: / ls_produto-descricao.
  
ENDLOOP.
```

### CHECK

```abap
" CHECK: continua apenas se condição verdadeira
LOOP AT lt_clientes INTO DATA(ls_cliente).
  
  CHECK ls_cliente-ativo = abap_true.  " Salta se não ativo
  CHECK ls_cliente-pais = 'PT'.        " Salta se não PT
  
  " Só chega aqui se ambas condições verdadeiras
  WRITE: / ls_cliente-nome.
  
ENDLOOP.
```

## Informação do Loop (sy-tabix, sy-subrc)

```abap
LOOP AT lt_produtos INTO DATA(ls_produto).
  
  " sy-tabix = índice atual (1-based)
  WRITE: / |Linha { sy-tabix }: { ls_produto-nome }|.
  
  " Processar apenas linhas pares
  IF sy-tabix MOD 2 = 0.
    " Lógica para pares
  ENDIF.
  
ENDLOOP.

" sy-subrc após loop
IF sy-subrc = 0.
  WRITE: / 'Loop processou pelo menos 1 registo'.
ELSE.
  WRITE: / 'Tabela vazia ou nenhum registo correspondeu'.
ENDIF.
```

## DO (Loop com Contador)

```abap
" Loop N vezes
DO 10 TIMES.
  WRITE: / |Iteração { sy-index }|.
ENDDO.

" Loop infinito (com EXIT)
DATA lv_contador TYPE i.

DO.
  lv_contador = lv_contador + 1.
  
  IF lv_contador > 100.
    EXIT.
  ENDIF.
  
  " Processar
ENDDO.

" Com variável inline
DO DATA(lv_max) TIMES.
  lv_max = 5.
  WRITE: / sy-index.
ENDDO.
```

## WHILE (Loop com Condição)

```abap
DATA lv_soma TYPE i VALUE 0.
DATA lv_num TYPE i VALUE 1.

WHILE lv_soma < 100.
  lv_soma = lv_soma + lv_num.
  lv_num = lv_num + 1.
ENDWHILE.

WRITE: / |Soma final: { lv_soma }|.

" Com múltiplas condições
WHILE lv_contador < 1000 AND lv_erro = abap_false.
  " Processar
  lv_contador = lv_contador + 1.
ENDWHILE.
```

## Loops Aninhados

```abap
" Loop dentro de loop
LOOP AT lt_clientes INTO DATA(ls_cliente).
  
  WRITE: / |Cliente: { ls_cliente-nome }|.
  
  LOOP AT lt_pedidos INTO DATA(ls_pedido)
    WHERE cliente_id = ls_cliente-id.
    
    WRITE: /10 |Pedido: { ls_pedido-numero }|.
    
  ENDLOOP.
  
ENDLOOP.

" Com controlo de fluxo
LOOP AT lt_categorias INTO DATA(ls_cat).
  
  DATA(lv_encontrado) = abap_false.
  
  LOOP AT lt_produtos INTO DATA(ls_prod)
    WHERE categoria = ls_cat-id.
    
    lv_encontrado = abap_true.
    EXIT.  " Apenas verifica se existe
    
  ENDLOOP.
  
  IF lv_encontrado = abap_true.
    WRITE: / |Categoria { ls_cat-nome } tem produtos|.
  ENDIF.
  
ENDLOOP.
```

## Loop em Grupos (GROUP BY)

```abap
" Agrupar e processar
LOOP AT lt_vendas INTO DATA(ls_venda)
  GROUP BY ( pais = ls_venda-pais 
             ano  = ls_venda-ano )
  ASSIGNING FIELD-SYMBOL(<group>).
  
  WRITE: / |País: { <group>-pais }, Ano: { <group>-ano }|.
  
  DATA lv_total TYPE p DECIMALS 2.
  CLEAR lv_total.
  
  " Loop nos membros do grupo
  LOOP AT GROUP <group> INTO DATA(ls_venda_grupo).
    lv_total = lv_total + ls_venda_grupo-valor.
  ENDLOOP.
  
  WRITE: / |Total: { lv_total }|.
  
ENDLOOP.
```

## Loop com Índice Personalizado

```abap
DATA lv_contador TYPE i.

LOOP AT lt_dados INTO DATA(ls_dado).
  
  lv_contador = lv_contador + 1.
  
  WRITE: / |Registo { lv_contador } de { lines( lt_dados ) }: { ls_dado-valor }|.
  
  " Processar último registo de forma diferente
  IF lv_contador = lines( lt_dados ).
    WRITE: / 'Este é o último registo'.
  ENDIF.
  
ENDLOOP.
```

## Parallel Cursor (Múltiplas Tabelas)

```abap
DATA lv_idx TYPE i VALUE 1.

LOOP AT lt_cabecalho INTO DATA(ls_cab).
  
  " Ler tabela de itens na posição correspondente
  READ TABLE lt_itens INDEX lv_idx INTO DATA(ls_item).
  IF sy-subrc = 0.
    WRITE: / |{ ls_cab-numero } - { ls_item-descricao }|.
  ENDIF.
  
  lv_idx = lv_idx + 1.
  
ENDLOOP.
```

## REDUCE (Redução/Agregação)

```abap
" Somar valores
DATA(lv_total) = REDUCE decfloat34( 
  INIT sum = 0
  FOR ls_produto IN lt_produtos
  NEXT sum = sum + ls_produto-preco 
).

" Concatenar strings
DATA(lv_nomes) = REDUCE string(
  INIT texto = ''
  FOR ls_cliente IN lt_clientes
  NEXT texto = texto && ls_cliente-nome && ', '
).

" Contar registos com condição
DATA(lv_ativos) = REDUCE i(
  INIT count = 0
  FOR ls_cliente IN lt_clientes
  WHERE ( ativo = abap_true )
  NEXT count = count + 1
).
```

## FOR (Iteração em Expressões)

```abap
" Construir tabela com FOR
DATA(lt_quadrados) = VALUE ty_t_numeros(
  FOR i = 1 THEN i + 1 WHILE i <= 10
  ( i * i )
).

" Transformar tabela
DATA(lt_nomes) = VALUE string_table(
  FOR ls_cliente IN lt_clientes
  ( ls_cliente-nome )
).

" Com condição WHERE
DATA(lt_ativos) = VALUE ty_t_clientes(
  FOR ls_cliente IN lt_clientes
  WHERE ( ativo = abap_true )
  ( ls_cliente )
).

" Transformação complexa
DATA(lt_resumo) = VALUE ty_t_resumo(
  FOR ls_venda IN lt_vendas
  ( id     = ls_venda-id
    total  = ls_venda-quantidade * ls_venda-preco_unit
    status = COND #( WHEN ls_venda-pago = abap_true 
                     THEN 'Pago' 
                     ELSE 'Pendente' )
  )
).
```

## Dicas de Boas Práticas

!!! tip "Performance"
    - Use `ASSIGNING FIELD-SYMBOL` para modificar registos (evita cópia)
    - Use `INTO DATA()` para apenas leitura
    - Use `REFERENCE INTO` para estruturas muito grandes
    - Aplique WHERE sempre que possível (filtra antes do loop)

!!! warning "Armadilhas Comuns"
    ```abap
    " ❌ Modificar tabela durante loop (pode causar problemas)
    LOOP AT lt_dados INTO DATA(ls_dado).
      APPEND ls_dado TO lt_dados.  " PERIGO!
    ENDLOOP.
    
    " ✅ Usar tabela auxiliar
    DATA lt_novos LIKE lt_dados.
    LOOP AT lt_dados INTO DATA(ls_dado).
      APPEND ls_dado TO lt_novos.
    ENDLOOP.
    APPEND LINES OF lt_novos TO lt_dados.
    
    " ❌ Esquecer de limpar work area
    DATA ls_cliente TYPE ty_cliente.
    LOOP AT lt_clientes INTO ls_cliente.
      " ls_cliente mantém valor da iteração anterior!
    ENDLOOP.
    
    " ✅ Usar declaração inline
    LOOP AT lt_clientes INTO DATA(ls_cliente).
      " ls_cliente é sempre limpo
    ENDLOOP.
    ```

!!! example "Loop Moderno vs. Tradicional"
    ```abap
    " ❌ Forma antiga
    DATA ls_produto TYPE ty_produto.
    LOOP AT lt_produtos INTO ls_produto.
      ls_produto-preco = ls_produto-preco * '1.1'.
      MODIFY lt_produtos FROM ls_produto.
    ENDLOOP.
    
    " ✅ Forma moderna
    LOOP AT lt_produtos ASSIGNING FIELD-SYMBOL(<ls_produto>).
      <ls_produto>-preco = <ls_produto>-preco * '1.1'.
    ENDLOOP.
    ```

!!! info "Escolher o Loop Correto"
    - **LOOP AT**: Tabelas internas
    - **DO**: Número fixo de iterações
    - **WHILE**: Condição dinâmica
    - **FOR**: Expressões de transformação
    - **REDUCE**: Agregações e cálculos

---

**Tags:** #Fundamentos #Loops #Modern-ABAP #Performance