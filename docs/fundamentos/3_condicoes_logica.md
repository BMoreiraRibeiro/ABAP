# Estruturas

## Declaração de Estruturas

### Estruturas Locais

```abap
TYPES: BEGIN OF ty_endereco,
         rua      TYPE string,
         numero   TYPE string,
         cidade   TYPE string,
         codigo   TYPE string,
         pais     TYPE land1,
       END OF ty_endereco.

TYPES: BEGIN OF ty_pessoa,
         id       TYPE i,
         nome     TYPE string,
         idade    TYPE i,
         endereco TYPE ty_endereco,
         salario  TYPE p DECIMALS 2,
       END OF ty_pessoa.

DATA ls_pessoa TYPE ty_pessoa.
```

### Estruturas Baseadas em DDIC

```abap
" Usar estrutura do dicionário
DATA ls_scarr TYPE scarr.
DATA ls_mara  TYPE mara.

" Declaração inline com SELECT
SELECT SINGLE * FROM kna1 INTO @DATA(ls_cliente) WHERE kunnr = '0000100000'.
```

## Inicialização de Estruturas

### VALUE Constructor

```abap
" Sintaxe moderna - preenchimento completo
DATA(ls_produto) = VALUE ty_produto(
  id          = 1001
  descricao   = 'Portátil Dell'
  preco       = '899.99'
  quantidade  = 15
  data_registo = sy-datum
).

" Preenchimento parcial
DATA(ls_config) = VALUE ty_config(
  ativo = abap_true
  " outros campos ficam com valor inicial
).

" Com BASE - manter valores existentes
ls_produto = VALUE #( BASE ls_produto
  preco      = '799.99'  " Atualiza apenas o preço
  quantidade = 20        " e a quantidade
).
```

### CORRESPONDING

```abap
" Copiar campos com mesmo nome
TYPES: BEGIN OF ty_origem,
         campo1 TYPE string,
         campo2 TYPE i,
         campo3 TYPE string,
       END OF ty_origem.

TYPES: BEGIN OF ty_destino,
         campo1 TYPE string,
         campo2 TYPE i,
         campo4 TYPE string,
       END OF ty_destino.

DATA(ls_origem) = VALUE ty_origem(
  campo1 = 'A'
  campo2 = 10
  campo3 = 'X'
).

" Copia campo1 e campo2
DATA(ls_destino) = CORRESPONDING #( ls_origem ).

" Com mapeamento adicional
ls_destino = CORRESPONDING #( BASE ( ls_destino ) ls_origem MAPPING campo4 = campo3 ).
```

## Acesso a Campos

### Acesso Direto

```abap
" Leitura
DATA(lv_nome) = ls_pessoa-nome.
DATA(lv_rua) = ls_pessoa-endereco-rua.

" Escrita
ls_pessoa-idade = 30.
ls_pessoa-endereco-cidade = 'Lisboa'.

" Com field-symbols
FIELD-SYMBOLS <lv_campo> TYPE any.
ASSIGN ls_pessoa-nome TO <lv_campo>.
<lv_campo> = 'João Silva'.
```

### Acesso Dinâmico

```abap
DATA lv_nome_campo TYPE string VALUE 'NOME'.

" Atribuição dinâmica
ASSIGN COMPONENT lv_nome_campo OF STRUCTURE ls_pessoa TO FIELD-SYMBOL(<lv_valor>).
IF sy-subrc = 0.
  <lv_valor> = 'Maria Santos'.
ENDIF.

" Leitura dinâmica
DATA lv_conteudo TYPE string.
ASSIGN COMPONENT 'IDADE' OF STRUCTURE ls_pessoa TO <lv_valor>.
IF sy-subrc = 0.
  lv_conteudo = <lv_valor>.
ENDIF.
```

## Estruturas Aninhadas

### Definição

```abap
TYPES: BEGIN OF ty_contacto,
         telefone TYPE string,
         email    TYPE string,
       END OF ty_contacto.

TYPES: BEGIN OF ty_funcionario,
         numero    TYPE i,
         nome      TYPE string,
         contacto  TYPE ty_contacto,
         morada    TYPE ty_endereco,
       END OF ty_funcionario.
```

### Manipulação

```abap
DATA(ls_funcionario) = VALUE ty_funcionario(
  numero = 12345
  nome   = 'Ana Costa'
  contacto = VALUE #(
    telefone = '+351 910000000'
    email    = 'ana.costa@empresa.pt'
  )
  morada = VALUE #(
    rua    = 'Rua das Flores'
    numero = '123'
    cidade = 'Porto'
    codigo = '4000-001'
  )
).

" Acesso em profundidade
WRITE: / ls_funcionario-contacto-email.
WRITE: / ls_funcionario-morada-cidade.
```

## Operações com Estruturas

### Comparação

```abap
DATA ls_pessoa1 TYPE ty_pessoa.
DATA ls_pessoa2 TYPE ty_pessoa.

ls_pessoa1 = VALUE #( id = 1 nome = 'João' idade = 25 ).
ls_pessoa2 = VALUE #( id = 1 nome = 'João' idade = 25 ).

" Comparação completa
IF ls_pessoa1 = ls_pessoa2.
  WRITE: / 'Estruturas iguais'.
ENDIF.

" Comparação de campos específicos
IF ls_pessoa1-nome = ls_pessoa2-nome.
  WRITE: / 'Nomes iguais'.
ENDIF.
```

### Limpeza

```abap
" Limpar toda a estrutura
CLEAR ls_pessoa.

" Sintaxe moderna
ls_pessoa = VALUE #( ).

" Limpar campos específicos (manter outros)
ls_pessoa = VALUE #( BASE ls_pessoa
  salario = 0
  idade   = 0
).
```

### Move-Corresponding

```abap
" Move campos com mesmo nome
DATA ls_origem TYPE ty_pessoa.
DATA ls_destino TYPE ty_funcionario.

ls_origem = VALUE #( nome = 'Pedro' ).

MOVE-CORRESPONDING ls_origem TO ls_destino.
" ou sintaxe moderna:
ls_destino = CORRESPONDING #( ls_origem ).
```

## Estruturas Include

```abap
TYPES: BEGIN OF ty_auditoria,
         criado_por TYPE syuname,
         criado_em  TYPE timestamp,
         alterado_por TYPE syuname,
         alterado_em  TYPE timestamp,
       END OF ty_auditoria.

TYPES: BEGIN OF ty_documento.
         INCLUDE TYPE ty_auditoria.
TYPES:   numero TYPE string,
         descricao TYPE string,
       END OF ty_documento.

" ty_documento contém todos os campos de ty_auditoria + os próprios
```

## Component Operator

```abap
" Acesso moderno a componentes
DATA(lv_email) = ls_funcionario-contacto-email.

" Verificar existência de componente
IF ls_funcionario CS 'NOME'.  " CS = Contains String (campo existe)
  WRITE: / 'Campo NOME existe'.
ENDIF.

" Número de componentes
DESCRIBE FIELD ls_funcionario COMPONENTS DATA(lv_num_campos).
```

## Trabalhar com Includes de Tabelas DDIC

```abap
" Estrutura que usa includes do dicionário
DATA ls_vbak TYPE vbak.  " Contém includes como VBAP_INCL

" Acesso normal aos campos incluídos
WRITE: / ls_vbak-vbeln.  " Campo da própria estrutura
WRITE: / ls_vbak-erdat.  " Campo de um include
```

## Estruturas Deep vs. Flat

### Estrutura Flat (sem tabelas internas ou referências)

```abap
TYPES: BEGIN OF ty_flat,
         campo1 TYPE c LENGTH 10,
         campo2 TYPE i,
         campo3 TYPE p DECIMALS 2,
       END OF ty_flat.

" Pode ser usada com: EXPORT/IMPORT, MOVE, operações binárias
```

### Estrutura Deep (com tabelas internas ou strings)

```abap
TYPES: BEGIN OF ty_deep,
         id     TYPE i,
         nome   TYPE string,           " STRING = deep
         itens  TYPE ty_t_itens,       " Tabela interna = deep
       END OF ty_deep.

" Requer tratamento especial em certas operações
```

## Dicas de Boas Práticas

!!! tip "Nomenclatura de Tipos"
    - Use `ty_` para tipos de estrutura: `ty_cliente`, `ty_pedido`
    - Use `ty_t_` para tipos de tabela: `ty_t_clientes`, `ty_t_pedidos`
    - Seja descritivo: `ty_pessoa` melhor que `ty_p`

!!! warning "Performance"
    - Evite estruturas demasiado grandes (muitos campos desnecessários)
    - Em loops, aceda diretamente aos campos necessários
    - Use `CORRESPONDING` com cuidado em loops (pode ser lento)

!!! example "VALUE vs. Atribuição Individual"
    ```abap
    " ❌ Menos legível
    DATA ls_cliente TYPE ty_cliente.
    ls_cliente-id = 1.
    ls_cliente-nome = 'João'.
    ls_cliente-ativo = abap_true.
    
    " ✅ Mais limpo
    DATA(ls_cliente) = VALUE ty_cliente(
      id    = 1
      nome  = 'João'
      ativo = abap_true
    ).
    ```

!!! info "Estruturas vs. Classes"
    - Estruturas: dados apenas (sem comportamento)
    - Classes: dados + métodos (comportamento)
    - Use estruturas para DTOs (Data Transfer Objects)
    - Use classes quando precisar de encapsulamento e lógica

---

**Tags:** #Fundamentos #Estruturas #Modern-ABAP