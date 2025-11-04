# Condições e Lógica

## 📋 Visão Geral

Este capítulo aborda as estruturas condicionais e lógicas em ABAP, essenciais para controlar o fluxo de execução do programa com base em condições.

---

## 🎯 Operadores Lógicos

### Operadores de Comparação

```abap
" Operadores relacionais
=   " Igual a
<>  " Diferente de (também NE)
<   " Menor que (também LT)
>   " Maior que (também GT)
<=  " Menor ou igual (também LE)
>=  " Maior ou igual (também GE)

" Exemplos
IF idade >= 18.
  WRITE: / 'Maior de idade'.
ENDIF.

IF status <> 'CANCELADO'.
  WRITE: / 'Pedido ativo'.
ENDIF.
```

### Operadores Booleanos

```abap
AND  " E lógico (ambas condições verdadeiras)
OR   " OU lógico (pelo menos uma verdadeira)
NOT  " Negação

" Exemplos
IF idade >= 18 AND tem_carta = abap_true.
  WRITE: / 'Pode conduzir'.
ENDIF.

IF categoria = 'VIP' OR valor_compra > 1000.
  WRITE: / 'Cliente premium'.
ENDIF.

IF NOT bloqueado = abap_true.
  WRITE: / 'Cliente ativo'.
ENDIF.
```

### Operadores de String

```abap
CO  " Contains Only (contém apenas)
CN  " Contains Not (não contém)
CA  " Contains Any (contém algum)
NA  " Not Any (não contém nenhum)
CS  " Contains String (contém texto)
NS  " Not String (não contém texto)
CP  " Contains Pattern (padrão com *)
NP  " Not Pattern

" Exemplos
DATA lv_texto TYPE string VALUE 'ABAP2025'.

IF lv_texto CO '0123456789'.
  " Contém apenas números
ENDIF.

IF lv_texto CA '0123456789'.
  " Contém pelo menos um número
  WRITE: / 'Tem números'.
ENDIF.

IF lv_texto CS 'ABAP'.
  " Contém a substring 'ABAP'
  WRITE: / 'É sobre ABAP'.
ENDIF.

IF lv_texto CP 'A*P'.
  " Padrão: começa com A e termina com P
  WRITE: / 'Match de padrão'.
ENDIF.
```

---

## 🔀 IF - ELSEIF - ELSE

### Sintaxe Básica

```abap
" IF simples
IF condicao.
  " código se verdadeiro
ENDIF.

" IF-ELSE
IF nota >= 10.
  WRITE: / 'Aprovado'.
ELSE.
  WRITE: / 'Reprovado'.
ENDIF.

" IF-ELSEIF-ELSE
IF nota >= 16.
  WRITE: / 'Excelente'.
ELSEIF nota >= 14.
  WRITE: / 'Muito Bom'.
ELSEIF nota >= 10.
  WRITE: / 'Aprovado'.
ELSE.
  WRITE: / 'Reprovado'.
ENDIF.
```

### Condições Múltiplas

```abap
DATA: lv_idade    TYPE i VALUE 25,
      lv_salario  TYPE p DECIMALS 2 VALUE '2500.00',
      lv_categoria TYPE char1 VALUE 'A'.

" Múltiplas condições com AND
IF lv_idade > 18 AND lv_salario > 2000 AND lv_categoria = 'A'.
  WRITE: / 'Cliente Premium'.
ENDIF.

" Múltiplas condições com OR
IF lv_categoria = 'A' OR lv_categoria = 'B' OR lv_salario > 5000.
  WRITE: / 'Desconto especial'.
ENDIF.

" Combinação de AND e OR (usar parênteses)
IF ( lv_idade > 60 OR lv_categoria = 'VIP' ) AND lv_salario > 1000.
  WRITE: / 'Benefício concedido'.
ENDIF.
```

### IF Inline (Expressão Ternária)

```abap
" Sintaxe moderna - expressão condicional
DATA(lv_descricao) = COND string( 
  WHEN idade >= 18 THEN 'Adulto'
  ELSE 'Menor'
).

" Com múltiplas condições
DATA(lv_categoria) = COND char1(
  WHEN nota >= 16 THEN 'A'
  WHEN nota >= 14 THEN 'B'
  WHEN nota >= 10 THEN 'C'
  ELSE 'F'
).

" Usar em atribuições
DATA(lv_preco_final) = COND p DECIMALS 2(
  WHEN cliente_vip = abap_true THEN preco * '0.8'  " 20% desconto
  WHEN valor_compra > 1000     THEN preco * '0.9'  " 10% desconto
  ELSE preco
).
```

---

## 🔄 CASE - WHEN

### Sintaxe Básica

```abap
" CASE simples
CASE mes.
  WHEN 1.
    WRITE: / 'Janeiro'.
  WHEN 2.
    WRITE: / 'Fevereiro'.
  WHEN 3.
    WRITE: / 'Março'.
  WHEN OTHERS.
    WRITE: / 'Outro mês'.
ENDCASE.

" CASE com múltiplos valores
CASE dia_semana.
  WHEN 1 OR 7.
    WRITE: / 'Fim de semana'.
  WHEN 2 OR 3 OR 4 OR 5 OR 6.
    WRITE: / 'Dia útil'.
ENDCASE.
```

### CASE com Ranges

```abap
DATA lv_nota TYPE i VALUE 15.

CASE lv_nota.
  WHEN 0 TO 9.
    WRITE: / 'Reprovado'.
  WHEN 10 TO 13.
    WRITE: / 'Suficiente'.
  WHEN 14 TO 15.
    WRITE: / 'Bom'.
  WHEN 16 TO 17.
    WRITE: / 'Muito Bom'.
  WHEN 18 TO 20.
    WRITE: / 'Excelente'.
  WHEN OTHERS.
    WRITE: / 'Nota inválida'.
ENDCASE.
```

### CASE Inline (SWITCH)

```abap
" Sintaxe moderna - expressão SWITCH
DATA(lv_trimestre) = SWITCH i(
  mes
  WHEN 1 OR 2 OR 3     THEN 1
  WHEN 4 OR 5 OR 6     THEN 2
  WHEN 7 OR 8 OR 9     THEN 3
  WHEN 10 OR 11 OR 12  THEN 4
).

" Com strings
DATA(lv_saudacao) = SWITCH string(
  hora
  WHEN 0 TO 5    THEN 'Boa madrugada'
  WHEN 6 TO 11   THEN 'Bom dia'
  WHEN 12 TO 17  THEN 'Boa tarde'
  WHEN 18 TO 23  THEN 'Boa noite'
).

" Em cálculos
DATA(lv_taxa) = SWITCH p DECIMALS 2(
  tipo_cliente
  WHEN 'VIP'    THEN '0.05'
  WHEN 'PREMIUM' THEN '0.10'
  WHEN 'NORMAL'  THEN '0.15'
  ELSE '0.20'
).
```

---

## ✅ CHECK e ASSERT

### CHECK - Sair se Condição Falsa

```abap
" Em procedimentos
METHOD processar_pedido.
  " Sai do método se pedido cancelado
  CHECK status <> 'CANCELADO'.
  
  " Continua processamento...
ENDMETHOD.

" Em loops
LOOP AT lt_clientes INTO DATA(ls_cliente).
  " Pula registos inativos
  CHECK ls_cliente-ativo = abap_true.
  
  " Processa apenas clientes ativos
  WRITE: / ls_cliente-nome.
ENDLOOP.

" Sintaxe clássica
DATA lv_numero TYPE i VALUE 5.
CHECK lv_numero > 0.
WRITE: / 'Número positivo'.  " Só executa se CHECK passar
```

### ASSERT - Validação em Desenvolvimento

```abap
" ASSERT para debugging (não usar em produção)
DATA lv_divisor TYPE i VALUE 0.

" Lança exceção se condição for falsa
ASSERT lv_divisor <> 0.  " Para execução se divisor for zero

DATA(lv_resultado) = 10 / lv_divisor.

" Melhor usar IF em produção
IF lv_divisor = 0.
  MESSAGE 'Divisão por zero' TYPE 'E'.
ELSE.
  lv_resultado = 10 / lv_divisor.
ENDIF.
```

---

## 🔍 Verificações Úteis

### Verificar Valores Iniciais

```abap
" IS INITIAL - verifica se está vazio/inicial
IF lv_nome IS INITIAL.
  WRITE: / 'Nome não preenchido'.
ENDIF.

IF lv_tabela IS NOT INITIAL.
  WRITE: / 'Tabela tem dados'.
ENDIF.

" Valores iniciais por tipo:
" - Números: 0
" - Strings: ''
" - Tabelas: vazia
" - Estruturas: todos campos iniciais
```

### Verificar Existência em Tabela

```abap
" LINE_EXISTS - moderna
IF line_exists( lt_clientes[ id = 100 ] ).
  WRITE: / 'Cliente existe'.
ENDIF.

" Alternativa clássica
READ TABLE lt_clientes WITH KEY id = 100 TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  WRITE: / 'Cliente existe'.
ENDIF.
```

### Verificar Intervalo (BETWEEN)

```abap
DATA lv_preco TYPE p DECIMALS 2 VALUE '50.00'.

IF lv_preco BETWEEN '10.00' AND '100.00'.
  WRITE: / 'Preço dentro do intervalo'.
ENDIF.

" Equivalente a:
IF lv_preco >= '10.00' AND lv_preco <= '100.00'.
  WRITE: / 'Preço dentro do intervalo'.
ENDIF.
```

### Verificar Tipo de Dados

```abap
DATA lv_valor TYPE string VALUE '123'.

" Verificar se é numérico
IF lv_valor CO '0123456789'.
  DATA(lv_numero) = CONV i( lv_valor ).
  WRITE: / 'Valor convertido:', lv_numero.
ENDIF.
```

---

## 🎨 Boas Práticas

### ✅ Fazer

```abap
" 1. Usar expressões modernas quando apropriado
DATA(lv_status) = COND string(
  WHEN nota >= 10 THEN 'Aprovado'
  ELSE 'Reprovado'
).

" 2. Extrair condições complexas para variáveis
DATA(lv_cliente_premium) = xsdbool( 
  categoria = 'VIP' OR valor_compra > 1000 
).

IF lv_cliente_premium = abap_true.
  " código...
ENDIF.

" 3. Usar CASE em vez de múltiplos IF-ELSEIF
CASE status.
  WHEN 'A'.
    processar_ativo( ).
  WHEN 'P'.
    processar_pendente( ).
  WHEN 'C'.
    processar_cancelado( ).
ENDCASE.
```

### ❌ Evitar

```abap
" 1. IFs aninhados excessivos
IF condicao1 = abap_true.
  IF condicao2 = abap_true.
    IF condicao3 = abap_true.
      IF condicao4 = abap_true.
        " Muito profundo! ❌
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

" Melhor: usar AND ou extrair para método
IF condicao1 = abap_true AND 
   condicao2 = abap_true AND
   condicao3 = abap_true AND
   condicao4 = abap_true.
  " Mais legível ✅
ENDIF.

" 2. Comparar com abap_true/abap_false desnecessariamente
IF ativo = abap_true.  " ❌ Redundante
IF ativo.              " ✅ Suficiente

" 3. ELSE vazio
IF condicao.
  " faz algo
ELSE.
  " nada  ❌
ENDIF.

" Melhor:
IF NOT condicao.  " ✅
  " faz algo
ENDIF.
```

---

## 💡 Exemplos Práticos

### Validação de Dados

```abap
METHOD validar_cliente.
  " Validações com mensagens claras
  IF nome IS INITIAL.
    MESSAGE 'Nome obrigatório' TYPE 'E'.
    RETURN.
  ENDIF.
  
  IF idade < 18.
    MESSAGE 'Cliente deve ser maior de idade' TYPE 'E'.
    RETURN.
  ENDIF.
  
  IF email NS '@'.
    MESSAGE 'Email inválido' TYPE 'E'.
    RETURN.
  ENDIF.
  
  " Todas validações passaram
  MESSAGE 'Cliente válido' TYPE 'S'.
ENDMETHOD.
```

### Cálculo de Desconto

```abap
METHOD calcular_desconto.
  DATA(lv_desconto) = SWITCH p DECIMALS 2(
    tipo_cliente
    WHEN 'VIP'      THEN valor * '0.20'  " 20%
    WHEN 'PREMIUM'  THEN valor * '0.15'  " 15%
    WHEN 'REGULAR'  THEN valor * '0.10'  " 10%
    ELSE valor * '0.05'                   " 5%
  ).
  
  " Desconto adicional por volume
  IF quantidade > 100.
    lv_desconto = lv_desconto + ( valor * '0.05' ).
  ENDIF.
  
  RETURN lv_desconto.
ENDMETHOD.
```

### Classificação de Pedido

```abap
METHOD classificar_pedido.
  DATA lv_prioridade TYPE char1.
  
  CASE abap_true.
    WHEN urgente = abap_true AND valor > 10000.
      lv_prioridade = 'A'.  " Urgente e alto valor
      
    WHEN urgente = abap_true.
      lv_prioridade = 'B'.  " Apenas urgente
      
    WHEN valor > 10000.
      lv_prioridade = 'C'.  " Apenas alto valor
      
    WHEN OTHERS.
      lv_prioridade = 'D'.  " Normal
  ENDCASE.
  
  RETURN lv_prioridade.
ENDMETHOD.
```

---

## 🔗 Próximos Passos

Agora que domina condições e lógica, continue para:

1. **[Loops](4_loops.md)** - Estruturas de repetição
2. **[Expressões](5_expressoes.md)** - Expressões modernas ABAP
3. **[Tabelas Internas](6_tabelas_internas.md)** - Trabalhar com coleções de dados

---

**Tags:** `#Fundamentos` `#Condições` `#IF` `#CASE` `#Lógica` `#Modern-ABAP`
