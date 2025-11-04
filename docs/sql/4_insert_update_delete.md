# INSERT, UPDATE e DELETE

## 📋 Visão Geral

Operações para manipular dados na base de dados: inserir novos registos, atualizar existentes e eliminar dados.

---

## ➕ INSERT - Inserir Dados

### INSERT Simples

```abap
" Inserir um registo
DATA ls_scarr TYPE scarr.

ls_scarr-carrid   = 'PT'.
ls_scarr-carrname = 'TAP Air Portugal'.
ls_scarr-currcode = 'EUR'.
ls_scarr-url      = 'https://www.flytap.com'.

INSERT scarr FROM ls_scarr.

IF sy-subrc = 0.
  WRITE: / 'Companhia inserida com sucesso'.
ELSE.
  WRITE: / 'Erro ao inserir:', sy-subrc.
ENDIF.
```

### INSERT com VALUE

```abap
" Sintaxe moderna
INSERT scarr FROM @( VALUE #(
  carrid   = 'PT'
  carrname = 'TAP Air Portugal'
  currcode = 'EUR'
  url      = 'https://www.flytap.com'
) ).
```

### INSERT em Massa (Array)

```abap
" Inserir múltiplos registos de uma vez
DATA lt_novos_voos TYPE TABLE OF sflight.

lt_novos_voos = VALUE #(
  ( carrid = 'PT' connid = '0001' fldate = '20250101' price = '299.00' )
  ( carrid = 'PT' connid = '0002' fldate = '20250102' price = '350.00' )
  ( carrid = 'PT' connid = '0003' fldate = '20250103' price = '275.00' )
).

INSERT sflight FROM TABLE lt_novos_voos.

IF sy-subrc = 0.
  WRITE: / sy-dbcnt, 'registos inseridos'.
ELSE.
  WRITE: / 'Erro ao inserir em massa'.
ENDIF.
```

### INSERT com ACCEPTING DUPLICATE KEYS

```abap
" Inserir e ignorar duplicados (não gera erro)
INSERT sflight FROM TABLE lt_novos_voos 
  ACCEPTING DUPLICATE KEYS.

" sy-dbcnt contém o número de registos inseridos
WRITE: / sy-dbcnt, 'registos inseridos (duplicados ignorados)'.
```

---

## 🔄 UPDATE - Atualizar Dados

### UPDATE Simples

```abap
" Atualizar um registo específico
DATA ls_scarr TYPE scarr.

ls_scarr-carrid   = 'PT'.
ls_scarr-carrname = 'TAP Air Portugal S.A.'.
ls_scarr-currcode = 'EUR'.
ls_scarr-url      = 'https://www.flytap.com'.

UPDATE scarr FROM ls_scarr.

IF sy-subrc = 0.
  WRITE: / 'Companhia atualizada'.
ELSE.
  WRITE: / 'Companhia não encontrada'.
ENDIF.
```

### UPDATE com SET

```abap
" Atualizar campos específicos com WHERE
UPDATE sflight
  SET price = price * '1.10'  " Aumentar 10%
  WHERE carrid = 'LH'
    AND fldate >= '20250101'.

WRITE: / sy-dbcnt, 'voos atualizados'.
```

### UPDATE em Massa

```abap
" Atualizar múltiplos registos
DATA lt_voos TYPE TABLE OF sflight.

" Buscar voos
SELECT * FROM sflight
  WHERE carrid = 'AA'
  INTO TABLE @lt_voos.

" Modificar preços
LOOP AT lt_voos ASSIGNING FIELD-SYMBOL(<ls_voo>).
  <ls_voo>-price = <ls_voo>-price * '0.95'.  " Desconto 5%
ENDLOOP.

" Atualizar na base de dados
UPDATE sflight FROM TABLE lt_voos.

WRITE: / sy-dbcnt, 'registos atualizados'.
```

### UPDATE com WHERE Complexo

```abap
" Atualizar com múltiplas condições
UPDATE sflight
  SET seatsocc = seatsmax,
      price    = price * '1.20'
  WHERE carrid = 'BA'
    AND connid IN ('0001', '0002', '0003')
    AND fldate >= '20250101'
    AND seatsocc / seatsmax < '0.5'.  " Menos de 50% ocupado

WRITE: / sy-dbcnt, 'voos atualizados'.
```

---

## ❌ DELETE - Eliminar Dados

### DELETE Simples

```abap
" Eliminar um registo específico
DELETE FROM scarr WHERE carrid = 'XX'.

IF sy-subrc = 0.
  WRITE: / 'Companhia eliminada'.
ELSE.
  WRITE: / 'Companhia não encontrada'.
ENDIF.
```

### DELETE com WHERE

```abap
" Eliminar voos antigos
DELETE FROM sflight
  WHERE fldate < '20200101'.

WRITE: / sy-dbcnt, 'voos antigos eliminados'.

" Eliminar voos cancelados
DELETE FROM sflight
  WHERE carrid = 'XX'
    AND fldate >= sy-datum.

WRITE: / sy-dbcnt, 'voos cancelados eliminados'.
```

### DELETE FROM TABLE

```abap
" Eliminar baseado em tabela interna
DATA lt_para_eliminar TYPE TABLE OF sflight.

" Buscar voos a eliminar
SELECT * FROM sflight
  WHERE carrid = 'ZZ'
  INTO TABLE @lt_para_eliminar.

" Eliminar
DELETE sflight FROM TABLE lt_para_eliminar.

WRITE: / sy-dbcnt, 'registos eliminados'.
```

---

## 🔀 MODIFY - Inserir ou Atualizar

### MODIFY Simples

```abap
" Se existir atualiza, senão insere
DATA ls_scarr TYPE scarr.

ls_scarr-carrid   = 'PT'.
ls_scarr-carrname = 'TAP Air Portugal'.
ls_scarr-currcode = 'EUR'.

MODIFY scarr FROM ls_scarr.

IF sy-subrc = 0.
  WRITE: / 'Operação bem sucedida (INSERT ou UPDATE)'.
ENDIF.
```

### MODIFY em Massa

```abap
" Inserir ou atualizar múltiplos registos
DATA lt_voos TYPE TABLE OF sflight.

lt_voos = VALUE #(
  ( carrid = 'LH' connid = '0400' fldate = '20250101' price = '500' )
  ( carrid = 'LH' connid = '0401' fldate = '20250102' price = '550' )
).

MODIFY sflight FROM TABLE lt_voos.

WRITE: / sy-dbcnt, 'registos modificados (insert/update)'.
```

---

## 🔒 Transações e COMMIT

### COMMIT WORK

```abap
" Todas as operações DML precisam de COMMIT
DATA ls_cliente TYPE kna1.

ls_cliente-kunnr = '0000100001'.
ls_cliente-name1 = 'Novo Cliente'.

INSERT kna1 FROM ls_cliente.

IF sy-subrc = 0.
  COMMIT WORK.  " Confirmar transação
  WRITE: / 'Cliente inserido e confirmado'.
ELSE.
  ROLLBACK WORK.  " Reverter em caso de erro
  WRITE: / 'Erro - transação revertida'.
ENDIF.
```

### COMMIT WORK AND WAIT

```abap
" Esperar pela confirmação
INSERT kna1 FROM ls_cliente.
COMMIT WORK AND WAIT.

" Garantir que o registo está disponível
SELECT SINGLE * FROM kna1
  WHERE kunnr = @ls_cliente-kunnr
  INTO @DATA(ls_verificar).

IF sy-subrc = 0.
  WRITE: / 'Registo confirmado e disponível'.
ENDIF.
```

---

## 💡 Exemplos Práticos

### Importação de Dados

```abap
METHOD importar_clientes.
  DATA lt_clientes TYPE TABLE OF kna1.
  
  " Ler dados de ficheiro (exemplo simplificado)
  lt_clientes = VALUE #(
    ( kunnr = '0000100001' name1 = 'Cliente A' ort01 = 'Lisboa' )
    ( kunnr = '0000100002' name1 = 'Cliente B' ort01 = 'Porto' )
    ( kunnr = '0000100003' name1 = 'Cliente C' ort01 = 'Faro' )
  ).
  
  " Inserir em massa
  INSERT kna1 FROM TABLE lt_clientes 
    ACCEPTING DUPLICATE KEYS.
  
  COMMIT WORK.
  
  MESSAGE |{ sy-dbcnt } clientes importados| TYPE 'S'.
ENDMETHOD.
```

### Atualização em Lote

```abap
METHOD atualizar_precos_promocao.
  " Aplicar desconto de 15% em voos selecionados
  UPDATE sflight
    SET price = price * '0.85'
    WHERE carrid IN ('AA', 'LH', 'BA')
      AND fldate BETWEEN '20250601' AND '20250831'
      AND seatsocc / seatsmax < '0.7'.  " Ocupação < 70%
  
  DATA(lv_atualizados) = sy-dbcnt.
  
  IF lv_atualizados > 0.
    COMMIT WORK.
    MESSAGE |{ lv_atualizados } voos em promoção| TYPE 'S'.
  ELSE.
    MESSAGE 'Nenhum voo elegível' TYPE 'I'.
  ENDIF.
ENDMETHOD.
```

### Limpeza de Dados

```abap
METHOD limpar_dados_antigos.
  DATA lv_data_limite TYPE datum.
  
  " Data limite: 2 anos atrás
  lv_data_limite = sy-datum - 730.
  
  " Eliminar registos antigos
  DELETE FROM sflight
    WHERE fldate < lv_data_limite.
  
  DATA(lv_eliminados) = sy-dbcnt.
  
  IF lv_eliminados > 0.
    COMMIT WORK.
    MESSAGE |{ lv_eliminados } registos antigos eliminados| TYPE 'S'.
  ELSE.
    MESSAGE 'Nenhum registo para eliminar' TYPE 'I'.
  ENDIF.
ENDMETHOD.
```

### Sincronização de Dados

```abap
METHOD sincronizar_com_externa.
  DATA lt_externos TYPE TABLE OF ty_cliente_externo.
  DATA lt_internos TYPE TABLE OF kna1.
  
  " Buscar dados do sistema externo
  CALL FUNCTION 'Z_GET_EXTERNAL_CLIENTS'
    TABLES
      et_clients = lt_externos.
  
  " Converter para estrutura interna
  lt_internos = CORRESPONDING #( lt_externos ).
  
  " Usar MODIFY para inserir novos ou atualizar existentes
  MODIFY kna1 FROM TABLE lt_internos.
  
  COMMIT WORK AND WAIT.
  
  MESSAGE |{ sy-dbcnt } clientes sincronizados| TYPE 'S'.
ENDMETHOD.
```

---

## 🎯 Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre verificar sy-subrc
INSERT scarr FROM ls_scarr.
IF sy-subrc = 0.
  COMMIT WORK.
ELSE.
  ROLLBACK WORK.
  " Tratar erro
ENDIF.

" 2. Usar operações em massa quando possível
INSERT sflight FROM TABLE lt_voos.  " ✅ Mais rápido

" Em vez de:
LOOP AT lt_voos INTO DATA(ls_voo).
  INSERT sflight FROM ls_voo.  " ❌ Lento
ENDLOOP.

" 3. Usar MODIFY quando não sabe se registo existe
MODIFY kna1 FROM ls_cliente.  " ✅ INSERT ou UPDATE automático

" 4. Limitar DELETE com WHERE específico
DELETE FROM sflight WHERE carrid = 'XX'.  " ✅ Seguro

" Nunca:
" DELETE FROM sflight.  " ❌ PERIGOSO! Apaga tudo!
```

### ❌ Evitar

```abap
" 1. Updates sem WHERE (perigoso!)
UPDATE sflight SET price = 0.  " ❌ Atualiza TODOS!

" Correto:
UPDATE sflight SET price = 0 
  WHERE carrid = 'XX'.  " ✅

" 2. Não fazer COMMIT em loops
LOOP AT lt_data INTO DATA(ls_data).
  INSERT table FROM ls_data.
  COMMIT WORK.  " ❌ Lento e problemático
ENDLOOP.

" Correto:
INSERT table FROM TABLE lt_data.
COMMIT WORK.  " ✅ Um único commit

" 3. Ignorar erros
INSERT scarr FROM ls_scarr.
" Sem verificar sy-subrc  " ❌ Má prática

" 4. DELETE sem condições claras
DELETE FROM sflight.  " ❌ PERIGO: apaga tudo!
```

---

## 🔒 Controlo de Autorização

```abap
" Verificar autorização antes de modificar
AUTHORITY-CHECK OBJECT 'S_TABU_DIS'
  ID 'DICBERCLS' FIELD 'SCARR'
  ID 'ACTVT' FIELD '02'.  " 02 = UPDATE

IF sy-subrc <> 0.
  MESSAGE 'Sem autorização para modificar' TYPE 'E'.
  RETURN.
ENDIF.

" Prosseguir com UPDATE
UPDATE scarr FROM ls_scarr.
```

---

## 📊 Performance

### Otimizações

1. **Usar operações em massa**: `FROM TABLE` é muito mais rápido
2. **Indexes no WHERE**: Garanta que campos no WHERE têm índices
3. **COMMIT uma vez**: Não faça COMMIT em loops
4. **MODIFY vs INSERT+UPDATE**: MODIFY é mais conveniente mas pode ser menos eficiente

```abap
" ❌ Lento (um por um)
LOOP AT lt_voos INTO DATA(ls_voo).
  UPDATE sflight FROM ls_voo.
ENDLOOP.

" ✅ Rápido (em massa)
UPDATE sflight FROM TABLE lt_voos.
```

---

## 🔗 Próximos Passos

- **[WHERE Dinâmico](5_where_dinamico.md)** - Consultas flexíveis
- **[Otimizações SQL](6_otimizacoes.md)** - Performance avançada
- **[Performance](../performance/index.md)** - Boas práticas gerais

---

**Tags:** `#SQL` `#INSERT` `#UPDATE` `#DELETE` `#MODIFY` `#DML` `#Database`
