---
tags:
  - ABAP
  - OO
  - Polimorfismo
  - Casting
  - Programação
---

# Polimorfismo em ABAP

## 📋 Visão Geral

**Polimorfismo** permite tratar objetos de diferentes classes através da mesma interface ou superclasse, possibilitando que um único código funcione com múltiplos tipos de objetos.

---

## 🎯 Tipos de Polimorfismo

1. **Polimorfismo por Herança**: Usar referências da superclasse
2. **Polimorfismo por Interface**: Usar referências de interface
3. **Method Overriding**: Redefinir métodos em subclasses

---

## 🔹 Polimorfismo com Herança

```abap
CLASS lcl_animal DEFINITION.
  PUBLIC SECTION.
    METHODS: fazer_som.
ENDCLASS.

CLASS lcl_animal IMPLEMENTATION.
  METHOD fazer_som.
    WRITE: / 'Som genérico'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cao DEFINITION INHERITING FROM lcl_animal.
  PUBLIC SECTION.
    METHODS: fazer_som REDEFINITION.
ENDCLASS.

CLASS lcl_cao IMPLEMENTATION.
  METHOD fazer_som.
    WRITE: / 'Au au!'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_gato DEFINITION INHERITING FROM lcl_animal.
  PUBLIC SECTION.
    METHODS: fazer_som REDEFINITION.
ENDCLASS.

CLASS lcl_gato IMPLEMENTATION.
  METHOD fazer_som.
    WRITE: / 'Miau!'.
  ENDMETHOD.
ENDCLASS.

" ========== USO POLIMÓRFICO ==========
START-OF-SELECTION.

  " Tabela de referências à superclasse
  DATA: lt_animais TYPE TABLE OF REF TO lcl_animal.
  
  " Adicionar diferentes tipos
  APPEND NEW lcl_cao( ) TO lt_animais.
  APPEND NEW lcl_gato( ) TO lt_animais.
  APPEND NEW lcl_animal( ) TO lt_animais.
  
  " Tratar todos uniformemente
  LOOP AT lt_animais INTO DATA(lo_animal).
    lo_animal->fazer_som( ).  " Chamada polimórfica
  ENDLOOP.
  
  " Saída:
  " Au au!
  " Miau!
  " Som genérico
```

---

## 🔹 Upcasting (Widening Cast)

Converter referência de subclasse para superclasse (sempre seguro).

```abap
DATA: lo_cao TYPE REF TO lcl_cao,
      lo_animal TYPE REF TO lcl_animal.

lo_cao = NEW lcl_cao( ).

" Upcasting - sempre funciona
lo_animal = lo_cao.  " ✅ Automático
lo_animal->fazer_som( ).  " Chama versão do cão

" Ou explicitamente
lo_animal = CAST lcl_animal( lo_cao ).
```

---

## 🔹 Downcasting (Narrowing Cast)

Converter referência de superclasse para subclasse (requer verificação).

```abap
DATA: lo_animal TYPE REF TO lcl_animal,
      lo_cao TYPE REF TO lcl_cao.

lo_animal = NEW lcl_cao( ).

" Downcasting - precisa de CAST e pode falhar
TRY.
    lo_cao = CAST lcl_cao( lo_animal ).  " ✅ OK
    WRITE: / 'Downcasting bem-sucedido'.
  CATCH cx_sy_move_cast_error.
    WRITE: / 'Erro: não é um cão'.
ENDTRY.

" Ou usar ?= (sintaxe antiga)
TRY.
    lo_cao ?= lo_animal.
  CATCH cx_sy_move_cast_error.
    WRITE: / 'Casting falhou'.
ENDTRY.
```

---

## 🔹 Verificar Tipo Antes de Cast

```abap
DATA: lo_animal TYPE REF TO lcl_animal.

lo_animal = NEW lcl_cao( ).

" Verificar tipo
IF lo_animal IS INSTANCE OF lcl_cao.
  DATA(lo_cao) = CAST lcl_cao( lo_animal ).
  WRITE: / 'É um cão!'.
ELSEIF lo_animal IS INSTANCE OF lcl_gato.
  DATA(lo_gato) = CAST lcl_gato( lo_animal ).
  WRITE: / 'É um gato!'.
ELSE.
  WRITE: / 'Animal genérico'.
ENDIF.
```

---

## 🔹 Polimorfismo com Interfaces

```abap
INTERFACE lif_voador.
  METHODS: voar.
ENDINTERFACE.

CLASS lcl_passaro DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_voador.
    DATA: mv_nome TYPE string.
ENDCLASS.

CLASS lcl_passaro IMPLEMENTATION.
  METHOD lif_voador~voar.
    WRITE: / |{ mv_nome } está a voar!|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_aviao DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_voador.
    DATA: mv_modelo TYPE string.
ENDCLASS.

CLASS lcl_aviao IMPLEMENTATION.
  METHOD lif_voador~voar.
    WRITE: / |Avião { mv_modelo } descolou!|.
  ENDMETHOD.
ENDCLASS.

" ========== USO POLIMÓRFICO ==========
START-OF-SELECTION.

  DATA: lt_voadores TYPE TABLE OF REF TO lif_voador.
  
  DATA(lo_passaro) = NEW lcl_passaro( ).
  lo_passaro->mv_nome = 'Águia'.
  
  DATA(lo_aviao) = NEW lcl_aviao( ).
  lo_aviao->mv_modelo = 'Boeing 747'.
  
  APPEND lo_passaro TO lt_voadores.
  APPEND lo_aviao TO lt_voadores.
  
  LOOP AT lt_voadores INTO DATA(lo_voador).
    lo_voador->voar( ).
  ENDLOOP.
```

---

## 🔹 CASE TYPE OF

Processar diferentes tipos de forma específica.

```abap
METHOD processar_animal.
  IMPORTING io_animal TYPE REF TO lcl_animal.
  
  CASE TYPE OF io_animal.
    WHEN TYPE lcl_cao.
      DATA(lo_cao) = CAST lcl_cao( io_animal ).
      WRITE: / 'Processando cão...'.
      lo_cao->fazer_som( ).
      
    WHEN TYPE lcl_gato.
      DATA(lo_gato) = CAST lcl_gato( io_animal ).
      WRITE: / 'Processando gato...'.
      lo_gato->fazer_som( ).
      
    WHEN OTHERS.
      WRITE: / 'Animal desconhecido'.
      io_animal->fazer_som( ).
  ENDCASE.
ENDMETHOD.
```

---

## 💡 Exemplo Completo: Sistema de Pagamentos

```abap
*&---------------------------------------------------------------------*
*& Report Z_OO_POLIMORFISMO_PAGAMENTO
*&---------------------------------------------------------------------*
REPORT z_oo_polimorfismo_pagamento.

" ========== INTERFACE ==========
INTERFACE lif_meio_pagamento.
  METHODS:
    processar
      IMPORTING iv_valor TYPE p DECIMALS 2
      RETURNING VALUE(rv_sucesso) TYPE abap_bool,
    obter_taxa
      RETURNING VALUE(rv_taxa) TYPE p DECIMALS 2,
    obter_descricao
      RETURNING VALUE(rv_desc) TYPE string.
ENDINTERFACE.

" ========== IMPLEMENTAÇÃO: CARTÃO ==========
CLASS lcl_cartao DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_meio_pagamento.
    
    METHODS:
      constructor IMPORTING iv_numero TYPE string
                            iv_tipo TYPE string.  " 'CREDITO' ou 'DEBITO'
                            
  PRIVATE SECTION.
    DATA: mv_numero TYPE string,
          mv_tipo TYPE string.
ENDCLASS.

CLASS lcl_cartao IMPLEMENTATION.
  METHOD constructor.
    mv_numero = iv_numero.
    mv_tipo = iv_tipo.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~processar.
    DATA(lv_taxa) = obter_taxa( ).
    DATA(lv_total) = iv_valor + ( iv_valor * lv_taxa / 100 ).
    
    WRITE: / |💳 Cartão { mv_tipo } (****{ substring( val = mv_numero off = 12 len = 4 ) })|.
    WRITE: / |   Valor: { iv_valor }€ + Taxa ({ lv_taxa }%): { lv_total }€|.
    rv_sucesso = abap_true.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~obter_taxa.
    rv_taxa = COND #( WHEN mv_tipo = 'CREDITO' THEN '2.5'
                      WHEN mv_tipo = 'DEBITO' THEN '1.0'
                      ELSE '0.0' ).
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~obter_descricao.
    rv_desc = |Cartão { mv_tipo }|.
  ENDMETHOD.
ENDCLASS.

" ========== IMPLEMENTAÇÃO: TRANSFERÊNCIA ==========
CLASS lcl_transferencia DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_meio_pagamento.
    
    METHODS:
      constructor IMPORTING iv_iban TYPE string.
      
  PRIVATE SECTION.
    DATA: mv_iban TYPE string.
ENDCLASS.

CLASS lcl_transferencia IMPLEMENTATION.
  METHOD constructor.
    mv_iban = iv_iban.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~processar.
    WRITE: / |🏦 Transferência bancária para { mv_iban }|.
    WRITE: / |   Valor: { iv_valor }€ (sem taxas)|.
    rv_sucesso = abap_true.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~obter_taxa.
    rv_taxa = '0.0'.  " Sem taxa
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~obter_descricao.
    rv_desc = 'Transferência Bancária'.
  ENDMETHOD.
ENDCLASS.

" ========== IMPLEMENTAÇÃO: DINHEIRO ==========
CLASS lcl_dinheiro DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_meio_pagamento.
ENDCLASS.

CLASS lcl_dinheiro IMPLEMENTATION.
  METHOD lif_meio_pagamento~processar.
    WRITE: / |💵 Pagamento em dinheiro|.
    WRITE: / |   Valor: { iv_valor }€|.
    rv_sucesso = abap_true.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~obter_taxa.
    rv_taxa = '0.0'.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~obter_descricao.
    rv_desc = 'Dinheiro'.
  ENDMETHOD.
ENDCLASS.

" ========== IMPLEMENTAÇÃO: MBWAY ==========
CLASS lcl_mbway DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_meio_pagamento.
    
    METHODS:
      constructor IMPORTING iv_telemovel TYPE string.
      
  PRIVATE SECTION.
    DATA: mv_telemovel TYPE string.
ENDCLASS.

CLASS lcl_mbway IMPLEMENTATION.
  METHOD constructor.
    mv_telemovel = iv_telemovel.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~processar.
    WRITE: / |📱 MB WAY para { mv_telemovel }|.
    WRITE: / |   Valor: { iv_valor }€ (taxa: 0.5%)|.
    rv_sucesso = abap_true.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~obter_taxa.
    rv_taxa = '0.5'.
  ENDMETHOD.
  
  METHOD lif_meio_pagamento~obter_descricao.
    rv_desc = 'MB WAY'.
  ENDMETHOD.
ENDCLASS.

" ========== PROCESSADOR DE PAGAMENTOS ==========
CLASS lcl_processador_pagamentos DEFINITION.
  PUBLIC SECTION.
    METHODS:
      processar_compra
        IMPORTING iv_valor TYPE p DECIMALS 2
                  it_meios TYPE TABLE.
ENDCLASS.

CLASS lcl_processador_pagamentos IMPLEMENTATION.
  METHOD processar_compra.
    DATA: lv_total_pago TYPE p DECIMALS 2 VALUE 0,
          lv_valor_falta TYPE p DECIMALS 2.
    
    WRITE: / |╔════════════════════════════════════════╗|.
    WRITE: / |║  PROCESSAMENTO DE PAGAMENTO            ║|.
    WRITE: / |╚════════════════════════════════════════╝|.
    WRITE: / |Valor da compra: { iv_valor }€|.
    SKIP.
    
    LOOP AT it_meios INTO DATA(lo_meio).
      CHECK lo_meio IS BOUND.
      
      lv_valor_falta = iv_valor - lv_total_pago.
      
      IF lv_valor_falta <= 0.
        EXIT.
      ENDIF.
      
      WRITE: / |───────────────────────────────────────|.
      
      " Polimorfismo em ação!
      IF lo_meio->processar( lv_valor_falta ) = abap_true.
        DATA(lv_taxa) = lo_meio->obter_taxa( ).
        DATA(lv_com_taxa) = lv_valor_falta + ( lv_valor_falta * lv_taxa / 100 ).
        lv_total_pago = lv_total_pago + lv_valor_falta.
        WRITE: / |✅ Pagamento confirmado|.
      ELSE.
        WRITE: / |❌ Pagamento falhou|.
      ENDIF.
      
      SKIP.
    ENDLOOP.
    
    WRITE: / |═══════════════════════════════════════|.
    IF lv_total_pago >= iv_valor.
      WRITE: / |✅ PAGAMENTO COMPLETO!|.
    ELSE.
      WRITE: / |⚠️  Falta pagar: { iv_valor - lv_total_pago }€|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" ========== PROGRAMA PRINCIPAL ==========
START-OF-SELECTION.

  " Criar diferentes meios de pagamento
  DATA: lt_meios TYPE TABLE OF REF TO lif_meio_pagamento.
  
  " Cenário 1: Pagamento com cartão de crédito
  WRITE: / |════════ CENÁRIO 1: Cartão de Crédito ════════|.
  SKIP.
  
  CLEAR lt_meios.
  APPEND NEW lcl_cartao(
    iv_numero = '1234567890123456'
    iv_tipo = 'CREDITO'
  ) TO lt_meios.
  
  DATA(lo_proc) = NEW lcl_processador_pagamentos( ).
  lo_proc->processar_compra(
    iv_valor = '100.00'
    it_meios = lt_meios
  ).
  
  SKIP 2.
  
  " Cenário 2: Pagamento misto
  WRITE: / |════════ CENÁRIO 2: Pagamento Misto ════════|.
  SKIP.
  
  CLEAR lt_meios.
  APPEND NEW lcl_dinheiro( ) TO lt_meios.
  APPEND NEW lcl_mbway( iv_telemovel = '912345678' ) TO lt_meios.
  APPEND NEW lcl_transferencia( iv_iban = 'PT50000000000000000000000' ) TO lt_meios.
  
  lo_proc->processar_compra(
    iv_valor = '250.00'
    it_meios = lt_meios
  ).
  
  SKIP 2.
  
  " Cenário 3: Demonstrar CASE TYPE OF
  WRITE: / |════════ CENÁRIO 3: Identificar Tipos ════════|.
  SKIP.
  
  DATA(lo_meio_qualquer) = CAST lif_meio_pagamento( NEW lcl_cartao(
    iv_numero = '9876543210987654'
    iv_tipo = 'DEBITO'
  ) ).
  
  " Downcasting com verificação
  DATA lo_obj TYPE REF TO object.
  lo_obj = lo_meio_qualquer.
  
  CASE TYPE OF lo_obj.
    WHEN TYPE lcl_cartao.
      DATA(lo_cartao) = CAST lcl_cartao( lo_obj ).
      WRITE: / |Tipo identificado: Cartão { lo_cartao->mv_tipo }|.
      
    WHEN TYPE lcl_transferencia.
      WRITE: / 'Tipo identificado: Transferência'.
      
    WHEN TYPE lcl_mbway.
      WRITE: / 'Tipo identificado: MB WAY'.
      
    WHEN TYPE lcl_dinheiro.
      WRITE: / 'Tipo identificado: Dinheiro'.
      
    WHEN OTHERS.
      WRITE: / 'Tipo desconhecido'.
  ENDCASE.
```

---

## 🎯 Quando Usar Polimorfismo

### ✅ Use Quando:

- Precisar tratar objetos diferentes de forma uniforme
- Implementar estratégias diferentes para o mesmo comportamento
- Criar código extensível sem modificar código existente
- Implementar padrões como Strategy, Factory, Decorator

### ❌ Evite Quando:

- As classes não têm relação conceitual
- Herança é forçada apenas para reutilizar código
- Polimorfismo torna o código mais complexo sem benefícios

---

## 🔹 Diferenças: Upcasting vs Downcasting

| Aspecto | Upcasting | Downcasting |
|---------|-----------|-------------|
| **Segurança** | Sempre seguro | Pode falhar |
| **Sintaxe** | Automático | Requer CAST |
| **Direção** | Subclasse → Superclasse | Superclasse → Subclasse |
| **Quando** | Polimorfismo | Acesso a métodos específicos |
| **Exemplo** | `lo_animal = lo_cao` | `lo_cao = CAST lcl_cao( lo_animal )` |

---

## 💡 Boas Práticas

### ✅ Fazer

```abap
" 1. Verificar tipo antes de downcast
IF lo_obj IS INSTANCE OF lcl_tipo_especifico.
  DATA(lo_especifico) = CAST lcl_tipo_especifico( lo_obj ).
ENDIF.

" 2. Usar TRY-CATCH para downcasting
TRY.
    DATA(lo_cast) = CAST lcl_classe( lo_objeto ).
  CATCH cx_sy_move_cast_error.
    " Tratar erro
ENDTRY.

" 3. Preferir interfaces a herança para polimorfismo
DATA: lt_processadores TYPE TABLE OF REF TO lif_processador.

" 4. Usar referências genéricas quando apropriado
DATA: lt_objetos TYPE TABLE OF REF TO object.
```

### ❌ Evitar

```abap
" 1. Downcasting sem verificação
DATA(lo_cao) = CAST lcl_cao( lo_animal ).  " ❌ Pode falhar!

" 2. CASE TYPE OF muito extenso
CASE TYPE OF lo_obj.
  WHEN TYPE tipo1.
  WHEN TYPE tipo2.
  " ... 50 tipos  " ❌ Redesenhar solução
ENDCASE.

" 3. Forçar polimorfismo onde não faz sentido
" Nem tudo precisa ser polimórfico!

" 4. Usar herança profunda para polimorfismo
" Classe -> Sub1 -> Sub2 -> Sub3 -> Sub4  " ❌
```

---

## 🔗 Próximos Passos

- **[Factory Pattern](7_factory_pattern.md)** - Criar objetos polimórficos
- **[Interfaces](4_interfaces.md)** - Base para polimorfismo
- **[Herança](5_heranca.md)** - Hierarquias de classes

---

**Tags:** `#OO` `#Polimorfismo` `#Casting` `#Interfaces` `#Herança` `#ABAP`
