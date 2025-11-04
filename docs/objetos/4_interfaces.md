---
tags:
  - ABAP
  - OO
  - Interfaces
  - Polimorfismo
  - Programação
---

# Interfaces em ABAP

## 📋 Visão Geral

Uma **interface** define um **contrato** que as classes devem cumprir. É uma forma de garantir que diferentes classes implementem os mesmos métodos, permitindo **polimorfismo** e **desacoplamento**.

---

## 🎯 O que é uma Interface?

- **Contrato**: Define métodos que devem ser implementados
- **Sem implementação**: Apenas assinaturas de métodos
- **Múltiplas interfaces**: Uma classe pode implementar várias interfaces
- **Polimorfismo**: Tratar objetos diferentes de forma uniforme

---

## 🔹 Definir uma Interface

```abap
INTERFACE lif_pagamento.
  METHODS:
    processar_pagamento 
      IMPORTING iv_valor TYPE p DECIMALS 2
      RETURNING VALUE(rv_sucesso) TYPE abap_bool,
    validar_dados
      RETURNING VALUE(rv_valido) TYPE abap_bool,
    obter_taxa
      RETURNING VALUE(rv_taxa) TYPE p DECIMALS 2.
ENDINTERFACE.
```

---

## 🔹 Implementar uma Interface

```abap
CLASS lcl_cartao_credito DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pagamento.
    
    METHODS constructor
      IMPORTING iv_numero TYPE string
                iv_cvv TYPE string.
                
  PRIVATE SECTION.
    DATA: mv_numero TYPE string,
          mv_cvv TYPE string.
ENDCLASS.

CLASS lcl_cartao_credito IMPLEMENTATION.
  METHOD constructor.
    mv_numero = iv_numero.
    mv_cvv = iv_cvv.
  ENDMETHOD.
  
  METHOD lif_pagamento~processar_pagamento.
    IF validar_dados( ) = abap_true.
      " Lógica de processamento do cartão
      WRITE: / |Pagamento de { iv_valor } processado via cartão|.
      rv_sucesso = abap_true.
    ELSE.
      rv_sucesso = abap_false.
    ENDIF.
  ENDMETHOD.
  
  METHOD lif_pagamento~validar_dados.
    " Validar número e CVV
    IF strlen( mv_numero ) = 16 AND strlen( mv_cvv ) = 3.
      rv_valido = abap_true.
    ELSE.
      rv_valido = abap_false.
    ENDIF.
  ENDMETHOD.
  
  METHOD lif_pagamento~obter_taxa.
    rv_taxa = '2.5'.  " 2.5% de taxa para cartão
  ENDMETHOD.
ENDCLASS.
```

---

## 🔹 Múltiplas Implementações

```abap
CLASS lcl_paypal DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pagamento.
    
    METHODS constructor
      IMPORTING iv_email TYPE string.
      
  PRIVATE SECTION.
    DATA: mv_email TYPE string.
ENDCLASS.

CLASS lcl_paypal IMPLEMENTATION.
  METHOD constructor.
    mv_email = iv_email.
  ENDMETHOD.
  
  METHOD lif_pagamento~processar_pagamento.
    WRITE: / |Pagamento de { iv_valor } via PayPal ({ mv_email })|.
    rv_sucesso = abap_true.
  ENDMETHOD.
  
  METHOD lif_pagamento~validar_dados.
    rv_valido = COND #( WHEN mv_email CS '@' THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
  
  METHOD lif_pagamento~obter_taxa.
    rv_taxa = '3.5'.  " 3.5% de taxa para PayPal
  ENDMETHOD.
ENDCLASS.

CLASS lcl_transferencia DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pagamento.
    
    METHODS constructor
      IMPORTING iv_iban TYPE string.
      
  PRIVATE SECTION.
    DATA: mv_iban TYPE string.
ENDCLASS.

CLASS lcl_transferencia IMPLEMENTATION.
  METHOD constructor.
    mv_iban = iv_iban.
  ENDMETHOD.
  
  METHOD lif_pagamento~processar_pagamento.
    WRITE: / |Transferência de { iv_valor } para { mv_iban }|.
    rv_sucesso = abap_true.
  ENDMETHOD.
  
  METHOD lif_pagamento~validar_dados.
    rv_valido = COND #( WHEN strlen( mv_iban ) = 25 THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
  
  METHOD lif_pagamento~obter_taxa.
    rv_taxa = '0.5'.  " 0.5% de taxa para transferência
  ENDMETHOD.
ENDCLASS.
```

---

## 🔹 Usar Interfaces (Polimorfismo)

```abap
START-OF-SELECTION.

  " Tabela de referências à interface
  DATA: lt_pagamentos TYPE TABLE OF REF TO lif_pagamento.
  
  " Criar diferentes formas de pagamento
  APPEND NEW lcl_cartao_credito( 
    iv_numero = '1234567890123456'
    iv_cvv = '123'
  ) TO lt_pagamentos.
  
  APPEND NEW lcl_paypal( 
    iv_email = 'user@exemplo.com'
  ) TO lt_pagamentos.
  
  APPEND NEW lcl_transferencia(
    iv_iban = 'PT50000000000000000000000'
  ) TO lt_pagamentos.
  
  " Processar todos os pagamentos de forma uniforme
  LOOP AT lt_pagamentos INTO DATA(lo_pagamento).
    DATA(lv_valor) = CONV p( 100 DECIMALS 2 ).
    DATA(lv_taxa) = lo_pagamento->obter_taxa( ).
    DATA(lv_total) = lv_valor + ( lv_valor * lv_taxa / 100 ).
    
    WRITE: / |Processando pagamento de { lv_total } (taxa: { lv_taxa }%)|.
    
    IF lo_pagamento->processar_pagamento( lv_total ) = abap_true.
      WRITE: / 'Sucesso!'.
    ELSE.
      WRITE: / 'Falhou!'.
    ENDIF.
    SKIP.
  ENDLOOP.
```

---

## 🔹 Interfaces com Atributos

```abap
INTERFACE lif_configuravel.
  DATA: mv_versao TYPE string READ-ONLY.
  CONSTANTS: mc_max_tentativas TYPE i VALUE 3.
  
  METHODS:
    configurar IMPORTING iv_config TYPE string,
    obter_status RETURNING VALUE(rv_status) TYPE string.
ENDINTERFACE.

CLASS lcl_servico DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_configuravel.
    
  PRIVATE SECTION.
    DATA: mv_configuracao TYPE string.
ENDCLASS.

CLASS lcl_servico IMPLEMENTATION.
  METHOD lif_configuravel~configurar.
    mv_configuracao = iv_config.
    lif_configuravel~mv_versao = '1.0.0'.
  ENDMETHOD.
  
  METHOD lif_configuravel~obter_status.
    rv_status = |Config: { mv_configuracao }, Versão: { lif_configuravel~mv_versao }|.
  ENDMETHOD.
ENDCLASS.
```

---

## 🔹 Implementar Múltiplas Interfaces

```abap
INTERFACE lif_logavel.
  METHODS: log IMPORTING iv_mensagem TYPE string.
ENDINTERFACE.

INTERFACE lif_serializavel.
  METHODS: 
    to_json RETURNING VALUE(rv_json) TYPE string,
    from_json IMPORTING iv_json TYPE string.
ENDINTERFACE.

CLASS lcl_entidade DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_logavel,
                lif_serializavel.
                
    DATA: mv_nome TYPE string,
          mv_id TYPE i.
ENDCLASS.

CLASS lcl_entidade IMPLEMENTATION.
  METHOD lif_logavel~log.
    WRITE: / |LOG: { iv_mensagem }|.
  ENDMETHOD.
  
  METHOD lif_serializavel~to_json.
    rv_json = |{{ "id": { mv_id }, "nome": "{ mv_nome }" }}|.
  ENDMETHOD.
  
  METHOD lif_serializavel~from_json.
    " Parsing simplificado
    mv_nome = 'Extraído do JSON'.
  ENDMETHOD.
ENDCLASS.

" Uso
START-OF-SELECTION.
  DATA(lo_obj) = NEW lcl_entidade( ).
  lo_obj->mv_id = 1.
  lo_obj->mv_nome = 'Teste'.
  
  lo_obj->lif_logavel~log( 'Objeto criado' ).
  DATA(lv_json) = lo_obj->lif_serializavel~to_json( ).
  WRITE: / lv_json.
```

---

## 🔹 Alias para Métodos de Interface

```abap
CLASS lcl_produto DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_serializavel
      ALL METHODS 
      ALIASES: exportar FOR lif_serializavel~to_json,
               importar FOR lif_serializavel~from_json.
ENDCLASS.

CLASS lcl_produto IMPLEMENTATION.
  METHOD lif_serializavel~to_json.
    rv_json = '{ "produto": "exemplo" }'.
  ENDMETHOD.
  
  METHOD lif_serializavel~from_json.
    " Implementação
  ENDMETHOD.
ENDCLASS.

" Uso com alias
DATA(lo_prod) = NEW lcl_produto( ).
DATA(lv_json) = lo_prod->exportar( ).  " Mais legível que lif_serializavel~to_json
```

---

## 🔹 Verificar Implementação de Interface

```abap
DATA lo_objeto TYPE REF TO object.
lo_objeto = NEW lcl_cartao_credito( iv_numero = '1234' iv_cvv = '123' ).

" Verificar se implementa a interface
IF lo_objeto IS INSTANCE OF lif_pagamento.
  WRITE: / 'Implementa lif_pagamento'.
  
  " Cast para interface
  DATA(lo_pag) = CAST lif_pagamento( lo_objeto ).
  lo_pag->processar_pagamento( '100.00' ).
ENDIF.
```

---

## 💡 Exemplo Completo: Sistema de Notificações

```abap
*&---------------------------------------------------------------------*
*& Report Z_OO_NOTIFICACOES
*&---------------------------------------------------------------------*
REPORT z_oo_notificacoes.

" Interface para notificações
INTERFACE lif_notificacao.
  METHODS:
    enviar 
      IMPORTING iv_destinatario TYPE string
                iv_mensagem TYPE string
      RETURNING VALUE(rv_sucesso) TYPE abap_bool,
    validar_destinatario
      IMPORTING iv_destinatario TYPE string
      RETURNING VALUE(rv_valido) TYPE abap_bool.
ENDINTERFACE.

" Implementação: Email
CLASS lcl_email DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_notificacao.
ENDCLASS.

CLASS lcl_email IMPLEMENTATION.
  METHOD lif_notificacao~enviar.
    IF validar_destinatario( iv_destinatario ) = abap_true.
      WRITE: / |📧 Email enviado para { iv_destinatario }: { iv_mensagem }|.
      rv_sucesso = abap_true.
    ELSE.
      WRITE: / |❌ Email inválido: { iv_destinatario }|.
      rv_sucesso = abap_false.
    ENDIF.
  ENDMETHOD.
  
  METHOD lif_notificacao~validar_destinatario.
    rv_valido = COND #( WHEN iv_destinatario CS '@' AND iv_destinatario CS '.'
                        THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.

" Implementação: SMS
CLASS lcl_sms DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_notificacao.
ENDCLASS.

CLASS lcl_sms IMPLEMENTATION.
  METHOD lif_notificacao~enviar.
    IF validar_destinatario( iv_destinatario ) = abap_true.
      WRITE: / |📱 SMS enviado para { iv_destinatario }: { iv_mensagem }|.
      rv_sucesso = abap_true.
    ELSE.
      WRITE: / |❌ Número inválido: { iv_destinatario }|.
      rv_sucesso = abap_false.
    ENDIF.
  ENDMETHOD.
  
  METHOD lif_notificacao~validar_destinatario.
    DATA(lv_len) = strlen( iv_destinatario ).
    rv_valido = COND #( WHEN lv_len >= 9 AND lv_len <= 15
                        THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.

" Implementação: Push Notification
CLASS lcl_push DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_notificacao.
ENDCLASS.

CLASS lcl_push IMPLEMENTATION.
  METHOD lif_notificacao~enviar.
    WRITE: / |🔔 Push enviado para dispositivo { iv_destinatario }: { iv_mensagem }|.
    rv_sucesso = abap_true.
  ENDMETHOD.
  
  METHOD lif_notificacao~validar_destinatario.
    rv_valido = COND #( WHEN strlen( iv_destinatario ) = 32
                        THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.

" Gestor de notificações
CLASS lcl_notificador DEFINITION.
  PUBLIC SECTION.
    METHODS:
      adicionar_canal 
        IMPORTING io_canal TYPE REF TO lif_notificacao,
      notificar_todos
        IMPORTING iv_destinatario TYPE string
                  iv_mensagem TYPE string.
                  
  PRIVATE SECTION.
    DATA: mt_canais TYPE TABLE OF REF TO lif_notificacao.
ENDCLASS.

CLASS lcl_notificador IMPLEMENTATION.
  METHOD adicionar_canal.
    APPEND io_canal TO mt_canais.
  ENDMETHOD.
  
  METHOD notificar_todos.
    LOOP AT mt_canais INTO DATA(lo_canal).
      lo_canal->enviar( 
        iv_destinatario = iv_destinatario
        iv_mensagem = iv_mensagem
      ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" Programa principal
START-OF-SELECTION.

  DATA(lo_notificador) = NEW lcl_notificador( ).
  
  " Adicionar canais de notificação
  lo_notificador->adicionar_canal( NEW lcl_email( ) ).
  lo_notificador->adicionar_canal( NEW lcl_sms( ) ).
  lo_notificador->adicionar_canal( NEW lcl_push( ) ).
  
  " Enviar notificação por todos os canais
  WRITE: / '=== Notificação 1 ==='.
  lo_notificador->notificar_todos(
    iv_destinatario = 'user@exemplo.com'
    iv_mensagem = 'Bem-vindo ao sistema!'
  ).
  
  SKIP.
  WRITE: / '=== Notificação 2 ==='.
  lo_notificador->notificar_todos(
    iv_destinatario = '912345678'
    iv_mensagem = 'Código de verificação: 1234'
  ).
```

---

## 🎯 Boas Práticas

### ✅ Fazer

```abap
" 1. Nomes descritivos para interfaces
INTERFACE lif_processador_pagamento.  " ✅
INTERFACE lif_serializavel.           " ✅

" 2. Interfaces pequenas e focadas (Interface Segregation Principle)
INTERFACE lif_gravavel.
  METHODS gravar.
ENDINTERFACE.

INTERFACE lif_removivel.
  METHODS remover.
ENDINTERFACE.

" 3. Usar tabelas de interfaces para polimorfismo
DATA: lt_processadores TYPE TABLE OF REF TO lif_processador.

" 4. Alias para métodos complexos
INTERFACES lif_longa~nome_muito_grande_de_metodo
  ALIASES metodo FOR lif_longa~nome_muito_grande_de_metodo.
```

### ❌ Evitar

```abap
" 1. Interfaces muito grandes (violação do ISP)
INTERFACE lif_tudo.  " ❌
  METHODS: metodo1, metodo2, metodo3, ..., metodo50.
ENDINTERFACE.

" 2. Interfaces sem propósito claro
INTERFACE lif_utils.  " ❌ Muito genérico
ENDINTERFACE.

" 3. Misturar implementação e interface
INTERFACE lif_exemplo.
  METHODS calcular.  " ✅ OK
  DATA mv_valor TYPE i.  " ⚠️ Evitar quando possível
ENDINTERFACE.
```

---

## 🔗 Próximos Passos

- **[Herança](5_heranca.md)** - Reutilizar código com INHERITING FROM
- **[Polimorfismo](6_polimorfismo.md)** - Casting e tratamento polimórfico
- **[Factory Pattern](7_factory_pattern.md)** - Criar objetos através de interfaces

---

**Tags:** `#OO` `#Interfaces` `#Polimorfismo` `#Contratos` `#ABAP`
