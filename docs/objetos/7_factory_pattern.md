---
tags:
  - ABAP
  - OO
  - Design Patterns
  - Factory
  - Programação
---

# Factory Pattern (Padrão Fábrica)

## 📋 Visão Geral

O **Factory Pattern** é um padrão de criação que fornece uma interface para criar objetos sem especificar suas classes concretas. Centraliza a lógica de criação de objetos, tornando o código mais flexível e fácil de manter.

---

## 🎯 Quando Usar

- Criação de objetos é complexa
- Tipo de objeto depende de condições em runtime
- Deseja desacoplar código cliente da implementação
- Precisa de lógica centralizada de criação
- Criar famílias de objetos relacionados

---

## 🔹 Simple Factory (Fábrica Simples)

```abap
" ========== INTERFACE ==========
INTERFACE lif_notificacao.
  METHODS: enviar IMPORTING iv_mensagem TYPE string.
ENDINTERFACE.

" ========== IMPLEMENTAÇÕES ==========
CLASS lcl_email DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_notificacao.
ENDCLASS.

CLASS lcl_email IMPLEMENTATION.
  METHOD lif_notificacao~enviar.
    WRITE: / |📧 Email: { iv_mensagem }|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sms DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_notificacao.
ENDCLASS.

CLASS lcl_sms IMPLEMENTATION.
  METHOD lif_notificacao~enviar.
    WRITE: / |📱 SMS: { iv_mensagem }|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_push DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_notificacao.
ENDCLASS.

CLASS lcl_push IMPLEMENTATION.
  METHOD lif_notificacao~enviar.
    WRITE: / |🔔 Push: { iv_mensagem }|.
  ENDMETHOD.
ENDCLASS.

" ========== FACTORY ==========
CLASS lcl_notificacao_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      criar
        IMPORTING iv_tipo TYPE string
        RETURNING VALUE(ro_notificacao) TYPE REF TO lif_notificacao
        RAISING cx_parameter_invalid.
ENDCLASS.

CLASS lcl_notificacao_factory IMPLEMENTATION.
  METHOD criar.
    CASE iv_tipo.
      WHEN 'EMAIL'.
        ro_notificacao = NEW lcl_email( ).
      WHEN 'SMS'.
        ro_notificacao = NEW lcl_sms( ).
      WHEN 'PUSH'.
        ro_notificacao = NEW lcl_push( ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

" ========== USO ==========
START-OF-SELECTION.

  TRY.
      DATA(lo_notif1) = lcl_notificacao_factory=>criar( 'EMAIL' ).
      lo_notif1->enviar( 'Bem-vindo!' ).
      
      DATA(lo_notif2) = lcl_notificacao_factory=>criar( 'SMS' ).
      lo_notif2->enviar( 'Código: 1234' ).
      
      DATA(lo_notif3) = lcl_notificacao_factory=>criar( 'PUSH' ).
      lo_notif3->enviar( 'Nova mensagem' ).
      
    CATCH cx_parameter_invalid.
      WRITE: / 'Tipo de notificação inválido'.
  ENDTRY.
```

---

## 🔹 Factory Method (Método Fábrica)

```abap
" ========== PRODUTO ==========
INTERFACE lif_documento.
  METHODS:
    abrir,
    salvar,
    fechar.
ENDINTERFACE.

CLASS lcl_documento_pdf DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_documento.
ENDCLASS.

CLASS lcl_documento_pdf IMPLEMENTATION.
  METHOD lif_documento~abrir.
    WRITE: / '📄 Abrindo PDF...'.
  ENDMETHOD.
  
  METHOD lif_documento~salvar.
    WRITE: / '💾 Salvando PDF...'.
  ENDMETHOD.
  
  METHOD lif_documento~fechar.
    WRITE: / '❌ Fechando PDF...'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_documento_word DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_documento.
ENDCLASS.

CLASS lcl_documento_word IMPLEMENTATION.
  METHOD lif_documento~abrir.
    WRITE: / '📝 Abrindo Word...'.
  ENDMETHOD.
  
  METHOD lif_documento~salvar.
    WRITE: / '💾 Salvando Word...'.
  ENDMETHOD.
  
  METHOD lif_documento~fechar.
    WRITE: / '❌ Fechando Word...'.
  ENDMETHOD.
ENDCLASS.

" ========== CREATOR (ABSTRATO) ==========
CLASS lcl_aplicacao DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      criar_documento ABSTRACT
        RETURNING VALUE(ro_doc) TYPE REF TO lif_documento,
      novo_documento.
ENDCLASS.

CLASS lcl_aplicacao IMPLEMENTATION.
  METHOD novo_documento.
    DATA(lo_doc) = criar_documento( ).
    lo_doc->abrir( ).
  ENDMETHOD.
ENDCLASS.

" ========== CONCRETE CREATORS ==========
CLASS lcl_app_pdf DEFINITION INHERITING FROM lcl_aplicacao.
  PUBLIC SECTION.
    METHODS: criar_documento REDEFINITION.
ENDCLASS.

CLASS lcl_app_pdf IMPLEMENTATION.
  METHOD criar_documento.
    ro_doc = NEW lcl_documento_pdf( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_app_word DEFINITION INHERITING FROM lcl_aplicacao.
  PUBLIC SECTION.
    METHODS: criar_documento REDEFINITION.
ENDCLASS.

CLASS lcl_app_word IMPLEMENTATION.
  METHOD criar_documento.
    ro_doc = NEW lcl_documento_word( ).
  ENDMETHOD.
ENDCLASS.

" ========== USO ==========
START-OF-SELECTION.

  DATA(lo_app_pdf) = NEW lcl_app_pdf( ).
  lo_app_pdf->novo_documento( ).
  
  SKIP.
  
  DATA(lo_app_word) = NEW lcl_app_word( ).
  lo_app_word->novo_documento( ).
```

---

## 🔹 Abstract Factory (Fábrica Abstrata)

Criar famílias de objetos relacionados.

```abap
" ========== PRODUTOS ==========
INTERFACE lif_botao.
  METHODS: renderizar.
ENDINTERFACE.

INTERFACE lif_checkbox.
  METHODS: renderizar.
ENDINTERFACE.

" ========== PRODUTOS WINDOWS ==========
CLASS lcl_botao_windows DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_botao.
ENDCLASS.

CLASS lcl_botao_windows IMPLEMENTATION.
  METHOD lif_botao~renderizar.
    WRITE: / '🖱️  [Botão Windows]'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_checkbox_windows DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_checkbox.
ENDCLASS.

CLASS lcl_checkbox_windows IMPLEMENTATION.
  METHOD lif_checkbox~renderizar.
    WRITE: / '☑️  Checkbox Windows'.
  ENDMETHOD.
ENDCLASS.

" ========== PRODUTOS MAC ==========
CLASS lcl_botao_mac DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_botao.
ENDCLASS.

CLASS lcl_botao_mac IMPLEMENTATION.
  METHOD lif_botao~renderizar.
    WRITE: / '🖱️  [Botão Mac]'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_checkbox_mac DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_checkbox.
ENDCLASS.

CLASS lcl_checkbox_mac IMPLEMENTATION.
  METHOD lif_checkbox~renderizar.
    WRITE: / '☑️  Checkbox Mac'.
  ENDMETHOD.
ENDCLASS.

" ========== ABSTRACT FACTORY ==========
INTERFACE lif_ui_factory.
  METHODS:
    criar_botao RETURNING VALUE(ro_botao) TYPE REF TO lif_botao,
    criar_checkbox RETURNING VALUE(ro_checkbox) TYPE REF TO lif_checkbox.
ENDINTERFACE.

" ========== CONCRETE FACTORIES ==========
CLASS lcl_windows_factory DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ui_factory.
ENDCLASS.

CLASS lcl_windows_factory IMPLEMENTATION.
  METHOD lif_ui_factory~criar_botao.
    ro_botao = NEW lcl_botao_windows( ).
  ENDMETHOD.
  
  METHOD lif_ui_factory~criar_checkbox.
    ro_checkbox = NEW lcl_checkbox_windows( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mac_factory DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ui_factory.
ENDCLASS.

CLASS lcl_mac_factory IMPLEMENTATION.
  METHOD lif_ui_factory~criar_botao.
    ro_botao = NEW lcl_botao_mac( ).
  ENDMETHOD.
  
  METHOD lif_ui_factory~criar_checkbox.
    ro_checkbox = NEW lcl_checkbox_mac( ).
  ENDMETHOD.
ENDCLASS.

" ========== CLIENTE ==========
CLASS lcl_aplicacao_ui DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_factory TYPE REF TO lif_ui_factory,
      renderizar_interface.
      
  PRIVATE SECTION.
    DATA: mo_factory TYPE REF TO lif_ui_factory.
ENDCLASS.

CLASS lcl_aplicacao_ui IMPLEMENTATION.
  METHOD constructor.
    mo_factory = io_factory.
  ENDMETHOD.
  
  METHOD renderizar_interface.
    DATA(lo_botao) = mo_factory->criar_botao( ).
    DATA(lo_check) = mo_factory->criar_checkbox( ).
    
    lo_botao->renderizar( ).
    lo_check->renderizar( ).
  ENDMETHOD.
ENDCLASS.

" ========== USO ==========
START-OF-SELECTION.

  WRITE: / '=== Interface Windows ==='.
  DATA(lo_app_win) = NEW lcl_aplicacao_ui( NEW lcl_windows_factory( ) ).
  lo_app_win->renderizar_interface( ).
  
  SKIP.
  
  WRITE: / '=== Interface Mac ==='.
  DATA(lo_app_mac) = NEW lcl_aplicacao_ui( NEW lcl_mac_factory( ) ).
  lo_app_mac->renderizar_interface( ).
```

---

## 💡 Exemplo Completo: Sistema de Exportação

```abap
*&---------------------------------------------------------------------*
*& Report Z_OO_FACTORY_EXPORTACAO
*&---------------------------------------------------------------------*
REPORT z_oo_factory_exportacao.

" ========== INTERFACE DO EXPORTADOR ==========
INTERFACE lif_exportador.
  METHODS:
    exportar
      IMPORTING it_dados TYPE STANDARD TABLE
      RETURNING VALUE(rv_resultado) TYPE string,
    validar_dados
      IMPORTING it_dados TYPE STANDARD TABLE
      RETURNING VALUE(rv_valido) TYPE abap_bool,
    obter_extensao
      RETURNING VALUE(rv_ext) TYPE string.
ENDINTERFACE.

" ========== EXPORTADOR CSV ==========
CLASS lcl_exportador_csv DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_exportador.
ENDCLASS.

CLASS lcl_exportador_csv IMPLEMENTATION.
  METHOD lif_exportador~exportar.
    CHECK validar_dados( it_dados ) = abap_true.
    
    rv_resultado = 'Nome,Idade,Email' && cl_abap_char_utilities=>newline.
    
    LOOP AT it_dados INTO DATA(ls_linha).
      rv_resultado = rv_resultado && |Dados CSV linha { sy-tabix }| 
                     && cl_abap_char_utilities=>newline.
    ENDLOOP.
    
    WRITE: / |✅ Exportado { lines( it_dados ) } linhas para CSV|.
  ENDMETHOD.
  
  METHOD lif_exportador~validar_dados.
    rv_valido = COND #( WHEN it_dados IS NOT INITIAL THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
  
  METHOD lif_exportador~obter_extensao.
    rv_ext = 'csv'.
  ENDMETHOD.
ENDCLASS.

" ========== EXPORTADOR JSON ==========
CLASS lcl_exportador_json DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_exportador.
ENDCLASS.

CLASS lcl_exportador_json IMPLEMENTATION.
  METHOD lif_exportador~exportar.
    CHECK validar_dados( it_dados ) = abap_true.
    
    rv_resultado = '[' && cl_abap_char_utilities=>newline.
    
    LOOP AT it_dados INTO DATA(ls_linha).
      rv_resultado = rv_resultado && 
                     |  {{ "linha": { sy-tabix } }}| &&
                     cl_abap_char_utilities=>newline.
    ENDLOOP.
    
    rv_resultado = rv_resultado && ']'.
    
    WRITE: / |✅ Exportado { lines( it_dados ) } objetos para JSON|.
  ENDMETHOD.
  
  METHOD lif_exportador~validar_dados.
    rv_valido = COND #( WHEN it_dados IS NOT INITIAL THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
  
  METHOD lif_exportador~obter_extensao.
    rv_ext = 'json'.
  ENDMETHOD.
ENDCLASS.

" ========== EXPORTADOR XML ==========
CLASS lcl_exportador_xml DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_exportador.
ENDCLASS.

CLASS lcl_exportador_xml IMPLEMENTATION.
  METHOD lif_exportador~exportar.
    CHECK validar_dados( it_dados ) = abap_true.
    
    rv_resultado = '<?xml version="1.0"?>' && cl_abap_char_utilities=>newline.
    rv_resultado = rv_resultado && '<dados>' && cl_abap_char_utilities=>newline.
    
    LOOP AT it_dados INTO DATA(ls_linha).
      rv_resultado = rv_resultado && 
                     |  <linha id="{ sy-tabix }" />| &&
                     cl_abap_char_utilities=>newline.
    ENDLOOP.
    
    rv_resultado = rv_resultado && '</dados>'.
    
    WRITE: / |✅ Exportado { lines( it_dados ) } elementos para XML|.
  ENDMETHOD.
  
  METHOD lif_exportador~validar_dados.
    rv_valido = COND #( WHEN it_dados IS NOT INITIAL THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
  
  METHOD lif_exportador~obter_extensao.
    rv_ext = 'xml'.
  ENDMETHOD.
ENDCLASS.

" ========== EXPORTADOR EXCEL ==========
CLASS lcl_exportador_excel DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_exportador.
ENDCLASS.

CLASS lcl_exportador_excel IMPLEMENTATION.
  METHOD lif_exportador~exportar.
    CHECK validar_dados( it_dados ) = abap_true.
    
    rv_resultado = |Excel Workbook com { lines( it_dados ) } linhas|.
    
    WRITE: / |✅ Exportado { lines( it_dados ) } linhas para Excel|.
  ENDMETHOD.
  
  METHOD lif_exportador~validar_dados.
    rv_valido = COND #( WHEN it_dados IS NOT INITIAL THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
  
  METHOD lif_exportador~obter_extensao.
    rv_ext = 'xlsx'.
  ENDMETHOD.
ENDCLASS.

" ========== FACTORY ==========
CLASS lcl_exportador_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      criar
        IMPORTING iv_formato TYPE string
        RETURNING VALUE(ro_exportador) TYPE REF TO lif_exportador
        RAISING cx_parameter_invalid,
      formatos_disponiveis
        RETURNING VALUE(rt_formatos) TYPE string_table.
ENDCLASS.

CLASS lcl_exportador_factory IMPLEMENTATION.
  METHOD criar.
    CASE to_upper( iv_formato ).
      WHEN 'CSV'.
        ro_exportador = NEW lcl_exportador_csv( ).
        
      WHEN 'JSON'.
        ro_exportador = NEW lcl_exportador_json( ).
        
      WHEN 'XML'.
        ro_exportador = NEW lcl_exportador_xml( ).
        
      WHEN 'EXCEL' OR 'XLSX'.
        ro_exportador = NEW lcl_exportador_excel( ).
        
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDCASE.
  ENDMETHOD.
  
  METHOD formatos_disponiveis.
    rt_formatos = VALUE #(
      ( |CSV| )
      ( |JSON| )
      ( |XML| )
      ( |EXCEL| )
    ).
  ENDMETHOD.
ENDCLASS.

" ========== SERVIÇO DE EXPORTAÇÃO ==========
CLASS lcl_servico_exportacao DEFINITION.
  PUBLIC SECTION.
    METHODS:
      exportar_dados
        IMPORTING it_dados TYPE STANDARD TABLE
                  iv_formato TYPE string.
ENDCLASS.

CLASS lcl_servico_exportacao IMPLEMENTATION.
  METHOD exportar_dados.
    TRY.
        " Factory cria o exportador apropriado
        DATA(lo_exportador) = lcl_exportador_factory=>criar( iv_formato ).
        
        " Usar o exportador
        DATA(lv_resultado) = lo_exportador->exportar( it_dados ).
        DATA(lv_extensao) = lo_exportador->obter_extensao( ).
        
        WRITE: / |Arquivo: dados.{ lv_extensao }|.
        
      CATCH cx_parameter_invalid.
        WRITE: / |❌ Formato inválido: { iv_formato }|.
        WRITE: / 'Formatos disponíveis:'.
        LOOP AT lcl_exportador_factory=>formatos_disponiveis( ) INTO DATA(lv_fmt).
          WRITE: / |  - { lv_fmt }|.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

" ========== PROGRAMA PRINCIPAL ==========
START-OF-SELECTION.

  " Dados de exemplo
  DATA: lt_dados TYPE TABLE OF string.
  lt_dados = VALUE #( ( |Linha 1| ) ( |Linha 2| ) ( |Linha 3| ) ).
  
  DATA(lo_servico) = NEW lcl_servico_exportacao( ).
  
  " Exportar para diferentes formatos
  WRITE: / '╔═══════════════════════════════════╗'.
  WRITE: / '║  SISTEMA DE EXPORTAÇÃO            ║'.
  WRITE: / '╚═══════════════════════════════════╝'.
  SKIP.
  
  WRITE: / '─── Exportar para CSV ───'.
  lo_servico->exportar_dados(
    it_dados = lt_dados
    iv_formato = 'CSV'
  ).
  SKIP.
  
  WRITE: / '─── Exportar para JSON ───'.
  lo_servico->exportar_dados(
    it_dados = lt_dados
    iv_formato = 'JSON'
  ).
  SKIP.
  
  WRITE: / '─── Exportar para XML ───'.
  lo_servico->exportar_dados(
    it_dados = lt_dados
    iv_formato = 'XML'
  ).
  SKIP.
  
  WRITE: / '─── Exportar para Excel ───'.
  lo_servico->exportar_dados(
    it_dados = lt_dados
    iv_formato = 'EXCEL'
  ).
  SKIP.
  
  WRITE: / '─── Formato Inválido ───'.
  lo_servico->exportar_dados(
    it_dados = lt_dados
    iv_formato = 'PDF'
  ).
```

---

## 🎯 Vantagens do Factory Pattern

| Vantagem | Descrição |
|----------|-----------|
| **Desacoplamento** | Cliente não conhece classes concretas |
| **Centralização** | Lógica de criação num único lugar |
| **Flexibilidade** | Fácil adicionar novos tipos |
| **Manutenção** | Mudanças isoladas na factory |
| **Testabilidade** | Fácil mockar objetos |

---

## 🔹 Variações do Padrão

### 1. **Static Factory Method**
```abap
CLASS lcl_pessoa DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      criar_adulto RETURNING VALUE(ro_pessoa) TYPE REF TO lcl_pessoa,
      criar_crianca RETURNING VALUE(ro_pessoa) TYPE REF TO lcl_pessoa.
ENDCLASS.
```

### 2. **Factory com Parâmetros**
```abap
CLASS lcl_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      criar
        IMPORTING iv_tipo TYPE string
                  iv_config TYPE any OPTIONAL
        RETURNING VALUE(ro_obj) TYPE REF TO lif_interface.
ENDCLASS.
```

### 3. **Factory com Registro**
```abap
CLASS lcl_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      registrar IMPORTING iv_tipo TYPE string
                          io_criador TYPE REF TO object,
      criar IMPORTING iv_tipo TYPE string
            RETURNING VALUE(ro_obj) TYPE REF TO object.
            
  PRIVATE SECTION.
    CLASS-DATA: st_registro TYPE HASHED TABLE OF ... WITH UNIQUE KEY tipo.
ENDCLASS.
```

---

## 💡 Boas Práticas

### ✅ Fazer

```abap
" 1. Retornar interfaces, não classes concretas
CLASS-METHODS criar RETURNING VALUE(ro_obj) TYPE REF TO lif_interface.

" 2. Lançar exceções para tipos inválidos
RAISE EXCEPTION TYPE cx_parameter_invalid.

" 3. Usar métodos estáticos para factories simples
CLASS-METHODS criar_pdf RETURNING VALUE(ro_doc) TYPE REF TO lif_documento.

" 4. Validar parâmetros
IF iv_tipo IS INITIAL.
  RAISE EXCEPTION TYPE cx_parameter_invalid.
ENDIF.
```

### ❌ Evitar

```abap
" 1. Factory que retorna tipos concretos
CLASS-METHODS criar RETURNING VALUE(ro_obj) TYPE REF TO lcl_classe_concreta.  " ❌

" 2. Factory sem tratamento de erros
CASE iv_tipo.
  WHEN 'A'. ro_obj = NEW lcl_a( ).
  " E se não for 'A'?  " ❌
ENDCASE.

" 3. Lógica de negócio na factory
" Factory deve APENAS criar objetos

" 4. Factory muito complexa
" Se ficou complexa demais, refatore
```

---

## 🔗 Próximos Passos

- **[Singleton Pattern](8_singleton.md)** - Garantir instância única
- **[Polimorfismo](6_polimorfismo.md)** - Trabalhar com objetos criados
- **[Interfaces](4_interfaces.md)** - Base para factories

---

**Tags:** `#OO` `#DesignPatterns` `#Factory` `#CreationalPatterns` `#ABAP`
