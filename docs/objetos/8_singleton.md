---
tags:
  - ABAP
  - OO
  - Design Patterns
  - Singleton
  - Programação
---

# Singleton Pattern (Padrão Singleton)

## 📋 Visão Geral

O **Singleton Pattern** garante que uma classe tenha **apenas uma instância** e fornece um ponto de acesso global a essa instância. É útil quando exatamente um objeto é necessário para coordenar ações em todo o sistema.

---

## 🎯 Quando Usar

- Configurações globais do sistema
- Pool de conexões de base de dados
- Logger centralizado
- Cache compartilhado
- Gestor de recursos únicos
- Fábricas de objetos

---

## 🔹 Implementação Básica

```abap
CLASS lcl_singleton DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_singleton.
        
    METHODS:
      fazer_algo.
      
  PRIVATE SECTION.
    CLASS-DATA: so_instance TYPE REF TO lcl_singleton.
    
    METHODS: constructor.
ENDCLASS.

CLASS lcl_singleton IMPLEMENTATION.
  METHOD get_instance.
    " Criar instância apenas se não existir
    IF so_instance IS NOT BOUND.
      CREATE OBJECT so_instance.
    ENDIF.
    
    ro_instance = so_instance.
  ENDMETHOD.
  
  METHOD constructor.
    WRITE: / 'Singleton criado pela primeira vez'.
  ENDMETHOD.
  
  METHOD fazer_algo.
    WRITE: / 'Executando ação do singleton'.
  ENDMETHOD.
ENDCLASS.

" ========== USO ==========
START-OF-SELECTION.

  " Obter instância
  DATA(lo_obj1) = lcl_singleton=>get_instance( ).
  lo_obj1->fazer_algo( ).
  
  " Obter novamente - mesma instância!
  DATA(lo_obj2) = lcl_singleton=>get_instance( ).
  lo_obj2->fazer_algo( ).
  
  " Verificar que são a mesma instância
  IF lo_obj1 = lo_obj2.
    WRITE: / '✅ Mesma instância confirmada'.
  ENDIF.
  
  " ❌ ERRO: não se pode criar diretamente
  " DATA(lo_obj3) = NEW lcl_singleton( ).  " Erro de compilação!
```

---

## 🔹 Singleton para Configuração

```abap
CLASS lcl_configuracao DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_configuracao.
        
    METHODS:
      set_parametro
        IMPORTING iv_chave TYPE string
                  iv_valor TYPE string,
      get_parametro
        IMPORTING iv_chave TYPE string
        RETURNING VALUE(rv_valor) TYPE string,
      listar_parametros.
      
  PRIVATE SECTION.
    CLASS-DATA: so_instance TYPE REF TO lcl_configuracao.
    
    TYPES: BEGIN OF ty_parametro,
             chave TYPE string,
             valor TYPE string,
           END OF ty_parametro.
           
    DATA: mt_parametros TYPE HASHED TABLE OF ty_parametro
                        WITH UNIQUE KEY chave.
                        
    METHODS:
      constructor,
      carregar_config_default.
ENDCLASS.

CLASS lcl_configuracao IMPLEMENTATION.
  METHOD get_instance.
    IF so_instance IS NOT BOUND.
      CREATE OBJECT so_instance.
    ENDIF.
    ro_instance = so_instance.
  ENDMETHOD.
  
  METHOD constructor.
    WRITE: / '⚙️  Inicializando configurações...'.
    carregar_config_default( ).
  ENDMETHOD.
  
  METHOD carregar_config_default.
    set_parametro( iv_chave = 'AMBIENTE' iv_valor = 'PRODUÇÃO' ).
    set_parametro( iv_chave = 'DEBUG' iv_valor = 'FALSE' ).
    set_parametro( iv_chave = 'TIMEOUT' iv_valor = '30' ).
    set_parametro( iv_chave = 'MAX_REGISTROS' iv_valor = '1000' ).
  ENDMETHOD.
  
  METHOD set_parametro.
    INSERT VALUE #( chave = iv_chave valor = iv_valor ) 
           INTO TABLE mt_parametros.
    
    IF sy-subrc <> 0.
      MODIFY TABLE mt_parametros FROM VALUE #( chave = iv_chave valor = iv_valor ).
    ENDIF.
  ENDMETHOD.
  
  METHOD get_parametro.
    READ TABLE mt_parametros 
         WITH TABLE KEY chave = iv_chave
         INTO DATA(ls_param).
         
    IF sy-subrc = 0.
      rv_valor = ls_param-valor.
    ELSE.
      rv_valor = ''.
    ENDIF.
  ENDMETHOD.
  
  METHOD listar_parametros.
    WRITE: / '═══ CONFIGURAÇÕES ═══'.
    LOOP AT mt_parametros INTO DATA(ls_param).
      WRITE: / |{ ls_param-chave WIDTH = 15 }: { ls_param-valor }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" ========== USO ==========
START-OF-SELECTION.

  " Usar configuração em diferentes partes do código
  DATA(lo_config) = lcl_configuracao=>get_instance( ).
  
  lo_config->listar_parametros( ).
  SKIP.
  
  " Alterar configuração
  lo_config->set_parametro( iv_chave = 'DEBUG' iv_valor = 'TRUE' ).
  
  " Em outro lugar do código, mesma instância
  DATA(lo_config2) = lcl_configuracao=>get_instance( ).
  DATA(lv_debug) = lo_config2->get_parametro( 'DEBUG' ).
  
  WRITE: / |Debug está: { lv_debug }|.  " TRUE
```

---

## 🔹 Singleton para Logger

```abap
CLASS lcl_logger DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_logger.
        
    METHODS:
      log_info
        IMPORTING iv_mensagem TYPE string,
      log_warning
        IMPORTING iv_mensagem TYPE string,
      log_error
        IMPORTING iv_mensagem TYPE string,
      mostrar_logs,
      limpar_logs.
      
  PRIVATE SECTION.
    CLASS-DATA: so_instance TYPE REF TO lcl_logger.
    
    TYPES: BEGIN OF ty_log,
             timestamp TYPE timestampl,
             nivel TYPE string,
             mensagem TYPE string,
           END OF ty_log.
           
    DATA: mt_logs TYPE TABLE OF ty_log.
    
    METHODS:
      adicionar_log
        IMPORTING iv_nivel TYPE string
                  iv_mensagem TYPE string.
ENDCLASS.

CLASS lcl_logger IMPLEMENTATION.
  METHOD get_instance.
    IF so_instance IS NOT BOUND.
      CREATE OBJECT so_instance.
    ENDIF.
    ro_instance = so_instance.
  ENDMETHOD.
  
  METHOD log_info.
    adicionar_log( iv_nivel = 'INFO' iv_mensagem = iv_mensagem ).
  ENDMETHOD.
  
  METHOD log_warning.
    adicionar_log( iv_nivel = 'WARNING' iv_mensagem = iv_mensagem ).
  ENDMETHOD.
  
  METHOD log_error.
    adicionar_log( iv_nivel = 'ERROR' iv_mensagem = iv_mensagem ).
  ENDMETHOD.
  
  METHOD adicionar_log.
    GET TIME STAMP FIELD DATA(lv_timestamp).
    
    APPEND VALUE #(
      timestamp = lv_timestamp
      nivel = iv_nivel
      mensagem = iv_mensagem
    ) TO mt_logs.
    
    " Imprimir imediatamente (opcional)
    DATA(lv_icone) = SWITCH string( iv_nivel
      WHEN 'INFO' THEN 'ℹ️'
      WHEN 'WARNING' THEN '⚠️'
      WHEN 'ERROR' THEN '❌'
      ELSE '📝'
    ).
    
    WRITE: / |{ lv_icone } [{ iv_nivel WIDTH = 7 }] { iv_mensagem }|.
  ENDMETHOD.
  
  METHOD mostrar_logs.
    WRITE: / '╔════════════════════════════════════════╗'.
    WRITE: / '║           HISTÓRICO DE LOGS            ║'.
    WRITE: / '╚════════════════════════════════════════╝'.
    
    LOOP AT mt_logs INTO DATA(ls_log).
      WRITE: / |{ sy-tabix }. [{ ls_log-nivel }] { ls_log-mensagem }|.
    ENDLOOP.
    
    WRITE: / |Total de logs: { lines( mt_logs ) }|.
  ENDMETHOD.
  
  METHOD limpar_logs.
    CLEAR mt_logs.
    WRITE: / '🗑️  Logs limpos'.
  ENDMETHOD.
ENDCLASS.

" ========== USO ==========
START-OF-SELECTION.

  " Usar logger em diferentes partes
  DATA(lo_log) = lcl_logger=>get_instance( ).
  
  lo_log->log_info( 'Aplicação iniciada' ).
  lo_log->log_info( 'Conectando à base de dados...' ).
  lo_log->log_warning( 'Cache não encontrado, criando novo' ).
  lo_log->log_error( 'Falha ao conectar ao servidor externo' ).
  lo_log->log_info( 'Aplicação finalizada' ).
  
  SKIP.
  lo_log->mostrar_logs( ).
```

---

## 🔹 Singleton Thread-Safe (Class Constructor)

```abap
CLASS lcl_singleton_seguro DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_singleton_seguro.
        
    METHODS:
      processar.
      
  PRIVATE SECTION.
    CLASS-DATA: so_instance TYPE REF TO lcl_singleton_seguro.
ENDCLASS.

CLASS lcl_singleton_seguro IMPLEMENTATION.
  METHOD class_constructor.
    " Executado automaticamente na primeira utilização da classe
    " Thread-safe por natureza em ABAP
    CREATE OBJECT so_instance.
    WRITE: / '🔒 Singleton criado de forma thread-safe'.
  ENDMETHOD.
  
  METHOD get_instance.
    ro_instance = so_instance.
  ENDMETHOD.
  
  METHOD processar.
    WRITE: / 'Processando...'.
  ENDMETHOD.
ENDCLASS.
```

---

## 💡 Exemplo Completo: Cache Singleton

```abap
*&---------------------------------------------------------------------*
*& Report Z_OO_SINGLETON_CACHE
*&---------------------------------------------------------------------*
REPORT z_oo_singleton_cache.

" ========== CACHE SINGLETON ==========
CLASS lcl_cache DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_cache.
        
    METHODS:
      put
        IMPORTING iv_chave TYPE string
                  iv_valor TYPE any,
      get
        IMPORTING iv_chave TYPE string
        RETURNING VALUE(rv_valor) TYPE string,
      existe
        IMPORTING iv_chave TYPE string
        RETURNING VALUE(rv_existe) TYPE abap_bool,
      remover
        IMPORTING iv_chave TYPE string,
      limpar,
      estatisticas.
      
  PRIVATE SECTION.
    CLASS-DATA: so_instance TYPE REF TO lcl_cache.
    
    TYPES: BEGIN OF ty_entrada_cache,
             chave TYPE string,
             valor TYPE string,
             timestamp TYPE timestampl,
             acessos TYPE i,
           END OF ty_entrada_cache.
           
    DATA: mt_cache TYPE HASHED TABLE OF ty_entrada_cache
                   WITH UNIQUE KEY chave.
                   
    METHODS:
      constructor.
ENDCLASS.

CLASS lcl_cache IMPLEMENTATION.
  METHOD get_instance.
    IF so_instance IS NOT BOUND.
      CREATE OBJECT so_instance.
    ENDIF.
    ro_instance = so_instance.
  ENDMETHOD.
  
  METHOD constructor.
    WRITE: / '💾 Cache inicializado'.
  ENDMETHOD.
  
  METHOD put.
    GET TIME STAMP FIELD DATA(lv_timestamp).
    
    DATA(ls_entrada) = VALUE ty_entrada_cache(
      chave = iv_chave
      valor = CONV string( iv_valor )
      timestamp = lv_timestamp
      acessos = 0
    ).
    
    INSERT ls_entrada INTO TABLE mt_cache.
    
    IF sy-subrc <> 0.
      MODIFY TABLE mt_cache FROM ls_entrada.
      WRITE: / |📝 Cache atualizado: { iv_chave }|.
    ELSE.
      WRITE: / |➕ Cache adicionado: { iv_chave }|.
    ENDIF.
  ENDMETHOD.
  
  METHOD get.
    READ TABLE mt_cache 
         WITH TABLE KEY chave = iv_chave
         ASSIGNING FIELD-SYMBOL(<ls_cache>).
         
    IF sy-subrc = 0.
      rv_valor = <ls_cache>-valor.
      <ls_cache>-acessos = <ls_cache>-acessos + 1.
      WRITE: / |✅ Cache hit: { iv_chave }|.
    ELSE.
      WRITE: / |❌ Cache miss: { iv_chave }|.
      rv_valor = ''.
    ENDIF.
  ENDMETHOD.
  
  METHOD existe.
    READ TABLE mt_cache 
         WITH TABLE KEY chave = iv_chave
         TRANSPORTING NO FIELDS.
         
    rv_existe = COND #( WHEN sy-subrc = 0 THEN abap_true
                        ELSE abap_false ).
  ENDMETHOD.
  
  METHOD remover.
    DELETE TABLE mt_cache WITH TABLE KEY chave = iv_chave.
    
    IF sy-subrc = 0.
      WRITE: / |🗑️  Removido do cache: { iv_chave }|.
    ENDIF.
  ENDMETHOD.
  
  METHOD limpar.
    DATA(lv_count) = lines( mt_cache ).
    CLEAR mt_cache.
    WRITE: / |🧹 Cache limpo ({ lv_count } entradas removidas)|.
  ENDMETHOD.
  
  METHOD estatisticas.
    WRITE: / '╔════════════════════════════════════════╗'.
    WRITE: / '║      ESTATÍSTICAS DO CACHE             ║'.
    WRITE: / '╚════════════════════════════════════════╝'.
    WRITE: / |Total de entradas: { lines( mt_cache ) }|.
    SKIP.
    
    IF mt_cache IS NOT INITIAL.
      WRITE: / 'Chave', 25 'Acessos', 40 'Timestamp'.
      WRITE: / '─────────────────────────────────────────'.
      
      LOOP AT mt_cache INTO DATA(ls_cache).
        WRITE: / ls_cache-chave, 25 ls_cache-acessos, 40 ls_cache-timestamp.
      ENDLOOP.
    ELSE.
      WRITE: / '(vazio)'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" ========== SERVIÇO QUE USA CACHE ==========
CLASS lcl_servico_dados DEFINITION.
  PUBLIC SECTION.
    METHODS:
      obter_usuario
        IMPORTING iv_id TYPE string
        RETURNING VALUE(rv_nome) TYPE string,
      obter_produto
        IMPORTING iv_id TYPE string
        RETURNING VALUE(rv_desc) TYPE string.
        
  PRIVATE SECTION.
    DATA: mo_cache TYPE REF TO lcl_cache.
    
    METHODS:
      buscar_usuario_bd
        IMPORTING iv_id TYPE string
        RETURNING VALUE(rv_nome) TYPE string,
      buscar_produto_bd
        IMPORTING iv_id TYPE string
        RETURNING VALUE(rv_desc) TYPE string.
ENDCLASS.

CLASS lcl_servico_dados IMPLEMENTATION.
  METHOD obter_usuario.
    " Usar cache singleton
    mo_cache = lcl_cache=>get_instance( ).
    
    DATA(lv_chave) = |USER_{ iv_id }|.
    
    " Verificar se está em cache
    IF mo_cache->existe( lv_chave ) = abap_true.
      rv_nome = mo_cache->get( lv_chave ).
    ELSE.
      " Buscar na BD (simulado)
      rv_nome = buscar_usuario_bd( iv_id ).
      
      " Guardar em cache
      mo_cache->put( iv_chave = lv_chave iv_valor = rv_nome ).
    ENDIF.
  ENDMETHOD.
  
  METHOD buscar_usuario_bd.
    WRITE: / |🔍 Buscando usuário { iv_id } na BD...|.
    rv_nome = |Usuário_{ iv_id }|.
  ENDMETHOD.
  
  METHOD obter_produto.
    mo_cache = lcl_cache=>get_instance( ).
    
    DATA(lv_chave) = |PROD_{ iv_id }|.
    
    IF mo_cache->existe( lv_chave ) = abap_true.
      rv_desc = mo_cache->get( lv_chave ).
    ELSE.
      rv_desc = buscar_produto_bd( iv_id ).
      mo_cache->put( iv_chave = lv_chave iv_valor = rv_desc ).
    ENDIF.
  ENDMETHOD.
  
  METHOD buscar_produto_bd.
    WRITE: / |🔍 Buscando produto { iv_id } na BD...|.
    rv_desc = |Produto_{ iv_id }|.
  ENDMETHOD.
ENDCLASS.

" ========== PROGRAMA PRINCIPAL ==========
START-OF-SELECTION.

  WRITE: / '════════════════════════════════════════'.
  WRITE: / '      DEMONSTRAÇÃO: CACHE SINGLETON'.
  WRITE: / '════════════════════════════════════════'.
  SKIP.
  
  DATA(lo_servico) = NEW lcl_servico_dados( ).
  
  " Primeira busca - vai à BD
  WRITE: / '─── Primeira busca de usuário 001 ───'.
  DATA(lv_user) = lo_servico->obter_usuario( '001' ).
  WRITE: / |Resultado: { lv_user }|.
  SKIP.
  
  " Segunda busca - vem do cache
  WRITE: / '─── Segunda busca de usuário 001 ───'.
  lv_user = lo_servico->obter_usuario( '001' ).
  WRITE: / |Resultado: { lv_user }|.
  SKIP.
  
  " Buscar produto
  WRITE: / '─── Buscar produto 100 ───'.
  DATA(lv_prod) = lo_servico->obter_produto( '100' ).
  WRITE: / |Resultado: { lv_prod }|.
  SKIP.
  
  " Buscar mesmo produto novamente
  WRITE: / '─── Buscar produto 100 novamente ───'.
  lv_prod = lo_servico->obter_produto( '100' ).
  WRITE: / |Resultado: { lv_prod }|.
  SKIP.
  
  " Mostrar estatísticas
  DATA(lo_cache) = lcl_cache=>get_instance( ).
  lo_cache->estatisticas( ).
  SKIP.
  
  " Limpar cache
  WRITE: / '─── Limpar cache ───'.
  lo_cache->limpar( ).
  SKIP.
  
  " Buscar novamente após limpar
  WRITE: / '─── Buscar usuário 001 após limpar cache ───'.
  lv_user = lo_servico->obter_usuario( '001' ).
  WRITE: / |Resultado: { lv_user }|.
```

---

## 🎯 Vantagens e Desvantagens

### ✅ Vantagens

- Acesso controlado à única instância
- Ponto de acesso global
- Economia de memória
- Inicialização tardia (lazy initialization)
- Estado partilhado consistente

### ❌ Desvantagens

- Dificulta testes unitários
- Viola Single Responsibility Principle
- Pode esconder dependências
- Problemas com concorrência (em outras linguagens)
- Estado global pode causar acoplamento

---

## 💡 Boas Práticas

### ✅ Fazer

```abap
" 1. Usar CREATE PRIVATE
CLASS lcl_singleton DEFINITION CREATE PRIVATE.

" 2. Verificar se instância existe
IF so_instance IS NOT BOUND.
  CREATE OBJECT so_instance.
ENDIF.

" 3. Usar class_constructor para thread-safety
CLASS-METHODS class_constructor.

" 4. Documentar que é singleton
" Singleton pattern - única instância global
```

### ❌ Evitar

```abap
" 1. Singleton para tudo
" Use apenas quando realmente necessário!

" 2. Estado mutável excessivo
" Mantenha estado mínimo e controlado

" 3. Lógica de negócio no singleton
" Singleton deve ser infraestrutura, não negócio

" 4. Dependências circulares
" Singleton A -> Singleton B -> Singleton A  " ❌
```

---

## 🔗 Alternativas ao Singleton

1. **Dependency Injection**: Passar dependências explicitamente
2. **Service Locator**: Registro central de serviços
3. **Static Class**: Se não precisa de estado
4. **Factory**: Para criar instâncias controladas

---

## 🔗 Próximos Passos

- **[Factory Pattern](7_factory_pattern.md)** - Criar objetos de forma controlada
- **[Interfaces](4_interfaces.md)** - Contratos para singletons
- **Performance** - Cache e otimizações

---

**Tags:** `#OO` `#DesignPatterns` `#Singleton` `#CreationalPatterns` `#Cache` `#ABAP`
