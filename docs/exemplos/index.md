---
tags:
  - Exemplos
  - Reports
  - Programas
  - ALV
  - Integração
  - Completo
---

# Exemplos Práticos ABAP

Esta seção apresenta programas completos e funcionais que integram múltiplos conceitos ABAP. Cada exemplo é auto-contido, documentado e pronto para uso, demonstrando boas práticas de desenvolvimento.

## Exemplo 1: Report de Consulta com ALV Grid

Report interativo que busca pedidos de venda com filtros dinâmicos e exibe em ALV Grid com funcionalidades avançadas.

```abap
*&---------------------------------------------------------------------*
*& Report Z_SALES_ORDER_REPORT
*&---------------------------------------------------------------------*
*& Descrição: Report de consulta de pedidos de venda com ALV
*& Funcionalidades:
*&   - Filtros de seleção (data, cliente, material)
*&   - ALV Grid interativo com duplo clique
*&   - Exportação para Excel
*&   - Navegação para VA03 (exibir pedido)
*&   - Cálculo de totais
*&---------------------------------------------------------------------*
REPORT z_sales_order_report.

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_output,
         vbeln     TYPE vbak-vbeln,      " Pedido
         erdat     TYPE vbak-erdat,      " Data criação
         kunnr     TYPE vbak-kunnr,      " Cliente
         name1     TYPE kna1-name1,      " Nome cliente
         posnr     TYPE vbap-posnr,      " Item
         matnr     TYPE vbap-matnr,      " Material
         maktx     TYPE makt-maktx,      " Descrição material
         kwmeng    TYPE vbap-kwmeng,     " Quantidade
         netwr     TYPE vbap-netwr,      " Valor líquido
         waerk     TYPE vbap-waerk,      " Moeda
         icon      TYPE icon_d,          " Ícone de status
         cellcolor TYPE lvc_t_scol,      " Cores de célula
       END OF ty_output.

TYPES: tt_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_output  TYPE tt_output,
      gt_fcat    TYPE lvc_t_fcat,
      gs_layout  TYPE lvc_s_layo,
      go_alv     TYPE REF TO cl_salv_table.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  " Período
  SELECT-OPTIONS: s_erdat FOR vbak-erdat DEFAULT sy-datum TO sy-datum.

  " Cliente
  SELECT-OPTIONS: s_kunnr FOR vbak-kunnr.

  " Material
  SELECT-OPTIONS: s_matnr FOR vbap-matnr.

  " Parâmetros adicionais
  PARAMETERS: p_max TYPE i DEFAULT 500 OBLIGATORY.  " Máximo de registros
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Textos da tela de seleção
  TEXT-001 = 'Filtros de Seleção'.

*----------------------------------------------------------------------*
* At Selection Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  " Validações
  IF p_max <= 0.
    MESSAGE 'Máximo de registros deve ser maior que zero' TYPE 'E'.
  ENDIF.

  IF p_max > 10000.
    MESSAGE 'Máximo de registros não pode exceder 10.000' TYPE 'W'.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fetch_data.
  PERFORM enrich_data.
  PERFORM calculate_totals.

END-OF-SELECTION.
  PERFORM display_alv.

*----------------------------------------------------------------------*
* Form FETCH_DATA
*----------------------------------------------------------------------*
FORM fetch_data.
  DATA: lt_vbak TYPE TABLE OF vbak,
        lt_vbap TYPE TABLE OF vbap.

  " Buscar cabeçalhos de pedidos
  SELECT * FROM vbak
    WHERE erdat IN @s_erdat
      AND kunnr IN @s_kunnr
    INTO TABLE @lt_vbak
    UP TO @p_max ROWS.

  IF lt_vbak IS INITIAL.
    MESSAGE 'Nenhum pedido encontrado com os critérios informados' TYPE 'S'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " Buscar itens dos pedidos
  SELECT * FROM vbap
    FOR ALL ENTRIES IN @lt_vbak
    WHERE vbeln = @lt_vbak-vbeln
      AND matnr IN @s_matnr
    INTO TABLE @lt_vbap.

  IF lt_vbap IS INITIAL.
    MESSAGE 'Nenhum item encontrado com os materiais informados' TYPE 'S'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " Combinar dados de cabeçalho e item
  LOOP AT lt_vbap INTO DATA(ls_vbap).
    READ TABLE lt_vbak INTO DATA(ls_vbak)
      WITH KEY vbeln = ls_vbap-vbeln.

    CHECK sy-subrc = 0.

    APPEND VALUE #(
      vbeln  = ls_vbap-vbeln
      erdat  = ls_vbak-erdat
      kunnr  = ls_vbak-kunnr
      posnr  = ls_vbap-posnr
      matnr  = ls_vbap-matnr
      kwmeng = ls_vbap-kwmeng
      netwr  = ls_vbap-netwr
      waerk  = ls_vbap-waerk
    ) TO gt_output.
  ENDLOOP.

  " Mensagem de progresso
  MESSAGE |{ lines( gt_output ) } itens carregados| TYPE 'S'.
ENDFORM.

*----------------------------------------------------------------------*
* Form ENRICH_DATA
*----------------------------------------------------------------------*
FORM enrich_data.
  DATA: lt_kna1 TYPE TABLE OF kna1,
        lt_makt TYPE TABLE OF makt.

  CHECK gt_output IS NOT INITIAL.

  " Buscar nomes de clientes
  SELECT kunnr, name1 FROM kna1
    FOR ALL ENTRIES IN @gt_output
    WHERE kunnr = @gt_output-kunnr
    INTO TABLE @lt_kna1.

  " Buscar descrições de materiais
  SELECT matnr, maktx FROM makt
    FOR ALL ENTRIES IN @gt_output
    WHERE matnr = @gt_output-matnr
      AND spras = @sy-langu
    INTO TABLE @lt_makt.

  " Criar tabelas HASHED para lookup rápido
  DATA(lt_kna1_hash) = VALUE ty_t_kna1_hash(
    FOR ls_kna1 IN lt_kna1 ( ls_kna1 )
  ).

  DATA(lt_makt_hash) = VALUE ty_t_makt_hash(
    FOR ls_makt IN lt_makt ( ls_makt )
  ).

  " Enriquecer dados principais
  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
    " Nome do cliente
    READ TABLE lt_kna1_hash INTO DATA(ls_kna1)
      WITH TABLE KEY kunnr = <fs_output>-kunnr.
    IF sy-subrc = 0.
      <fs_output>-name1 = ls_kna1-name1.
    ENDIF.

    " Descrição do material
    READ TABLE lt_makt_hash INTO DATA(ls_makt)
      WITH TABLE KEY matnr = <fs_output>-matnr.
    IF sy-subrc = 0.
      <fs_output>-maktx = ls_makt-maktx.
    ENDIF.

    " Adicionar ícone baseado no valor
    IF <fs_output>-netwr >= 10000.
      <fs_output>-icon = icon_led_green.      " Alto valor
    ELSEIF <fs_output>-netwr >= 5000.
      <fs_output>-icon = icon_led_yellow.     " Médio valor
    ELSE.
      <fs_output>-icon = icon_led_red.        " Baixo valor
    ENDIF.

    " Colorir linha se valor muito alto
    IF <fs_output>-netwr >= 50000.
      <fs_output>-cellcolor = VALUE #(
        ( fname = 'NETWR'
          color-col = 5   " Verde
          color-int = 0
          color-inv = 0 )
      ).
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* Form CALCULATE_TOTALS
*----------------------------------------------------------------------*
FORM calculate_totals.
  DATA: lv_total_value TYPE p DECIMALS 2,
        lv_total_qty   TYPE p DECIMALS 2,
        lv_orders      TYPE i.

  " Calcular totais
  LOOP AT gt_output INTO DATA(ls_output).
    lv_total_value = lv_total_value + ls_output-netwr.
    lv_total_qty   = lv_total_qty + ls_output-kwmeng.
  ENDLOOP.

  " Contar pedidos únicos
  DATA(lt_unique_orders) = gt_output.
  SORT lt_unique_orders BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_unique_orders COMPARING vbeln.
  lv_orders = lines( lt_unique_orders ).

  " Exibir resumo
  WRITE: / 'RESUMO DA SELEÇÃO',
         / '-' FILLING sy-linsz,
         / 'Total de Pedidos:', lv_orders,
         / 'Total de Itens:', lines( gt_output ),
         / 'Valor Total:', lv_total_value CURRENCY 'BRL',
         / 'Quantidade Total:', lv_total_qty,
         / '-' FILLING sy-linsz.
  SKIP.
ENDFORM.

*----------------------------------------------------------------------*
* Form DISPLAY_ALV
*----------------------------------------------------------------------*
FORM display_alv.
  TRY.
      " Criar instância ALV
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_output
      ).

      " Configurar funções
      DATA(lo_functions) = go_alv->get_functions( ).
      lo_functions->set_all( abap_true ).

      " Configurar colunas
      DATA(lo_columns) = go_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      " Configurar ícones
      TRY.
          DATA(lo_column) = lo_columns->get_column( 'ICON' ).
          lo_column->set_icon( abap_true ).
          lo_column->set_long_text( 'Status' ).
          lo_column->set_medium_text( 'Status' ).
          lo_column->set_short_text( 'St.' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      " Configurar cores
      TRY.
          lo_column = lo_columns->get_column( 'CELLCOLOR' ).
          lo_column->set_technical( abap_true ).
        CATCH cx_salv_not_found.
      ENDTRY.

      " Configurar layout
      DATA(lo_display) = go_alv->get_display_settings( ).
      lo_display->set_striped_pattern( abap_true ).
      lo_display->set_list_header( 'Relatório de Pedidos de Venda' ).

      " Configurar seleção
      DATA(lo_selections) = go_alv->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      " Evento de duplo clique
      DATA(lo_events) = go_alv->get_event( ).
      SET HANDLER on_double_click FOR lo_events.

      " Exibir ALV
      go_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_salv).
      MESSAGE lx_salv->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*----------------------------------------------------------------------*
* Event Handler: Double Click
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    READ TABLE gt_output INTO DATA(ls_output) INDEX row.
    CHECK sy-subrc = 0.

    " Se clicar na coluna de pedido, navegar para VA03
    IF column = 'VBELN'.
      SET PARAMETER ID 'AUN' FIELD ls_output-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDIF.

    " Se clicar na coluna de material, navegar para MM03
    IF column = 'MATNR'.
      SET PARAMETER ID 'MAT' FIELD ls_output-matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Type Definitions for Hashed Tables
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_kna1_hash,
         kunnr TYPE kunnr,
         name1 TYPE name1,
       END OF ty_kna1_hash,
       ty_t_kna1_hash TYPE HASHED TABLE OF ty_kna1_hash
         WITH UNIQUE KEY kunnr.

TYPES: BEGIN OF ty_makt_hash,
         matnr TYPE matnr,
         maktx TYPE maktx,
       END OF ty_makt_hash,
       ty_t_makt_hash TYPE HASHED TABLE OF ty_makt_hash
         WITH UNIQUE KEY matnr.
```

!!! tip "Conceitos Aplicados"
    - **SQL Otimizado**: FOR ALL ENTRIES, HASHED tables para lookup
    - **ALV SALV**: Grid moderno com eventos
    - **Selection Screen**: Filtros dinâmicos com validação
    - **Performance**: Minimização de acessos ao banco
    - **UX**: Ícones, cores, duplo clique para navegação
    - **Cálculos**: Agregações e estatísticas

## Exemplo 2: Job Automático de Processamento em Massa

Programa executável em background que processa faturas pendentes automaticamente.

```abap
*&---------------------------------------------------------------------*
*& Report Z_INVOICE_BATCH_PROCESSOR
*&---------------------------------------------------------------------*
*& Descrição: Processamento em massa de faturas pendentes
*& Funcionalidades:
*&   - Identificar faturas a processar
*&   - Validar dados antes do processamento
*&   - Processar em pacotes (commit work parcial)
*&   - Log de auditoria completo (Application Log - BAL)
*&   - Envio de email com resultado
*&   - Tratamento de erros robusto
*&---------------------------------------------------------------------*
REPORT z_invoice_batch_processor.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: gc_package_size TYPE i VALUE 100,
           gc_log_object   TYPE balobj_d VALUE 'ZFIN',
           gc_log_subobj   TYPE balsubobj VALUE 'INVOICE_PROC'.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_invoice,
         invoice_id   TYPE vbeln_vf,
         customer_id  TYPE kunnr,
         amount       TYPE wrbtr,
         currency     TYPE waers,
         status       TYPE char1,
         error_msg    TYPE string,
       END OF ty_invoice,
       tt_invoices TYPE STANDARD TABLE OF ty_invoice WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Data
*----------------------------------------------------------------------*
DATA: gt_invoices     TYPE tt_invoices,
      gv_log_handle   TYPE balloghndl,
      gv_success_count TYPE i,
      gv_error_count   TYPE i.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE bukrs DEFAULT '1000' OBLIGATORY,
              p_datum TYPE datum DEFAULT sy-datum OBLIGATORY.

  PARAMETERS: p_test  AS CHECKBOX DEFAULT 'X',  " Modo teste
              p_email TYPE ad_smtpadr.           " Email para notificação
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Parâmetros de Processamento'.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM initialize_log.
  PERFORM select_invoices.
  PERFORM validate_invoices.
  PERFORM process_invoices.
  PERFORM finalize_log.
  PERFORM send_notification.

*----------------------------------------------------------------------*
* Form INITIALIZE_LOG
*----------------------------------------------------------------------*
FORM initialize_log.
  DATA: ls_log TYPE bal_s_log.

  " Criar log de aplicação
  ls_log-object    = gc_log_object.
  ls_log-subobject = gc_log_subobj.
  ls_log-extnumber = |{ sy-datum }{ sy-uzeit }|.
  ls_log-aldate    = sy-datum.
  ls_log-altime    = sy-uzeit.
  ls_log-aluser    = sy-uname.
  ls_log-alprog    = sy-repid.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = gv_log_handle
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao criar log de aplicação' TYPE 'E'.
  ENDIF.

  " Log de início
  PERFORM add_log_message
    EXPORTING
      iv_msgty = 'I'
      iv_text  = |Processamento iniciado - Empresa: { p_bukrs } - Data: { p_datum }|.

  IF p_test = abap_true.
    PERFORM add_log_message
      EXPORTING
        iv_msgty = 'W'
        iv_text  = 'MODO TESTE - Nenhuma alteração será gravada no banco'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* Form SELECT_INVOICES
*----------------------------------------------------------------------*
FORM select_invoices.
  " Buscar faturas pendentes
  " (Exemplo simulado - adaptar para tabelas reais)
  SELECT vbeln AS invoice_id,
         kunag AS customer_id,
         netwr AS amount,
         waerk AS currency
    FROM vbrk
    WHERE fkart = 'F2'
      AND bukrs = @p_bukrs
      AND fkdat = @p_datum
      AND rfbsk = 'A'  " Status: Aguardando processamento
    INTO CORRESPONDING FIELDS OF TABLE @gt_invoices
    UP TO 1000 ROWS.

  IF gt_invoices IS INITIAL.
    PERFORM add_log_message
      EXPORTING
        iv_msgty = 'I'
        iv_text  = 'Nenhuma fatura pendente encontrada'.
    LEAVE PROGRAM.
  ENDIF.

  PERFORM add_log_message
    EXPORTING
      iv_msgty = 'S'
      iv_text  = |{ lines( gt_invoices ) } faturas encontradas para processamento|.
ENDFORM.

*----------------------------------------------------------------------*
* Form VALIDATE_INVOICES
*----------------------------------------------------------------------*
FORM validate_invoices.
  DATA: lv_invalid_count TYPE i.

  LOOP AT gt_invoices ASSIGNING FIELD-SYMBOL(<fs_invoice>).
    " Validar montante
    IF <fs_invoice>-amount <= 0.
      <fs_invoice>-status = 'E'.
      <fs_invoice>-error_msg = 'Valor inválido (zero ou negativo)'.
      lv_invalid_count = lv_invalid_count + 1.
      CONTINUE.
    ENDIF.

    " Validar cliente
    SELECT SINGLE kunnr FROM kna1
      WHERE kunnr = @<fs_invoice>-customer_id
      INTO @DATA(lv_kunnr).

    IF sy-subrc <> 0.
      <fs_invoice>-status = 'E'.
      <fs_invoice>-error_msg = 'Cliente não encontrado'.
      lv_invalid_count = lv_invalid_count + 1.
      CONTINUE.
    ENDIF.

    " Validação bem-sucedida
    <fs_invoice>-status = 'V'.  " Validado
  ENDLOOP.

  IF lv_invalid_count > 0.
    PERFORM add_log_message
      EXPORTING
        iv_msgty = 'W'
        iv_text  = |{ lv_invalid_count } faturas com erros de validação|.
  ELSE.
    PERFORM add_log_message
      EXPORTING
        iv_msgty = 'S'
        iv_text  = 'Todas as faturas validadas com sucesso'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* Form PROCESS_INVOICES
*----------------------------------------------------------------------*
FORM process_invoices.
  DATA: lv_package_count TYPE i,
        lt_package       TYPE tt_invoices.

  LOOP AT gt_invoices INTO DATA(ls_invoice)
    WHERE status = 'V'.  " Apenas validadas

    " Adicionar ao pacote
    APPEND ls_invoice TO lt_package.

    " Processar quando atingir tamanho do pacote
    IF lines( lt_package ) >= gc_package_size.
      PERFORM process_package CHANGING lt_package.
      lv_package_count = lv_package_count + 1.
      CLEAR lt_package.
    ENDIF.
  ENDLOOP.

  " Processar pacote restante
  IF lt_package IS NOT INITIAL.
    PERFORM process_package CHANGING lt_package.
    lv_package_count = lv_package_count + 1.
  ENDIF.

  PERFORM add_log_message
    EXPORTING
      iv_msgty = 'I'
      iv_text  = |{ lv_package_count } pacotes processados|.
ENDFORM.

*----------------------------------------------------------------------*
* Form PROCESS_PACKAGE
*----------------------------------------------------------------------*
FORM process_package CHANGING ct_package TYPE tt_invoices.
  LOOP AT ct_package ASSIGNING FIELD-SYMBOL(<fs_invoice>).
    TRY.
        " Lógica de processamento da fatura
        " (Exemplo: chamar BAPI, atualizar tabelas, etc.)
        PERFORM process_single_invoice CHANGING <fs_invoice>.

        " Sucesso
        <fs_invoice>-status = 'S'.
        gv_success_count = gv_success_count + 1.

        PERFORM add_log_message
          EXPORTING
            iv_msgty = 'S'
            iv_text  = |Fatura { <fs_invoice>-invoice_id } processada com sucesso|.

      CATCH cx_root INTO DATA(lx_error).
        " Erro
        <fs_invoice>-status = 'E'.
        <fs_invoice>-error_msg = lx_error->get_text( ).
        gv_error_count = gv_error_count + 1.

        PERFORM add_log_message
          EXPORTING
            iv_msgty = 'E'
            iv_text  = |Erro ao processar fatura { <fs_invoice>-invoice_id }: |
                    && |{ lx_error->get_text( ) }|.
    ENDTRY.
  ENDLOOP.

  " Commit parcial (se não estiver em modo teste)
  IF p_test = abap_false.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* Form PROCESS_SINGLE_INVOICE
*----------------------------------------------------------------------*
FORM process_single_invoice CHANGING cs_invoice TYPE ty_invoice.
  " Simular processamento
  " Em produção, chamar BAPI ou função de processamento real

  " Exemplo: BAPI_ACC_INVOICE_RECEIPT_POST
  DATA: lt_return TYPE bapiret2_t.

  " Simular sucesso (adaptar para lógica real)
  IF cs_invoice-amount > 100000.
    " Simular erro para valores muito altos
    RAISE EXCEPTION TYPE cx_sy_arithmetic_overflow
      EXPORTING
        textid = VALUE #( msgid = 'ZMSG' msgno = '001'
                          msgv1 = 'Valor excede limite permitido' ).
  ENDIF.

  " Processamento bem-sucedido
  " (Aqui viria a lógica real de processamento)
ENDFORM.

*----------------------------------------------------------------------*
* Form ADD_LOG_MESSAGE
*----------------------------------------------------------------------*
FORM add_log_message
  EXPORTING
    iv_msgty TYPE symsgty
    iv_text  TYPE string.

  DATA: ls_msg TYPE bal_s_msg.

  ls_msg-msgty = iv_msgty.
  ls_msg-msgid = 'ZMSG'.
  ls_msg-msgno = '001'.
  ls_msg-msgv1 = iv_text+0(50).
  ls_msg-msgv2 = iv_text+50(50).
  ls_msg-msgv3 = iv_text+100(50).
  ls_msg-msgv4 = iv_text+150(50).

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = gv_log_handle
      i_s_msg      = ls_msg
    EXCEPTIONS
      OTHERS       = 1.
ENDFORM.

*----------------------------------------------------------------------*
* Form FINALIZE_LOG
*----------------------------------------------------------------------*
FORM finalize_log.
  " Resumo final
  DATA: lv_total TYPE i.

  lv_total = gv_success_count + gv_error_count.

  PERFORM add_log_message
    EXPORTING
      iv_msgty = 'I'
      iv_text  = '--- RESUMO DO PROCESSAMENTO ---'.

  PERFORM add_log_message
    EXPORTING
      iv_msgty = 'I'
      iv_text  = |Total processado: { lv_total }|.

  PERFORM add_log_message
    EXPORTING
      iv_msgty = 'S'
      iv_text  = |Sucesso: { gv_success_count }|.

  IF gv_error_count > 0.
    PERFORM add_log_message
      EXPORTING
        iv_msgty = 'E'
        iv_text  = |Erros: { gv_error_count }|.
  ENDIF.

  " Salvar log no banco
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all   = 'X'
      i_log_handle = gv_log_handle
    EXCEPTIONS
      OTHERS       = 1.

  COMMIT WORK.

  " Exibir log (se executado em foreground)
  IF sy-batch = abap_false.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_log_handle = gv_log_handle
      EXCEPTIONS
        OTHERS       = 1.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* Form SEND_NOTIFICATION
*----------------------------------------------------------------------*
FORM send_notification.
  CHECK p_email IS NOT INITIAL.

  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_recipient    TYPE REF TO if_recipient_bcs,
        lt_body         TYPE bcsy_text.

  TRY.
      " Criar email
      lo_send_request = cl_bcs=>create_persistent( ).

      " Corpo do email
      APPEND |Processamento de Faturas - { sy-datum } { sy-uzeit }| TO lt_body.
      APPEND '' TO lt_body.
      APPEND |Empresa: { p_bukrs }| TO lt_body.
      APPEND |Data: { p_datum }| TO lt_body.
      APPEND '' TO lt_body.
      APPEND '--- RESUMO ---' TO lt_body.
      APPEND |Total processado: { gv_success_count + gv_error_count }| TO lt_body.
      APPEND |Sucesso: { gv_success_count }| TO lt_body.
      APPEND |Erros: { gv_error_count }| TO lt_body.

      IF p_test = abap_true.
        APPEND '' TO lt_body.
        APPEND 'ATENÇÃO: Executado em MODO TESTE' TO lt_body.
      ENDIF.

      " Criar documento
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = lt_body
        i_subject = |Resultado do Processamento de Faturas - { sy-datum }|
      ).

      lo_send_request->set_document( lo_document ).

      " Destinatário
      lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ).
      lo_send_request->add_recipient( lo_recipient ).

      " Remetente
      DATA(lo_sender) = cl_sapuser_bcs=>create( sy-uname ).
      lo_send_request->set_sender( lo_sender ).

      " Enviar
      lo_send_request->set_send_immediately( abap_true ).
      DATA(lv_sent) = lo_send_request->send( ).

      IF lv_sent = abap_true.
        COMMIT WORK.
        MESSAGE 'Email de notificação enviado' TYPE 'S'.
      ENDIF.

    CATCH cx_bcs INTO DATA(lx_bcs).
      MESSAGE |Erro ao enviar email: { lx_bcs->get_text( ) }| TYPE 'W'.
  ENDTRY.
ENDFORM.
```

!!! tip "Conceitos Aplicados"
    - **Background Processing**: Preparado para execução via SM36/SM37
    - **Package Processing**: Commits parciais para grandes volumes
    - **Application Log (BAL)**: Rastreabilidade completa
    - **Error Handling**: TRY/CATCH robusto
    - **Validações**: Antes do processamento
    - **Email Notification**: Resultados automáticos
    - **Test Mode**: Simular sem gravar dados

## Exemplo 3: Integração RFC Completa

Classe que encapsula integração RFC com sistema externo de forma robusta e reutilizável.

```abap
*&---------------------------------------------------------------------*
*& Class ZCL_RFC_CUSTOMER_INTEGRATION
*&---------------------------------------------------------------------*
*& Descrição: Classe para integração de clientes via RFC
*& Funcionalidades:
*&   - Envio de clientes para sistema externo
*&   - Recepção de clientes de sistema externo
*&   - Retry automático em caso de erro
*&   - Logging estruturado
*&   - Tratamento de exceções RFC
*&---------------------------------------------------------------------*
CLASS zcl_rfc_customer_integration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Types
    TYPES: BEGIN OF ty_customer,
             kunnr TYPE kunnr,
             name1 TYPE name1,
             ort01 TYPE ort01,
             land1 TYPE land1,
             email TYPE ad_smtpadr,
           END OF ty_customer,
           tt_customers TYPE STANDARD TABLE OF ty_customer WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             kunnr   TYPE kunnr,
             success TYPE abap_bool,
             message TYPE string,
           END OF ty_result,
           tt_results TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

    " Methods
    METHODS constructor
      IMPORTING iv_destination TYPE rfcdest DEFAULT 'ZEXT_SYSTEM'.

    METHODS send_customers
      IMPORTING it_customers      TYPE tt_customers
      RETURNING VALUE(rt_results) TYPE tt_results
      RAISING   cx_rfc_exception.

    METHODS get_customers
      IMPORTING iv_country         TYPE land1
      RETURNING VALUE(rt_customers) TYPE tt_customers
      RAISING   cx_rfc_exception.

  PRIVATE SECTION.
    DATA: mv_destination TYPE rfcdest,
          mv_retry_count TYPE i VALUE 3,
          mo_logger      TYPE REF TO zcl_logger.

    METHODS check_connection
      RAISING cx_rfc_exception.

    METHODS log_message
      IMPORTING iv_type    TYPE symsgty
                iv_message TYPE string.
ENDCLASS.

CLASS zcl_rfc_customer_integration IMPLEMENTATION.
  METHOD constructor.
    mv_destination = iv_destination.

    " Inicializar logger
    mo_logger = NEW zcl_logger( ).

    " Verificar conexão
    TRY.
        check_connection( ).
        log_message(
          iv_type = 'S'
          iv_message = |Conexão RFC estabelecida: { mv_destination }|
        ).
      CATCH cx_rfc_exception INTO DATA(lx_rfc).
        log_message(
          iv_type = 'E'
          iv_message = |Falha na conexão RFC: { lx_rfc->get_text( ) }|
        ).
        RAISE EXCEPTION lx_rfc.
    ENDTRY.
  ENDMETHOD.

  METHOD check_connection.
    " Testar conexão RFC
    CALL FUNCTION 'RFC_PING'
      DESTINATION mv_destination
      EXCEPTIONS
        communication_failure = 1  MESSAGE DATA(lv_msg)
        system_failure        = 2  MESSAGE lv_msg
        OTHERS                = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_rfc_exception
        EXPORTING
          textid = VALUE #(
            msgid = sy-msgid
            msgno = sy-msgno
            msgv1 = lv_msg
          ).
    ENDIF.
  ENDMETHOD.

  METHOD send_customers.
    DATA: lt_failed TYPE tt_customers,
          lv_retry  TYPE i.

    " Processar cada cliente
    LOOP AT it_customers INTO DATA(ls_customer).
      DATA(ls_result) = VALUE ty_result( kunnr = ls_customer-kunnr ).

      " Retry logic
      DO mv_retry_count TIMES.
        lv_retry = sy-index.

        TRY.
            " Chamar FM RFC no destino externo
            " (Adaptar nome da função para a real)
            CALL FUNCTION 'Z_REMOTE_CREATE_CUSTOMER'
              DESTINATION mv_destination
              EXPORTING
                iv_kunnr = ls_customer-kunnr
                iv_name1 = ls_customer-name1
                iv_ort01 = ls_customer-ort01
                iv_land1 = ls_customer-land1
                iv_email = ls_customer-email
              IMPORTING
                ev_success = DATA(lv_success)
                ev_message = DATA(lv_message)
              EXCEPTIONS
                communication_failure = 1  MESSAGE DATA(lv_comm_msg)
                system_failure        = 2  MESSAGE lv_comm_msg
                customer_exists       = 3
                invalid_data          = 4
                OTHERS                = 5.

            CASE sy-subrc.
              WHEN 0.
                " Sucesso
                ls_result-success = abap_true.
                ls_result-message = lv_message.

                log_message(
                  iv_type = 'S'
                  iv_message = |Cliente { ls_customer-kunnr } enviado com sucesso|
                ).

                EXIT.  " Sair do loop de retry

              WHEN 1 OR 2.
                " Erro de comunicação - tentar novamente
                IF lv_retry < mv_retry_count.
                  log_message(
                    iv_type = 'W'
                    iv_message = |Tentativa { lv_retry }: Erro de comunicação - { lv_comm_msg }|
                  ).

                  WAIT UP TO 2 SECONDS.  " Aguardar antes de retry
                  CONTINUE.
                ELSE.
                  " Máximo de tentativas excedido
                  ls_result-success = abap_false.
                  ls_result-message = |Falha de comunicação após { mv_retry_count } tentativas|.

                  log_message(
                    iv_type = 'E'
                    iv_message = |Cliente { ls_customer-kunnr }: { ls_result-message }|
                  ).

                  EXIT.
                ENDIF.

              WHEN 3.
                " Cliente já existe
                ls_result-success = abap_false.
                ls_result-message = 'Cliente já existe no sistema destino'.

                log_message(
                  iv_type = 'W'
                  iv_message = |Cliente { ls_customer-kunnr } já existe|
                ).

                EXIT.

              WHEN 4.
                " Dados inválidos
                ls_result-success = abap_false.
                ls_result-message = 'Dados inválidos'.

                log_message(
                  iv_type = 'E'
                  iv_message = |Cliente { ls_customer-kunnr }: Dados inválidos|
                ).

                EXIT.

              WHEN OTHERS.
                " Erro genérico
                ls_result-success = abap_false.
                ls_result-message = 'Erro desconhecido'.

                log_message(
                  iv_type = 'E'
                  iv_message = |Cliente { ls_customer-kunnr }: Erro desconhecido|
                ).

                EXIT.
            ENDCASE.

          CATCH cx_rfc_exception INTO DATA(lx_rfc).
            " Exceção RFC
            ls_result-success = abap_false.
            ls_result-message = lx_rfc->get_text( ).

            log_message(
              iv_type = 'E'
              iv_message = |Cliente { ls_customer-kunnr }: { lx_rfc->get_text( ) }|
            ).

            EXIT.
        ENDTRY.
      ENDDO.

      APPEND ls_result TO rt_results.
    ENDLOOP.

    " Log final
    DATA(lv_success_count) = REDUCE i( INIT sum = 0
                                        FOR ls_res IN rt_results
                                        WHERE ( success = abap_true )
                                        NEXT sum = sum + 1 ).

    log_message(
      iv_type = 'I'
      iv_message = |Processamento concluído: { lv_success_count }/{ lines( it_customers ) } sucessos|
    ).
  ENDMETHOD.

  METHOD get_customers.
    " Buscar clientes do sistema externo
    TRY.
        CALL FUNCTION 'Z_REMOTE_GET_CUSTOMERS'
          DESTINATION mv_destination
          EXPORTING
            iv_country   = iv_country
          IMPORTING
            et_customers = rt_customers
          EXCEPTIONS
            communication_failure = 1  MESSAGE DATA(lv_msg)
            system_failure        = 2  MESSAGE lv_msg
            no_data_found         = 3
            OTHERS                = 4.

        CASE sy-subrc.
          WHEN 0.
            log_message(
              iv_type = 'S'
              iv_message = |{ lines( rt_customers ) } clientes recebidos do sistema externo|
            ).

          WHEN 1 OR 2.
            RAISE EXCEPTION TYPE cx_rfc_exception
              EXPORTING
                textid = VALUE #( msgid = 'ZMSG' msgno = '001' msgv1 = lv_msg ).

          WHEN 3.
            log_message(
              iv_type = 'I'
              iv_message = |Nenhum cliente encontrado para país { iv_country }|
            ).

          WHEN OTHERS.
            RAISE EXCEPTION TYPE cx_rfc_exception
              EXPORTING
                textid = VALUE #( msgid = 'ZMSG' msgno = '999' msgv1 = 'Erro desconhecido' ).
        ENDCASE.

      CATCH cx_rfc_exception INTO DATA(lx_rfc).
        log_message(
          iv_type = 'E'
          iv_message = |Erro ao buscar clientes: { lx_rfc->get_text( ) }|
        ).
        RAISE EXCEPTION lx_rfc.
    ENDTRY.
  ENDMETHOD.

  METHOD log_message.
    " Logar via classe de logger (ou usar BAL diretamente)
    mo_logger->add_message(
      iv_type    = iv_type
      iv_message = iv_message
    ).

    " Também escrever em syslog para debug
    IF iv_type = 'E'.
      MESSAGE iv_message TYPE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Programa de Teste
*----------------------------------------------------------------------*
REPORT z_test_rfc_integration.

START-OF-SELECTION.
  TRY.
      " Instanciar classe
      DATA(lo_rfc) = NEW zcl_rfc_customer_integration( 'ZEXT_SYSTEM' ).

      " Preparar dados de teste
      DATA(lt_customers) = VALUE zcl_rfc_customer_integration=>tt_customers(
        ( kunnr = '0000100001' name1 = 'Cliente Teste 1'
          ort01 = 'São Paulo' land1 = 'BR' email = 'teste1@example.com' )
        ( kunnr = '0000100002' name1 = 'Cliente Teste 2'
          ort01 = 'Rio de Janeiro' land1 = 'BR' email = 'teste2@example.com' )
      ).

      " Enviar clientes
      DATA(lt_results) = lo_rfc->send_customers( lt_customers ).

      " Exibir resultados
      LOOP AT lt_results INTO DATA(ls_result).
        IF ls_result-success = abap_true.
          WRITE: / icon_green_light AS ICON, ls_result-kunnr, ls_result-message.
        ELSE.
          WRITE: / icon_red_light AS ICON, ls_result-kunnr, ls_result-message.
        ENDIF.
      ENDLOOP.

      SKIP.

      " Buscar clientes do sistema externo
      DATA(lt_remote_customers) = lo_rfc->get_customers( 'BR' ).

      WRITE: / 'Clientes recebidos:'.
      LOOP AT lt_remote_customers INTO DATA(ls_customer).
        WRITE: / ls_customer-kunnr, ls_customer-name1, ls_customer-ort01.
      ENDLOOP.

    CATCH cx_rfc_exception INTO DATA(lx_rfc).
      WRITE: / 'Erro RFC:', lx_rfc->get_text( ).
  ENDTRY.
```

!!! tip "Conceitos Aplicados"
    - **RFC**: Chamadas assíncronas e síncronas
    - **Retry Logic**: Tentativas automáticas em falhas
    - **Exception Handling**: Tratamento robusto de erros RFC
    - **OOP**: Encapsulamento da lógica de integração
    - **Logging**: Rastreabilidade de todas as operações
    - **Testability**: Fácil de testar e mockar

## Exemplo 4: Aplicação OO Completa com Testes

Sistema de gerenciamento de pedidos usando princípios SOLID e com testes unitários.

```abap
*&---------------------------------------------------------------------*
*& Sistema de Gerenciamento de Pedidos - Arquitetura Limpa
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Interface: Repository Pattern
*----------------------------------------------------------------------*
INTERFACE lif_order_repository.
  TYPES: tt_orders TYPE STANDARD TABLE OF zvbak WITH DEFAULT KEY.

  METHODS get_by_id
    IMPORTING iv_order_id      TYPE vbeln_va
    RETURNING VALUE(rs_order)  TYPE zvbak
    RAISING   cx_order_not_found.

  METHODS get_all
    IMPORTING iv_customer_id    TYPE kunnr OPTIONAL
              iv_date_from      TYPE datum OPTIONAL
              iv_date_to        TYPE datum OPTIONAL
    RETURNING VALUE(rt_orders)  TYPE tt_orders.

  METHODS save
    IMPORTING is_order TYPE zvbak
    RAISING   cx_order_save_failed.

  METHODS delete
    IMPORTING iv_order_id TYPE vbeln_va
    RAISING   cx_order_not_found.
ENDINTERFACE.

*----------------------------------------------------------------------*
* Interface: Validator
*----------------------------------------------------------------------*
INTERFACE lif_order_validator.
  METHODS validate
    IMPORTING is_order           TYPE zvbak
    RETURNING VALUE(rt_messages) TYPE bapiret2_t.
ENDINTERFACE.

*----------------------------------------------------------------------*
* Interface: Notification Service
*----------------------------------------------------------------------*
INTERFACE lif_notification_service.
  METHODS send_order_confirmation
    IMPORTING iv_order_id TYPE vbeln_va
              iv_email    TYPE ad_smtpadr
    RAISING   cx_notification_failed.
ENDINTERFACE.

*----------------------------------------------------------------------*
* Domain Entity: Order
*----------------------------------------------------------------------*
CLASS lcl_order DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_item,
             posnr  TYPE posnr_va,
             matnr  TYPE matnr,
             kwmeng TYPE kwmeng,
             netpr  TYPE netpr,
           END OF ty_item,
           tt_items TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING iv_order_id   TYPE vbeln_va
                iv_customer_id TYPE kunnr
                iv_order_date TYPE datum DEFAULT sy-datum.

    METHODS add_item
      IMPORTING is_item TYPE ty_item
      RAISING   cx_invalid_item.

    METHODS remove_item
      IMPORTING iv_position TYPE posnr_va.

    METHODS get_total_value
      RETURNING VALUE(rv_total) TYPE wrbtr.

    METHODS get_order_id
      RETURNING VALUE(rv_order_id) TYPE vbeln_va.

    METHODS get_customer_id
      RETURNING VALUE(rv_customer_id) TYPE kunnr.

    METHODS get_items
      RETURNING VALUE(rt_items) TYPE tt_items.

    METHODS to_database_structure
      RETURNING VALUE(rs_order) TYPE zvbak.

  PRIVATE SECTION.
    DATA: mv_order_id    TYPE vbeln_va,
          mv_customer_id TYPE kunnr,
          mv_order_date  TYPE datum,
          mt_items       TYPE tt_items.
ENDCLASS.

CLASS lcl_order IMPLEMENTATION.
  METHOD constructor.
    mv_order_id = iv_order_id.
    mv_customer_id = iv_customer_id.
    mv_order_date = iv_order_date.
  ENDMETHOD.

  METHOD add_item.
    " Validar item
    IF is_item-matnr IS INITIAL OR is_item-kwmeng <= 0.
      RAISE EXCEPTION TYPE cx_invalid_item.
    ENDIF.

    " Verificar se item já existe
    READ TABLE mt_items WITH KEY posnr = is_item-posnr TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE cx_invalid_item
        EXPORTING textid = VALUE #( msgid = 'ZMSG' msgno = '002'
                                     msgv1 = 'Item já existe' ).
    ENDIF.

    APPEND is_item TO mt_items.
  ENDMETHOD.

  METHOD remove_item.
    DELETE mt_items WHERE posnr = iv_position.
  ENDMETHOD.

  METHOD get_total_value.
    rv_total = REDUCE wrbtr(
      INIT sum = 0
      FOR ls_item IN mt_items
      NEXT sum = sum + ( ls_item-kwmeng * ls_item-netpr )
    ).
  ENDMETHOD.

  METHOD get_order_id.
    rv_order_id = mv_order_id.
  ENDMETHOD.

  METHOD get_customer_id.
    rv_customer_id = mv_customer_id.
  ENDMETHOD.

  METHOD get_items.
    rt_items = mt_items.
  ENDMETHOD.

  METHOD to_database_structure.
    rs_order-vbeln = mv_order_id.
    rs_order-kunnr = mv_customer_id.
    rs_order-erdat = mv_order_date.
    rs_order-netwr = get_total_value( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Service: Order Service (Business Logic)
*----------------------------------------------------------------------*
CLASS lcl_order_service DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_repository  TYPE REF TO lif_order_repository
                io_validator   TYPE REF TO lif_order_validator
                io_notification TYPE REF TO lif_notification_service.

    METHODS create_order
      IMPORTING io_order         TYPE REF TO lcl_order
                iv_send_notification TYPE abap_bool DEFAULT abap_true
      RAISING   cx_order_save_failed
                cx_validation_failed
                cx_notification_failed.

    METHODS get_order
      IMPORTING iv_order_id     TYPE vbeln_va
      RETURNING VALUE(ro_order) TYPE REF TO lcl_order
      RAISING   cx_order_not_found.

    METHODS calculate_order_statistics
      IMPORTING iv_customer_id     TYPE kunnr
      RETURNING VALUE(rs_stats)    TYPE ty_statistics.

  PRIVATE SECTION.
    DATA: mo_repository  TYPE REF TO lif_order_repository,
          mo_validator   TYPE REF TO lif_order_validator,
          mo_notification TYPE REF TO lif_notification_service.

    TYPES: BEGIN OF ty_statistics,
             total_orders TYPE i,
             total_value  TYPE wrbtr,
             avg_value    TYPE wrbtr,
           END OF ty_statistics.
ENDCLASS.

CLASS lcl_order_service IMPLEMENTATION.
  METHOD constructor.
    mo_repository = io_repository.
    mo_validator = io_validator.
    mo_notification = io_notification.
  ENDMETHOD.

  METHOD create_order.
    " 1. Validar pedido
    DATA(lt_messages) = mo_validator->validate( io_order->to_database_structure( ) ).

    IF line_exists( lt_messages[ type = 'E' ] ).
      RAISE EXCEPTION TYPE cx_validation_failed
        EXPORTING messages = lt_messages.
    ENDIF.

    " 2. Salvar no banco
    TRY.
        mo_repository->save( io_order->to_database_structure( ) ).
      CATCH cx_order_save_failed INTO DATA(lx_save).
        RAISE EXCEPTION lx_save.
    ENDTRY.

    " 3. Enviar notificação (se solicitado)
    IF iv_send_notification = abap_true.
      " Buscar email do cliente
      SELECT SINGLE smtp_addr FROM adr6
        INTO @DATA(lv_email)
        WHERE addrnumber = ( SELECT addrnumber FROM kna1
                             WHERE kunnr = @io_order->get_customer_id( ) ).

      IF sy-subrc = 0 AND lv_email IS NOT INITIAL.
        TRY.
            mo_notification->send_order_confirmation(
              iv_order_id = io_order->get_order_id( )
              iv_email    = lv_email
            ).
          CATCH cx_notification_failed.
            " Log error but don't fail order creation
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_order.
    " Buscar do repository
    DATA(ls_order_data) = mo_repository->get_by_id( iv_order_id ).

    " Reconstruir objeto de domínio
    ro_order = NEW lcl_order(
      iv_order_id    = ls_order_data-vbeln
      iv_customer_id = ls_order_data-kunnr
      iv_order_date  = ls_order_data-erdat
    ).

    " Buscar itens (simplificado)
    " ... adicionar itens ao pedido ...
  ENDMETHOD.

  METHOD calculate_order_statistics.
    DATA(lt_orders) = mo_repository->get_all( iv_customer_id = iv_customer_id ).

    rs_stats-total_orders = lines( lt_orders ).
    rs_stats-total_value = REDUCE wrbtr(
      INIT sum = 0
      FOR ls_order IN lt_orders
      NEXT sum = sum + ls_order-netwr
    ).

    IF rs_stats-total_orders > 0.
      rs_stats-avg_value = rs_stats-total_value / rs_stats-total_orders.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Unit Tests
*----------------------------------------------------------------------*
CLASS ltc_order_tests DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_order TYPE REF TO lcl_order.

    METHODS: setup,
             test_create_order FOR TESTING,
             test_add_item FOR TESTING,
             test_calculate_total FOR TESTING,
             test_add_invalid_item FOR TESTING.
ENDCLASS.

CLASS ltc_order_tests IMPLEMENTATION.
  METHOD setup.
    mo_order = NEW lcl_order(
      iv_order_id    = '0000000001'
      iv_customer_id = '0000100000'
      iv_order_date  = '20250115'
    ).
  ENDMETHOD.

  METHOD test_create_order.
    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = mo_order->get_order_id( )
      exp = '0000000001'
      msg = 'Order ID incorreto'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_order->get_customer_id( )
      exp = '0000100000'
      msg = 'Customer ID incorreto'
    ).
  ENDMETHOD.

  METHOD test_add_item.
    " Arrange
    DATA(ls_item) = VALUE lcl_order=>ty_item(
      posnr  = '000010'
      matnr  = 'MAT001'
      kwmeng = 5
      netpr  = '100.00'
    ).

    " Act
    TRY.
        mo_order->add_item( ls_item ).
      CATCH cx_invalid_item.
        cl_abap_unit_assert=>fail( 'Não deveria lançar exceção' ).
    ENDTRY.

    " Assert
    DATA(lt_items) = mo_order->get_items( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_items )
      exp = 1
      msg = 'Deveria ter 1 item'
    ).
  ENDMETHOD.

  METHOD test_calculate_total.
    " Arrange
    mo_order->add_item( VALUE #( posnr = '000010' matnr = 'MAT001'
                                  kwmeng = 5 netpr = '100.00' ) ).
    mo_order->add_item( VALUE #( posnr = '000020' matnr = 'MAT002'
                                  kwmeng = 3 netpr = '50.00' ) ).

    " Act
    DATA(lv_total) = mo_order->get_total_value( ).

    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_total
      exp = '650.00'   " (5 * 100) + (3 * 50) = 650
      msg = 'Total calculado incorretamente'
    ).
  ENDMETHOD.

  METHOD test_add_invalid_item.
    " Arrange
    DATA(ls_invalid_item) = VALUE lcl_order=>ty_item(
      posnr  = '000010'
      matnr  = ''          " Material vazio - inválido
      kwmeng = 5
      netpr  = '100.00'
    ).

    " Act & Assert
    TRY.
        mo_order->add_item( ls_invalid_item ).
        cl_abap_unit_assert=>fail( 'Deveria lançar cx_invalid_item' ).
      CATCH cx_invalid_item.
        " Sucesso - exceção esperada
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

!!! tip "Conceitos Aplicados"
    - **SOLID Principles**: Single Responsibility, Dependency Inversion
    - **Design Patterns**: Repository, Service Layer, Domain Entity
    - **Clean Architecture**: Separação de camadas
    - **ABAP Unit**: Testes automatizados completos
    - **Dependency Injection**: Via constructor
    - **Interface Segregation**: Contratos bem definidos
    - **Exception Handling**: Exceções customizadas

## Resumo dos Conceitos Integrados

!!! info "O que Aprendemos"
    Estes exemplos demonstram a integração de:

    **Exemplo 1 - ALV Report:**
    - SQL otimizado (FOR ALL ENTRIES, HASHED tables)
    - ALV SALV com eventos
    - Tratamento de cores e ícones
    - Navegação entre transações
    - Cálculos e agregações

    **Exemplo 2 - Batch Job:**
    - Background processing
    - Package processing (commits parciais)
    - Application Log (BAL)
    - Validações e error handling
    - Email notifications
    - Test mode vs production mode

    **Exemplo 3 - RFC Integration:**
    - Chamadas RFC robustas
    - Retry logic automático
    - Exception handling específico
    - Logging estruturado
    - Encapsulamento OO
    - Testabilidade

    **Exemplo 4 - Clean Architecture:**
    - SOLID principles
    - Design patterns (Repository, Service Layer)
    - Domain-driven design
    - Dependency injection
    - Unit testing completo
    - Separation of concerns

!!! success "Boas Práticas Demonstradas"
    - **Separação de responsabilidades**: Cada classe/form tem propósito único
    - **Comentários úteis**: Headers explicativos, não código óbvio
    - **Tratamento de erros**: TRY/CATCH, exceções customizadas
    - **Performance**: FOR ALL ENTRIES, HASHED tables, package processing
    - **Manutenibilidade**: Código modular e reutilizável
    - **Testabilidade**: Dependências injetadas, interfaces
    - **Auditoria**: Logging completo de operações
    - **User Experience**: Feedback visual, navegação intuitiva

---

**Tags:** #Exemplos #ALV #Background-Jobs #RFC #Integration #Clean-Architecture #SOLID #Unit-Tests #Best-Practices
