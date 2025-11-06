# Classe Manipulação de Ficheiros Excel

## 📋 Visão Geral

Classe utilitária para upload, download e manipulação de ficheiros Excel (formato `.xlsx`) em ABAP. Permite conversão de dados entre tabelas internas SAP e ficheiros Excel, incluindo formatação de datas e números decimais.

---

## ⚠️ **REQUISITO CRÍTICO - Classe de Exceção**

**É OBRIGATÓRIO criar a classe de exceção `ZCX_U_EXCEL_FILE` antes de utilizar esta classe!**

Esta classe deve ser do tipo **Exception Class** e conter, no mínimo, os seguintes IDs de texto (TEXTID):

- `cx_create_excel_itab` - Erro ao criar ficheiro Excel a partir de tabela interna
- `cx_no_file_selected` - Nenhum ficheiro foi selecionado
- `cx_no_worksheet` - Worksheet não encontrado (com parâmetro msgv1 para nome do worksheet)

### Criação da Classe de Exceção

```abap
CLASS zcx_u_excel_file DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.
    
    CONSTANTS:
      BEGIN OF cx_create_excel_itab,
        msgid TYPE symsgid VALUE 'Z_MSG_CLASS',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cx_create_excel_itab,
      
      BEGIN OF cx_no_file_selected,
        msgid TYPE symsgid VALUE 'Z_MSG_CLASS',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cx_no_file_selected,
      
      BEGIN OF cx_no_worksheet,
        msgid TYPE symsgid VALUE 'Z_MSG_CLASS',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cx_no_worksheet.
      
    DATA msgv1 TYPE string.
    
    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        msgv1    TYPE string OPTIONAL.
        
ENDCLASS.
```

---

## 📦 Tipos de Dados

### `ts_excel_file`
Estrutura que contém os atributos de um ficheiro Excel.

```abap
BEGIN OF ts_excel_file,
  name  TYPE filename,    " Nome do ficheiro
  size  TYPE i,           " Tamanho do ficheiro
  xdata TYPE solix_tab,   " Dados binários do ficheiro
END OF ts_excel_file
```

### `ts_number_delimiters`
Estrutura para delimitadores de números decimais.

```abap
BEGIN OF ts_number_delimiters,
  thousand TYPE char01,   " Delimitador de milhares (ex: . ou ,)
  decimal  TYPE char01,   " Delimitador decimal (ex: , ou .)
END OF ts_number_delimiters
```

---

## 🔢 Constantes (Expressões Regulares)

| Constante | Valor | Descrição |
|-----------|-------|-----------|
| `co_int_pattern` | `^\d+$` | Padrão para número inteiro |
| `co_decimals_pattern` | `^[+-]?(\d+\D\d+)$` | Padrão para número decimal |
| `co_thd_dec_pattern` | `^[+-]?((\d+(\T?\d+)*)(\D\d+)?)$` | Padrão para número com delimitador de milhares |

---

## 🔧 Métodos Públicos

### 1. `upload_local_excel`

**Descrição:** Faz upload de ficheiro Excel local e converte para tabela interna.

**Parâmetros:**

```abap
METHODS upload_local_excel
  IMPORTING
    iv_filename       TYPE string OPTIONAL      " Caminho do ficheiro
    iv_open_dialog    TYPE abap_bool DEFAULT space  " Abrir dialog de seleção
    iv_worksheet_name TYPE string OPTIONAL      " Nome da worksheet (se vazio, usa primeira)
  EXPORTING
    er_excel_data     TYPE REF TO data          " Referência aos dados do Excel
  RAISING
    zcx_u_excel_file                           " Exceção customizada
    cx_fdt_excel_core.                         " Exceção standard
```

**Exemplo de Uso:**

```abap
DATA: lo_excel TYPE REF TO zcl_util_excel,
      lr_data  TYPE REF TO data.

FIELD-SYMBOLS: <ft_data> TYPE STANDARD TABLE.

TRY.
    CREATE OBJECT lo_excel.
    
    " Opção 1: Com dialog de seleção
    lo_excel->upload_local_excel(
      EXPORTING
        iv_open_dialog    = abap_true
        iv_worksheet_name = 'Dados'
      IMPORTING
        er_excel_data = lr_data
    ).
    
    " Opção 2: Com caminho específico
    lo_excel->upload_local_excel(
      EXPORTING
        iv_filename       = 'C:\Temp\dados.xlsx'
        iv_worksheet_name = 'Sheet1'
      IMPORTING
        er_excel_data = lr_data
    ).
    
    " Atribuir dados a field-symbol
    ASSIGN lr_data->* TO <ft_data>.
    
    " Processar dados
    LOOP AT <ft_data> ASSIGNING FIELD-SYMBOL(<fs_row>).
      " Processamento...
    ENDLOOP.
    
  CATCH zcx_u_excel_file INTO DATA(lx_excel).
    MESSAGE lx_excel->get_text( ) TYPE 'E'.
    
  CATCH cx_fdt_excel_core INTO DATA(lx_core).
    MESSAGE lx_core->get_text( ) TYPE 'E'.
ENDTRY.
```

---

### 2. `download_itab_local_excel` (Estático)

**Descrição:** Faz download de tabela interna para ficheiro Excel local.

**Parâmetros:**

```abap
CLASS-METHODS download_itab_local_excel
  IMPORTING
    iv_filename TYPE string           " Nome do ficheiro de destino
    it_table    TYPE REF TO data      " Referência à tabela interna
  RAISING
    zcx_u_excel_file.
```

**Exemplo de Uso:**

```abap
DATA: lt_employees TYPE TABLE OF zemployee,
      lr_table     TYPE REF TO data.

" Preencher dados
SELECT * FROM zemployee INTO TABLE lt_employees UP TO 100 ROWS.

" Obter referência
GET REFERENCE OF lt_employees INTO lr_table.

TRY.
    zcl_util_excel=>download_itab_local_excel(
      iv_filename = 'Funcionarios_2025.xlsx'
      it_table    = lr_table
    ).
    
    MESSAGE 'Ficheiro exportado com sucesso!' TYPE 'S'.
    
  CATCH zcx_u_excel_file INTO DATA(lx).
    MESSAGE lx->get_text( ) TYPE 'E'.
ENDTRY.
```

---

### 3. `format_date` (Estático)

**Descrição:** Converte formato de data externa para formato interno SAP (YYYYMMDD).

**Parâmetros:**

```abap
CLASS-METHODS format_date
  IMPORTING
    iv_date_ext    TYPE clike         " Data em formato externo
  RETURNING
    VALUE(rv_date) TYPE datum.        " Data em formato SAP
```

**Suporta os seguintes formatos:**

- Número Excel (ex: `44927` = 01.01.2023)
- Formato com separadores: `DD.MM.YYYY`, `MM/DD/YYYY`, `YYYY-MM-DD`
- Adapta-se ao formato de data do utilizador (USER01-DATFM)

**Exemplo de Uso:**

```abap
DATA: lv_date_ext TYPE string VALUE '15.03.2025',
      lv_date_sap TYPE datum.

" Converter data externa para formato SAP
lv_date_sap = zcl_util_excel=>format_date( lv_date_ext ).
" Resultado: 20250315

" Também funciona com números do Excel
lv_date_sap = zcl_util_excel=>format_date( '45737' ).
" Resultado: 20250315
```

---

### 4. `format_number_dec` (Estático)

**Descrição:** Converte formato decimal externo para formato interno SAP.

**Parâmetros:**

```abap
CLASS-METHODS format_number_dec
  IMPORTING
    iv_value_ext         TYPE clike           " Número em formato externo
  RETURNING
    VALUE(rv_value_user) TYPE string.         " Número formatado
```

**Suporta os seguintes formatos:**

- `1.234,56` (formato alemão)
- `1,234.56` (formato anglo-saxónico)
- `1 234,56` (formato francês)
- Notação científica (ex: `1.23E+05`)

**Exemplo de Uso:**

```abap
DATA: lv_value_ext TYPE string VALUE '1.234,56',
      lv_value_sap TYPE string.

" Converter número decimal externo
lv_value_sap = zcl_util_excel=>format_number_dec( lv_value_ext ).
" Resultado: '1234.56' (formato SAP com ponto decimal)

" Outros formatos
lv_value_sap = zcl_util_excel=>format_number_dec( '1,234.56' ).
" Resultado: '1234.56'

" Notação científica
lv_value_sap = zcl_util_excel=>format_number_dec( '1.23E+05' ).
" Resultado: '123000'
```

---

## 🔒 Métodos Protegidos

Estes métodos são para uso interno da classe:

| Método | Descrição |
|--------|-----------|
| `handle_frontend` | Controla a pop-up de seleção de ficheiro local |
| `set_excel_data` | Define os dados em formato Excel |
| `build_excel_xdata` | Constrói o ficheiro Excel (XSTRING) a partir de tabela interna |
| `export_local` | Exporta dados para ficheiro local (pop-up) |
| `convert_date_numb_to_internal` | Converte número Excel para data SAP |
| `conv_date_user_format` | Converte data para formato do utilizador |
| `extract_number_format` | Extrai formato de número decimal (delimitadores) |
| `check_pattern` | Valida padrões usando expressões regulares |

---

## 🧾 Código completo da classe (colapsável)

<details>
<summary>Mostrar/ocultar código fonte completo da classe `ZCL_UTIL_EXCEL`</summary>

```abap
*&---------------------------------------------------------------------*
*& Classe: ZCL_UTIL_EXCEL
*& Descrição: Classe utilitária para manipulação de ficheiros Excel
*& Autor: [Seu Nome]
*& Data: 06.11.2025
*&---------------------------------------------------------------------*
CLASS zcl_util_excel DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="pt">Atributos ficheiro EXCEL</p>
      BEGIN OF ts_excel_file,
        name  TYPE filename,
        size  TYPE i,
        xdata TYPE solix_tab,
      END OF ts_excel_file.

    TYPES:
      "! <p class="shorttext synchronized" lang="pt">Delimitadores numeros decimais</p>
      BEGIN OF ts_number_delimiters,
        thousand TYPE char01,
        decimal  TYPE char01,
      END OF ts_number_delimiters.


    CONSTANTS:
      "! <p class="shorttext synchronized" lang="pt">Expressão regular numero inteiro</p>
      co_int_pattern      TYPE string VALUE `^\d+$` ##NO_TEXT,
      "! <p class="shorttext synchronized" lang="pt">Expressão regular numero decimal</p>
      co_decimals_pattern TYPE string VALUE `^[+-]?(\d+\D\d+)$` ##NO_TEXT,
      "! <p class="shorttext synchronized" lang="pt">Expressão regular numero decimal com delimitador mil.</p>
      co_thd_dec_pattern  TYPE string VALUE `^[+-]?((\d+(\T?\d+)*)(\D\d+)?)$` ##NO_TEXT.


    "! <p class="shorttext synchronized" lang="pt">Upload de ficheiro local (EXCEL)</p>
    METHODS upload_local_excel IMPORTING iv_filename       TYPE string OPTIONAL
                                         iv_open_dialog    TYPE abap_bool DEFAULT space
                                         iv_worksheet_name TYPE string OPTIONAL
                               EXPORTING er_excel_data     TYPE REF TO data
                               RAISING   zcx_u_excel_file
                                         cx_fdt_excel_core.

    "! <p class="shorttext synchronized" lang="pt">Download de tabela a ficheiro local (EXCEL)</p>
    CLASS-METHODS download_itab_local_excel IMPORTING iv_filename TYPE string
                                                      it_table    TYPE REF TO data
                                            RAISING   zcx_u_excel_file.

    "! <p class="shorttext synchronized" lang="pt">Converter formato data externa a interno SAP</p>
    CLASS-METHODS format_date IMPORTING iv_date_ext    TYPE clike
                              RETURNING VALUE(rv_date) TYPE datum.

    "! <p class="shorttext synchronized" lang="pt">Converter formato decimal externo a interno SAP</p>
    CLASS-METHODS format_number_dec IMPORTING iv_value_ext         TYPE clike
                                    RETURNING VALUE(rv_value_user) TYPE string.


  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="pt">Atributos do ficheiro EXCEL</p>
    DATA ms_excel_file TYPE ts_excel_file .

    "! <p class="shorttext synchronized" lang="pt">Controlo da POP-UP para seleção de ficheiro local</p>
    METHODS handle_frontend IMPORTING iv_filename    TYPE string
                                      iv_open_dialog TYPE abap_bool
                            RAISING   zcx_u_excel_file.

    "! <p class="shorttext synchronized" lang="pt">Definição dos dados em formato de EXCEL</p>
    METHODS set_excel_data IMPORTING iv_worksheet_name TYPE string
                           EXPORTING er_excel_data     TYPE REF TO data
                           RAISING   zcx_u_excel_file
                                     cx_fdt_excel_core.

    "! <p class="shorttext synchronized" lang="pt">Construção do EXCEL (download)</p>
    CLASS-METHODS build_excel_xdata IMPORTING it_table TYPE REF TO data
                                    EXPORTING ev_xdata TYPE xstring
                                    RAISING   zcx_u_excel_file.

    "! <p class="shorttext synchronized" lang="pt">Exportar tabela download (POP-UP)</p>
    CLASS-METHODS export_local IMPORTING iv_filename TYPE string
                                         iv_xdata    TYPE xstring
                               RAISING   zcx_u_excel_file.

    "! <p class="shorttext synchronized" lang="pt">Fórmula de conversão de numero a data</p>
    CLASS-METHODS convert_date_numb_to_internal IMPORTING iv_date_ext              TYPE clike
                                                EXPORTING ev_date                  TYPE datum
                                                RETURNING VALUE(rv_date_is_number) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="pt">Conversão data formato externo ao formato de user</p>
    CLASS-METHODS conv_date_user_format IMPORTING iv_date_ext         TYPE clike
                                        RETURNING VALUE(rv_date_user) TYPE string.

    "! <p class="shorttext synchronized" lang="pt">Extrai o formato decimal externo</p>
    CLASS-METHODS extract_number_format IMPORTING iv_value_ext            TYPE clike
                                        RETURNING VALUE(rs_number_format) TYPE ts_number_delimiters.

    "! <p class="shorttext synchronized" lang="pt">Validação de padrões de formatos de um decimal</p>
    CLASS-METHODS check_pattern IMPORTING iv_value_ext TYPE clike
                                          iv_pattern   TYPE string
                                RETURNING VALUE(rv_ok) TYPE abap_bool .


  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_util_excel IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>BUILD_EXCEL_XDATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TABLE                       TYPE REF TO DATA
* | [<---] EV_XDATA                       TYPE        XSTRING
* | [!CX!] ZCX_U_EXCEL_FILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_excel_xdata.

    CLEAR ev_xdata.

    GET REFERENCE OF it_table->* INTO DATA(lr_excel_struct).
    DATA(lo_table_desc) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( lr_excel_struct ) ).
    DATA(lo_row_desc) = CAST cl_abap_structdescr( lo_table_desc->get_table_line_type( ) ).
    DATA(lt_fields) = lo_row_desc->get_ddic_field_list( p_langu = sy-langu ).

    TRY.
        DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel( r_data = lr_excel_struct ).

        DATA(lo_config) = lo_tool_xls->configuration( ).
        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) .
          lo_config->add_column( header_text = CONV string( <ls_field>-scrtext_l )
                                 field_name = CONV string( <ls_field>-fieldname )
                                 display_type = if_salv_bs_model_column=>uie_text_view ).
        ENDLOOP.

        lo_tool_xls->read_result( IMPORTING content = ev_xdata ).

      CATCH cx_salv_ill_export_format_path cx_salv_export_error cx_salv_not_index_table INTO DATA(lx).
        DATA(lv_msg) = lx->get_text( ). " only for debug
        RAISE EXCEPTION TYPE zcx_u_excel_file
          EXPORTING
            textid = zcx_u_excel_file=>cx_create_excel_itab.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>CHECK_PATTERN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE_EXT                   TYPE        CLIKE
* | [--->] IV_PATTERN                     TYPE        STRING
* | [<-()] RV_OK                          TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_pattern.

    rv_ok = abap_false.
    TRY.
        IF cl_abap_matcher=>create_pcre( pattern = iv_pattern text = iv_value_ext )->match( ) EQ abap_true.
          rv_ok = abap_true.
        ENDIF.

      CATCH cx_sy_regex cx_sy_matcher INTO DATA(lx).
        DATA(lv_msg) = lx->get_text( ). " only for debug
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>CONVERT_DATE_NUMB_TO_INTERNAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE_EXT                    TYPE        CLIKE
* | [<---] EV_DATE                        TYPE        DATUM
* | [<-()] RV_DATE_IS_NUMBER              TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_date_numb_to_internal.

    CLEAR: ev_date.
    rv_date_is_number = abap_false.

    TRY.
        IF cl_abap_matcher=>create_pcre( pattern = co_int_pattern text = iv_date_ext )->match( ) EQ abap_true.

          ev_date = '19000101'. " initial date in excel
*---------------------------------------------------------------------
* subtract 2 because 01.01.19000 counts as a day and it count the
* 29.02.1900 that does not exist
          ev_date = ( ev_date + ( iv_date_ext DIV 1 ) ) - 2.
*---------------------------------------------------------------------
          rv_date_is_number = abap_true.
        ENDIF.

      CATCH cx_sy_regex cx_sy_matcher INTO DATA(lx).
        DATA(lv_msg) = lx->get_text( ). " only for debug
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>CONV_DATE_USER_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE_EXT                    TYPE        CLIKE
* | [<-()] RV_DATE_USER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_date_user_format.

    DATA: lv_datfm TYPE usr01-datfm.

    CLEAR rv_date_user.
    CHECK iv_date_ext IS NOT INITIAL.

    DATA(lv_date_ext) = iv_date_ext.

    " user date format
    SELECT SINGLE datfm FROM usr01
      INTO lv_datfm WHERE bname = sy-uname.

    DATA(lv_date_separator) = SWITCH #( lv_datfm WHEN 1 OR 4 THEN |.| \
                                                 WHEN 2 OR 5 THEN |/| \
                                                 WHEN 3 OR 6 THEN |-| ).

    IF find_any_not_of( val = lv_date_ext sub = '0123456789' occ = 1 ) = 4 AND ( lv_datfm = 1 OR lv_datfm = 2 OR lv_datfm = 3 ).

      lv_date_ext = lv_date_ext+8(2) && lv_date_separator && lv_date_ext+5(2)  && lv_date_separator && lv_date_ext(4).

    ENDIF.

    rv_date_user = SWITCH #( lv_datfm WHEN 1 OR 2 OR 3 " DD.MM.YYYY or MM/DD/YYYY or MM-DD-YYYY
                                      THEN |{ lv_date_ext(2) }{ lv_date_separator }{ lv_date_ext+3(2) }{ lv_date_separator }{ lv_date_ext+6(4) }|
                                      WHEN 4 OR 5 OR 6 " YYYY.MM.DD or YYYY/MM/DD or YYYY-MM-DD
                                      THEN |{ lv_date_ext(4) }{ lv_date_separator }{ lv_date_ext+5(2) }{ lv_date_separator }{ lv_date_ext+8(2) }| ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTIL_EXCEL=>DOWNLOAD_ITAB_LOCAL_EXCEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        STRING
* | [--->] IT_TABLE                       TYPE REF TO DATA
* | [!CX!] ZCX_U_EXCEL_FILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_itab_local_excel.

    TRY.
        build_excel_xdata( EXPORTING it_table = it_table IMPORTING ev_xdata = DATA(lv_xdata) ).
        export_local( iv_filename = iv_filename iv_xdata = lv_xdata ).

      CATCH zcx_u_excel_file INTO DATA(lx).
        RAISE EXCEPTION lx.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>EXPORT_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        STRING
* | [--->] IV_XDATA                       TYPE        XSTRING
* | [!CX!] ZCX_U_EXCEL_FILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD export_local.

    CHECK iv_xdata IS NOT INITIAL.

    CALL FUNCTION 'XML_EXPORT_DIALOG'
      EXPORTING
        i_xml                      = iv_xdata
        i_default_extension        = 'XLSX'
        i_initial_directory        = ''
        i_default_file_name        = iv_filename
        i_mask                     = 'Excel (*.XLSX)|*.XLSX' ##NO_TEXT
        i_application              = ''
      EXCEPTIONS
        application_not_executable = 1
        others                     = 2.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_u_excel_file MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>EXTRACT_NUMBER_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE_EXT                   TYPE        CLIKE
* | [<-()] RS_NUMBER_FORMAT               TYPE        TS_NUMBER_DELIMITERS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD extract_number_format.
    CLEAR rs_number_format.

    " format only with decimals 1.23
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = co_decimals_pattern sub = 'D' with = '.' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = || decimal = |.| ).
      RETURN.
    ENDIF.

    " format only with decimals 1,23
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = co_decimals_pattern sub = 'D' with = ',' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = || decimal = |,| ).
      RETURN.
    ENDIF.

    " format 1.234.567,89
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = replace( val = co_thd_dec_pattern sub = 'D' with = ',' ) sub = 'T' with = '.' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = |.| decimal = |,| ).
      RETURN.
    ENDIF.

    " format 1,234,567.89
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = replace( val = co_thd_dec_pattern sub = 'D' with = '.' ) sub = 'T' with = ',' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = |,| decimal = |.| ).
      RETURN.
    ENDIF.

    " format 1 234 567,89
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = replace( val = co_thd_dec_pattern sub = 'D' with = ',' ) sub = 'T' with = ' ' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = | | decimal = |,| ).
      RETURN.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTIL_EXCEL=>FORMAT_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE_EXT                    TYPE        CLIKE
* | [<-()] RV_DATE                        TYPE        DATUM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_date.
    CLEAR rv_date.

    CHECK iv_date_ext IS NOT INITIAL.

    IF convert_date_numb_to_internal( EXPORTING iv_date_ext = iv_date_ext IMPORTING ev_date = rv_date ) EQ abap_true.
      RETURN.
    ENDIF.

    DATA(lv_date_ext) = conv_date_user_format( iv_date_ext ).
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_date_ext
      IMPORTING
        date_internal            = rv_date
      EXCEPTIONS
        date_external_is_invalid = 1
        others                   = 2.

    IF sy-subrc <> 0 AND lv_date_ext IS NOT INITIAL.
      rv_date = lv_date_ext+6(4) && lv_date_ext+3(2) && lv_date_ext(2).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTIL_EXCEL=>FORMAT_NUMBER_DEC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE_EXT                   TYPE        CLIKE
* | [<-()] RV_VALUE_USER                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_number_dec.

    CLEAR rv_value_user.

    IF iv_value_ext CS 'E'.
      DATA lv_char_field TYPE cha_class_view-sollwert.

      CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
        EXPORTING
          i_number_of_digits = 10
          i_fltp_value       = CONV cha_class_data-sollwert( iv_value_ext )
        IMPORTING
          e_char_field       = lv_char_field.

      rv_value_user = lv_char_field.
      CONDENSE rv_value_user NO-GAPS.
    ENDIF.

    DATA(ls_format) = extract_number_format( COND #( WHEN rv_value_user IS NOT INITIAL THEN rv_value_user ELSE iv_value_ext ) ).
    rv_value_user = COND #( WHEN rv_value_user IS NOT INITIAL THEN rv_value_user ELSE iv_value_ext ).

    TRANSLATE: rv_value_user USING |{ ls_format-thousand } |,
               rv_value_user USING |{ ls_format-decimal }.|.

    CONDENSE rv_value_user NO-GAPS.

  ENDMETHOD.
```

</details>


## 💡 Casos de Uso Práticos

### Caso 1: Importar Dados de Excel e Processar

```abap
DATA: lo_util  TYPE REF TO zcl_util_excel,
      lr_data  TYPE REF TO data,
      lv_lines TYPE i.

FIELD-SYMBOLS: <ft_excel> TYPE STANDARD TABLE,
               <fs_row>   TYPE any,
               <fs_field> TYPE any.

TRY.
    CREATE OBJECT lo_util.
    
    " Upload com dialog
    lo_util->upload_local_excel(
      EXPORTING
        iv_open_dialog = abap_true
      IMPORTING
        er_excel_data = lr_data
    ).
    
    ASSIGN lr_data->* TO <ft_excel>.
    
    DESCRIBE TABLE <ft_excel> LINES lv_lines.
    MESSAGE |{ lv_lines } linhas importadas com sucesso!| TYPE 'S'.
    
    " Processar cada linha
    LOOP AT <ft_excel> ASSIGNING <fs_row>.
      " Aceder a campos específicos
      ASSIGN COMPONENT 'CAMPO1' OF STRUCTURE <fs_row> TO <fs_field>.
      IF sy-subrc = 0.
        " Processar campo...
      ENDIF.
    ENDLOOP.
    
  CATCH zcx_u_excel_file cx_fdt_excel_core INTO DATA(lx).
    MESSAGE lx->get_text( ) TYPE 'E'.
ENDTRY.
```

### Caso 2: Exportar Relatório para Excel

```abap
TYPES: BEGIN OF ty_report,
         employee_id   TYPE pernr,
         employee_name TYPE emnam,
         department    TYPE orgeh,
         salary        TYPE decfloat16,
       END OF ty_report.

DATA: lt_report TYPE TABLE OF ty_report,
      lr_data   TYPE REF TO data.

" Preencher relatório
" ... código para popular lt_report ...

" Exportar para Excel
GET REFERENCE OF lt_report INTO lr_data.

TRY.
    zcl_util_excel=>download_itab_local_excel(
      iv_filename = |Relatorio_RH_{ sy-datum }.xlsx|
      it_table    = lr_data
    ).
    
  CATCH zcx_u_excel_file INTO DATA(lx).
    MESSAGE lx->get_text( ) TYPE 'E'.
ENDTRY.
```

### Caso 3: Conversão de Formatos em Loop

```abap
DATA: lt_data TYPE TABLE OF ty_custom_data.

FIELD-SYMBOLS: <fs_data> TYPE ty_custom_data.

LOOP AT lt_data ASSIGNING <fs_data>.
  " Converter data
  <fs_data>-datum = zcl_util_excel=>format_date( <fs_data>-date_string ).
  
  " Converter valor decimal
  <fs_data>-amount = zcl_util_excel=>format_number_dec( <fs_data>-amount_string ).
ENDLOOP.
```

---

## ⚙️ Dependências SAP Standard

Esta classe utiliza os seguintes componentes SAP:

- `CL_FDT_XL_SPREADSHEET` - Manipulação de Excel
- `CL_SALV_EXPORT_TOOL_ATS_XLS` - Exportação SALV para Excel
- `CL_GUI_FRONTEND_SERVICES` - Serviços de frontend (upload/download)
- `CL_BCS_CONVERT` - Conversão de formatos
- `CL_ABAP_MATCHER` - Expressões regulares

---

## 📝 Notas Importantes

1. **Formato de Ficheiro:** Esta classe trabalha exclusivamente com ficheiros `.xlsx` (Excel 2007+)

2. **Conversão de Datas:** A classe suporta tanto datas em formato de texto quanto números do Excel (onde 1 = 01.01.1900)

3. **Formato de Números:** Detecta automaticamente o formato decimal baseado nos delimitadores utilizados

4. **Performance:** Para ficheiros grandes (>10.000 linhas), considere processar em lotes

5. **Worksheets:** Se não especificar o nome da worksheet, a primeira será utilizada por padrão

6. **Cabeçalhos:** Os cabeçalhos das colunas no Excel devem corresponder aos nomes dos campos da estrutura/tabela

7. **Autorização:** O utilizador precisa de autorização para acesso ao frontend (transação SAP GUI)

---

## 🐛 Tratamento de Erros

Todas as exceções devem ser capturadas e tratadas adequadamente:

```abap
TRY.
    " Código...
    
  CATCH zcx_u_excel_file INTO DATA(lx_excel).
    " Tratar erros específicos da classe
    MESSAGE lx_excel->get_text( ) TYPE 'E'.
    
  CATCH cx_fdt_excel_core INTO DATA(lx_core).
    " Tratar erros do componente Excel SAP
    MESSAGE lx_core->get_text( ) TYPE 'E'.
    
  CATCH cx_root INTO DATA(lx_root).
    " Tratar erros genéricos
    MESSAGE lx_root->get_text( ) TYPE 'E'.
ENDTRY.
```

---

## 📚 Tags

`#ABAP` `#Excel` `#Upload` `#Download` `#Utilities` `#FileHandling` `#DataConversion` `#XLSX` `#Frontend` `#Exception`

---

## 🔄 Versão

**Documentação criada em:** 06.11.2025  
**Classe:** ZCL_UTIL_EXCEL  
**Tipo:** Utility Class  
**Categoria:** File Handling / Excel Processing

---

## ✅ Checklist de Implementação

- [ ] Criar classe de exceção `ZCX_U_EXCEL_FILE`
- [ ] Criar mensagens na classe de mensagem (ex: Z_MSG_CLASS)
- [ ] Testar upload com diferentes formatos de Excel
- [ ] Testar download com diferentes tipos de tabelas
- [ ] Validar conversões de data
- [ ] Validar conversões de números decimais
- [ ] Documentar casos de uso específicos do projeto
- [ ] Criar programa de teste (report) para validação

---
---

## 💻 Código Completo da Classe

<details>
<summary><strong>Clique para expandir o código completo de ZCL_UTIL_EXCEL</strong></summary>

```abap
CLASS zcl_util_excel DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="pt">Atributos ficheiro EXCEL</p>
      BEGIN OF ts_excel_file,
        name  TYPE filename,
        size  TYPE i,
        xdata TYPE solix_tab,
      END OF ts_excel_file.

    TYPES:
      "! <p class="shorttext synchronized" lang="pt">Delimitadores numeros decimais</p>
      BEGIN OF ts_number_delimiters,
        thousand TYPE char01,
        decimal  TYPE char01,
      END OF ts_number_delimiters.


    CONSTANTS:
      "! <p class="shorttext synchronized" lang="pt">Expressão regular numero inteiro</p>
      co_int_pattern      TYPE string VALUE `^\d+$` ##NO_TEXT,
      "! <p class="shorttext synchronized" lang="pt">Expressão regular numero decimal</p>
      co_decimals_pattern TYPE string VALUE `^[+-]?(\d+\D\d+)$` ##NO_TEXT,
      "! <p class="shorttext synchronized" lang="pt">Expressão regular numero decimal com delimitador mil.</p>
      co_thd_dec_pattern  TYPE string VALUE `^[+-]?((\d+(\T?\d+)*)(\D\d+)?)$` ##NO_TEXT.


    "! <p class="shorttext synchronized" lang="pt">Upload de ficheiro local (EXCEL)</p>
    METHODS upload_local_excel IMPORTING iv_filename       TYPE string OPTIONAL
                                         iv_open_dialog    TYPE abap_bool DEFAULT space
                                         iv_worksheet_name TYPE string OPTIONAL
                               EXPORTING er_excel_data     TYPE REF TO data
                               RAISING   zcx_u_excel_file
                                         cx_fdt_excel_core.

    "! <p class="shorttext synchronized" lang="pt">Download de tabela a ficheiro local (EXCEL)</p>
    CLASS-METHODS download_itab_local_excel IMPORTING iv_filename TYPE string
                                                      it_table    TYPE REF TO data
                                            RAISING   zcx_u_excel_file.

    "! <p class="shorttext synchronized" lang="pt">Converter formato data externa a interno SAP</p>
    CLASS-METHODS format_date IMPORTING iv_date_ext    TYPE clike
                              RETURNING VALUE(rv_date) TYPE datum.

    "! <p class="shorttext synchronized" lang="pt">Converter formato decimal externo a interno SAP</p>
    CLASS-METHODS format_number_dec IMPORTING iv_value_ext         TYPE clike
                                    RETURNING VALUE(rv_value_user) TYPE string.


  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="pt">Atributos do ficheiro EXCEL</p>
    DATA ms_excel_file TYPE ts_excel_file .

    "! <p class="shorttext synchronized" lang="pt">Controlo da POP-UP para seleção de ficheiro local</p>
    METHODS handle_frontend IMPORTING iv_filename    TYPE string
                                      iv_open_dialog TYPE abap_bool
                            RAISING   zcx_u_excel_file.

    "! <p class="shorttext synchronized" lang="pt">Definição dos dados em formato de EXCEL</p>
    METHODS set_excel_data IMPORTING iv_worksheet_name TYPE string
                           EXPORTING er_excel_data     TYPE REF TO data
                           RAISING   zcx_u_excel_file
                                     cx_fdt_excel_core.

    "! <p class="shorttext synchronized" lang="pt">Construção do EXCEL (download)</p>
    CLASS-METHODS build_excel_xdata IMPORTING it_table TYPE REF TO data
                                    EXPORTING ev_xdata TYPE xstring
                                    RAISING   zcx_u_excel_file.

    "! <p class="shorttext synchronized" lang="pt">Exportar tabela download (POP-UP)</p>
    CLASS-METHODS export_local IMPORTING iv_filename TYPE string
                                         iv_xdata    TYPE xstring
                               RAISING   zcx_u_excel_file.

    "! <p class="shorttext synchronized" lang="pt">Fórmula de conversão de numero a data</p>
    CLASS-METHODS convert_date_numb_to_internal IMPORTING iv_date_ext              TYPE clike
                                                EXPORTING ev_date                  TYPE datum
                                                RETURNING VALUE(rv_date_is_number) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="pt">Conversão data formato externo ao formato de user</p>
    CLASS-METHODS conv_date_user_format IMPORTING iv_date_ext         TYPE clike
                                        RETURNING VALUE(rv_date_user) TYPE string.

    "! <p class="shorttext synchronized" lang="pt">Extrai o formato decimal externo</p>
    CLASS-METHODS extract_number_format IMPORTING iv_value_ext            TYPE clike
                                        RETURNING VALUE(rs_number_format) TYPE ts_number_delimiters.

    "! <p class="shorttext synchronized" lang="pt">Validação de padrões de formatos de um decimal</p>
    CLASS-METHODS check_pattern IMPORTING iv_value_ext TYPE clike
                                          iv_pattern   TYPE string
                                RETURNING VALUE(rv_ok) TYPE abap_bool .


  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_util_excel IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>BUILD_EXCEL_XDATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TABLE                       TYPE REF TO DATA
* | [<---] EV_XDATA                       TYPE        XSTRING
* | [!CX!] ZCX_U_EXCEL_FILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_excel_xdata.

    CLEAR ev_xdata.

    GET REFERENCE OF it_table->* INTO DATA(lr_excel_struct).
    DATA(lo_table_desc) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( lr_excel_struct ) ).
    DATA(lo_row_desc) = CAST cl_abap_structdescr( lo_table_desc->get_table_line_type( ) ).
    DATA(lt_fields) = lo_row_desc->get_ddic_field_list( p_langu = sy-langu ).

    TRY.
        DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel( r_data = lr_excel_struct ).

        DATA(lo_config) = lo_tool_xls->configuration( ).
        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) .
          lo_config->add_column( header_text = CONV string( <ls_field>-scrtext_l )
                                 field_name = CONV string( <ls_field>-fieldname )
                                 display_type = if_salv_bs_model_column=>uie_text_view ).
        ENDLOOP.

        lo_tool_xls->read_result( IMPORTING content = ev_xdata ).

      CATCH cx_salv_ill_export_format_path cx_salv_export_error cx_salv_not_index_table INTO DATA(lx).
        DATA(lv_msg) = lx->get_text( ). " only for debug
        RAISE EXCEPTION TYPE zcx_u_excel_file
          EXPORTING
            textid = zcx_u_excel_file=>cx_create_excel_itab.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>CHECK_PATTERN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE_EXT                   TYPE        CLIKE
* | [--->] IV_PATTERN                     TYPE        STRING
* | [<-()] RV_OK                          TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_pattern.

    rv_ok = abap_false.
    TRY.
        IF cl_abap_matcher=>create_pcre( pattern = iv_pattern text = iv_value_ext )->match( ) EQ abap_true.
          rv_ok = abap_true.
        ENDIF.

      CATCH cx_sy_regex cx_sy_matcher INTO DATA(lx).
        DATA(lv_msg) = lx->get_text( ). " only for debug
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>CONVERT_DATE_NUMB_TO_INTERNAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE_EXT                    TYPE        CLIKE
* | [<---] EV_DATE                        TYPE        DATUM
* | [<-()] RV_DATE_IS_NUMBER              TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_date_numb_to_internal.

    CLEAR: ev_date.
    rv_date_is_number = abap_false.

    TRY.
        IF cl_abap_matcher=>create_pcre( pattern = co_int_pattern text = iv_date_ext )->match( ) EQ abap_true.

          ev_date = '19000101'. " initial date in excel
*---------------------------------------------------------------------
* subtract 2 because 01.01.19000 counts as a day and it count the
* 29.02.1900 that does not exist
          ev_date = ( ev_date + ( iv_date_ext DIV 1 ) ) - 2.
*---------------------------------------------------------------------
          rv_date_is_number = abap_true.
        ENDIF.

      CATCH cx_sy_regex cx_sy_matcher INTO DATA(lx).
        DATA(lv_msg) = lx->get_text( ). " only for debug
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>CONV_DATE_USER_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE_EXT                    TYPE        CLIKE
* | [<-()] RV_DATE_USER                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_date_user_format.

    DATA: lv_datfm TYPE usr01-datfm.

    CLEAR rv_date_user.
    CHECK iv_date_ext IS NOT INITIAL.

    DATA(lv_date_ext) = iv_date_ext.

    " user date format
    SELECT SINGLE datfm FROM usr01
      INTO lv_datfm WHERE bname = sy-uname.

    DATA(lv_date_separator) = SWITCH #( lv_datfm WHEN 1 OR 4 THEN |.|
                                                 WHEN 2 OR 5 THEN |/|
                                                 WHEN 3 OR 6 THEN |-| ).

    IF find_any_not_of( val = lv_date_ext sub = '0123456789' occ = 1 ) = 4 AND ( lv_datfm = 1 OR lv_datfm = 2 OR lv_datfm = 3 ).

      lv_date_ext = lv_date_ext+8(2) && lv_date_separator && lv_date_ext+5(2)  && lv_date_separator && lv_date_ext(4).

    ENDIF.

    rv_date_user = SWITCH #( lv_datfm WHEN 1 OR 2 OR 3 " DD.MM.YYYY or MM/DD/YYYY or MM-DD-YYYY
                                      THEN |{ lv_date_ext(2) }{ lv_date_separator }{ lv_date_ext+3(2) }{ lv_date_separator }{ lv_date_ext+6(4) }|
                                      WHEN 4 OR 5 OR 6 " YYYY.MM.DD or YYYY/MM/DD or YYYY-MM-DD
                                      THEN |{ lv_date_ext(4) }{ lv_date_separator }{ lv_date_ext+5(2) }{ lv_date_separator }{ lv_date_ext+8(2) }| ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTIL_EXCEL=>DOWNLOAD_ITAB_LOCAL_EXCEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        STRING
* | [--->] IT_TABLE                       TYPE REF TO DATA
* | [!CX!] ZCX_U_EXCEL_FILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_itab_local_excel.

    TRY.
        build_excel_xdata( EXPORTING it_table = it_table IMPORTING ev_xdata = DATA(lv_xdata) ).
        export_local( iv_filename = iv_filename iv_xdata = lv_xdata ).

      CATCH zcx_u_excel_file INTO DATA(lx).
        RAISE EXCEPTION lx.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>EXPORT_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        STRING
* | [--->] IV_XDATA                       TYPE        XSTRING
* | [!CX!] ZCX_U_EXCEL_FILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD export_local.

    CHECK iv_xdata IS NOT INITIAL.

    CALL FUNCTION 'XML_EXPORT_DIALOG'
      EXPORTING
        i_xml                      = iv_xdata
        i_default_extension        = 'XLSX'
        i_initial_directory        = ''
        i_default_file_name        = iv_filename
        i_mask                     = 'Excel (*.XLSX)|*.XLSX' ##NO_TEXT
        i_application              = ''
      EXCEPTIONS
        application_not_executable = 1
        others                     = 2.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_u_excel_file MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Protected Method ZCL_UTIL_EXCEL=>EXTRACT_NUMBER_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE_EXT                   TYPE        CLIKE
* | [<-()] RS_NUMBER_FORMAT               TYPE        TS_NUMBER_DELIMITERS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD extract_number_format.
    CLEAR rs_number_format.

    " format only with decimals 1.23
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = co_decimals_pattern sub = 'D' with = '.' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = || decimal = |.| ).
      RETURN.
    ENDIF.

    " format only with decimals 1,23
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = co_decimals_pattern sub = 'D' with = ',' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = || decimal = |,| ).
      RETURN.
    ENDIF.

    " format 1.234.567,89
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = replace( val = co_thd_dec_pattern sub = 'D' with = ',' ) sub = 'T' with = '.' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = |.| decimal = |,| ).
      RETURN.
    ENDIF.

    " format 1,234,567.89
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = replace( val = co_thd_dec_pattern sub = 'D' with = '.' ) sub = 'T' with = ',' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = |,| decimal = |.| ).
      RETURN.
    ENDIF.

    " format 1 234 567,89
    IF check_pattern( iv_value_ext = iv_value_ext iv_pattern = replace( val = replace( val = co_thd_dec_pattern sub = 'D' with = ',' ) sub = 'T' with = ' ' ) ) EQ abap_true.
      rs_number_format = VALUE #( thousand = | | decimal = |,| ).
      RETURN.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTIL_EXCEL=>FORMAT_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE_EXT                    TYPE        CLIKE
* | [<-()] RV_DATE                        TYPE        DATUM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_date.
    CLEAR rv_date.

    CHECK iv_date_ext IS NOT INITIAL.

    IF convert_date_numb_to_internal( EXPORTING iv_date_ext = iv_date_ext IMPORTING ev_date = rv_date ) EQ abap_true.
      RETURN.
    ENDIF.

    DATA(lv_date_ext) = conv_date_user_format( iv_date_ext ).
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_date_ext
      IMPORTING
        date_internal            = rv_date
      EXCEPTIONS
        date_external_is_invalid = 1
        others                   = 2.

    IF sy-subrc <> 0 AND lv_date_ext IS NOT INITIAL.
      rv_date = lv_date_ext+6(4) && lv_date_ext+3(2) && lv_date_ext(2).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTIL_EXCEL=>FORMAT_NUMBER_DEC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE_EXT                   TYPE        CLIKE
* | [<-()] RV_VALUE_USER                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD format_number_dec.

    CLEAR rv_value_user.

    IF iv_value_ext CS 'E'.
      DATA lv_char_field TYPE cha_class_view-sollwert.

      CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
        EXPORTING
          i_number_of_digits = 10
          i_fltp_value       = CONV cha_class_data-sollwert( iv_value_ext )
        IMPORTING
          e_char_field       = lv_char_field.

      rv_value_user = lv_char_field.
      CONDENSE rv_value_user NO-GAPS.
    ENDIF.

    DATA(ls_format) = extract_number_format( COND #( WHEN rv_value_user IS NOT INITIAL THEN rv_value_user ELSE iv_value_ext ) ).
    rv_value_user = COND #( WHEN rv_value_user IS NOT INITIAL THEN rv_value_user ELSE iv_value_ext ).

    TRANSLATE: rv_value_user USING |{ ls_format-thousand } |,
               rv_value_user USING |{ ls_format-decimal }.|.

    CONDENSE rv_value_user NO-GAPS.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_UTIL_EXCEL->HANDLE_FRONTEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        STRING
* | [--->] IV_OPEN_DIALOG                 TYPE        ABAP_BOOL
* | [!CX!] ZCX_U_EXCEL_FILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_frontend.

    DATA: lv_rc        TYPE i,
          lv_file_size TYPE i.

    DATA: lt_file TYPE filetable.

    CLEAR ms_excel_file.
    ms_excel_file-name = iv_filename.

    IF iv_open_dialog EQ abap_true.
      CLEAR ms_excel_file-name.
      cl_gui_frontend_services=>file_open_dialog( EXPORTING  file_filter             = |xls (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_all }|
                                                  CHANGING   file_table              = lt_file
                                                             rc                      = lv_rc
                                                  EXCEPTIONS file_open_dialog_failed = 1
                                                             cntl_error              = 2
                                                             error_no_gui            = 3
                                                             not_supported_by_gui    = 4
                                                             others                  = 5 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_u_excel_file MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF lt_file IS INITIAL.
        RAISE EXCEPTION TYPE zcx_u_excel_file
          EXPORTING
            textid = zcx_u_excel_file=>cx_no_file_selected.
      ENDIF.

      ms_excel_file-name = lt_file[ 1 ]-filename.
    ENDIF.

    cl_gui_frontend_services=>gui_upload( EXPORTING filename   = CONV #( ms_excel_file-name )
                                                    filetype   = 'BIN'
                                          IMPORTING filelength = ms_excel_file-size
                                          CHANGING  data_tab   = ms_excel_file-xdata
                                          EXCEPTIONS file_open_error         = 1
                                                     file_read_error         = 2
                                                     no_batch                = 3
                                                     gui_refuse_filetransfer = 4
                                                     invalid_type            = 5
                                                     no_authority            = 6
                                                     unknown_error           = 7
                                                     bad_data_format         = 8
                                                     header_not_allowed      = 9
                                                     separator_not_allowed   = 10
                                                     header_too_long         = 11
                                                     unknown_dp_error        = 12
                                                     access_denied           = 13
                                                     dp_out_of_memory        = 14
                                                     disk_full               = 15
                                                     dp_timeout              = 16
                                                     not_supported_by_gui    = 17
                                                     error_no_gui            = 18
                                                     others                  = 19 ).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_u_excel_file MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_UTIL_EXCEL->SET_EXCEL_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WORKSHEET_NAME              TYPE        STRING
* | [<---] ER_EXCEL_DATA                  TYPE REF TO DATA
* | [!CX!] ZCX_U_EXCEL_FILE
* | [!CX!] CX_FDT_EXCEL_CORE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_excel_data.

    CLEAR er_excel_data.
    TRY.
        DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet( document_name = CONV #( ms_excel_file-name )
                                                    xdocument     = cl_bcs_convert=>solix_to_xstring( it_solix = ms_excel_file-xdata ) ).

        lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_worksheet_names) ).

        DATA(lv_worksheet) = COND i( WHEN iv_worksheet_name IS NOT INITIAL THEN line_index( lt_worksheet_names[ table_line = iv_worksheet_name ] )
                                                                           ELSE 1 ).

        IF lv_worksheet EQ 0.
          RAISE EXCEPTION TYPE zcx_u_excel_file
            EXPORTING
              textid = zcx_u_excel_file=>cx_no_worksheet
              msgv1  = CONV #( iv_worksheet_name ).
        ENDIF.

        er_excel_data = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet_names[ lv_worksheet ] ).


      CATCH cx_fdt_excel_core INTO DATA(lx).
        RAISE EXCEPTION lx.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_UTIL_EXCEL->UPLOAD_LOCAL_EXCEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        STRING(optional)
* | [--->] IV_OPEN_DIALOG                 TYPE        ABAP_BOOL (default =SPACE)
* | [--->] IV_WORKSHEET_NAME              TYPE        STRING(optional)
* | [<---] ER_EXCEL_DATA                  TYPE REF TO DATA
* | [!CX!] ZCX_U_EXCEL_FILE
* | [!CX!] CX_FDT_EXCEL_CORE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_local_excel.

    CLEAR er_excel_data.

    handle_frontend( iv_filename    = iv_filename
                     iv_open_dialog = iv_open_dialog ).

    set_excel_data( EXPORTING iv_worksheet_name = iv_worksheet_name
                    IMPORTING er_excel_data     = er_excel_data ).

  ENDMETHOD.
ENDCLASS.
```

</details>

---

## 🔴 Código Completo da Classe de Exceção

<details>
<summary><strong>Clique para expandir o código completo de ZCX_U_EXCEL_FILE</strong></summary>

```abap
CLASS zcx_u_excel_file DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    CONSTANTS:
      BEGIN OF zcx_u_excel_file,
        msgid TYPE symsgid VALUE 'ZEXCEL',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_u_excel_file .
    CONSTANTS:
      BEGIN OF cx_create_excel_itab,
        msgid TYPE symsgid VALUE 'ZEXCEL',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cx_create_excel_itab .
    CONSTANTS:
      BEGIN OF cx_no_file_selected,
        msgid TYPE symsgid VALUE 'ZEXCEL',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cx_no_file_selected .
    CONSTANTS:
      BEGIN OF cx_no_worksheet,
        msgid TYPE symsgid VALUE 'ZEXCEL',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cx_no_worksheet .
    DATA msgv1 TYPE string .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_u_excel_file IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCX_U_EXCEL_FILE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] TEXTID                         LIKE        IF_T100_MESSAGE=>T100KEY(optional)
* | [--->] PREVIOUS                       LIKE        PREVIOUS(optional)
* | [--->] MSGV1                          TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->msgv1 = msgv1 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_u_excel_file .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

**Nota:** Após criar a classe, é necessário criar as mensagens na SE91:
- Classe de Mensagens: `ZEXCEL`
- Mensagem 001: "Erro ao criar ficheiro Excel a partir de tabela interna"
- Mensagem 002: "Nenhum ficheiro foi selecionado"
- Mensagem 003: "Worksheet '&1' não encontrado"

</details>

---

**Fim da Documentação**
