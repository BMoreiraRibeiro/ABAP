# 🧰 Utilitários e Funções Comuns

Snippets, utilitários e **funções auxiliares** úteis em desenvolvimento ABAP: datas, JSON, strings, conversões e muito mais.

---

## 📖 O que vais encontrar

- Manipulação de datas e horas
- Conversão JSON/XML
- Operações com strings
- Funções de conversão
- Geração de GUIDs
- Trabalhar com ficheiros
- Envio de emails
- PopUps e mensagens
- Utilitários de tabela

---

## 🎯 Categorias

### 📅 [Datas e Horas](datas.md)

```abap
" Data atual
DATA(lv_hoje) = sy-datum.

" Adicionar dias
DATA(lv_amanha) = lv_hoje + 1.
DATA(lv_proxima_semana) = lv_hoje + 7.

" Diferença entre datas
DATA lv_dias TYPE i.
CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
  EXPORTING
    begda = lv_data_inicio
    endda = lv_data_fim
  IMPORTING
    days  = lv_dias.

" Primeiro dia do mês
DATA(lv_primeiro_dia) = lv_hoje.
lv_primeiro_dia+6(2) = '01'.

" Último dia do mês
CALL FUNCTION 'LAST_DAY_OF_MONTHS'
  EXPORTING
    day_in  = lv_hoje
  IMPORTING
    last_day_of_month = DATA(lv_ultimo_dia).

" Formatar data
WRITE lv_hoje TO DATA(lv_data_formatada) DD/MM/YYYY.
```

---

### 📦 [JSON](json.md)

```abap
" Serializar (ABAP → JSON)
DATA: lt_dados TYPE TABLE OF sflight.
SELECT * FROM sflight INTO TABLE lt_dados UP TO 10 ROWS.

DATA(lv_json) = /ui2/cl_json=>serialize( 
  data = lt_dados 
  compress = abap_false 
  pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

" Deserializar (JSON → ABAP)
/ui2/cl_json=>deserialize(
  EXPORTING json = lv_json
  CHANGING  data = lt_dados ).

" JSON com formatação bonita
DATA(lv_json_pretty) = /ui2/cl_json=>serialize( 
  data = lt_dados 
  compress = abap_false
  pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
```

---

### 🔤 Strings

#### Concatenar
```abap
DATA(lv_nome_completo) = |{ lv_primeiro_nome } { lv_apelido }|.

" Ou com CONCATENATE
CONCATENATE lv_primeiro_nome lv_apelido 
  INTO lv_nome_completo SEPARATED BY space.
```

#### Dividir
```abap
SPLIT lv_email AT '@' INTO lv_user lv_domain.
```

#### Substituir
```abap
REPLACE ALL OCCURRENCES OF '.' IN lv_texto WITH ','.
```

#### Maiúsculas/Minúsculas
```abap
DATA(lv_maiusculas) = to_upper( lv_texto ).
DATA(lv_minusculas) = to_lower( lv_texto ).
```

#### Remover espaços
```abap
CONDENSE lv_texto NO-GAPS.  " Remove todos os espaços
CONDENSE lv_texto.           " Remove espaços extra
```

---

### 🔄 Conversões

#### String ↔ Número
```abap
" String para número
DATA(lv_numero) = CONV i( '123' ).

" Número para string
DATA(lv_texto) = |{ lv_numero }|.
DATA(lv_texto2) = CONV string( lv_numero ).
```

#### Data ↔ String
```abap
" Data para string
DATA lv_data_str TYPE string.
WRITE sy-datum TO lv_data_str DD/MM/YYYY.

" String para data
CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
  EXPORTING
    date_external = '31/12/2024'
    accept_initial_date = abap_true
  IMPORTING
    date_internal = DATA(lv_data).
```

---

### 🆔 GUID / UUID

```abap
TRY.
    DATA(lv_guid) = cl_system_uuid=>create_uuid_x16_static( ).
    DATA(lv_guid_c32) = cl_system_uuid=>convert_uuid_x16_static( lv_guid ).
    WRITE: / lv_guid_c32.
  CATCH cx_uuid_error.
    MESSAGE 'Erro ao gerar GUID' TYPE 'E'.
ENDTRY.
```

---

### ✉️ Envio de Email

```abap
DATA: lt_receivers TYPE TABLE OF somlreci1,
      lt_body      TYPE TABLE OF soli.

APPEND VALUE #( receiver = 'user@empresa.com' rec_type = 'U' ) TO lt_receivers.
APPEND 'Olá,' TO lt_body.
APPEND 'Este é um email de teste.' TO lt_body.

CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
  EXPORTING
    document_data = VALUE sodocchgi1(
      obj_name  = 'EMAIL'
      obj_descr = 'Assunto do Email' )
  TABLES
    object_content = lt_body
    receivers      = lt_receivers
  EXCEPTIONS
    OTHERS = 99.

IF sy-subrc = 0.
  COMMIT WORK.
  MESSAGE 'Email enviado com sucesso' TYPE 'S'.
ENDIF.
```

---

### 📁 Ficheiros

#### Ler ficheiro local
```abap
DATA: lt_dados TYPE TABLE OF string.

CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename = 'C:\temp\dados.txt'
  TABLES
    data_tab = lt_dados.
```

#### Gravar ficheiro local
```abap
DATA: lt_output TYPE TABLE OF string.
APPEND 'Linha 1' TO lt_output.
APPEND 'Linha 2' TO lt_output.

CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename = 'C:\temp\output.txt'
  TABLES
    data_tab = lt_output.
```

---

### 💬 PopUps e Mensagens

#### Popup de Confirmação
```abap
CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
    titlebar       = 'Confirmar'
    text_question  = 'Tem a certeza?'
    icon_button_1  = 'icon_okay'
    icon_button_2  = 'icon_cancel'
  IMPORTING
    answer         = DATA(lv_resposta).

IF lv_resposta = '1'.  " Sim
  WRITE: / 'Confirmado'.
ENDIF.
```

#### Popup com Input
```abap
DATA lv_valor TYPE string.

CALL FUNCTION 'POPUP_GET_VALUES'
  EXPORTING
    popup_title = 'Introduzir Valor'
  TABLES
    fields      = VALUE sval_tab(
      ( tabname = 'SCARR' fieldname = 'CARRID' value = lv_valor ) ).

WRITE: / 'Valor:', lv_valor.
```

#### Mensagem de Progresso
```abap
CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
  EXPORTING
    percentage = 50
    text       = 'A processar... 50%'.
```

---

### 🔢 Operações com Tabelas

#### Remover Duplicados
```abap
SORT lt_dados.
DELETE ADJACENT DUPLICATES FROM lt_dados.
```

#### Contar Linhas com Condição
```abap
DATA(lv_count) = REDUCE i( INIT x = 0
                           FOR wa IN lt_dados WHERE ( price > 1000 )
                           NEXT x = x + 1 ).
```

#### Filtrar Tabela
```abap
DATA(lt_filtrada) = FILTER #( lt_dados WHERE carrid = 'LH' ).
```

#### Mapear/Transformar
```abap
DATA(lt_nomes) = VALUE stringtab( FOR wa IN lt_pessoas ( wa-nome ) ).
```

---

## 📚 Exercícios Práticos

Exercícios disponíveis (serão desenvolvidos):
- `ex01.md` → Manipulação de datas
- `ex02.md` → JSON e XML
- `ex03.md` → Operações com strings
- `ex04.md` → Upload/Download ficheiros
- `ex05.md` → Envio de emails
- `ex06-ex10.md` → Casos práticos diversos

---

## 🧩 Funções Úteis

| Função | Descrição |
|--------|-----------|
| `POPUP_TO_CONFIRM` | Popup de confirmação |
| `POPUP_GET_VALUES` | Popup com input |
| `GUI_UPLOAD` | Upload ficheiro |
| `GUI_DOWNLOAD` | Download ficheiro |
| `SO_NEW_DOCUMENT_ATT_SEND_API1` | Enviar email |
| `CONVERT_DATE_TO_INTERNAL` | Converter data |
| `HR_99S_INTERVAL_BETWEEN_DATES` | Diferença entre datas |
| `LAST_DAY_OF_MONTHS` | Último dia do mês |

---

## 🔗 Próximos Passos

1. Leia [Datas](datas.md) e [JSON](json.md)
2. Experimente os snippets no seu sistema
3. Crie uma biblioteca pessoal de funções reutilizáveis
4. Pratique com `ex01.md` a `ex10.md`
5. Partilhe snippets úteis com a equipa
