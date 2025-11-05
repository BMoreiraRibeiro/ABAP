---
tags:
  - ABAP
  - Utilitários
  - Files
---

# Manipulação de Ficheiros

## 📋 Visão Geral

ABAP diferencia Presentation Server (PC do usuário) e Application Server (servidor SAP). Principais comandos: `OPEN DATASET`, `TRANSFER`, `READ DATASET`, `CLOSE DATASET`.

---

## Application Server (SERVER)

```abap
DATA: lv_path TYPE string VALUE '/usr/sap/trans/myfile.txt'.

OPEN DATASET lv_path FOR INPUT IN TEXT MODE ENCODING DEFAULT.
IF sy-subrc <> 0.
  MESSAGE 'Erro ao abrir ficheiro' TYPE 'E'.
  RETURN.
ENDIF.

READ DATASET lv_path INTO DATA(lv_line).
WHILE sy-subrc = 0.
  WRITE: / lv_line.
  READ DATASET lv_path INTO lv_line.
ENDWHILE.

CLOSE DATASET lv_path.
```

---

## Presentation Server (PC)

- Use `GUI_UPLOAD` / `GUI_DOWNLOAD` (cl_gui_frontend_services)

```abap
DATA: lt_lines TYPE TABLE OF string.
cl_gui_frontend_services=>gui_upload(
  EXPORTING
    filename = 'C:\temp\file.txt'
  CHANGING
    data_tab = lt_lines ).
```

---

## Segurança e Permissões

- Restrinja paths permitidos (whitelist).
- Evite permitir upload arbitrário sem validação.
- Evite executar ficheiros carregados.

---

## Boas Práticas

- Use `OPEN DATASET` com `IN TEXT MODE` e `ENCODING` adequado.
- Sempre `CLOSE DATASET` em tratamento de exceções (TRY/CATCH ou CLEANUP).
- Ao escrever ficheiros sensíveis, controle permissões e rotas de transporte.

---

## Próximos Passos

- `5_mensagens.md` — mensagens e logging

**Tags:** `#files` `#open_dataset` `#gui_upload`