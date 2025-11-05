---
tags:
  - ABAP
  - Security
  - Best Practices
  - Secure Coding
---

# Práticas Seguras de Código

## 📋 Visão Geral

**Práticas seguras** protegem contra vulnerabilidades, vazamento de dados e acessos não autorizados.

---

## 🔐 Princípio do Menor Privilégio

Dar **apenas** autorizações **necessárias**.

### ✅ Correto
```abap
AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
  ID 'VKORG' FIELD '1000'  " ✅ Específico
  ID 'ACTVT' FIELD '03'.   " ✅ Apenas visualizar
```

### ❌ Incorreto
```abap
" ❌ Wildcards demais
ID 'VKORG' FIELD '*'
ID 'ACTVT' FIELD '*'
```

---

## 🛡️ Validar Sempre Inputs

### ✅ Validação Completa
```abap
PARAMETERS: p_kunnr TYPE kunnr.

START-OF-SELECTION.
  " Validar formato
  IF p_kunnr IS INITIAL OR strlen( p_kunnr ) <> 10.
    MESSAGE 'Customer ID inválido' TYPE 'E'.
    RETURN.
  ENDIF.
  
  " Validar existência
  SELECT SINGLE @abap_true FROM kna1
    WHERE kunnr = @p_kunnr
    INTO @DATA(lv_exists).
  
  IF sy-subrc <> 0.
    MESSAGE |Cliente { p_kunnr } não existe| TYPE 'E'.
    RETURN.
  ENDIF.
  
  " Validar autorização
  AUTHORITY-CHECK OBJECT 'F_KNA1_GEN'
    ID 'KUNNR' FIELD p_kunnr
    ID 'ACTVT' FIELD '03'.
  
  IF sy-subrc <> 0.
    MESSAGE 'Sem autorização' TYPE 'E'.
    RETURN.
  ENDIF.
  
  " Processar
ENDMETHOD.
```

---

## 🚫 Nunca Hardcodear Credenciais

### ❌ Ruim
```abap
" ❌ NUNCA fazer isso!
DATA: lv_user TYPE string VALUE 'ADMIN',
      lv_pass TYPE string VALUE 'Password123'.

CALL FUNCTION 'RFC_CONNECTION'
  EXPORTING
    username = lv_user
    password = lv_pass.
```

### ✅ Bom
```abap
" ✅ Usar Secure Store
CALL METHOD cl_sec_sxml_writer=>get_instance
  RECEIVING
    r_instance = DATA(lo_sec_store).

lo_sec_store->get_credentials(
  IMPORTING
    ev_username = DATA(lv_user)
    ev_password = DATA(lv_pass) ).
```

---

## 🔒 Proteger Dados Sensíveis

### Logs e Mensagens

```abap
" ❌ Expor senha em log
WRITE: / |Login: { lv_user } / { lv_password }|.

" ✅ Ocultar dados sensíveis
WRITE: / |Login: { lv_user } / ********|.
```

### Não Logar Dados Pessoais (GDPR)

```abap
" ❌ Logar CPF/Email completo
MESSAGE |Processando CPF { lv_cpf }| TYPE 'I'.

" ✅ Mascarar
DATA(lv_masked) = |{ lv_cpf(3) }.***.***-**|.
MESSAGE |Processando CPF { lv_masked }| TYPE 'I'.
```

---

## 🔐 Sessões e Timeouts

```abap
" Verificar timeout de sessão
IF sy-datum > lv_session_date + 1 OR
   sy-uzeit > lv_session_time + 3600.  " 1 hora
  
  MESSAGE 'Sessão expirada. Faça login novamente.' TYPE 'E'.
  LEAVE PROGRAM.
ENDIF.
```

---

## 🛡️ Proteção contra Ataques

### SQL Injection (ver página dedicada)

```abap
" ✅ Usar @-escaping
SELECT * FROM kna1
  WHERE kunnr = @lv_customer  " ✅ Seguro
  INTO TABLE @DATA(lt_customers).
```

### Command Injection

```abap
" ❌ Perigoso
DATA(lv_command) = |rm -rf { lv_path }|.
CALL 'SYSTEM' ID 'COMMAND' FIELD lv_command.

" ✅ Validar input
IF lv_path CA '/\*?<>|'.
  MESSAGE 'Caracteres inválidos no path' TYPE 'E'.
  RETURN.
ENDIF.
```

---

## 📋 Auditoria e Logging

### Logar Operações Críticas

```abap
METHOD delete_customer.
  " Verificar autorização
  AUTHORITY-CHECK OBJECT 'F_KNA1_DEL'
    ID 'KUNNR' FIELD iv_customer
    ID 'ACTVT' FIELD '06'.
  
  IF sy-subrc <> 0.
    " Logar tentativa falhada
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg = VALUE #(
          msgty = 'E'
          msgv1 = |User { sy-uname } tentou deletar { iv_customer }| ).
    
    MESSAGE 'Sem autorização' TYPE 'E'.
    RETURN.
  ENDIF.
  
  " Logar sucesso
  DELETE FROM kna1 WHERE kunnr = iv_customer.
  
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_s_msg = VALUE #(
        msgty = 'I'
        msgv1 = |Cliente { iv_customer } deletado por { sy-uname }| ).
ENDMETHOD.
```

---

## 🔐 Criptografia

### Dados Sensíveis

```abap
" Criptografar senha antes de gravar
DATA(lo_encryptor) = cl_sec_sxml_writer=>create( ).

lo_encryptor->encrypt(
  EXPORTING
    plaintext  = lv_password
  IMPORTING
    ciphertext = lv_encrypted_password ).

INSERT INTO ztab_credentials VALUES
  ( user = lv_user password = lv_encrypted_password ).
```

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Sempre validar inputs
IF p_input IS INITIAL OR p_input CA '*?/\'.
  MESSAGE 'Input inválido' TYPE 'E'.
ENDIF.

" 2. Verificar autorizações ANTES de processar
AUTHORITY-CHECK ...
IF sy-subrc = 0.
  " Processar
ENDIF.

" 3. Usar parametrized queries
SELECT * FROM kna1
  WHERE kunnr = @lv_customer  " ✅

" 4. Logar operações críticas
CALL FUNCTION 'BAL_LOG_MSG_ADD' ...

" 5. Timeout de sessões
IF session_expired( ).
  LEAVE PROGRAM.
ENDIF.

" 6. Mascarar dados sensíveis em logs
lv_masked = mask_cpf( lv_cpf ).
```

### ❌ Evitar

```abap
" 1. Hardcodear credenciais
DATA: lv_pass VALUE 'admin123'.  " ❌

" 2. SQL dinâmico sem validação
DATA(lv_sql) = |SELECT * FROM { lv_table }|.  " ❌

" 3. Expor dados sensíveis
WRITE: / lv_credit_card.  " ❌

" 4. Não validar input
SELECT * WHERE name = p_name.  " ❌ E se p_name = "'; DROP TABLE--"?

" 5. Confiar apenas em UI
" ❌ SEMPRE validar no backend também

" 6. Não logar operações críticas
DELETE FROM important_table.  " ❌ Sem log!
```

---

## 🎓 Checklist de Segurança

```
[ ] Autorizações verificadas antes de operações
[ ] Inputs validados (formato, range, existência)
[ ] Dados sensíveis NÃO em logs
[ ] Credenciais em Secure Store, não hardcoded
[ ] SQL parametrizado (@-escaping)
[ ] Operações críticas logadas
[ ] Timeouts de sessão implementados
[ ] Dados criptografados quando necessário
[ ] Erro handling sem expor detalhes internos
[ ] Code review de segurança realizado
```

---

## 🔗 Próximos Passos

- **[SQL Injection](6_sql_injection.md)** - Prevenir ataques
- **[AUTHORITY-CHECK](2_authority_check.md)** - Verificações
- **[Roles e Perfis](3_roles_perfis.md)** - Gestão de acessos

---

**Tags:** `#Secure-Coding` `#Best-Practices` `#GDPR` `#Encryption`
