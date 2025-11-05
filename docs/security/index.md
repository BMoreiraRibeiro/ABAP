# 🔒 Segurança e Autorização

Práticas e exemplos para **verificar autorizações** e proteger programas ABAP contra acessos não autorizados.

---

## 📖 O que vais aprender

- Objetos de autorização
- AUTHORITY-CHECK
- Roles e perfis de utilizador (PFCG)
- SU53 para análise de erros
- Proteção de transações customizadas
- Boas práticas de segurança
- Auditoria e logs

---

## 🎯 Conceitos Principais

### Objeto de Autorização
Define **o quê** pode ser protegido (ex: S_TCODE para transações)

### Campo de Autorização
Define **como** é protegido (ex: TCD = código da transação)

### Perfil de Autorização
Conjunto de autorizações atribuídas a um utilizador

---

## 🔑 AUTHORITY-CHECK

### [Verificação Básica](2_authority_check.md)

```abap
AUTHORITY-CHECK OBJECT 'S_TCODE'
  ID 'TCD' FIELD 'SE38'.

IF sy-subrc <> 0.
  MESSAGE 'Sem permissão para executar SE38' TYPE 'E'.
ENDIF.
```

### Verificação com Múltiplos Campos

```abap
AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
  ID 'ACTVT' FIELD '02'      " 02 = Alteração
  ID 'WERKS' FIELD lv_planta.

CASE sy-subrc.
  WHEN 0.
    " Tem autorização
  WHEN 4.
    MESSAGE 'Sem autorização para alterar material nesta planta' TYPE 'E'.
  WHEN 12.
    MESSAGE 'Objeto de autorização não configurado' TYPE 'E'.
  WHEN OTHERS.
    MESSAGE 'Erro ao verificar autorização' TYPE 'E'.
ENDCASE.
```

---

## 📊 Objetos de Autorização Comuns

| Objeto | Descrição |
|--------|-----------|
| `S_TCODE` | Acesso a transações |
| `S_TABU_NAM` | Acesso a tabelas |
| `S_PROGRAM` | Execução de programs |
| `S_DEVELOP` | Objetos de desenvolvimento |
| `M_MATE_WRK` | Materiais por planta |
| `F_BKPF_BUK` | Documentos contabilísticos |
| `V_VBAK_VKO` | Ordens de venda |

Consultar todos: Transação **SU21**

---

## 💡 Exemplo Completo

```abap
REPORT z_protegido.

" Parâmetro de seleção
PARAMETERS: p_bukrs TYPE bukrs.

START-OF-SELECTION.

  " ✅ Verificar autorização para a empresa
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD p_bukrs
    ID 'ACTVT' FIELD '03'.  " 03 = Visualização

  IF sy-subrc <> 0.
    MESSAGE |Sem autorização para empresa { p_bukrs }| TYPE 'E'.
  ENDIF.

  " Continuar processamento...
  SELECT * FROM bkpf
    INTO TABLE @DATA(lt_docs)
    WHERE bukrs = @p_bukrs
    UP TO 100 ROWS.

  WRITE: / |Encontrados { lines( lt_docs ) } documentos|.
```

---

## 🛠️ Ferramentas de Análise

### SU53 - Análise de Autorização
Após um erro de autorização, executar **SU53** para ver:
- Objeto que faltou
- Campos e valores verificados
- Utilizador e timestamp

### PFCG - Gestão de Roles
Criar e manter roles (perfis de autorização):
1. Transação **PFCG**
2. Criar role (ex: Z_VENDAS_READONLY)
3. Adicionar transações e objetos
4. Gerar perfil
5. Atribuir a utilizadores

### ST01 - System Trace
Rastrear todas as verificações de autorização:
1. Ativar trace em **ST01**
2. Executar programa
3. Desativar trace
4. Analisar verificações realizadas

---

## 🚨 Boas Práticas

### ✅ Fazer

1. **Sempre verificar** antes de operações críticas
2. **Mensagens claras** sobre o que falta
3. **Logar tentativas** de acesso não autorizado
4. **Princípio do menor privilégio**: dar apenas o necessário
5. **Usar objetos custom** para funcionalidades próprias (Z*)
6. **Documentar** requirements de autorização

### ❌ Evitar

1. Hardcoded bypass (ex: `IF sy-uname = 'ADMIN'`)
2. Autorização genérica demais (ex: S_TCODE = *)
3. Confiar apenas em UI (sempre validar no backend)
4. Não tratar sy-subrc após AUTHORITY-CHECK
5. Expor dados sensíveis em logs

---

## 🎓 Exemplo Avançado: Role Customizado

### 1. Criar Objeto de Autorização (SU21)

```
Objeto: Z_VENDAS
Classe: BC_A (Application-specific)

Campos:
- ACTVT (Atividade): 01=Criar, 02=Alterar, 03=Visualizar
- VKORG (Organização de vendas)
- SPART (Setor de atividade)
```

### 2. Usar no Código

```abap
AUTHORITY-CHECK OBJECT 'Z_VENDAS'
  ID 'ACTVT' FIELD '02'           " Alterar
  ID 'VKORG' FIELD ls_ordem-vkorg
  ID 'SPART' FIELD ls_ordem-spart.

IF sy-subrc <> 0.
  MESSAGE 'Sem autorização para alterar ordem desta org. vendas' TYPE 'E'.
ENDIF.
```

### 3. Criar Role (PFCG)

1. PFCG → Criar role **Z_VENDEDOR**
2. Aba **Autorizações**
3. Adicionar objeto Z_VENDAS
4. Configurar valores:
   - ACTVT: 01, 03 (Criar e Visualizar)
   - VKORG: 1000
   - SPART: *
5. Gerar perfil e atribuir a utilizadores

---

## 🔐 Auditoria e Logs

### Logar Tentativas Falhadas

```abap
IF sy-subrc <> 0.
  " Logar em Application Log (SLG1)
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log = VALUE bal_s_log(
        object    = 'ZSECURITY'
        subobject = 'AUTH_CHECK'
        aldate    = sy-datum
        altime    = sy-uzeit
        aluser    = sy-uname )
    IMPORTING
      e_log_handle = DATA(lv_handle).

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = lv_handle
      i_s_msg      = VALUE bal_s_msg(
        msgty = 'E'
        msgid = 'ZZ'
        msgno = '001'
        msgv1 = |Tentativa falha: { sy-tcode }| ).

  COMMIT WORK.
ENDIF.
```

---

## 📚 Exercícios Práticos

Exercícios disponíveis (serão desenvolvidos):
- `ex01.md` → AUTHORITY-CHECK básico
- `ex02.md` → Criar objeto customizado
- `ex03.md` → Role com múltiplas autorizações
- `ex04.md` → Auditoria de acessos
- `ex05.md` → Análise com SU53/ST01

---

## 🔗 Próximos Passos

1. Leia **[Autorizações](1_autorizacoes.md)** - Entender sistema de autorizações
2. Pratique **[Authority Check](2_authority_check.md)** - Implementar verificações
3. Configure **[Roles e Perfis](3_roles_perfis.md)** - Gestão em PFCG
4. Estude **[S_DEVELOP](4_s_develop.md)** - Proteger desenvolvimento
5. Aplique **[Práticas Seguras](5_praticas_seguras.md)** - Segurança no código
6. Previna **[SQL Injection](6_sql_injection.md)** - Proteger contra ataques
