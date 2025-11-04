---
tags:
  - ABAP
  - Performance
  - Buffering
  - Cache
  - Database
---

# Table Buffering (Buffer de Tabelas)

O **buffering** armazena dados em memória para evitar acessos repetidos à base de dados.

---

## 🔹 O que é Buffering?

Quando uma tabela está **bufferizada**, os dados são guardados em **memória** (cache) do servidor de aplicação.

**Vantagens:**
- ✅ Acesso **muito mais rápido** (memória vs disco)
- ✅ Reduz carga na base de dados
- ✅ Melhora performance para tabelas de configuração

**Desvantagens:**
- ❌ Dados podem ficar **desatualizados**
- ❌ Consome memória do servidor
- ❌ Não adequado para dados que mudam frequentemente

---

## 🔹 Tipos de Buffering

### 1. Single Record Buffering

Buffer por **registo individual** (por chave primária).

**Quando usar:**
- Tabelas grandes
- Acesso sempre por chave completa
- Só alguns registos são acedidos frequentemente

**Exemplo:** Tabela de materiais (MARA)

```abap
" Primeiro acesso: vai à BD
SELECT SINGLE * FROM mara
  INTO @DATA(ls_mara)
  WHERE matnr = '000000000000001234'.

" Segundo acesso: vem do buffer ✅
SELECT SINGLE * FROM mara
  INTO @ls_mara
  WHERE matnr = '000000000000001234'.  " Muito rápido!
```

### 2. Generic Buffering

Buffer por **área de chave** (primeiros campos da chave).

**Quando usar:**
- Acesso por parte da chave
- Grupos lógicos de dados (ex: todos registos de um cliente)

**Exemplo:** Tabela de textos por idioma

```abap
" Buffer todos os textos de idioma 'PT'
SELECT * FROM t001t
  INTO TABLE @DATA(lt_textos)
  WHERE spras = 'PT'.  " Todos registos PT vêm do buffer
```

### 3. Full Buffering

**Toda a tabela** fica em memória.

**Quando usar:**
- Tabelas pequenas (< 10.000 registos)
- Dados raramente alterados
- Tabelas de configuração/customizing

**Exemplos:**
- T001 (Empresas)
- T005 (Países)
- T005T (Textos de países)

```abap
" Toda a tabela T005T está em memória
SELECT * FROM t005t
  INTO TABLE @DATA(lt_paises)
  WHERE spras = sy-langu.  " Muito rápido! ✅
```

---

## 🔹 Configurar Buffering

### SE11 - Dicionário ABAP

1. Transação **SE11**
2. Inserir nome da tabela
3. **Technical Settings** (botão)
4. Campo **Buffering**:
   - `Not buffered` (sem buffer)
   - `Single records` (por registo)
   - `Generic areas` (por área)
   - `Fully buffered` (completo)

!!! warning "Atenção"
    Só pode configurar buffering em **tabelas próprias** (Z*, Y*).
    Tabelas SAP standard já vêm configuradas.

---

## 🔹 Como Funciona o Buffer

### Ciclo de Vida

```
1. Primeira consulta → BD (slow)
   ↓
2. Dados copiados para BUFFER
   ↓
3. Próximas consultas → BUFFER (fast!) ✅
   ↓
4. Após X tempo ou alteração → INVALIDAR buffer
   ↓
5. Voltar ao passo 1
```

### Tempo de Expiração

Buffer expira após:
- **Alteração de dados** (INSERT/UPDATE/DELETE)
- **Tempo máximo** (configurável)
- **Reinício** do servidor de aplicação

---

## 🔹 Bypass do Buffer

Às vezes queremos **forçar** leitura da BD (ignorar buffer):

```abap
" ❌ Usa buffer (pode estar desatualizado)
SELECT SINGLE * FROM t001
  INTO @DATA(ls_company)
  WHERE bukrs = '1000'.

" ✅ Bypass buffer (sempre atualizado)
SELECT SINGLE * FROM t001 BYPASSING BUFFER
  INTO @ls_company
  WHERE bukrs = '1000'.
```

**Quando usar BYPASSING BUFFER:**
- Dados críticos que mudam frequentemente
- Após UPDATE/INSERT imediato
- Validações financeiras

---

## 🔹 Exemplo Prático

### Tabela de Configuração Custom

```abap
" Tabela: ZCONFIG (fully buffered)
" Campos: CONFIG_KEY, CONFIG_VALUE
```

**Configurar buffering:**
1. SE11 → ZCONFIG
2. Technical Settings
3. Buffering: **Fully buffered**
4. Save & Activate

### Usar a Tabela

```abap
" Primeira execução: vai à BD
SELECT SINGLE config_value FROM zconfig
  INTO @DATA(lv_value)
  WHERE config_key = 'MAX_ITEMS'.

" Execuções seguintes: buffer ✅ (microsegundos!)
SELECT SINGLE config_value FROM zconfig
  INTO @lv_value
  WHERE config_key = 'TAX_RATE'.

SELECT SINGLE config_value FROM zconfig
  INTO @lv_value
  WHERE config_key = 'TIMEOUT'.
```

**Performance:**
- Sem buffer: 5ms por query
- Com buffer: 0.05ms por query
- **Ganho: 100x mais rápido!** 🚀

---

## 🔹 Monitorizar Buffer

### ST02 - Tune Summary

1. Transação **ST02**
2. Ver estatísticas de buffer:
   - **Hit ratio**: % de acessos que vieram do buffer
   - **Swaps**: Quantas vezes buffer foi limpo
   - **Size**: Tamanho do buffer

**Hit Ratio Ideal:**
```
> 98%: ✅ Excelente
90-98%: ✅ Bom
< 90%: ⚠️ Revisar configuração
```

### ST10 - Table Call Statistics

Ver quais tabelas são mais acedidas e se estão bufferizadas.

```
Tabela    | Acessos | Buffered | Hit Ratio
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
T001      | 50,000  | Yes      | 99.8% ✅
ZCONFIG   | 10,000  | Yes      | 100%  ✅
VBAK      | 5,000   | No       | 0%    ❌
T005T     | 2,000   | Yes      | 99.5% ✅
```

---

## 🔹 Invalidar Buffer Manualmente

### $TAB - Reset Buffer

Quando alterar dados manualmente (SE16), o buffer **não** é invalidado automaticamente.

**Solução:**
1. Transação **$TAB**
2. Inserir nome da tabela
3. **Delete** (invalidar buffer)

Ou via código:
```abap
" Invalidar buffer de uma tabela
CALL FUNCTION 'DB_DELETE_TABLE_BUFFER'
  EXPORTING
    tab_name = 'ZCONFIG'.
```

---

## 🔹 Quando NÃO Usar Buffering

### ❌ Tabelas Transacionais

```abap
" NÃO bufferizar:
- VBAK (Ordens de venda) → mudam constantemente
- BSEG (Itens contabilísticos) → volume enorme
- ZTRANSACTIONS (Transações) → dados em tempo real
```

### ❌ Tabelas Grandes

```abap
" Tabela com 10 milhões de registos
" Full buffering → consome MUITA memória! ❌
```

### ❌ Dados em Tempo Real

```abap
" Stock em tempo real
" Preços de bolsa
" Saldos bancários
→ Sempre ler da BD!
```

---

## 🔹 Exemplo: Tabela de Países

### Sem Buffering (T005 - Países)

```abap
" Executar 1000x
DO 1000 TIMES.
  SELECT SINGLE landx FROM t005t
    INTO @DATA(lv_pais)
    WHERE spras = 'PT'
      AND land1 = 'PT'.
ENDDO.

" Tempo: 5000ms (5 segundos) ❌
```

### Com Buffering (T005T é fully buffered)

```abap
" Executar 1000x
DO 1000 TIMES.
  SELECT SINGLE landx FROM t005t
    INTO @DATA(lv_pais)
    WHERE spras = 'PT'
      AND land1 = 'PT'.
ENDDO.

" Tempo: 50ms (0.05 segundos) ✅
" Ganho: 100x mais rápido! 🚀
```

---

## 🔹 Boas Práticas

| ✅ Fazer | ❌ Evitar |
|---------|-----------|
| Bufferizar tabelas de config pequenas | Bufferizar tabelas gigantes |
| Usar full buffering para < 10k registos | Bufferizar dados transacionais |
| Monitorizar hit ratio (ST02) | Nunca verificar performance |
| BYPASSING BUFFER para dados críticos | Assumir que buffer está atualizado |
| Invalidar após updates manuais | Deixar buffer desatualizado |

---

## 🔹 Exemplo Completo

```abap
*&---------------------------------------------------------------------*
*& Report Z_BUFFER_DEMO
*&---------------------------------------------------------------------*
REPORT z_buffer_demo.

" Tabela de configuração (fully buffered)
TYPES: BEGIN OF ty_config,
         key   TYPE char20,
         value TYPE char100,
       END OF ty_config.

DATA: lv_start TYPE timestampl,
      lv_end   TYPE timestampl,
      lv_diff  TYPE int8.

START-OF-SELECTION.

  " ===== TESTE 1: COM BUFFER =====
  WRITE: / 'Teste 1: Com Buffer (T005T)'.
  GET TIME STAMP FIELD lv_start.

  DO 100 TIMES.
    SELECT SINGLE landx FROM t005t
      INTO @DATA(lv_pais)
      WHERE spras = 'PT' AND land1 = 'PT'.
  ENDDO.

  GET TIME STAMP FIELD lv_end.
  lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_end tstmp2 = lv_start ).
  WRITE: / |Tempo: { lv_diff } microssegundos|.

  SKIP.

  " ===== TESTE 2: BYPASS BUFFER =====
  WRITE: / 'Teste 2: Bypass Buffer'.
  GET TIME STAMP FIELD lv_start.

  DO 100 TIMES.
    SELECT SINGLE landx FROM t005t BYPASSING BUFFER
      INTO @lv_pais
      WHERE spras = 'PT' AND land1 = 'PT'.
  ENDDO.

  GET TIME STAMP FIELD lv_end.
  lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_end tstmp2 = lv_start ).
  WRITE: / |Tempo: { lv_diff } microssegundos|.

  SKIP.
  WRITE: / '✅ Note a diferença de performance!'.
```

---

## 💡 Resumo

- **Full Buffering**: Tabelas pequenas e estáticas
- **Generic Buffering**: Acesso por grupos
- **Single Record**: Tabelas grandes, acesso por chave
- **No Buffering**: Dados transacionais e em tempo real
- **Monitorizar**: ST02 e ST10
- **BYPASSING BUFFER**: Para dados críticos

---

## 🚀 Próximo Passo

Aprenda sobre [Tabelas Internas Eficientes](5_tabelas_internas.md) para otimizar o processamento em memória.
