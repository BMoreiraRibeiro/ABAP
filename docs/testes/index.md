# 🧪 Testes Unitários (ABAP Unit)

Aprenda a criar **testes automatizados** em ABAP para garantir qualidade, facilitar refatoração e documentar comportamento esperado do código.

---

## 📖 O que vais aprender

- ABAP Unit Framework
- Estruturar classes de teste
- Assertions e verificações
- Test Doubles (Mocks e Stubs)
- Injeção de dependências
- Code Coverage
- Test-Driven Development (TDD)
- Boas práticas de testes

---

## 🎯 Por que Testar?

### Benefícios

✅ **Detectar bugs cedo** - Antes de chegar a produção  
✅ **Refatoração segura** - Alterar código com confiança  
✅ **Documentação viva** - Testes mostram como usar o código  
✅ **Design melhor** - Código testável é código bem estruturado  
✅ **Menos debugging** - Testes automatizam verificações  
✅ **Regressão** - Garantir que correções não quebram funcionalidades  

---

## 📚 Tópicos

### 1️⃣ [ABAP Unit Básico](1_abap_unit_basico.md)
Primeiros passos com ABAP Unit.

- Anatomia de uma classe de teste
- Atributos `FOR TESTING`, `RISK LEVEL`, `DURATION`
- Criar primeiro teste
- Executar testes (Ctrl+Shift+F10)
- Interpretar resultados

### 2️⃣ [Test Fixtures](2_test_fixtures.md)
Preparar e limpar ambiente de teste.

- Métodos setup() e teardown()
- class_setup() e class_teardown()
- Ciclo de vida dos testes
- Dados de teste reutilizáveis

### 3️⃣ [Assertions](3_assertions.md)
Verificações e validações em testes.

- assert_equals / assert_not_equals
- assert_initial / assert_not_initial
- assert_bound / assert_not_bound
- assert_true / assert_false
- assert_subrc
- assert_table_contains
- Mensagens personalizadas

### 4️⃣ [Test Doubles e Mocking](4_test_doubles.md)
Isolar código sob teste.

- O que são Test Doubles
- ABAP Test Double Framework
- Mockar dependências
- Stubs vs Mocks vs Fakes
- configure_call() e times()

### 5️⃣ [Injeção de Dependências](5_injecao_dependencias.md)
Tornar código testável.

- Princípio de Inversão de Dependência
- Interfaces para desacoplamento
- Constructor Injection
- Setter Injection
- Exemplos práticos

### 6️⃣ [Cobertura de Código](6_code_coverage.md)
Medir qualidade dos testes.

- O que é Code Coverage
- Visualizar no Eclipse ADT
- Coverage Analyzer (SCOV)
- Metas de cobertura (70-80%)
- Statement vs Branch coverage

### 7️⃣ [TDD - Test Driven Development](7_tdd.md)
Desenvolver orientado a testes.

- Ciclo Red-Green-Refactor
- Escrever teste antes do código
- Baby steps
- Vantagens e desvantagens
- Exemplo completo TDD

---

## 🚀 Exemplo Rápido

### Classe a Testar

```abap
CLASS zcl_calculator DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS add
      IMPORTING iv_a TYPE i
                iv_b TYPE i
      RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS zcl_calculator IMPLEMENTATION.
  METHOD add.
    rv_result = iv_a + iv_b.
  ENDMETHOD.
ENDCLASS.
```

### Classe de Teste

```abap
CLASS ltc_calculator DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_calculator.  " Class Under Test
    
    METHODS:
      setup,
      test_add_positive_numbers FOR TESTING.
ENDCLASS.

CLASS ltc_calculator IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_add_positive_numbers.
    " Arrange (preparar)
    DATA(lv_a) = 5.
    DATA(lv_b) = 3.
    
    " Act (executar)
    DATA(lv_result) = mo_cut->add( iv_a = lv_a iv_b = lv_b ).
    
    " Assert (verificar)
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 8
      msg = 'Soma de 5 + 3 deveria ser 8' ).
  ENDMETHOD.
ENDCLASS.
```

**Executar:** Ctrl+Shift+F10 (Eclipse) ou F9 (SAP GUI)

---

## 🛠️ Ferramentas

### Eclipse ADT (Recomendado)
- Ctrl+Shift+F10: Executar testes
- Ctrl+Shift+F11: Executar com coverage
- View de resultados integrada
- Coverage destacado no código

### SAP GUI
- SE80/SE24: Editar classe
- F9: Executar testes (Unit Test)
- F8: Coverage Analyzer (SCOV)

---

## ⚡ Boas Práticas

### ✅ Fazer

```abap
" 1. Nomes descritivos
METHOD test_add_returns_sum_of_two_numbers.

" 2. Arrange-Act-Assert pattern
METHOD test_something.
  " Arrange - preparar dados
  DATA(lv_input) = 'test'.
  
  " Act - executar ação
  DATA(lv_result) = mo_cut->process( lv_input ).
  
  " Assert - verificar resultado
  cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'expected' ).
ENDMETHOD.

" 3. Um conceito por teste
METHOD test_validate_email_valid.
  " Testa APENAS emails válidos
ENDMETHOD.

METHOD test_validate_email_invalid.
  " Testa APENAS emails inválidos (teste separado)
ENDMETHOD.

" 4. Usar setup para código comum
METHOD setup.
  CREATE OBJECT mo_cut.
  mo_cut->set_config( 'test_mode' ).
ENDMETHOD.

" 5. Mensagens claras
cl_abap_unit_assert=>assert_equals(
  act = lv_count
  exp = 5
  msg = |Esperado 5 itens, mas encontrou { lv_count }| ).
```

### ❌ Evitar

```abap
" 1. Testes dependentes
METHOD test_step1.
  mo_cut->create_order( ).  " ❌ test_step2 depende deste
ENDMETHOD.

METHOD test_step2.
  mo_cut->process_order( ).  " ❌ Falha se test_step1 não executou
ENDMETHOD.

" 2. Testar implementação, não comportamento
cl_abap_unit_assert=>assert_equals(
  act = mo_cut->mv_internal_counter  " ❌ Detalhe de implementação
  exp = 3 ).

" 3. Testes sem assertions
METHOD test_something.
  mo_cut->process( ).  " ❌ Não verifica nada!
ENDMETHOD.

" 4. Muitas responsabilidades num teste
METHOD test_everything.
  " Testa validação, processamento, gravação...  ❌
ENDMETHOD.

" 5. Ignorar testes falhando
" ❌ Testes devem SEMPRE passar em produção
```

---

## 🎓 Estatísticas de Qualidade

### Metas Recomendadas

| Métrica | Meta | Crítico |
|---------|------|---------|
| **Code Coverage** | 70-80% | >60% |
| **Testes passando** | 100% | 100% |
| **Tempo execução** | <5 min | <10 min |
| **Testes por classe** | 5-15 | >3 |

---

## 🔗 Próximos Passos

1. Comece com **[ABAP Unit Básico](1_abap_unit_basico.md)** - Primeiro teste
2. Aprenda **[Assertions](3_assertions.md)** - Verificações essenciais
3. Pratique **[Test Fixtures](2_test_fixtures.md)** - Setup e teardown
4. Domine **[Test Doubles](4_test_doubles.md)** - Isolar dependências
5. Estude **[Injeção de Dependências](5_injecao_dependencias.md)** - Código testável
6. Meça com **[Code Coverage](6_code_coverage.md)** - Qualidade dos testes
7. Experimente **[TDD](7_tdd.md)** - Teste antes do código

---

**Tags:** `#Testing` `#ABAP-Unit` `#TDD` `#Quality` `#Best-Practices`