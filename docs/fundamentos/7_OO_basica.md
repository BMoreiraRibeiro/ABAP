# Orientação a Objetos - Fundamentos

## Definição de Classe

### Estrutura Básica

```abap
CLASS lcl_calculadora DEFINITION.
  
  PUBLIC SECTION.
    " Métodos públicos (acessíveis externamente)
    METHODS somar
      IMPORTING iv_num1 TYPE i
                iv_num2 TYPE i
      RETURNING VALUE(rv_resultado) TYPE i.
    
    METHODS subtrair
      IMPORTING iv_num1 TYPE i
                iv_num2 TYPE i
      RETURNING VALUE(rv_resultado) TYPE i.
  
  PROTECTED SECTION.
    " Acessível na classe e subclasses
    METHODS validar_numero
      IMPORTING iv_numero TYPE i
      RETURNING VALUE(rv_valido) TYPE abap_bool.
  
  PRIVATE SECTION.
    " Apenas nesta classe
    DATA mv_historico TYPE string.
    
    METHODS registar_operacao
      IMPORTING iv_operacao TYPE string.
    
ENDCLASS.

CLASS lcl_calculadora IMPLEMENTATION.
  
  METHOD somar.
    rv_resultado = iv_num1 + iv_num2.
    registar_operacao( |Soma: { iv_num1 } + { iv_num2 } = { rv_resultado }| ).
  ENDMETHOD.
  
  METHOD subtrair.
    rv_resultado = iv_num1 - iv_num2.
    registar_operacao( |Subtração: { iv_num1 } - { iv_num2 } = { rv_resultado }| ).
  ENDMETHOD.
  
  METHOD validar_numero.
    rv_valido = xsdbool( iv_numero >= 0 ).
  ENDMETHOD.
  
  METHOD registar_operacao.
    mv_historico = mv_historico && iv_operacao && cl_abap_char_utilities=>newline.
  ENDMETHOD.
  
ENDCLASS.
```

## Instanciação e Uso

### Criar Objeto

```abap
" Forma tradicional
DATA lo_calc TYPE REF TO lcl_calculadora.
CREATE OBJECT lo_calc.

" Forma moderna (preferir)
DATA(lo_calc) = NEW lcl_calculadora( ).

" Verificar se criado
IF lo_calc IS BOUND.
  " Objeto existe
ENDIF.
```

### Chamar Métodos

```abap
" Chamada de método
DATA(lv_resultado) = lo_calc->somar( 
  iv_num1 = 10 
  iv_num2 = 20 
).

WRITE: / |Resultado: { lv_resultado }|.  " 30

" Método sem retorno
lo_calc->registar_operacao( 'Operação manual' ).

" Encadear chamadas (method chaining)
DATA(lv_final) = NEW lcl_calculadora( )->somar( 
  iv_num1 = 5 
  iv_num2 = 3 
).
```

## Atributos (Variáveis de Instância)

### Tipos de Atributos

```abap
CLASS lcl_cliente DEFINITION.
  
  PUBLIC SECTION.
    " Atributos públicos (evitar quando possível)
    DATA mv_id TYPE i READ-ONLY.  " Leitura pública, escrita privada
    
    " Métodos getter/setter (preferir)
    METHODS get_nome
      RETURNING VALUE(rv_nome) TYPE string.
    
    METHODS set_nome
      IMPORTING iv_nome TYPE string.
  
  PRIVATE SECTION.
    DATA mv_nome TYPE string.
    DATA mv_email TYPE string.
    DATA mv_ativo TYPE abap_bool.
    DATA mv_data_criacao TYPE timestamp.
    
ENDCLASS.

CLASS lcl_cliente IMPLEMENTATION.
  
  METHOD get_nome.
    rv_nome = mv_nome.
  ENDMETHOD.
  
  METHOD set_nome.
    mv_nome = iv_nome.
    " Pode adicionar validação aqui
  ENDMETHOD.
  
ENDCLASS.
```

### Constantes de Classe

```abap
CLASS lcl_config DEFINITION.
  
  PUBLIC SECTION.
    CONSTANTS:
      gc_versao TYPE string VALUE '1.0.0',
      gc_max_tentativas TYPE i VALUE 3,
      gc_timeout TYPE i VALUE 30.
    
    CONSTANTS:
      BEGIN OF gc_status,
        ativo     TYPE c LENGTH 1 VALUE 'A',
        inativo   TYPE c LENGTH 1 VALUE 'I',
        pendente  TYPE c LENGTH 1 VALUE 'P',
        cancelado TYPE c LENGTH 1 VALUE 'C',
      END OF gc_status.
    
ENDCLASS.

" Uso
IF lv_status = lcl_config=>gc_status-ativo.
  " Processar
ENDIF.
```

## Construtor

### Constructor Simples

```abap
CLASS lcl_produto DEFINITION.
  
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_id        TYPE i
                iv_descricao TYPE string
                iv_preco     TYPE p.
  
  PRIVATE SECTION.
    DATA: mv_id        TYPE i,
          mv_descricao TYPE string,
          mv_preco     TYPE p.
    
ENDCLASS.

CLASS lcl_produto IMPLEMENTATION.
  
  METHOD constructor.
    mv_id = iv_id.
    mv_descricao = iv_descricao.
    mv_preco = iv_preco.
  ENDMETHOD.
  
ENDCLASS.

" Criar com construtor
DATA(lo_produto) = NEW lcl_produto(
  iv_id        = 1
  iv_descricao = 'Laptop'
  iv_preco     = '999.99'
).
```

### Construtor com Validação

```abap
CLASS lcl_email DEFINITION.
  
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_endereco TYPE string
      RAISING   cx_invalid_email.
    
    METHODS get_endereco
      RETURNING VALUE(rv_endereco) TYPE string.
  
  PRIVATE SECTION.
    DATA mv_endereco TYPE string.
    
    METHODS validar_email
      IMPORTING iv_email         TYPE string
      RETURNING VALUE(rv_valido) TYPE abap_bool.
    
ENDCLASS.

CLASS lcl_email IMPLEMENTATION.
  
  METHOD constructor.
    IF NOT validar_email( iv_endereco ).
      RAISE EXCEPTION TYPE cx_invalid_email.
    ENDIF.
    mv_endereco = iv_endereco.
  ENDMETHOD.
  
  METHOD get_endereco.
    rv_endereco = mv_endereco.
  ENDMETHOD.
  
  METHOD validar_email.
    rv_valido = xsdbool( iv_email CS '@' AND iv_email CS '.' ).
  ENDMETHOD.
  
ENDCLASS.

" Uso com tratamento de exceção
TRY.
    DATA(lo_email) = NEW lcl_email( 'usuario@example.com' ).
  CATCH cx_invalid_email.
    WRITE: / 'Email inválido'.
ENDTRY.
```

## Métodos Estáticos

### Definição e Uso

```abap
CLASS lcl_utilitarios DEFINITION.
  
  PUBLIC SECTION.
    " Métodos de classe (estáticos)
    CLASS-METHODS formatar_moeda
      IMPORTING iv_valor          TYPE p
                iv_moeda          TYPE waers DEFAULT 'EUR'
      RETURNING VALUE(rv_formato) TYPE string.
    
    CLASS-METHODS gerar_uuid
      RETURNING VALUE(rv_uuid) TYPE sysuuid_c32.
  
  PRIVATE SECTION.
    " Atributos de classe (compartilhados)
    CLASS-DATA gv_contador TYPE i.
    
ENDCLASS.

CLASS lcl_utilitarios IMPLEMENTATION.
  
  METHOD formatar_moeda.
    rv_formato = |{ iv_valor CURRENCY = iv_moeda }|.
  ENDMETHOD.
  
  METHOD gerar_uuid.
    TRY.
        rv_uuid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        " Tratar erro
    ENDTRY.
  ENDMETHOD.
  
ENDCLASS.

" Chamar método estático (sem instanciar)
DATA(lv_preco_fmt) = lcl_utilitarios=>formatar_moeda( 
  iv_valor = '1234.56' 
).

DATA(lv_uuid) = lcl_utilitarios=>gerar_uuid( ).
```

## Me e Self-Reference

```abap
CLASS lcl_exemplo DEFINITION.
  
  PUBLIC SECTION.
    METHODS set_valor
      IMPORTING iv_valor TYPE i
      RETURNING VALUE(ro_self) TYPE REF TO lcl_exemplo.
    
    METHODS processar
      RETURNING VALUE(ro_self) TYPE REF TO lcl_exemplo.
    
    METHODS exibir.
  
  PRIVATE SECTION.
    DATA mv_valor TYPE i.
    
ENDCLASS.

CLASS lcl_exemplo IMPLEMENTATION.
  
  METHOD set_valor.
    mv_valor = iv_valor.  " 'me->' é opcional para atributos
    ro_self = me.         " Retorna referência a si mesmo
  ENDMETHOD.
  
  METHOD processar.
    me->mv_valor = me->mv_valor * 2.
    ro_self = me.
  ENDMETHOD.
  
  METHOD exibir.
    WRITE: / |Valor: { mv_valor }|.
  ENDMETHOD.
  
ENDCLASS.

" Method chaining (encadeamento)
NEW lcl_exemplo( )->set_valor( 10 )->processar( )->exibir( ).
" Output: Valor: 20
```

## Herança Simples

### Classe Base e Derivada

```abap
" Classe base
CLASS lcl_veiculo DEFINITION.
  
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_marca TYPE string
                iv_modelo TYPE string.
    
    METHODS acelerar.
    METHODS travar.
    
    METHODS get_info
      RETURNING VALUE(rv_info) TYPE string.
  
  PROTECTED SECTION.
    DATA: mv_marca     TYPE string,
          mv_modelo    TYPE string,
          mv_velocidade TYPE i.
    
ENDCLASS.

CLASS lcl_veiculo IMPLEMENTATION.
  
  METHOD constructor.
    mv_marca = iv_marca.
    mv_modelo = iv_modelo.
    mv_velocidade = 0.
  ENDMETHOD.
  
  METHOD acelerar.
    mv_velocidade = mv_velocidade + 10.
  ENDMETHOD.
  
  METHOD travar.
    mv_velocidade = COND #( WHEN mv_velocidade >= 10 
                            THEN mv_velocidade - 10 
                            ELSE 0 ).
  ENDMETHOD.
  
  METHOD get_info.
    rv_info = |{ mv_marca } { mv_modelo } - { mv_velocidade } km/h|.
  ENDMETHOD.
  
ENDCLASS.

" Classe derivada
CLASS lcl_carro DEFINITION INHERITING FROM lcl_veiculo.
  
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_marca      TYPE string
                iv_modelo     TYPE string
                iv_num_portas TYPE i.
    
    " Sobrescrever método (redefinição)
    METHODS acelerar REDEFINITION.
    
    METHODS get_num_portas
      RETURNING VALUE(rv_portas) TYPE i.
  
  PRIVATE SECTION.
    DATA mv_num_portas TYPE i.
    
ENDCLASS.

CLASS lcl_carro IMPLEMENTATION.
  
  METHOD constructor.
    super->constructor(
      iv_marca  = iv_marca
      iv_modelo = iv_modelo
    ).
    mv_num_portas = iv_num_portas.
  ENDMETHOD.
  
  METHOD acelerar.
    " Carros aceleram mais rápido
    mv_velocidade = mv_velocidade + 20.
  ENDMETHOD.
  
  METHOD get_num_portas.
    rv_portas = mv_num_portas.
  ENDMETHOD.
  
ENDCLASS.

" Uso
DATA(lo_carro) = NEW lcl_carro(
  iv_marca      = 'Tesla'
  iv_modelo     = 'Model 3'
  iv_num_portas = 4
).

lo_carro->acelerar( ).
WRITE: / lo_carro->get_info( ).  " Tesla Model 3 - 20 km/h
```

## Interfaces Básicas

### Definir e Implementar

```abap
" Definir interface
INTERFACE lif_imprimivel.
  METHODS imprimir.
ENDINTERFACE.

INTERFACE lif_exportavel.
  METHODS exportar_pdf
    RETURNING VALUE(rv_pdf) TYPE xstring.
  
  METHODS exportar_excel
    RETURNING VALUE(rv_excel) TYPE xstring.
ENDINTERFACE.

" Classe que implementa interfaces
CLASS lcl_relatorio DEFINITION.
  
  PUBLIC SECTION.
    INTERFACES: lif_imprimivel,
                lif_exportavel.
    
    ALIASES: imprimir      FOR lif_imprimivel~imprimir,
             exportar_pdf  FOR lif_exportavel~exportar_pdf,
             exportar_excel FOR lif_exportavel~exportar_excel.
  
  PRIVATE SECTION.
    DATA mv_conteudo TYPE string.
    
ENDCLASS.

CLASS lcl_relatorio IMPLEMENTATION.
  
  METHOD lif_imprimivel~imprimir.
    WRITE: / 'Imprimindo relatório...'.
    WRITE: / mv_conteudo.
  ENDMETHOD.
  
  METHOD lif_exportavel~exportar_pdf.
    " Lógica de exportação PDF
    WRITE: / 'Exportando para PDF...'.
  ENDMETHOD.
  
  METHOD lif_exportavel~exportar_excel.
    " Lógica de exportação Excel
    WRITE: / 'Exportando para Excel...'.
  ENDMETHOD.
  
ENDCLASS.

" Uso com polimorfismo
DATA lo_relatorio TYPE REF TO lcl_relatorio.
lo_relatorio = NEW #( ).

" Chamar através da interface
DATA lo_imprimivel TYPE REF TO lif_imprimivel.
lo_imprimivel = lo_relatorio.
lo_imprimivel->imprimir( ).

" Ou direto através de alias
lo_relatorio->imprimir( ).
lo_relatorio->exportar_pdf( ).
```

## Friend Classes (Classes Amigas)

```abap
CLASS lcl_conta_bancaria DEFINITION.
  
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_numero TYPE string
                iv_saldo  TYPE p.
    
    METHODS get_saldo
      RETURNING VALUE(rv_saldo) TYPE p.
  
  PRIVATE SECTION.
    DATA: mv_numero TYPE string,
          mv_saldo  TYPE p.
    
    " Permitir acesso privado a esta classe
    FRIENDS lcl_banco.
    
ENDCLASS.

CLASS lcl_banco DEFINITION.
  
  PUBLIC SECTION.
    METHODS transferir
      IMPORTING io_origem  TYPE REF TO lcl_conta_bancaria
                io_destino TYPE REF TO lcl_conta_bancaria
                iv_valor   TYPE p.
    
ENDCLASS.

CLASS lcl_banco IMPLEMENTATION.
  
  METHOD transferir.
    " Pode aceder diretamente aos atributos privados
    io_origem->mv_saldo = io_origem->mv_saldo - iv_valor.
    io_destino->mv_saldo = io_destino->mv_saldo + iv_valor.
  ENDMETHOD.
  
ENDCLASS.
```

## Referências e CAST

```abap
" Referência genérica
DATA lo_objeto TYPE REF TO object.
lo_objeto = NEW lcl_carro(
  iv_marca      = 'BMW'
  iv_modelo     = 'X5'
  iv_num_portas = 5
).

" Cast para tipo específico
DATA(lo_carro) = CAST lcl_carro( lo_objeto ).
lo_carro->acelerar( ).

" Cast seguro com verificação
IF lo_objeto IS INSTANCE OF lcl_carro.
  DATA(lo_meu_carro) = CAST lcl_carro( lo_objeto ).
ENDIF.

" Com exceção
TRY.
    DATA(lo_veiculo) = CAST lcl_veiculo( lo_objeto ).
  CATCH cx_sy_move_cast_error.
    WRITE: / 'Cast falhou'.
ENDTRY.
```

## Factory Pattern Simples

```abap
CLASS lcl_veiculo_factory DEFINITION.
  
  PUBLIC SECTION.
    CLASS-METHODS criar_veiculo
      IMPORTING iv_tipo           TYPE string
                iv_marca          TYPE string
                iv_modelo         TYPE string
      RETURNING VALUE(ro_veiculo) TYPE REF TO lcl_veiculo.
    
ENDCLASS.

CLASS lcl_veiculo_factory IMPLEMENTATION.
  
  METHOD criar_veiculo.
    CASE iv_tipo.
      WHEN 'CARRO'.
        ro_veiculo = NEW lcl_carro(
          iv_marca      = iv_marca
          iv_modelo     = iv_modelo
          iv_num_portas = 4
        ).
        
      WHEN 'MOTA'.
        ro_veiculo = NEW lcl_mota(
          iv_marca  = iv_marca
          iv_modelo = iv_modelo
        ).
        
      WHEN OTHERS.
        " Retornar tipo base
        ro_veiculo = NEW lcl_veiculo(
          iv_marca  = iv_marca
          iv_modelo = iv_modelo
        ).
    ENDCASE.
  ENDMETHOD.
  
ENDCLASS.

" Uso
DATA(lo_veiculo) = lcl_veiculo_factory=>criar_veiculo(
  iv_tipo   = 'CARRO'
  iv_marca  = 'Audi'
  iv_modelo = 'A4'
).
```

## Singleton Pattern

```abap
CLASS lcl_config_manager DEFINITION.
  
  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_config_manager.
    
    METHODS get_config
      IMPORTING iv_chave        TYPE string
      RETURNING VALUE(rv_valor) TYPE string.
    
    METHODS set_config
      IMPORTING iv_chave TYPE string
                iv_valor TYPE string.
  
  PRIVATE SECTION.
    CLASS-DATA go_instance TYPE REF TO lcl_config_manager.
    
    DATA mt_config TYPE HASHED TABLE OF ty_config WITH UNIQUE KEY chave.
    
    " Construtor privado
    METHODS constructor.
    
ENDCLASS.

CLASS lcl_config_manager IMPLEMENTATION.
  
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.
  
  METHOD constructor.
    " Carregar configurações
  ENDMETHOD.
  
  METHOD get_config.
    TRY.
        rv_valor = mt_config[ chave = iv_chave ]-valor.
      CATCH cx_sy_itab_line_not_found.
        rv_valor = ''.
    ENDTRY.
  ENDMETHOD.
  
  METHOD set_config.
    INSERT VALUE #( chave = iv_chave valor = iv_valor ) 
      INTO TABLE mt_config.
  ENDMETHOD.
  
ENDCLASS.

" Uso (sempre retorna a mesma instância)
DATA(lo_config) = lcl_config_manager=>get_instance( ).
lo_config->set_config( iv_chave = 'TIMEOUT' iv_valor = '30' ).

DATA(lo_config2) = lcl_config_manager=>get_instance( ).
DATA(lv_timeout) = lo_config2->get_config( 'TIMEOUT' ).  " '30'
```

## Event Handling (Eventos)

```abap
" Classe que dispara eventos
CLASS lcl_processador DEFINITION.
  
  PUBLIC SECTION.
    " Definir evento
    EVENTS processamento_completo
      EXPORTING VALUE(ev_resultado) TYPE string.
    
    METHODS processar.
    
ENDCLASS.

CLASS lcl_processador IMPLEMENTATION.
  
  METHOD processar.
    " Processar dados
    WAIT UP TO 1 SECONDS.
    
    " Disparar evento
    RAISE EVENT processamento_completo
      EXPORTING ev_resultado = 'Processamento concluído'.
  ENDMETHOD.
  
ENDCLASS.

" Classe que trata eventos
CLASS lcl_listener DEFINITION.
  
  PUBLIC SECTION.
    METHODS on_processamento_completo
      FOR EVENT processamento_completo OF lcl_processador
      IMPORTING ev_resultado.
    
ENDCLASS.

CLASS lcl_listener IMPLEMENTATION.
  
  METHOD on_processamento_completo.
    WRITE: / |Evento recebido: { ev_resultado }|.
  ENDMETHOD.
  
ENDCLASS.

" Uso
DATA(lo_processador) = NEW lcl_processador( ).
DATA(lo_listener) = NEW lcl_listener( ).

" Registar handler
SET HANDLER lo_listener->on_processamento_completo FOR lo_processador.

" Executar (dispara evento)
lo_processador->processar( ).
```

## Dicas de Boas Práticas

!!! tip "Encapsulamento"
    - Mantenha atributos privados, use getters/setters
    - Use READ-ONLY para atributos que só podem ser lidos externamente
    - Evite atributos públicos exceto para DTOs simples

!!! warning "Armadilhas Comuns"
    ```abap
    " ❌ Esquecer de verificar se objeto está bound
    DATA lo_cliente TYPE REF TO lcl_cliente.
    lo_cliente->processar( ).  " DUMP se não criado!
    
    " ✅ Sempre verificar
    IF lo_cliente IS BOUND.
      lo_cliente->processar( ).
    ENDIF.
    
    " ❌ Modificar atributos diretamente
    lo_produto->mv_preco = '100.00'.  " Se público
    
    " ✅ Usar métodos
    lo_produto->set_preco( '100.00' ).
    ```

!!! example "NEW vs. CREATE OBJECT"
    ```abap
    " ❌ Forma antiga
    DATA lo_objeto TYPE REF TO lcl_classe.
    CREATE OBJECT lo_objeto
      EXPORTING
        iv_param1 = lv_val1
        iv_param2 = lv_val2.
    
    " ✅ Forma moderna
    DATA(lo_objeto) = NEW lcl_classe(
      iv_param1 = lv_val1
      iv_param2 = lv_val2
    ).
    ```

!!! info "Quando Usar Métodos Estáticos"
    - Utilitários que não precisam de estado
    - Factory methods
    - Funções puras (mesmo input = mesmo output)
    - **NÃO** use para aceder a dados da base de dados (dificulta testes)

!!! tip "Organização de Código"
    - Uma responsabilidade por classe (Single Responsibility)
    - Nomes descritivos: `lcl_validador_email` melhor que `lcl_util`
    - Métodos pequenos e focados
    - Use interfaces para desacoplar dependências

---

**Tags:** #Fundamentos #OO #Classes #Objetos #Modern-ABAP