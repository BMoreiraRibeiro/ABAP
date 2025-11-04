# 🧱 Programação Orientada a Objetos (OO)

Introdução à programação orientada a objetos em ABAP: **classes, métodos, interfaces, herança e padrões de design**.

---

## 📖 O que vais aprender

- Criar classes e objetos
- Definir e implementar métodos
- Atributos públicos, protegidos e privados
- Construtores (CONSTRUCTOR)
- Interfaces e polimorfismo
- Herança e redefinição de métodos
- Padrões de design (Singleton, Factory, etc.)
- Eventos e tratadores

---

## 🎯 Ordem de Aprendizagem

### 1️⃣ [Classes e Objetos](1_classes_objetos.md)
Como definir e usar classes locais e globais.

**Exemplo simples:**
```abap
CLASS lcl_calculadora DEFINITION.
  PUBLIC SECTION.
    METHODS: somar IMPORTING iv_a TYPE i iv_b TYPE i 
                   RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS lcl_calculadora IMPLEMENTATION.
  METHOD somar.
    rv_result = iv_a + iv_b.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_calc) = NEW lcl_calculadora( ).
  DATA(lv_soma) = lo_calc->somar( iv_a = 10 iv_b = 20 ).
  WRITE: / |Resultado: { lv_soma }|.
```

### 2️⃣ [Atributos e Métodos](2_atributos_metodos.md)
Controlar acesso aos dados da classe e definir comportamentos.

### 3️⃣ [Construtores](3_construtores.md)
Inicializar objetos com `CONSTRUCTOR`.

### 4️⃣ [Interfaces](4_interfaces.md)
Definir contratos que classes devem implementar.

### 5️⃣ [Herança](5_heranca.md)
Reutilizar código através de `INHERITING FROM`.

### 6️⃣ [Polimorfismo](6_polimorfismo.md)
Tratar objetos diferentes através da mesma interface.

### 7️⃣ [Factory Pattern](7_factory_pattern.md)
Padrão de criação de objetos.

### 8️⃣ [Singleton Pattern](8_singleton.md)
Garantir uma única instância de uma classe.

---

## 🧠 Conceitos Importantes

### Visibilidade de Atributos

| Seção | Acesso |
|-------|--------|
| `PUBLIC SECTION` | Acessível de qualquer lugar |
| `PROTECTED SECTION` | Acessível na classe e subclasses |
| `PRIVATE SECTION` | Apenas dentro da classe |

### Criar Classes Globais vs. Locais

- **Globais**: SE24 / SE80 → reutilizáveis em todo o sistema
- **Locais**: dentro do programa → escopo limitado

---

## 💡 Exemplo com Interface

```abap
INTERFACE lif_forma.
  METHODS calcular_area RETURNING VALUE(rv_area) TYPE f.
ENDINTERFACE.

CLASS lcl_circulo DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_forma.
    METHODS constructor IMPORTING iv_raio TYPE f.
  PRIVATE SECTION.
    DATA mv_raio TYPE f.
ENDCLASS.

CLASS lcl_circulo IMPLEMENTATION.
  METHOD constructor.
    mv_raio = iv_raio.
  ENDMETHOD.
  
  METHOD lif_forma~calcular_area.
    CONSTANTS lc_pi TYPE f VALUE '3.14159'.
    rv_area = lc_pi * mv_raio * mv_raio.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_circulo) = NEW lcl_circulo( iv_raio = 5 ).
  DATA(lv_area) = lo_circulo->lif_forma~calcular_area( ).
  WRITE: / |Área do círculo: { lv_area }|.
```

---

## 🚀 Próximos Passos

1. Comece por [Classes e Objetos](1_classes_objetos.md)
2. Continue com [Atributos e Métodos](2_atributos_metodos.md)
3. Aprenda sobre [Construtores](3_construtores.md)
4. Explore padrões de design em projetos reais
5. Avance para [ALV OO](../alvs/index.md) para aplicar OO em interfaces

---

**Tags:** `#OO` `#Classes` `#ABAP` `#Programação-Orientada-Objetos`
