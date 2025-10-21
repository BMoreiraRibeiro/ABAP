# Ecrã de Seleção (Selection Screen)

Permite criar uma interface simples de entrada de parâmetros.

---

### 🔹 Exemplo

```abap
REPORT z_demo_select.

PARAMETERS p_carr TYPE s_carr_id DEFAULT 'LH'.
SELECT-OPTIONS so_price FOR sflight-price.

SELECT * FROM sflight
  INTO TABLE @DATA(lt)
  WHERE carrid = @p_carr AND price IN @so_price.

WRITE: / lines( lt ), 'registos encontrados'.
```
