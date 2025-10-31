# Tipos de ALV em ABAP

Esta secção aborda os diferentes tipos de ALV (ABAP List Viewer) disponíveis em ABAP para apresentação de dados.

---

## 📊 O que é ALV?

ALV (ABAP List Viewer) é uma ferramenta SAP para exibir dados de forma estruturada com funcionalidades automáticas como:
- Ordenação
- Filtros
- Exportação (Excel, PDF)
- Totalizações
- Layout personalizável

---

## 🎯 Tipos de ALV

### 1. **ALV Clássico (Function Modules)**
- `REUSE_ALV_GRID_DISPLAY`
- `REUSE_ALV_LIST_DISPLAY`
- `REUSE_ALV_HIERSEQ_LIST_DISPLAY`

### 2. **ALV Orientado a Objetos**
- `CL_SALV_TABLE` (simples e completo)
- `CL_GUI_ALV_GRID` (com container)

### 3. **ALV Tree**
- Apresentação hierárquica de dados

### 4. **ALV Popup**
- Janelas modais com dados

### 5. **ALV em Fullscreen**
- Ocupação total do ecrã

---

**Índice de Exemplos:**
1. [ALV Clássico Grid](alv_classico_grid.md)
2. [ALV Clássico List](alv_classico_list.md)
3. [ALV OO com CL_SALV_TABLE](alv_oo_salv_table.md)
4. [ALV OO com CL_GUI_ALV_GRID](alv_oo_gui_grid.md)
5. [ALV Tree Hierárquico](alv_tree.md)
6. [ALV Popup](alv_popup.md)
7. [ALV com Edição](alv_editavel.md)
8. [ALV com Eventos](alv_eventos.md)
9. [ALV com Botões Customizados](alv_botoes_custom.md)
10. [ALV com Layout e Variantes](alv_layout_variantes.md)
