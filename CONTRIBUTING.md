# Como Contribuir para o Almanaque ABAP

Este documento fornece um guia para contribuir com o Almanaque ABAP, um projeto que visa criar uma base de conhecimento abrangente para a linguagem de programação ABAP.

## Estrutura do Projeto

O projeto utiliza o `mkdocs` com o tema `material` para gerar um site estático a partir de arquivos Markdown.

*   `mkdocs.yml`: O arquivo de configuração principal do `mkdocs`.
*   `docs/`: O diretório que contém todos os arquivos Markdown que formam o conteúdo do site.

### Adicionando Novas Páginas

Para adicionar uma nova página, crie um novo arquivo `.md` dentro do diretório `docs`. A estrutura de diretórios dentro de `docs` se reflete na navegação do site.

## Markdown e Funcionalidades do `mkdocs`

Utilizamos várias extensões e plugins do Markdown para enriquecer o conteúdo. Aqui estão alguns dos mais importantes:

### Blocos de Código ABAP

Para garantir que o código ABAP seja exibido corretamente e com destaque de sintaxe, utilize a seguinte formatação:

````markdown
```abap
*&--------------------------------------------------------------------*
*& Report Z_MEU_RELATORIO
*&--------------------------------------------------------------------*
REPORT z_meu_relatorio.

WRITE: / 'Olá, Mundo!'.
```
````

### Admonitions (Caixas de Destaque)

Use admonitions para destacar informações importantes. Os tipos disponíveis são: `note`, `info`, `warning`, `danger`, `success`, `question`, `tip`, `example`.

````markdown
!!! note "Nota"
    Esta é uma nota importante.

!!! warning "Atenção"
    Isto requer atenção.
````

### Abas (Tabbed Content)

Use abas para agrupar conteúdos relacionados.

````markdown
=== "Aba 1"
    Conteúdo da aba 1.

=== "Aba 2"
    Conteúdo da aba 2.
````

### Tags

Adicione tags a cada página para facilitar a busca e a organização do conteúdo. As tags são adicionadas no metadados do arquivo Markdown:

````markdown
---
tags:
  - ABAP
  - CDS
  - Performance
---

# Título da Página

Conteúdo da página...
````

## Personalizando a Navegação com `awesome-pages`

O plugin `awesome-pages` permite personalizar a navegação do site usando um arquivo `.pages` em cada diretório.

Exemplo de um arquivo `.pages`:

```yaml
---
title: Minha Seção
nav:
  - "index.md"
  - "outra_pagina.md"
  - ...
---
```

Consulte a documentação do plugin `awesome-pages` para mais detalhes.

## Executando o Site Localmente

Para visualizar suas alterações localmente, execute o seguinte comando na raiz do projeto:

```bash
mkdocs serve
```

Isto iniciará um servidor web local e você poderá acessar o site em `http://127.0.0.1:8000`.
