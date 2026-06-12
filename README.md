# Bíblias

[![Última atualização](https://img.shields.io/github/last-commit/damarals/biblias?color=blue&label=atualizado&style=for-the-badge)](https://github.com/damarals/biblias/commits)
[![Licença](https://img.shields.io/github/license/damarals/biblias?style=for-the-badge)](LICENSE)

Bíblias em português, prontas para os principais softwares de projeção de igreja, geradas a partir de uma fonte canônica única. O projeto disponibiliza três formatos:

- **SQLite** — para o [OpenLP](https://openlp.org/);
- **Zefania XML** — para o [Quelea](https://quelea.org/) (e outros que importam Zefania);
- **JSON** — formato aberto, útil para qualquer outro uso.

> **Onde baixar:** os arquivos prontos ficam em [**Releases**](https://github.com/damarals/biblias/releases). Baixe o pacote da versão desejada e importe no seu programa.

## Traduções

O catálogo cobre 20 traduções. As de **domínio público** (Bíblia Livre, Almeida 1911, Tradução Brasileira) podem ser redistribuídas livremente; as demais são de suas respectivas editoras.

| Sigla | Versão | Sigla | Versão |
|---|---|---|---|
| ACF | Almeida Corrigida e Fiel | NTLH | Nova Tradução na Linguagem de Hoje |
| ARA | Almeida Revista e Atualizada | NVI | Nova Versão Internacional |
| ARC | Almeida Revista e Corrigida | NVT | Nova Versão Transformadora |
| AS21 | Almeida Século 21 | TB | Tradução Brasileira |
| JFAA | Almeida Atualizada | **BLIVRE** | **Bíblia Livre** (domínio público) |
| KJA | King James Atualizada | **ALM1911** | **Almeida 1911** (domínio público) |
| KJF | King James Fiel | OL | O Livro |
| NAA | Nova Almeida Atualizada | MENS | A Mensagem |
| NBV | Nova Bíblia Viva | NTJud | Novo Testamento Judaico |
| | | VFL · CNBB | Versão Fácil de Ler · Bíblia CNBB |

## Qualidade dos dados

As traduções de fontes comunitárias gratuitas contêm, em alguns versículos, **truncagens** herdadas de um mesmo *upstream* compartilhado (texto cortado no fim). O projeto não esconde isso:

- um **validador** marca os versículos suspeitos por severidade (palavras perdidas, pontuação faltando, agrupamento de versos) e gera uma *worklist* por versão;
- correções feitas à mão são **duráveis** (registradas em `data/corrections/` e protegidas contra sobrescrita).

Para as versões com direito autoral **não existe** fonte gratuita e limpa de outra linhagem, então alguns versículos podem permanecer incompletos — ver a *worklist* da versão. Correções da comunidade são bem-vindas (veja abaixo).

## Para mantenedores e contribuidores

O projeto é um *toolkit* em Python (gerenciado com [uv](https://docs.astral.sh/uv/)). Os arquivos de saída são gerados a partir do canônico em `data/canonical/` (JSON fatiado por livro, versionado).

```bash
uv sync                                   # instala o ambiente

uv run biblias fetch KJA --source bolls   # busca uma versão de uma fonte → canônico
uv run biblias validate                   # valida o canônico e grava as worklists
uv run biblias diff-sources NTLH          # compara a versão entre fontes (a "caça")
uv run biblias build KJA --format zefania,sqlite,json --out dist
```

Fontes disponíveis: `openlp` (SQLite local), `bolls`, `getbible`. O comando `diff-sources` ajuda a escolher, por versão, a fonte mais limpa.

### Como contribuir com uma correção

1. Edite o versículo no JSON canônico em `data/canonical/<VERSÃO>/<LIVRO>.json`.
2. Registre o *ref* corrigido em `data/corrections/<VERSÃO>.json` para protegê-lo.
3. Abra um *pull request*. (Correções como a do `@misaelbr` em Provérbios da KJA entram por aqui.)

---

### Ajude o Projeto com uma Doação

Clique no botão abaixo e patrocine, com qualquer valor pelo *PayPal*, o empenho e a manutenção desse repositório.

[![button-donate-paypal](https://www.paypalobjects.com/pt_BR/i/btn/btn_donate_SM.gif)](https://www.paypal.com/donate/?business=RSNGTLP66QMXU&no_recurring=0&currency_code=BRL)
