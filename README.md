
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Pacote Bíblias

<!-- badges: start -->

[![R-CMD-check](https://img.shields.io/github/workflow/status/damarals/biblias/R-CMD-check?style=for-the-badge)](https://github.com/damarals/biblias/actions/workflows/R-CMD-check.yaml)
[![GitHub latest
commit](https://img.shields.io/github/last-commit/damarals/biblias?color=blue&label=atualizado&style=for-the-badge)](https://GitHub.com/damarals/biblias/commit/)
<!-- badges: end -->

O objetivo deste pacote é disponibilizar várias biblias em português e
em diferentes formatos:

-   *USX*: Arquivo XML utilizado por padrão para armazenamento das
    biblias em programas de apresentação em Igrejas, como o
    [Propresenter](https://renewedvision.com/propresenter/);
-   *SQLite*: Arquivo SQL utilizado por padrão para armazenamento das
    biblias em programas de apresentação em Igrejas, geralmente de
    código livre, como o [OpenLP](https://openlp.org/);
-   *JSON*: Arquivo JSON utilizado nesse projeto para conversão de
    arquivos SQLite em USX, mas que pode ser utilizado para outros fins.

## Traduções disponíveis

Atualmente, o projeto conta com `13` Bíblias disponíveis.

Na tabela abaixo encontram-se algumas informações e estatísticas das
versões disponíveis no projeto, assim como seus respectivos links para
*download*.

| **Biblias em Português**                | **Sigla** | **Ano** |   **Editora**    | **Versículos** | **Baixar**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|-----------------------------------------|:---------:|:-------:|:----------------:|:--------------:|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Almeida Corrigida e Fiel                |    ACF    |  1994   |       SBTB       |     31102      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/ACF.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/ACF.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/ACF.json?raw=true)    |
| Almeida Revista e Atualizada            |    ARA    |  1993   |       SBB        |     31104      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/ARA.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/ARA.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/ARA.json?raw=true)    |
| Almeida Revista e Corrigida             |    ARC    |  1995   |       SBB        |     31105      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/ARC.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/ARC.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/ARC.json?raw=true)    |
| Almeida Século XXI                      |   AS21    |  2009   |   Vida<br>Nova   |     31104      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/AS21.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/AS21.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/AS21.json?raw=true) |
| Almeida Atualizada \*                   |   JFAA    |         |                  |     31104      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/JFAA.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/JFAA.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/JFAA.json?raw=true) |
| King James Atualizada                   |    KJA    |  1999   |  Abba<br>Press   |     31102      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/KJA.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/KJA.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/KJA.json?raw=true)    |
| King James Fiel                         |    KJF    |  1611   |     BVBooks      |     31102      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/KJF.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/KJF.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/KJF.json?raw=true)    |
| Nova Almeida Atualizada                 |    NAA    |  2017   |       SBB        |     31105      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/NAA.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/NAA.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/NAA.json?raw=true)    |
| Nova Bíblia Viva                        |    NBV    |  2007   | Mundo<br>Cristão |     31105      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/NBV.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/NBV.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/NBV.json?raw=true)    |
| Nova Tradução na Linguagem de Hoje \*\* |   NTLH    |  1988   |       SBB        |     31103      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/NTLH.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/NTLH.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/NTLH.json?raw=true) |
| Nova Versão Internacional               |    NVI    |         |                  |                | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/NVI.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/NVI.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/NVI.json?raw=true)    |
| Nova Versão Transformadora              |    NVT    |  2016   | Mundo<br>Cristão |     31102      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/NVT.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/NVT.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/NVT.json?raw=true)    |
| Tradução Brasileira                     |    TB     |  2010   |       SBB        |     31100      | [![custom-badge-usx](https://custom-icon-badges.herokuapp.com/badge/Baixar-USX-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/TB.zip?raw=true)<br>[![custom-badge-sqlite](https://custom-icon-badges.herokuapp.com/badge/Baixar-SQLite-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/sql/TB.sqlite?raw=true)<br>[![custom-badge-json](https://custom-icon-badges.herokuapp.com/badge/Baixar-JSON-F25278?style=for-the-badge&logo=download&logoColor=white)](https://github.com/damarals/biblias/blob/master/inst/json/TB.json?raw=true)       |

<span style="font-size: 0.75rem"> \* Esta versão da (JFAA) não é a de
1987. Pode ser lida
[on-line](https://www.bibliatodo.com/pt/a-biblia/versao/Joao-ferreira-de-almeida-atualizada)<br>
\*\* A versão NTLH possui [versículos
agrupados](http://altamiro.comunidades.net/agrupados-na-ntlh). </span>

------------------------------------------------------------------------

### Uso com o `ProPresenter`

Caso queira utilizar as bíblias acima com o ProPresenter, disponibilizo
neste
[link](https://github.com/damarals/biblias/blob/master/inst/usx/traducao/biblias-propresenter.zip?raw=true)
a pasta com todas as 13 bíblias do projeto no formato aceito pelo
programa. Essa pasta deverá ser extraída na seguinte localização em seu
computador: `C:\ProgramData\RenewedVision\ProPresenter\Bibles`.

**Atenção**: Qualquer outra bíblia instalada anteriormente será
removida.

------------------------------------------------------------------------

### Ajude o Projeto com uma Doação

Clique no botão abaixo e patrocine, com qualquer valor pelo *PayPal*, o
empenho e a manutenção desse repositório.

[![button-donate-paypal](https://www.paypalobjects.com/pt_BR/i/btn/btn_donate_SM.gif)](https://www.paypal.com/donate/?business=RSNGTLP66QMXU&no_recurring=0&currency_code=BRL)
