from dataclasses import dataclass


@dataclass(frozen=True)
class CatalogEntry:
    code: str
    name: str
    year: int | None
    publisher: str | None
    license: str  # "public-domain" | "copyright"
    scope: str    # "full" | "nt"


_ENTRIES: tuple[CatalogEntry, ...] = (
    CatalogEntry("ACF", "Almeida Corrigida e Fiel", 1994, "SBTB", "copyright", "full"),
    CatalogEntry("ARA", "Almeida Revista e Atualizada", 1993, "SBB", "copyright", "full"),
    CatalogEntry("ARC", "Almeida Revista e Corrigida", 1995, "SBB", "copyright", "full"),
    CatalogEntry("AS21", "Almeida Século 21", 2009, "Vida Nova", "copyright", "full"),
    CatalogEntry("JFAA", "Almeida Atualizada", None, None, "copyright", "full"),
    CatalogEntry("KJA", "King James Atualizada", 1999, "Abba Press", "copyright", "full"),
    CatalogEntry("KJF", "King James Fiel", 1611, "BVBooks", "copyright", "full"),
    CatalogEntry("NAA", "Nova Almeida Atualizada", 2017, "SBB", "copyright", "full"),
    CatalogEntry("NBV", "Nova Bíblia Viva", 2007, "Mundo Cristão", "copyright", "full"),
    CatalogEntry("NTLH", "Nova Tradução na Linguagem de Hoje", 1988, "SBB", "copyright", "full"),
    CatalogEntry("NVI", "Nova Versão Internacional", None, "Biblica", "copyright", "full"),
    CatalogEntry("NVT", "Nova Versão Transformadora", 2016, "Mundo Cristão", "copyright", "full"),
    CatalogEntry("TB", "Tradução Brasileira", 2010, "SBB", "public-domain", "full"),
    CatalogEntry("BLIVRE", "Bíblia Livre", 2018, None, "public-domain", "full"),
    CatalogEntry("ALM1911", "Almeida 1911", 1911, None, "public-domain", "full"),
    CatalogEntry("OL", "O Livro", 2000, "Biblica", "copyright", "full"),
    CatalogEntry("MENS", "A Mensagem", 2016, "Editora Vida", "copyright", "full"),
    CatalogEntry("NTJud", "Novo Testamento Judaico", None, "Editora Vida", "copyright", "nt"),
    CatalogEntry("VFL", "Versão Fácil de Ler", 2017, "Bible League International", "copyright", "full"),
    CatalogEntry("CNBB", "Bíblia CNBB", 2018, "Edições CNBB", "copyright", "full"),
)

CATALOG: dict[str, CatalogEntry] = {e.code: e for e in _ENTRIES}


def get(code: str) -> CatalogEntry:
    return CATALOG[code]
