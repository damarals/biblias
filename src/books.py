from dataclasses import dataclass


@dataclass(frozen=True)
class BookRef:
    id: int        # 1..66
    code: str      # USFM, e.g. "GEN"
    name: str      # Portuguese full name
    abbrev: str    # Portuguese abbreviation
    testament: str  # "OT" | "NT"


# (id, USFM code, Portuguese name, Portuguese abbreviation)
_DATA: tuple[tuple[int, str, str, str], ...] = (
    (1, "GEN", "Gênesis", "Gn"),
    (2, "EXO", "Êxodo", "Êx"),
    (3, "LEV", "Levítico", "Lv"),
    (4, "NUM", "Números", "Nm"),
    (5, "DEU", "Deuteronômio", "Dt"),
    (6, "JOS", "Josué", "Js"),
    (7, "JDG", "Juízes", "Jz"),
    (8, "RUT", "Rute", "Rt"),
    (9, "1SA", "1 Samuel", "1Sm"),
    (10, "2SA", "2 Samuel", "2Sm"),
    (11, "1KI", "1 Reis", "1Rs"),
    (12, "2KI", "2 Reis", "2Rs"),
    (13, "1CH", "1 Crônicas", "1Cr"),
    (14, "2CH", "2 Crônicas", "2Cr"),
    (15, "EZR", "Esdras", "Ed"),
    (16, "NEH", "Neemias", "Ne"),
    (17, "EST", "Ester", "Et"),
    (18, "JOB", "Jó", "Jó"),
    (19, "PSA", "Salmos", "Sl"),
    (20, "PRO", "Provérbios", "Pv"),
    (21, "ECC", "Eclesiastes", "Ec"),
    (22, "SNG", "Cânticos", "Ct"),
    (23, "ISA", "Isaías", "Is"),
    (24, "JER", "Jeremias", "Jr"),
    (25, "LAM", "Lamentações de Jeremias", "Lm"),
    (26, "EZK", "Ezequiel", "Ez"),
    (27, "DAN", "Daniel", "Dn"),
    (28, "HOS", "Oséias", "Os"),
    (29, "JOL", "Joel", "Jl"),
    (30, "AMO", "Amós", "Am"),
    (31, "OBA", "Obadias", "Ob"),
    (32, "JON", "Jonas", "Jn"),
    (33, "MIC", "Miquéias", "Mq"),
    (34, "NAM", "Naum", "Na"),
    (35, "HAB", "Habacuque", "Hc"),
    (36, "ZEP", "Sofonias", "Sf"),
    (37, "HAG", "Ageu", "Ag"),
    (38, "ZEC", "Zacarias", "Zc"),
    (39, "MAL", "Malaquias", "Ml"),
    (40, "MAT", "Mateus", "Mt"),
    (41, "MRK", "Marcos", "Mc"),
    (42, "LUK", "Lucas", "Lc"),
    (43, "JHN", "João", "Jo"),
    (44, "ACT", "Atos", "At"),
    (45, "ROM", "Romanos", "Rm"),
    (46, "1CO", "1 Coríntios", "1Co"),
    (47, "2CO", "2 Coríntios", "2Co"),
    (48, "GAL", "Gálatas", "Gl"),
    (49, "EPH", "Efésios", "Ef"),
    (50, "PHP", "Filipenses", "Fp"),
    (51, "COL", "Colossenses", "Cl"),
    (52, "1TH", "1 Tessalonicenses", "1Ts"),
    (53, "2TH", "2 Tessalonicenses", "2Ts"),
    (54, "1TI", "1 Timóteo", "1Tm"),
    (55, "2TI", "2 Timóteo", "2Tm"),
    (56, "TIT", "Tito", "Tt"),
    (57, "PHM", "Filemom", "Fm"),
    (58, "HEB", "Hebreus", "Hb"),
    (59, "JAS", "Tiago", "Tg"),
    (60, "1PE", "1 Pedro", "1Pe"),
    (61, "2PE", "2 Pedro", "2Pe"),
    (62, "1JN", "1 João", "1Jo"),
    (63, "2JN", "2 João", "2Jo"),
    (64, "3JN", "3 João", "3Jo"),
    (65, "JUD", "Judas", "Jd"),
    (66, "REV", "Apocalipse", "Ap"),
)

BOOKS: tuple[BookRef, ...] = tuple(
    BookRef(i, code, name, abbrev, "OT" if i <= 39 else "NT")
    for (i, code, name, abbrev) in _DATA
)

_BY_ID = {b.id: b for b in BOOKS}
_BY_CODE = {b.code: b for b in BOOKS}


def by_id(book_id: int) -> BookRef:
    return _BY_ID[book_id]


def by_code(code: str) -> BookRef:
    return _BY_CODE[code]
