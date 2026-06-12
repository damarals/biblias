from pydantic import BaseModel, ConfigDict, Field


class _Frozen(BaseModel):
    model_config = ConfigDict(frozen=True)


class Verse(_Frozen):
    number: int = Field(ge=1)
    text: str


class Chapter(_Frozen):
    number: int = Field(ge=1)
    verses: list[Verse]


class Book(_Frozen):
    id: int = Field(ge=1, le=66)
    code: str
    name: str
    abbrev: str
    chapters: list[Chapter]


class BibleMeta(_Frozen):
    code: str
    name: str
    year: int | None = None
    publisher: str | None = None
    license: str
    scope: str  # "full" | "nt"
    source: str


class Bible(_Frozen):
    meta: BibleMeta
    books: list[Book]
