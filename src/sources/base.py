from typing import Protocol

from model import Bible


class SourceAdapter(Protocol):
    def fetch(self, version: str) -> Bible:
        ...
