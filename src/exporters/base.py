from pathlib import Path
from typing import Protocol

from model import Bible


class Exporter(Protocol):
    def export(self, bible: Bible, path: Path) -> None:
        ...
