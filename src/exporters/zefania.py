import xml.etree.ElementTree as ET
from pathlib import Path

from model import Bible


class ZefaniaExporter:
    """Serialises a canonical Bible to Zefania XML (zef2005 structure)."""

    def export(self, bible: Bible, path: Path) -> None:
        root = ET.Element("XMLBIBLE", {"biblename": bible.meta.name, "type": "x-bible"})

        info = ET.SubElement(root, "INFORMATION")
        ET.SubElement(info, "title").text = bible.meta.name
        ET.SubElement(info, "language").text = "PT"
        if bible.meta.publisher:
            ET.SubElement(info, "publisher").text = bible.meta.publisher
        ET.SubElement(info, "rights").text = bible.meta.license

        for book in bible.books:
            be = ET.SubElement(root, "BIBLEBOOK", {
                "bnumber": str(book.id), "bname": book.name, "bsname": book.abbrev,
            })
            for chapter in book.chapters:
                ce = ET.SubElement(be, "CHAPTER", {"cnumber": str(chapter.number)})
                for verse in chapter.verses:
                    ve = ET.SubElement(ce, "VERS", {"vnumber": str(verse.number)})
                    ve.text = verse.text

        tree = ET.ElementTree(root)
        ET.indent(tree)
        path.parent.mkdir(parents=True, exist_ok=True)
        tree.write(path, encoding="utf-8", xml_declaration=True)
