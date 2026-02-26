import csv
import io
import urllib.request
from urllib.parse import urlparse

from .base import SheetSource


class CsvSheetSource(SheetSource):
    def __init__(self, filename: str):
        self.filename = filename

    def _is_http_url(self, s: str) -> bool:
        try:
            p = urlparse(s)
            return p.scheme in ("http", "https")
        except Exception:
            return False

    def get_rows(self):
        if self._is_http_url(self.filename):
            with urllib.request.urlopen(self.filename) as resp:
                # decode bytes -> text, handle UTF-8 BOM via utf-8-sig
                text = resp.read().decode("utf-8-sig")
            f = io.StringIO(text)
            return list(csv.reader(f, skipinitialspace=True))

        with open(self.filename, "r", encoding="utf-8-sig") as f:
            return list(csv.reader(f, skipinitialspace=True))
