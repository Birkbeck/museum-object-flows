from .base import SheetSource


class ExcelSheetSource(SheetSource):
    def __init__(self, filename: str, sheet_name: str):
        self.filename = filename
        self.sheet_name = sheet_name

    def get_rows(self):
        import openpyxl

        workbook = openpyxl.load_workbook(self.filename)
        spreadsheet = workbook[self.sheet_name]
        return [
            ["" if cell is None else str(cell) for cell in row]
            for row in spreadsheet.iter_rows(values_only=True)
            if not all(cell is None for cell in row)
        ]
