from .base import SheetSource


class GoogleSheetSource(SheetSource):
    def __init__(self, service, spreadsheet_id: str, sheet_name: str):
        self.service = service
        self.spreadsheet_id = spreadsheet_id
        self.sheet_name = sheet_name

    def get_rows(self):
        sheet = self.service.spreadsheets()
        result = (
            sheet.values()
            .get(
                spreadsheetId=self.spreadsheet_id,
                range=self.sheet_name,
            )
            .execute()
        )
        values = result.get("values", [])
        return values
