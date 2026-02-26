from .base import SheetSource
from .csv_sheet_source import CsvSheetSource
from .excel_sheet_source import ExcelSheetSource
from .google_sheet_source import GoogleSheetSource


def make_sheet_source(sheet_config, *, google_service=None) -> SheetSource:
    backend = sheet_config.get("backend", "csv")

    if backend == "csv":
        return CsvSheetSource(sheet_config["file"])

    if backend == "excel":
        return ExcelSheetSource(sheet_config["file"], sheet_config["sheet"])

    if backend == "google":
        if google_service is None:
            raise ValueError("google_service is required for Google backend")
        return GoogleSheetSource(
            google_service,
            sheet_config["spreadsheet_id"],
            sheet_config["sheet"],
        )

    raise ValueError(f"Unknown backend: {backend}")
