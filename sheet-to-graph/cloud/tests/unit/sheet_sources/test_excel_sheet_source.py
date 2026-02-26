import openpyxl

from sheet_to_graph.sheet_sources.excel_sheet_source import ExcelSheetSource


def test_excel_sheet_source_reads_xlsx_and_skips_empty_rows(tmp_path):
    xlsx_path = tmp_path / "test.xlsx"

    wb = openpyxl.Workbook()
    ws = wb.active
    ws.title = "DataSheet"
    ws.append(["col1", "col2"])
    ws.append([1, 2])
    ws.append([None, None])  # should be filtered out
    wb.save(xlsx_path)

    source = ExcelSheetSource(str(xlsx_path), "DataSheet")
    rows = source.get_rows()

    assert rows == [
        ["col1", "col2"],
        ["1", "2"],
    ]
