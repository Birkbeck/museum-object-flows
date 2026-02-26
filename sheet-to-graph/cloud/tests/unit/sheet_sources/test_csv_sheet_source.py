from sheet_to_graph.sheet_sources.csv_sheet_source import CsvSheetSource


def test_csv_sheet_source_reads_csv_as_list_of_lists(tmp_path):
    csv_path = tmp_path / "test.csv"
    csv_contents = "col1,col2\n1,2\n3,4\n"
    csv_path.write_text(csv_contents, encoding="utf-8")

    source = CsvSheetSource(str(csv_path))
    rows = source.get_rows()

    assert rows == [
        ["col1", "col2"],
        ["1", "2"],
        ["3", "4"],
    ]
