from sheet_to_graph.sheet_sources.google_sheet_source import GoogleSheetSource


def test_google_sheet_source_calls_service_and_returns_values():
    """GoogleSheetSource should call the Sheets API and return the values list."""

    expected_spreadsheet_id = "spreadsheet-id-123"
    expected_sheet_name = "Sheet1"
    expected_rows = [["a", "b"], ["c", "d"]]

    # --- Fake Google Sheets API client structure ---

    class FakeRequest:
        def __init__(self, return_values):
            self._return_values = return_values

        def execute(self):
            # Mimic the real API: {'values': [...]}
            return {"values": self._return_values}

    class FakeValues:
        def __init__(self, return_values):
            self._return_values = return_values
            self.calls = []

        def get(self, spreadsheetId, range):
            # Record the call for assertions
            self.calls.append((spreadsheetId, range))
            return FakeRequest(self._return_values)

    class FakeSpreadsheets:
        def __init__(self, values):
            self._values = values

        def values(self):
            return self._values

    class FakeService:
        def __init__(self, spreadsheets):
            self._spreadsheets = spreadsheets

        def spreadsheets(self):
            return self._spreadsheets

    # --- Wire up the fake service ---
    fake_values = FakeValues(expected_rows)
    fake_service = FakeService(FakeSpreadsheets(fake_values))

    source = GoogleSheetSource(
        fake_service,
        spreadsheet_id=expected_spreadsheet_id,
        sheet_name=expected_sheet_name,
    )

    rows = source.get_rows()

    # It should return the "values" from execute()
    assert rows == expected_rows

    # And it should have called the API with the correct parameters
    assert fake_values.calls == [(expected_spreadsheet_id, expected_sheet_name)]


def test_google_sheet_source_returns_empty_list_when_no_values():
    """If the API response has no 'values' key, get_rows() should return []."""

    class FakeRequest:
        def execute(self):
            return {}  # no 'values' key

    class FakeValues:
        def __init__(self):
            self.calls = []

        def get(self, spreadsheetId, range):
            self.calls.append((spreadsheetId, range))
            return FakeRequest()

    class FakeSpreadsheets:
        def __init__(self, values):
            self._values = values

        def values(self):
            return self._values

    class FakeService:
        def __init__(self, spreadsheets):
            self._spreadsheets = spreadsheets

        def spreadsheets(self):
            return self._spreadsheets

    fake_values = FakeValues()
    fake_service = FakeService(FakeSpreadsheets(fake_values))

    source = GoogleSheetSource(fake_service, "sid", "Sheet1!A1:B2")

    rows = source.get_rows()

    assert rows == []
    assert fake_values.calls == [("sid", "Sheet1!A1:B2")]
