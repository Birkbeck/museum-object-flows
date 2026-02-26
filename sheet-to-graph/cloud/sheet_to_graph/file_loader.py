import copy
import json

from sheet_to_graph.sheet_sources import make_sheet_source


class FileLoader:
    """This class loads data from the csv/xlsx files specified in a config.
    Initialize with the classmethod from_config_file.
    Use the method get_sheet_as_list_of_lists to load data from the specified sheet_name.
    The sheet_name must match with a sheet pointed to in the config file.
    """

    def __init__(self, google_service_factory=None):
        self.google_service_factory = google_service_factory

    def get_csv_as_list_of_lists(self, csv_location: str):
        source = make_sheet_source({"backend": "csv", "file": csv_location})
        return source.get_rows()

    def get_sheet_as_list_of_lists(self, spreadsheet_id: str, sheet_name: str):
        sheet_config = {
            "backend": "google",
            "spreadsheet_id": spreadsheet_id,
            "sheet": sheet_name,
        }
        source = make_sheet_source(
            sheet_config,
            google_service=self.google_service_factory()
            if self.google_service_factory is not None
            else None,
        )
        return source.get_rows()
