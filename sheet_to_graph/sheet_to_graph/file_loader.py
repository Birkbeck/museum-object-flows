import csv
import openpyxl


class FileLoader:
    """This class loads data from the csv/xlsx files specified in a config.
    Initialize with the classmethod from_config_file.
    Use the method get_sheet_as_list_of_lists to load data from the specified sheet_name.
    The sheet_name must match with a sheet pointed to in the config file.
    """

    def __init__(self, values):
        self.values = values

    @classmethod
    def from_config_file(cls, filename: str):
        values = json.load(filename)
        return cls(values)

    def get_sheet_as_list_of_lists(self, sheet_name: str):
        sheet_file_name = self.values["sheets"][sheet_name]["file"]
        if sheet_file_name == "":
            sheet_file_name = self.values["dispersal_sheet_file"]
        sheet_sheet_name = self.values["sheets"][sheet_name]["sheet"]

        is_a_csv_file = sheet_sheet_name == ""

        if is_a_csv_file:
            with open(sheet_file_name, "r", encoding="utf-8-sig") as f:
                rows = list(csv.reader(f, skipinitialspace=True))
        else:  # is an xlsx file
            workbook = openpyxl.load_workbook(sheet_file_name)
            spreadsheet = workbook[sheet_sheet_name]
            rows = [
                ["" if cell is None else str(cell) for cell in row]
                for row in spreadsheet.iter_rows(values_only=True)
                if not all([cell is None for cell in row])
            ]

        return rows
