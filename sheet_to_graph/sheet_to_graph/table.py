from sheet_to_graph.columns import FormulaColumn, OptionalColumn
from sheet_to_graph import FilePreprocessor


class Table:
    """A Table object specifies a schema for data and its translation into a graph.
    Data is loaded from an Excel spreadsheet or from csv.
    Data is validated according to column constraints and table-wide rules.
    Data is inferred in formula columns or according to inference rules.

    Initialize with:
    - name: the name of the table
    - columns: a list of sheet_to_graph.Column objects
    - error_rules: a list of rules that produce an error if their validate method fails.
    - warning_rules: a list of rules that produce a warning if their validate method fails.
    - inference_rules: a list of rules that alter the data in the columns.
    """

    def __init__(
        self,
        name: str,
        columns: list,
        error_rules: list = None,
        warning_rules: list = None,
        inference_rules: list = None,
    ):
        self.name = name
        self.columns_list = columns
        self.columns = {}
        for column in columns:
            self.columns |= column.as_dict()
        self.rows = []
        self.error_rules = [] if error_rules is None else error_rules
        self.warning_rules = [] if warning_rules is None else warning_rules
        self.inference_rules = [] if inference_rules is None else inference_rules
        self.validation_errors = []
        self.validation_warnings = []
        self.size = 0
        self.data_source_columns = {
            column.name: column
            for column in columns
            if not isinstance(column, FormulaColumn)
        }
        self.calculated_columns_ordered = [
            column.name for column in columns if isinstance(column, FormulaColumn)
        ]
        self.calculated_columns = {
            column.name: column
            for column in columns
            if isinstance(column, FormulaColumn)
        }
        for column in columns:
            column.parent_table = self

    def __iter__(self):
        return (r for r in self.rows)

    def __getitem__(self, index: int):
        return self.rows[index]

    def get_column_values(self, column_name: str) -> list:
        return [r[column_name] for r in self.rows]

    def import_from_list_of_dicts(self, rows: list):
        header = rows[0].keys()
        self._raise_exception_if_columns_missing_from_file(header)
        for row in rows:
            row = {
                k: v
                for k, v in row.items()
                if k in self.data_source_columns or k == "row_number"
            }
            self._validate_row(row)
            self._add_row(row)
        for column_name, column in self.columns.items():
            column_error = column.validate_entire_column()
            if column_error is not None:
                self.validation_errors.append(f"Column [{column_name}] {column_error}")

    def import_from_list_of_lists(
        self,
        rows: list,
        header_mapping: dict = None,
        preprocessor: FilePreprocessor = None,
    ):
        preprocessor = FilePreprocessor() if preprocessor is None else preprocessor
        preprocessed_rows = preprocessor.preprocess(rows, header_mapping=header_mapping)
        header = preprocessed_rows[0].keys()
        self._raise_exception_if_columns_missing_from_file(header, header_mapping)
        for row in preprocessed_rows:
            row = {
                k: v
                for k, v in row.items()
                if k in self.data_source_columns or k == "row_number"
            }
            self._validate_row(row)
            self._add_row(row)
        for column_name, column in self.columns.items():
            column_error = column.validate_entire_column()
            if column_error is not None:
                self.validation_errors.append(f"Column [{column_name}] {column_error}")

    def filter(self, **terms) -> list:
        return [
            row for row in self.rows if all([row[k] == v for k, v in terms.items()])
        ]

    def remove_duplicates(self, keep_blank_rows: bool = False) -> "Table":
        """
        Removes rows containing duplicated values in all columns except unique columns.
        If keep_blank_rows is True then rows where all columns (not including unique columns)
        are empty are not removed.
        """
        new_rows = []
        for row in self.rows:
            if "longitude" not in row:
                print(row)
            if keep_blank_rows and all(
                [
                    row[column.name] == ""
                    for column in self.columns_list
                    if not column.unique
                ]
            ):
                new_rows.append(row)
            elif all(
                [
                    any(
                        [
                            row[column.name] != new_row[column.name]
                            for column in self.columns_list
                            if not column.unique
                        ]
                    )
                    for new_row in new_rows
                ]
            ):
                new_rows.append(row)
        self.rows = new_rows
        self.size = len(new_rows)

    def _raise_exception_if_columns_missing_from_file(
        self, raw_header: list, header_mapping: dict = None
    ):
        header = (
            raw_header
            if header_mapping is None
            else [header_mapping[h] if h in header_mapping else h for h in raw_header]
        )
        missing_columns = [
            c
            for c in self.data_source_columns
            if c not in header
            and not isinstance(self.data_source_columns[c], OptionalColumn)
        ]
        if len(missing_columns) > 0:
            missing_column_names = ", ".join(missing_columns)
            raise Exception(
                f"{self.name}: columns missing from file: {missing_column_names}"
            )

    def _validate_row(self, row: dict):
        row_number = row["row_number"] if "row_number" in row else None
        for key, value in row.items():
            if key == "row_number":
                continue
            validation_error = None
            try:
                validation_error = self.data_source_columns[key].validate(value)
            except KeyError:
                self.validation_warnings.append(
                    f"Row [{row_number}] Values from column {key} ignored"
                )
            if validation_error is not None:
                self.validation_errors.append(f"Row [{row_number}] {validation_error}")
        for rule in self.error_rules:
            validation_error = rule.validate(self._clean_row(row))
            if validation_error is not None:
                self.validation_errors.append(f"Row [{row_number}] {validation_error}")
        for rule in self.warning_rules:
            validation_warning = rule.validate(self._clean_row(row))
            if validation_warning is not None:
                self.validation_warnings.append(
                    f"Row [{row_number}] {validation_warning}"
                )

    def _add_row(self, row: dict):
        row_number = row["row_number"] if "row_number" in row else self.size
        row_index = self.size
        clean_row = self._clean_row(row)
        for column in clean_row:
            if clean_row[column] == "" and self.columns[column].fill and self.size > 0:
                clean_row[column] = self.rows[-1][column]
        self.rows.append(clean_row)
        self.size += 1
        for column_name, column in self.columns.items():
            if isinstance(column, OptionalColumn) and column_name not in clean_row:
                clean_row[column_name] = ""
        for column_name in self.calculated_columns_ordered:
            try:
                clean_row[column_name] = self.columns[column_name].formula(
                    self, row_index
                )
            except Exception as e:
                self.validation_errors.append(
                    f"Row [{row_number}] Failed to infer {column_name}, "
                    + "check required details are present"
                )
        for rule in self.inference_rules:
            clean_row = rule.make_inference(clean_row)

    def _clean_row(self, row: dict):
        clean_row = {}
        for k, v in row.items():
            if k == "row_number":
                clean_row |= {k: v}
            else:
                clean_row |= self.data_source_columns[k].format_as_dict(v)
        return clean_row
