from sheet_to_graph import Column


class FormulaColumn(Column):
    """
    Calculates the value for cells in the column according to a formula supplied as a callable.
    Formula is a function with 2 arguments: table and row_index.
    """

    def __init__(
        self,
        name: str,
        formula: callable,
        unique: bool = False,
        primary_key: bool = False,
        type_label: str = None,
        type_label_of: str = None,
        property_of: str = None,
        relation_from: str = None,
        relation_to: str = None,
        reference_table: "Table" = None,
        reference_column: str = None,
        ignore: bool = False,
    ):
        super().__init__(
            name,
            unique,
            primary_key=primary_key,
            type_label=type_label,
            type_label_of=type_label_of,
            property_of=property_of,
            relation_from=relation_from,
            relation_to=relation_to,
            reference_table=reference_table,
            reference_column=reference_column,
            ignore=ignore,
        )
        self.formula = formula

    def _validate(self, value) -> str:
        pass

    def _format(self, value) -> str:
        pass
