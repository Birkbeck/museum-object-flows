from sheet_to_graph import Column


class ReferenceColumn(Column):
    """
    Validation ensures values appear in the column being referred to.
    If optional is true, an empty string is an acceptable value.
    """

    def __init__(
        self,
        name: str,
        reference_column: str,
        reference_table: "Table" = None,
        optional: bool = True,
        unique: bool = False,
        fill: bool = False,
        default: str = "",
        primary_key: bool = False,
        type_label: str = None,
        type_label_of: str = None,
        property_of: str = None,
        relation_from: str = None,
        relation_to: str = None,
        ignore: bool = False,
    ):
        super().__init__(
            name,
            unique,
            optional,
            fill,
            default,
            primary_key=primary_key,
            type_label=type_label,
            type_label_of=type_label_of,
            property_of=property_of,
            relation_from=relation_from,
            relation_to=relation_to,
            ignore=ignore,
        )
        self.reference_column_name = reference_column
        self._reference_table = reference_table

    @property
    def reference_table(self):
        return (
            self._reference_table
            if self._reference_table is not None
            else self.parent_table
        )

    def _validate(self, value) -> str:
        if value == "" and self.optional:
            return None

    def _validate_entire_column(self) -> str:
        missing_values = set()
        for value in self:
            if value != "" and value not in self.reference_column:
                missing_values.add(value)
        if len(missing_values) == 0:
            return None
        missing_values = ", ".join(missing_values)
        return (
            f"Column {self.reference_table.name}->{self.reference_column_name}"
            + f" does not contain values {missing_values}"
        )
