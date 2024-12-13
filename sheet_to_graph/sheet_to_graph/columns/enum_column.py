from sheet_to_graph import Column


class EnumColumn(Column):
    """
    Validation maps values according to provided mapping.
    If optional is true, an empty string is an acceptable value.
    """

    def __init__(
        self,
        name: str,
        mapping: dict,
        unique: bool = False,
        optional: bool = True,
        fill: bool = False,
        default: str = "",
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
            optional,
            fill,
            default,
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
        self.mapping = mapping

    def _validate(self, value) -> str:
        if value == "" and self.optional:
            return None
        if value in self.mapping:
            return None
        return f"The value '{value}' is not allowed"

    def _format(self, value) -> str:
        try:
            return self.mapping[value]
        except KeyError:
            return self.default
