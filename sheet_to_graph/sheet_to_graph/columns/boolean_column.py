from sheet_to_graph import Column


class BooleanColumn(Column):
    """
    Validation maps values according to provided mapping.
    If optional is true, an empty string is an acceptable value.
    """

    def _validate(self, value) -> str:
        if value == "" and self.optional:
            return None
        if value.upper() in {"TRUE", "FALSE"}:
            return None
        return f"The value '{value}' is not allowed"

    def _format(self, value) -> str:
        return value.upper() == "TRUE"
