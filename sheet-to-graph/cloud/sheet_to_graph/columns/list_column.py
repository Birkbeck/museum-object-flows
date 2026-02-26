from sheet_to_graph import Column


class ListColumn(Column):
    """
    Value can either be a single string or multiple strings separated by ";".
    Field can also be left empty.
    """

    def to_string(self, value):
        return "; ".join(value)

    def _format(self, value):
        return [value.strip() for value in value.split(";")]
