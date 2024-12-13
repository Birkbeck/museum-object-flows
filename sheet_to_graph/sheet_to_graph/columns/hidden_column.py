from sheet_to_graph import Column


class OptionalColumn(Column):
    """
    A column which can be but does not have to be in the data source.
    """

    def __init__(
        self,
        name: str,
    ):
        self.name = name

    def _validate(self, value) -> str:
        pass

    def _format(self, value) -> str:
        pass
