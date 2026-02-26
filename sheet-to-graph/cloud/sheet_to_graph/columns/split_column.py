from sheet_to_graph import Column


class SplitColumn(Column):
    """
    Column contains multiple sub-columns with their own validation rules.
    """

    def __init__(
        self,
        name: str,
        sub_columns: list,
        split_on: str = None,
        split_before: str = None,
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
        ignore: bool = True,
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
        self.sub_columns = sub_columns
        if len([s for s in [split_before, split_on] if s is not None]) != 1:
            raise Exception("You must provide one way to split the column")
        self.split_on = split_on
        self.split_before = split_before

    @property
    def parent_table(self):
        return self._parent_table

    @parent_table.setter
    def parent_table(self, table):
        self._parent_table = table
        for sub_col in self.sub_columns:
            sub_col.parent_table = table

    def as_dict(self) -> dict:
        return {self.name: self} | {
            sub_column.name: sub_column for sub_column in self.sub_columns
        }

    def _validate(self, value) -> str:
        values = self._split_value(value)
        validation_messages = [
            column._validate(cell) for column, cell in zip(self.sub_columns, values)
        ]
        if all(message is None for message in validation_messages):
            return None
        else:
            return "; ".join(
                [message for message in validation_messages if message is not None]
            )

    def format_as_dict(self, value):
        values = self._split_value(value)
        while len(values) < len(self.sub_columns):
            values.append("")
        return {self.name: value} | {
            column.name: column.format(cell)
            for column, cell in zip(self.sub_columns, values)
        }

    def _split_value(self, value) -> list:
        if self.split_before is not None:
            return [
                v if index == 0 else self.split_before + v
                for index, v in enumerate(value.split(self.split_before))
            ]
        else:
            return value.split(self.split_on)
