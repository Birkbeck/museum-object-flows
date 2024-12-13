class Column:
    """
    This class defines a column for validation.
    If unique is True, every row must have a unique value.
    If fill is True, rows with no value inherit the value from the previous row.
    The default value replaces empty strings.

    To implement a column type, implement the private methods _validate() for row-wise validation
    and _validate_entire_column() for column-wise validation.
    Implement the private method _format() to determine automatic changes to values stored.
    """

    def __init__(
        self,
        name: str,
        unique: bool = False,
        optional: bool = True,
        fill: bool = False,
        default: str = "",
        # the below properties are used in the table-to-neo4j mapping
        # they define which element of the graph database the column contents represent
        primary_key: bool = False,  # this column denotes a new node
        type_label: str = None,  # specify the type label for this node or relation
        # labels, properties, relations must point to a column that is a primary key
        type_label_of: str = None,  # specify the column of the node or relation this is the label of
        property_of: str = None,  # specify the column of the node or relation this is a property of
        relation_from: str = None,  # specify the column of the node the relation starts from
        relation_to: str = None,  # specify the column of the node the relation ends at
        reference_table: "Table" = None,  # specify the table where this column's contents are defined
        reference_column: str = None,  # specify the column where this column's contents are defined
        ignore: bool = False,  # don't include in graph database
    ):
        self.name = name
        self.unique = unique
        self.optional = optional
        self.fill = fill
        self._parent_table = None
        self.default = default

        if (
            len(
                [
                    test_if_true
                    for test_if_true in [
                        primary_key,
                        type_label_of is not None,
                        property_of is not None,
                        relation_from is not None,
                        relation_to is not None,
                    ]
                    if test_if_true
                ]
            )
            > 1
        ):
            raise Exception(
                "A column can only play one role in the graph database "
                + "(primary key, type_label_of, property_of, relation_from, relation_to)"
            )
        if (
            any([primary_key, relation_from is not None, relation_to is not None])
            and type_label is None
        ):
            raise Exception(
                "If a column is a primary key or a relation, it must have a type label"
            )

        self.primary_key = primary_key
        self.type_label = type_label
        self.type_label_of = type_label_of
        self.property_of = property_of
        self.relation_from = relation_from
        self.relation_to = relation_to
        self._reference_table = reference_table
        self.reference_column_name = reference_column
        self.ignore = ignore

    def __iter__(self):
        return (row[self.name] for row in self.parent_table.rows)

    @property
    def reference_table(self):
        return (
            self._reference_table
            if self._reference_table is not None
            else self.parent_table
        )

    @property
    def reference_column(self):
        return self.reference_table.columns[self.reference_column_name]

    @property
    def values(self) -> list:
        return [row[self.name] for row in self.parent_table.rows]

    @property
    def parent_table(self):
        return self._parent_table

    @parent_table.setter
    def parent_table(self, table):
        self._parent_table = table

    def as_dict(self):
        return {self.name: self}

    def to_string(self, value):
        return str(value)

    def validate(self, value) -> str:
        """Returns an error message if value fails validation and cannot be added to the column."""
        clean_value = value.strip()
        validation_error = self._validate(clean_value)
        if validation_error is not None:
            return f"Column [{self.name}] {validation_error}"
        if (self.unique and not self.optional) or (self.unique and clean_value != ""):
            if clean_value in self.values:
                return f"Column [{self.name}] must be unique, but '{value}' already appears above."

    def validate_entire_column(self) -> str:
        """Returns an error message if there is an error in the column."""
        return self._validate_entire_column()

    def format(self, value):
        """Returns the value as it should be stored in the table."""
        clean_value = value.strip()
        return self._format(clean_value)

    def format_as_dict(self, value):
        """Returns a dict {column-name: cell-value} with correct format for cell value."""
        return {self.name: self.format(value)}

    def _validate(self, value) -> str:
        """Returns an error message if value fails data-type specific validation."""
        pass

    def _validate_entire_column(self):
        """Returns an error message if the column fails data-type specific validation."""
        pass

    def _format(self, value):
        """Returns value in correct format for storage."""
        return value if value != "" else self.default
