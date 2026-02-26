import edtf

from sheet_to_graph import Column


class ExtendedDateTimeColumn(Column):
    """
    Validation ensures values conform to Library of Congress Extended Date/Time Format
    https://www.loc.gov/standards/datetime/
    Field can also be left empty.
    """

    def _validate(self, value) -> str:
        return None
        if value == "":
            return None
        try:
            edtf.parse_edtf(value)
        except edtf.parser.edtf_exceptions.EDTFParseException:
            return f"Date '{value}' does not conform to LoC Extended Date Time Format"
