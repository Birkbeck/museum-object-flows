from sheet_to_graph import Column


class DateRangeColumn(Column):
    """
    Validation ensures values are dates or date ranges.
    Value can either be a single date or two dates separated by ":".
    Each date can be in format yyyy-mm-dd, yyyy-mm, or yyyy.
    Field can also be left empty.
    """

    def _validate(self, value) -> str:
        if value == "":
            return None
        for character in value:
            if character not in "0123456789-:":
                return (
                    "Date range must contain only numbers, dashes and a colon "
                    + "e.g. yyyy:yyyy, yyyy-mm-dd, yyyy-mm, or yyyy"
                )
        dates = value.split(":")
        if len(dates) > 2:
            return f"Date range {value} contains too many dates"
        if len(dates) == 0:
            return None
        for date in dates:
            date_components = date.split("-")
            if len(date_components[0]) != 4:
                return (
                    f"Date year must be 4 digits long (dates are formatted yyyy-mm-dd)."
                )
            try:
                int(date_components[0])
            except ValueError:
                return f"Date year must be an integer."
            try:
                if int(date_components[1]) < 1 or int(date_components[1]) > 12:
                    return f"Date month must be between 1 and 12 (dates are formatted yyyy-mm-dd)."
            except ValueError:
                return f"Date month must be an integer."
            except IndexError:
                pass
            try:
                if int(date_components[2]) < 1 or int(date_components[2]) > 31:
                    return f"Date day must be between 1 and 31 (dates are formatted yyyy-mm-dd)."
            except ValueError:
                return f"Date day must be an integer."
            except IndexError:
                pass
