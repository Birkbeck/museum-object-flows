class Rule:
    """Rules are used to validate entire rows of data"""

    def validate(self, row) -> str:
        raise NotImplementedError
