from src import LabelDefiner


class LabelDefinerNote(LabelDefiner):
    def __init__(self):
        pass

    def get_label_definition(self, label, note):
        return f"{label}. {note}"
