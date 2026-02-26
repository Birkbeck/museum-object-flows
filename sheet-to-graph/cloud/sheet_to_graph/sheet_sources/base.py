from abc import ABC, abstractmethod


class SheetSource(ABC):
    @abstractmethod
    def get_rows(self):
        pass
