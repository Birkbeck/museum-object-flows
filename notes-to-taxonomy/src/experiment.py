from abc import ABC, abstractmethod
from typing import Iterable


class Experiment(ABC):
    def __init__(self):
        self.results = []

    def run(self):
        self.results = []
        for configuration in self.parameter_combinations():
            self.results.append(self.run_test(configuration))
        return self.evaluation_summary()

    @abstractmethod
    def parameter_combinations(self) -> Iterable:
        """Returns an iterator of every set of parameters that will be tested."""
        pass

    @abstractmethod
    def run_test(self, configuration: dict) -> dict:
        """Runs one test and returns a dictionary of evaluation statistics."""
        pass

    @abstractmethod
    def evaluation_summary(self):
        """Returns summary statistics of how parameters affect model performance."""
        pass

    @abstractmethod
    def best_configuration(self, peformance_metric: str) -> dict:
        """Returns the parameter config that resulted in the best performance."""
        pass
