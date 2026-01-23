from abc import ABC, abstractmethod
from typing import Dict, Iterable, List


class Experiment(ABC):
    def __init__(self):
        self.results: List[Dict] = []

    def run(self):
        for configuration in self.parameter_combinations():
            self.results.append(self.run_test(configuration))
        return self.evaluation_summary()

    def best_configuration(self, performance_metric: str) -> dict:
        """Returns the parameter config that resulted in the best performance."""
        best_score = 0
        best_configuration = {}
        for result in self.results:
            if result[performance_metric] > best_score:
                best_score = result[performance_metric]
                best_configuration = result
        return best_configuration

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
