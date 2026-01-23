import json

from src.experiments import TaxonomyBuildingExperiment


def main():
    with open("experiment-configs/taxonomy-building-local.json", "r") as f:
        config = json.load(f)
    experiment = TaxonomyBuildingExperiment.from_config(
        config, save_label_definitions=False  # TODO: set to true before running on HPC
    )
    results = experiment.run()
    print(results)


if __name__ == "__main__":
    main()
