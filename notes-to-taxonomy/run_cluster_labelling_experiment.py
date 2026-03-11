import json

from src.experiments import ClusterLabellingExperiment


def main():
    with open("experiment-configs/cluster-labelling.json", "r") as f:
        config = json.load(f)
    experiment = ClusterLabellingExperiment.from_config(config)
    results = experiment.run()
    print(results)


if __name__ == "__main__":
    main()
