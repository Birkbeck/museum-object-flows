import json

from src import TaxonomyJudge


def main():
    with open("experiment-configs/taxonomy-judging-local.json", "r") as f:
        config = json.load(f)
    judge = TaxonomyJudge.from_config(config)
    with open("data/candidate-taxonomies.json", "r") as f:
        taxonomies = json.load(f)
    print(taxonomies)
    judgements = judge.rank_taxonomies(taxonomies)
    print(judgements)


if __name__ == "__main__":
    main()
