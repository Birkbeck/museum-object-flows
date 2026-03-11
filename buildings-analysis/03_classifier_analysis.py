import os
import statistics

import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
from statsmodels.stats.anova import anova_lm


results_directory = "results6"


def evaluate_experiment(file_name):
    experiment_parameters = file_name.split("-")
    model_name = experiment_parameters[0]
    role_name = experiment_parameters[1]
    task_name = experiment_parameters[2]
    number_of_shots = experiment_parameters[3]
    temperature = experiment_parameters[4]
    results = pd.read_csv(file_name)
    mean_status_similarity = results["status_similarity"].mean()
    std_status_similarity = results["status_similarity"].std()
    mean_use_similarity = results["use_similarity"].mean()
    std_use_similarity = results["use_similarity"].std()
    mean_responsibility_similarity = results["responsibility_similarity"].mean()
    std_responsibility_similarity = results["responsibility_similarity"].std()
    overall_mean_similarity = results["mean_similarity"].mean()
    median_status_similarity = results["status_similarity"].median()
    median_use_similarity = results["use_similarity"].median()
    median_responsibility_similarity = results["responsibility_similarity"].median()

    return pd.DataFrame(
        [
            {
                # "model_name": model_name,
                "role_name": role_name,
                "task_name": task_name,
                "number_of_shots": number_of_shots,
                "temperature": temperature,
                "mean_status_similarity": mean_status_similarity,
                "std_status_similarity": std_status_similarity,
                "mean_use_similarity": mean_use_similarity,
                "std_use_similarity": std_use_similarity,
                "mean_responsibility_similarity": mean_responsibility_similarity,
                "std_responsibility_similarity": std_responsibility_similarity,
                "overall_mean_similarity": overall_mean_similarity,
                # "median_status_similarity": median_status_similarity,
                # "median_use_similarity": median_use_similarity,
                # "median_responsibility_similarity": median_responsibility_similarity,
            }
        ]
    ).round(3)


if __name__ == "__main__":
    results_data_frame = pd.DataFrame(
        columns=[
            # "model_name",
            "role_name",
            "task_name",
            "number_of_shots",
            "temperature",
            "mean_status_similarity",
            "std_status_similarity",
            "mean_use_similarity",
            "std_use_similarity",
            "mean_responsibility_similarity",
            "std_responsibility_similarity",
            "overall_mean_similarity",
            # "median_status_similarity",
            # "median_use_similarity",
            # "median_responsibility_similarity",
        ]
    )
    results_files = os.listdir(results_directory)
    for results_file in results_files:
        print(results_file)
        results_data_frame = pd.concat(
            [
                results_data_frame,
                evaluate_experiment(f"{results_directory}/{results_file}"),
            ]
        )

    results_data_frame["temperature"] = results_data_frame["temperature"].map(
        {
            "t0.01.csv": 0.01,
            "t0.05.csv": 0.05,
            "t0.1.csv": 0.1,
            "t0.5.csv": 0.5,
            "t1.0.csv": 1.0,
        }
    )
    results_data_frame["number_of_shots"] = results_data_frame["number_of_shots"].map(
        {
            "0shots": 0,
            "1shots": 1,
            "2shots": 2,
            "3shots": 3,
            "5shots": 5,
        }
    )
    results_data_frame["task_name"] = results_data_frame["task_name"].map(
        {
            "all": "short",
            "longer": "medium",
            "extended": "long",
        }
    )

    results_data_frame.to_csv("classifier_results_summary5.csv", index=False)

    # row with largest overall mean similarity
    best_row = results_data_frame.loc[
        results_data_frame["overall_mean_similarity"].idxmax()
    ]
    print("Best overall mean similarity:")
    print(best_row)

    model = smf.ols(
        "overall_mean_similarity ~ C(role_name) + C(task_name) + C(temperature)",
        data=results_data_frame[results_data_frame["number_of_shots"] == 5],
    ).fit()

    print(model.summary())

    print(anova_lm(model, typ=2))

    AXIS_MARGINS = dict(left=0.22, right=0.98, top=0.88, bottom=0.22)

    # plot box and whisker diagram of overall mean similarity by number of shots
    fig, ax = plt.subplots(figsize=(10, 4))
    fig.subplots_adjust(**AXIS_MARGINS)
    results_data_frame.boxplot(
        column="overall_mean_similarity",
        by="number_of_shots",
        vert=False,
        ax=ax,
    )
    ax.set_title(
        "Similarity of LLM and human labels by number of shots in prompt", fontsize=16
    )
    fig.suptitle("")
    ax.set_xlabel("Mean cosine similarity", fontsize=14)
    ax.set_ylabel("Number of shots", fontsize=14)
    ax.tick_params(axis="both", labelsize=12)
    ax.grid(False)
    ax.set_xlim(left=0, right=0.6)
    fig.savefig("plots/overall_mean_similarity_by_number_of_shots.png")
    plt.close(fig)

    # plot box and whisker diagram of overall mean similarity by temperature
    fig, ax = plt.subplots(figsize=(10, 4))
    fig.subplots_adjust(**AXIS_MARGINS)
    results_data_frame[results_data_frame["number_of_shots"] == 5].boxplot(
        column="overall_mean_similarity", by="temperature", vert=False, ax=ax
    )
    ax.set_title(
        "Similarity of LLM and human labels by decoding temperature (5-shot prompts)",
        fontsize=16,
    )
    fig.suptitle("")
    ax.set_xlabel("Mean cosine similarity", fontsize=14)
    ax.set_ylabel("Decoding temperature", fontsize=14)
    ax.tick_params(axis="both", labelsize=12)
    ax.grid(False)
    ax.set_xlim(left=0, right=0.6)
    fig.savefig("plots/overall_mean_similarity_by_temperature_5_shots.png")
    plt.close(fig)

    # plot box and whisker diagram of overall mean similarity by task name
    fig, ax = plt.subplots(figsize=(10, 4))
    fig.subplots_adjust(**AXIS_MARGINS)
    results_data_frame[results_data_frame["number_of_shots"] == 5].boxplot(
        column="overall_mean_similarity", by="task_name", vert=False, ax=ax
    )
    ax.set_title(
        "Similarity of LLM and human labels by task description (5-shot prompts)",
        fontsize=16,
    )
    fig.suptitle("")
    ax.set_xlabel("Mean cosine similarity", fontsize=14)
    ax.set_ylabel("Task description", fontsize=14)
    ax.tick_params(axis="both", labelsize=12)
    ax.grid(False)
    ax.set_xlim(left=0, right=0.6)
    plt.savefig("plots/overall_mean_similarity_by_task_description_5_shots.png")
    plt.close()
