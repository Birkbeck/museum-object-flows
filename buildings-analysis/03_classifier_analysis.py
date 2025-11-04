import os
import statistics

import pandas as pd
import statsmodels.formula.api as smf


results_directory = "results5"


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
                # "number_of_shots": number_of_shots,
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
            # "number_of_shots",
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
    results_data_frame.to_csv("classifier_results_summary5.csv", index=False)

    results_data_frame["temperature"] = results_data_frame["temperature"].map(
        {
            "t0.01.csv": 0.01,
            "t0.05.csv": 0.05,
            "t0.1.csv": 0.1,
            "t0.5.csv": 0.5,
            "t1.0.csv": 1.0,
        }
    )
    model = smf.ols(
        "overall_mean_similarity ~ C(role_name) + C(task_name) + temperature",
        data=results_data_frame,
    ).fit()

    print(model.summary())
