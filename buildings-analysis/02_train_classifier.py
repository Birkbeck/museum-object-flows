import re
import statistics

import pandas as pd

from sentence_transformers import SentenceTransformer, util
from transformers import AutoModelForCausalLM, AutoTokenizer, pipeline, set_seed

BUILDING_NOTES_FILE_NAME = "buildings_dataset_with_labels.csv"
RESULTS_DIRECTORY = "scratch/results5"

SENTENCE_MODEL = SentenceTransformer("all-MiniLM-L6-v2")

CACHED_EMBEDDINGS = {}

ROLES = {
    "noRole": "",
    # "researcher": "You are an expert researching what happened to museums' buildings after they closed.",
    # "modeller": "You are a data modeller good at coming up with succinct classifications of pieces of text.",
    "mixed": "You are a museums expert good at coming up with succinct classifications of notes on museum closure.",
}

TASKS = {
    # "status": "Provide classifications of the change in status of museum buildings after closure as described by the notes",
    # "use": "Provide classifications of the change in use of museum buildings after closure as described by the notes",
    # "responsibility": "Provide classifications of the change in responsibility of museum buildings after closure as described by the notes",
    "all": "Provide classifications of the change in status, use, and responsibility of museum buildings after closure as described by the notes",
    "longer": "Read the notes about what happened to museum buildings after they closed. Classify the building's change in status (did the building remain, get demolished, or altered?). Classify the building's change in use (what was it used for after the museum closed?). Classify the change in responsibility (was there a new owner or occupier?). State 'no information' or 'no change' if relevant. Attach '?' to labels that are likely but not certain.",
    "extended": "Provide classifications of the change in status, use, and responsibility of museum buildings after closure as described by the notes. Focus on the building and not the museum. Change in status should describe whether the building remained, was demolished, or was substantially altered. The fact that the museum closed does not need to be mentioned. Change in use should describe any new uses the building was put to. Change in responsibility should describe whether there was a change in the owner, occupier, or any other people who were responsible for the building. If there is no information for a field, state 'no information'. Similarly if there was no change, state 'no change'. If you think a change is likely but uncertain provide a label and attach '?' to the end of the label.",
}

MODELS = {
    # "nullmodel": None,
    # "gpt2": pipeline(
    #    "text-generation",
    #    model=AutoModelForCausalLM.from_pretrained("gpt2"),
    #    tokenizer=AutoTokenizer.from_pretrained("gpt2"),
    #    device=0,
    # ),
    "llama3": pipeline(
        "text-generation",
        model=AutoModelForCausalLM.from_pretrained(
            "/users/k2480370/llama3.1-8B",
            trust_remote_code=True,
        ),
        tokenizer=AutoTokenizer.from_pretrained(
            "/users/k2480370/llama3.1-8B",
            trust_remote_code=True,
        ),
        device=0,
        pad_token_id=0,
    )
}

NUMBERS_OF_EXAMPLES = [
    # 0,
    # 1,
    # 3,
    5,
]

TEMPERATURES = [
    0.01,
    0.05,
    0.1,
    0.5,
    1.0,
]


def generate_prompt(role: str, task: str, examples: list, note: str):
    response_format = [
        "Format your response like this:",
        "change in status: ...",
        "change in use: ...",
        "change in responsibility: ...",
    ]
    return "\n\n".join([role, task] + response_format + examples + [f"Notes: {note}"])


def make_example_text(
    note: str, change_in_status: str, change_in_use: str, change_in_responsibility: str
):
    return "\n".join(
        [
            f"Notes: {note}",
            f"Change in status: {change_in_status}",
            f"Change in use: {change_in_use}",
            f"Change in responsibility: {change_in_responsibility}",
        ]
    )


def remove_urls(text, placeholder="[website]"):
    """Replace URLs in the input text with the placeholder"""
    url_pattern = r"https?://\S+|www\.\S+"
    return re.sub(url_pattern, placeholder, text)


def get_llm_response(model, prompt, temperature, seed=123):
    """Returns the text output by llm when given prompt"""
    set_seed(seed)
    try:
        response = model(
            prompt,
            num_return_sequences=1,
            max_new_tokens=50,
            temperature=temperature,
        )
        response_text = response[0]["generated_text"][len(prompt) :]
    except RuntimeError as e:
        print("Caught CUDA error:", e)
        torch.cuda.empty_cache()
        torch.cuda.synchronize()
        response_text = f"CUDA error, {e}"
    return response_text


def extract_labels(text, label_type):
    """Returns a sub-string of text containing the labels of the required type"""
    label_lists = text.lower().split("change in ")
    try:
        sub_string = [l for l in label_lists if l[: len(label_type)] == label_type][0]
        sub_string_no_label_type = sub_string[len(label_type) + 1 :]
        sub_string_no_notes = sub_string_no_label_type.split("\nnotes")[0]
        return sub_string_no_notes.strip()
    except IndexError:
        return ""


def get_cached_embedding(text):
    try:
        return CACHED_EMBEDDINGS[text]
    except KeyError:
        CACHED_EMBEDDINGS[text] = SENTENCE_MODEL.encode(text, convert_to_tensor=True)
        return CACHED_EMBEDDINGS[text]


def evaluate_labels(predicted_labels, human_labels):
    """Returns cosine similarity of candidate and human labels"""
    if predicted_labels == "":
        return 0.0
    prediction_embedding = SENTENCE_MODEL.encode(
        predicted_labels, convert_to_tensor=True
    )
    human_embedding = get_cached_embedding(human_labels)
    similarity = util.cos_sim(prediction_embedding, human_embedding)
    return similarity.item()


def run_experiment(
    model_name,
    role_name,
    task_name,
    number_of_shots,
    temperature,
    building_notes_file_name,
):
    building_notes = pd.read_csv(building_notes_file_name, encoding="ISO-8859-1")
    building_notes["building_notes"] = building_notes["building_notes"].map(remove_urls)
    support_rows = building_notes[
        building_notes["support_development_or_test"] == "support"
    ]
    examples = [
        make_example_text(
            support_rows.iloc[i]["building_notes"],
            support_rows.iloc[i]["change_in_status"],
            support_rows.iloc[i]["change_in_use"],
            support_rows.iloc[i]["change_in_responsibility"],
        )
        for i in range(number_of_shots)
    ]
    development_rows = building_notes[
        building_notes["support_development_or_test"] == "development"
    ].copy()
    development_rows["prompt"] = development_rows.apply(
        lambda row: generate_prompt(
            ROLES[role_name], TASKS[task_name], examples, row["building_notes"]
        ),
        axis=1,
    )
    development_rows["llm_response"] = development_rows.apply(
        lambda row: get_llm_response(MODELS[model_name], row["prompt"], temperature),
        axis=1,
    )
    development_rows["predicted_change_in_status"] = development_rows.apply(
        lambda row: extract_labels(row["llm_response"], "status"),
        axis=1,
    )
    development_rows["predicted_change_in_use"] = development_rows.apply(
        lambda row: extract_labels(row["llm_response"], "use"),
        axis=1,
    )
    development_rows["predicted_change_in_responsibility"] = development_rows.apply(
        lambda row: extract_labels(row["llm_response"], "responsibility"),
        axis=1,
    )
    development_rows["status_similarity"] = development_rows.apply(
        lambda row: evaluate_labels(
            row["predicted_change_in_status"], row["change_in_status"]
        ),
        axis=1,
    )
    development_rows["use_similarity"] = development_rows.apply(
        lambda row: evaluate_labels(
            row["predicted_change_in_use"], row["change_in_use"]
        ),
        axis=1,
    )
    development_rows["responsibility_similarity"] = development_rows.apply(
        lambda row: evaluate_labels(
            row["predicted_change_in_responsibility"], row["change_in_responsibility"]
        ),
        axis=1,
    )
    development_rows["mean_similarity"] = development_rows.apply(
        lambda row: statistics.fmean(
            [
                row["status_similarity"],
                row["use_similarity"],
                row["responsibility_similarity"],
            ]
        ),
        axis=1,
    )
    file_name = f"{model_name}-{role_name}-{task_name}-{number_of_shots}shots-t{temperature}.csv"
    development_rows.to_csv(
        f"{RESULTS_DIRECTORY}/{file_name}", index=False, encoding="utf-8-sig"
    )


if __name__ == "__main__":
    for model_name in MODELS:
        for role_name in ROLES:
            for task_name in TASKS:
                for number_of_shots in NUMBERS_OF_EXAMPLES:
                    for temperature in TEMPERATURES:
                        run_experiment(
                            model_name,
                            role_name,
                            task_name,
                            number_of_shots,
                            temperature,
                            BUILDING_NOTES_FILE_NAME,
                        )
