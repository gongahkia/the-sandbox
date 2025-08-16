# FUA
    # consider experimenting with other ollama models for this
    # augment prompt to be more interesting

# ----- REQUIRED IMPORT -----

import os
import json
import random
import ollama  

# ----- HELPER FUNCTIONS -----

def select_random(items):
    """
    randomly selects an item from a provided list
    """
    if not items:
        raise ValueError("Error: The list is empty. Cannot select an item.")
    return random.choice(items)

def safe_write_json(filepath, data):
    """
    appends provided JSON  data to a specified JSON file
    """
    os.makedirs(os.path.dirname(filepath), exist_ok=True)
    if not os.path.isfile(filepath):
        with open(filepath, 'w') as f:
            json.dump([], f)
    with open(filepath, 'r') as f:
        existing_data = json.load(f)
    if isinstance(existing_data, list):
        existing_data.append(data)
    else:
        raise ValueError("Error: The existing JSON data should be a list.")
    with open(filepath, 'w') as f:
        json.dump(existing_data, f, indent=4)

def start_model():
    """
    attempts to start and return an ollama model client 
    """
    try:
        client = ollama.Client()
        return client
    except Exception as e:
        print(f"Error starting model: {e}")
        return None

def generate_response(client, model_name, prompt, worldview=None, discipline=None):
    """
    generates a response from the specified ollama model based on the provided prompt, worldview, and discipline
    """
    augmented_prompt = prompt
    if worldview and discipline:
        augmented_prompt += f" Respond from a {worldview} perspective and consider insights from {discipline}."
    else:
        print("Error: Both worldview and discipline must be provided.")
    print(augmented_prompt)
    try:
        response = client.generate(prompt=augmented_prompt, model=model_name)
        print("------")
        print("Complete response from model:")
        print(response)
        response_text = response["response"].strip()
        return response_text
    except Exception as e:
        print(f"Error generating response: {e}")
        return None

def debate(prompt, model_a_type , model_b_type, worldview_a, discipline_a, worldview_b, discipline_b, model_a_client=None, model_b_client=None):
    """
    conducts a debate between two models based on the provided prompt, worldviews, and disciplines
    """
    model_a_client = start_model()
    model_b_client = start_model()
    if model_a_client is None or model_b_client is None:
        print("Error: Failed to start one or both models")
        return (False, None)
    print(f"STARTING DEBATE ON:\n{prompt}")
    response_a = generate_response(model_a_client, model_a_type, f"{prompt} (Model A's argument)", worldview_a, discipline_a)
    response_b = generate_response(model_b_client, model_b_type, f"{prompt} (Model B's argument)", worldview_b, discipline_b)
    return (True, {
        "response_a": response_a, 
        "response_b": response_b
    })

# ----- EXECUTION CODE -----

if __name__ == "__main__":

    WORLDVIEWS = [
        "Naturalism",
        "Materialism",
        "Idealism",
        "Existentialism",
        "Nihilism",
        "Stoicism",
        "Rationalism",
        "Empiricism",
        "Theism",
        "Deism",
        "Pantheism",
        "Confucianism",
        "Taoism (Daoism)",
        "Humanism",
        "Relativism",
        "Determinism",
        "Utilitarianism",
        "Postmodernism",
        "Feminist Theory",
        "Critical Theory"
    ]


    DISCIPLINES = [
        "Philosophy",
        "Science",
        "Material Engineering",
        "Psychology",
        "Sociology",
        "Theology",
        "Political Science",
        "Cultural Studies",
        "Literature",
        "Art",
        "Economics",
        "History",
        "Ethics",
        "Environmental Science",
        "Education",
        "Anthropology",
        "Physics",
        "Mathematics",
        "Computer Science",
        "Linguistics"
    ]

    debate_prompt = "Why did the chicken cross the road?"

    worldview_a = WORLDVIEWS[0]
    discipline_a = DISCIPLINES[0]
    worldview_b = WORLDVIEWS[1]
    discipline_b = DISCIPLINES[1]

    LOG_FILEPATH = "./chat_log.json"
    MODEL_A_TYPE = "llama2:7b"
    MODEL_B_TYPE = "llama2:7b"

    responses_tuple = debate(debate_prompt, MODEL_A_TYPE, MODEL_B_TYPE, select_random(WORLDVIEWS), select_random(DISCIPLINES), select_random(WORLDVIEWS), select_random(DISCIPLINES))
    
    if responses_tuple[0]:
        safe_write_json(LOG_FILEPATH, responses_tuple[1])
        response_a, response_b = responses_tuple[1]["response_a"], responses_tuple[1]["response_b"]
        print(f"MODEL A:\n{response_a}")
        print(f"MODEL B:\n{response_b}")