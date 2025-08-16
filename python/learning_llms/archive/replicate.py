# !NOTE
    # 1. sign up for replicate
    # 2. replace the below api_key variable value with your actual API key

import random
import requests

def flip_a_coin():
    '''Returns heads or tails'''
    return random.choice(['heads', 'tails'])

def generate_text(prompt):
    api_token = "YOUR_REPLICATE_API_TOKEN" 
    headers = {
        "Authorization": f"Token {api_token}",
        "Content-Type": "application/json"
    }
    model_url = "https://api.replicate.com/v1/predictions"
    data = {
        "version": "YOUR_MODEL_VERSION", 
        "input": {
            "prompt": prompt,
            "max_length": 50
        }
    }
    response = requests.post(model_url, headers=headers, json=data)
    if response.status_code == 201:
        return response.json()['output']  # Get the generated text
    else:
        return f"Error: {response.json()}"

def chat(message):
    if "flip a coin" in message.lower():
        return flip_a_coin()
    else:
        return generate_text(message)

if __name__ == "__main__":
    user_message = input("You: ")
    print(f"User: {user_message}")
    print(f"Bot: {chat(user_message)}")