# !NOTE
    # 1. sign up for AI21 studio
    # 2. replace the value of the API_KEY variable with your actual AI21 api key

import random
import requests

API_KEY = 'your_ai21_api_key'

def flip_a_coin():
    '''Returns heads or tails'''
    return random.choice(['heads', 'tails'])

def query_ai21(prompt):
    '''Query the AI21 API for text generation'''
    url = "https://api.ai21.com/v1/j1-large/complete"
    headers = {
        "Authorization": f"Bearer {API_KEY}",
        "Content-Type": "application/json"
    }
    data = {
        "prompt": prompt,
        "maxTokens": 50,
        "temperature": 0.7,
        "topP": 1,
        "stopSequences": ["\n"]
    }
    
    response = requests.post(url, headers=headers, json=data)
    
    if response.status_code == 200:
        response_json = response.json()
        return response_json['completions'][0]['text']
    else:
        return f"Error: {response.status_code}, {response.text}"

def chat(message):
    if "flip a coin" in message.lower():
        return flip_a_coin()
    else:
        return query_ai21(message)

if __name__ == "__main__":
    user_message = input("You: ") 
    response = chat(user_message)
    print(f"Bot: {response}")