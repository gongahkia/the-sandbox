# !NOTE
    # 1. sign up for cohere
    # 2. replace the below api_key variable value with your actual API key

import random
import cohere

api_key = 'YOUR_API_KEY'  
co = cohere.Client(api_key)

def flip_a_coin():
    '''Returns heads or tails'''
    return random.choice(['heads', 'tails'])

def chat(message):
    if "flip a coin" in message.lower():
        return flip_a_coin()
    else:
        response = co.generate(
            model='xlarge', 
            prompt=message,
            max_tokens=50,
            temperature=0.7,
            stop_sequences=["\n"]
        )
        return response.generations[0].text.strip()

if __name__ == "__main__":
    print("Welcome to the chat! Type your message (type 'exit' to quit):")
    while True:
        user_message = input("You: ") 
        if user_message.lower() == 'exit':
            break
        response = chat(user_message)
        print(f"Bot: {response}")