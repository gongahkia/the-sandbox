# !NOTE
    # 1. create a google cloud account 
    # 2. go to the google cloud console and create a new project
    # 3. enable the natural language API
    # 4. create service account credentials
    # 5. generate a new JSON key file and save it securely

import random
from google.cloud import language_v1

def flip_a_coin():
    '''Returns heads or tails'''
    return random.choice(['heads', 'tails'])

def analyze_text(text):
    '''Uses Google Cloud Natural Language API to analyze the text'''
    client = language_v1.LanguageServiceClient()
    document = language_v1.Document(content=text, type_=language_v1.Document.Type.PLAIN_TEXT)
    response = client.analyze_sentiment(request={'document': document})
    sentiment = response.document_sentiment

    return f'Sentiment Score: {sentiment.score}, Magnitude: {sentiment.magnitude}'

def chat(message):
    if "flip a coin" in message.lower():
        return flip_a_coin()
    else:
        return analyze_text(message)

if __name__ == "__main__":
    user_message = input("You: ")  
    response = chat(user_message)
    print(f"Bot: {response}")