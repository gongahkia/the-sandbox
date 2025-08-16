# default example implementation specified in the aisuite github repo README

import aisuite as ai

client = ai.Client()

messages = [
    {"role": "system", "content": "Respond in Pirate English."},
    {"role": "user", "content": "Tell me a joke."},
]

response = client.chat.completions.create(
    model="openai:gpt-4o",
    messages=messages,
    temperature=0.75
)

print("Response from OpenAI:")
print(response.choices[0].message.content)

response = client.chat.completions.create(
    model="anthropic:claude-3-5",
    messages=messages,
    temperature=0.75
)

print("Response from Anthropic:")
print(response.choices[0].message.content)
