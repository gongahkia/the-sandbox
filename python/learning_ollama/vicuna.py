"""
Vicuna is a chat assistant model with 3 sizes trained 
using conversations collected from ShareGPT

v1.3 is trained by fine-tuning Llama and has a 
context size of 2048 tokens

v1.5 is trained by fine-tuning Llama 2 and has a 
context size of 2048 tokens

v1.5-16k is trained by fine-tuning Llama 2 and has 
a context size of 16k tokens
"""

import ollama

client = ollama.Client()

response = client.generate(
    prompt="What is the meaning of life? Explain it in 5 paragraphs.",
    model="vicuna:latest"
)

# print(response)

print(response["response"])