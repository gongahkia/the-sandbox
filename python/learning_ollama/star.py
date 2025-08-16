"""
StarCoder is a code generation model trained 
on 80+ programming languages
"""

import ollama

client = ollama.Client()

response = client.generate(
    prompt="Write an implementation of a Ruby function that lists all prime numbers between X and Y, where X is a minimum non-negative integer and Y is the maximum non-negative integer.",
    model="starcoder2:latest"
)

# print(response)

print(response["response"])