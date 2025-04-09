"""
Code Llama is a model for generating and discussing code, 
built on top of Llama 2. It’s designed to make workflows 
faster and efficient for developers and make it easier for 
people to learn how to code. 

It can generate both code and natural language about code 
using text prompts.
"""

import ollama

client = ollama.Client()

response = client.generate(
    prompt="Write me a function that outputs the fibonacci sequence in Bash",
    model="codellama:latest"
)

# print(response)

print(response["response"])