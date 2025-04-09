"""
General purpose LLM model 

Orca Mini is a Llama and Llama 2 model trained 
on Orca Style datasets created using the approaches 
defined in the paper, Orca: Progressive Learning from 
Complex Explanation Traces of GPT-4

The original Orca Mini is based on Llama in 3, 7, and 
13 billion parameter sizes, and v3 based on Llama 2 in 
7, 13, and 70 billion parameter sizes.
"""

import ollama

client = ollama.Client()
response = client.generate(
    prompt="Why is C so hard to learn?",
    model="orca-mini:latest"
)

# print(response)

print(response["response"])