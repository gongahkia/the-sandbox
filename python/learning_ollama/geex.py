"""
CodeGeeX4 is a multilingual code generation model 
continually trained on the GLM-4-9B, significantly 
enhancing its code generation capabilities.

CodeGeeX4-ALL-9B has achieved highly competitive performance 
on public benchmarks, such as BigCodeBench and NaturalCodeBench. 
It is currently the most powerful code generation model 
with less than 10B parameters, even surpassing much larger 
general-purpose models, achieving the best balance in terms 
of inference speed and model performance.
"""

import ollama

client = ollama.Client()

response = client.generate(
    prompt="""
You are a coding assistant. Write a Python function that lists all prime numbers between X and Y, where X is a minimum non-negative integer and Y is the maximum non-negative integer.

Function signature:
def find_all_prime(X:int, Y:int) -> List[int]:
            """,
    model="codegeex4:latest"
)

# print(response)

print(response["response"])