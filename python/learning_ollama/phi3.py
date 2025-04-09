"""
phi3 is excellent in 

1) memory/compute constrained environments
2) latency bound scenarios
3) strong reasoning (especially math and logic)
4) long context

Phi-3 Mini is a 3.8B parameters, lightweight, 
state-of-the-art open model trained with the 
Phi-3 datasets that includes both synthetic data 
and the filtered publicly available websites data 
with a focus on high-quality and reasoning dense properties.

When assessed against benchmarks testing common sense, 
language understanding, math, code, long context and logical 
reasoning, Phi-3 Mini-4K-Instruct showcased a robust and 
state-of-the-art performance among models with less than 
13 billion parameters.
"""

import ollama

client = ollama.Client()
response = client.generate(
    # prompt=" Three people—Alice, Bob, and Charlie—are sitting in a row. Each person is wearing a hat, which is either red or blue. None of them can see their own hat, but they can see the hats of the people in front of them. Alice can see both Bob and Charlie’s hats, Bob can see only Charlie’s hat, and Charlie can’t see anyone’s hat. The organizer of the puzzle tells them that at least one of them is wearing a red hat. The organizer asks them to guess the color of their own hat. After a moment of silence, one of them figures it out and correctly guesses their own hat's color. Who figures it out, and how do they know?",
    prompt="What is 2 to the power of 10",
    model="phi3:mini"
)

# print(response)

print(response["response"])