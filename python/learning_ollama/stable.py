"""
stablelm-zephyr is a lightweight chat model 
that is preference tuned for instruction following 
and Q&A-type tasks
"""

import ollama

client = ollama.Client()

response = client.generate(
    prompt="""
You are an intelligent assistant. Read the following passage carefully and answer the questions that follow.

Passage:
In ancient Rome, gladiators were often slaves or prisoners of war, trained to fight in arenas for public entertainment. These fighters, known for their strength and skill, engaged in combat with one another and sometimes with wild animals. The outcome of these battles was critical for their survival and fame, with successful gladiators gaining significant rewards and public adoration.

Questions:
1. Who were typically gladiators in ancient Rome?
2. What was the main purpose of gladiatorial combat?
3. What could successful gladiators achieve?

Answer the questions based on the passage above.
    """,
    model="stablelm-zephyr:latest"
)

# print(response)

print(response["response"])