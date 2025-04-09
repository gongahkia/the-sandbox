from transformers import pipeline

generator = pipeline('text-generation', model='gpt2')

def generate_possible_words_from_clues(clues):
    possible_words = set()
    for clue in clues:
        prompt = f"Given the clue '{clue}', provide a list of four-letter words related to it. Only give relevant words."
        generated = generator(prompt, max_length=50, num_return_sequences=1)
        generated_text = generated[0]['generated_text']
        words = generated_text.split()
        for word in words:
            if len(word) == 4: 
                possible_words.add(word)
    return possible_words

clues = [
    "small piece of paper that contains one's job title, contact information, etc.",
    "a physical structure, especially one of large size."
]
possible_words = generate_possible_words_from_clues(clues)
print("Possible words:", list(possible_words))