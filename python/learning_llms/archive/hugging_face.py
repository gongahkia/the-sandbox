from transformers import pipeline, set_seed
from diffusers import StableDiffusionPipeline

"""
~ gpt2, EleutherAI/gpt-neo-1.3B, distilgpt2 ~

these models generates text based on probabilistic trends
relating to the probability of certain words appearing
alongside other words, often used for generating 
semi-coherent and contextually relevant text
"""
generator = pipeline('text-generation', model='gpt2')
# generator = pipeline('text-generation', model='EleutherAI/gpt-neo-1.3B')  
# generator = pipeline('text-generation', model='distilgpt2')  

# -----

"""
~ distilbert-base-uncased-finetuned-sst-2-english ~

this model is used for text classification, enabling 
the categorization of text into predefined classes, 
such as sentiment analysis
"""
classifier = pipeline('text-classification', model='distilbert-base-uncased-finetuned-sst-2-english')

# -----

"""
~ distilbert-base-uncased-distilled-squad ~

this model is designed for question answering, 
allowing for the retrieval of answers based on 
provided context
"""
question_answerer = pipeline('question-answering', model='distilbert-base-uncased-distilled-squad')

# -----

"""
~ dbmdz/bert-large-cased-finetuned-conll03-english ~

this model performs Named Entity Recognition (NER), 
which identifies and classifies named entities in 
the text, such as people and organizations
"""
ner = pipeline('ner', model='dbmdz/bert-large-cased-finetuned-conll03-english')

# -----

"""
~ facebook/bart-large-cnn ~

this model is used for summarization, condensing long 
texts into shorter, more concise versions while 
preserving key information
"""
summarizer = pipeline('summarization', model='facebook/bart-large-cnn')

"""
~ Helsinki-NLP/opus-mt-en-fr ~

this model is used for translation, allowing text 
to be translated between different languages
"""
translator = pipeline('translation_en_to_fr', model='Helsinki-NLP/opus-mt-en-fr')

# -----

"""
~ bert-base-uncased ~

this model performs fill-mask tasks, predicting 
missing words in a sentence to complete the context
"""
fill_mask = pipeline('fill-mask', model='bert-base-uncased')

# -----

"""
~ facebook/deit-base-distilled-patch16-224 ~

this model is used for image classification, categorizing 
images into predefined classes based on visual content
"""
image_classifier = pipeline('image-classification', model='facebook/deit-base-distilled-patch16-224')

# -----

"""
~ CompVis/stable-diffusion-v-1-4 ~

this model generates images from textual descriptions, 
enabling image creation based on provided prompts
"""
pipe = StableDiffusionPipeline.from_pretrained("CompVis/stable-diffusion-v-1-4")

# ~ actual code execution ~

set_seed(42) # ensures reproducibility (optional)

def chat(message):
    generated = generator(message, max_length=50, num_return_sequences=1, temperature=0.3)
    return generated[0]['generated_text']

if __name__ == "__main__":
    user_message = input("You: ") 
    print(f"User: {user_message}")
    print(f"Bot: {chat(user_message)}")
