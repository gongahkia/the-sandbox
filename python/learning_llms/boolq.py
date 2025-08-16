from transformers import BartForSequenceClassification, BartTokenizer
from transformers import pipeline
from datasets import load_dataset

dataset = load_dataset('boolq')
model_name = "facebook/bart-large"
tokenizer = BartTokenizer.from_pretrained(model_name)
model = BartForSequenceClassification.from_pretrained(model_name)
qa_pipeline = pipeline("text-classification", model=model, tokenizer=tokenizer)
context = dataset['train'][0]['passage']
question = "Is the sky blue?"
input_text = f"question: {question} context: {context}"
result = qa_pipeline(input_text)

print(f"Question: {question}")
print(f"Context: {context}")
print(f"Answer: {'Yes' if result[0]['label'] == 'LABEL_1' else 'No'}")