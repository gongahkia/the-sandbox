from transformers import RobertaTokenizer, RobertaForQuestionAnswering
import torch

tokenizer = RobertaTokenizer.from_pretrained("deepset/roberta-base-squad2")
model = RobertaForQuestionAnswering.from_pretrained("deepset/roberta-base-squad2")

context = """RoBERTa, which stands for A Robustly Optimized BERT Pretraining Approach, is an optimized variant of BERT.
It is created to improve NLP task performance by training on larger datasets with optimized hyperparameters."""

question = "What is RoBERTa designed for?"

inputs = tokenizer(question, context, return_tensors="pt")
with torch.no_grad():
    outputs = model(**inputs)
    start_logits = outputs.start_logits
    end_logits = outputs.end_logits

start_index = torch.argmax(start_logits)
end_index = torch.argmax(end_logits) + 1  
answer = tokenizer.convert_tokens_to_string(tokenizer.convert_ids_to_tokens(inputs['input_ids'][0][start_index:end_index]))

print(f"Question: {question}")
print(f"Answer: {answer}")