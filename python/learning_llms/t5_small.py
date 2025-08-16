from transformers import pipeline

qa_pipeline = pipeline("text2text-generation", model="t5-small")

context = """
T5 (Text-to-Text Transfer Transformer) is a transformer model that reformulates all NLP tasks into a text-to-text format, where 
both the inputs and outputs are text strings. This unified approach allows T5 to handle tasks such as translation, summarization, 
and question answering with a single model architecture. T5 can be fine-tuned on a variety of tasks, making it flexible and 
adaptable to different types of NLP challenges.
"""
question = "How does T5 handle different NLP tasks?"

input_text = f"question: {question}  context: {context}"
result = qa_pipeline(input_text, max_length=50, clean_up_tokenization_spaces=True)[0]
print("Answer:", result['generated_text'])

# ~ fine-tuning is also possible as seen below here ~

# from transformers import T5Tokenizer, T5ForConditionalGeneration, Trainer, TrainingArguments
# from datasets import load_dataset

# dataset = load_dataset('your_dataset')
# model = T5ForConditionalGeneration.from_pretrained("t5-small")
# tokenizer = T5Tokenizer.from_pretrained("t5-small")

# def preprocess_function(examples):
#     inputs = [f"question: {q} context: {c}" for q, c in zip(examples['input_text'], examples['context'])]
#     model_inputs = tokenizer(inputs, max_length=512, truncation=True)
#     with tokenizer.as_target_tokenizer():
#         labels = tokenizer(examples['target_text'], max_length=128, truncation=True)
#     model_inputs["labels"] = labels["input_ids"]
#     return model_inputs

# tokenized_dataset = dataset.map(preprocess_function, batched=True)

# training_args = TrainingArguments(
#     output_dir="./results",
#     evaluation_strategy="epoch",
#     learning_rate=3e-5,
#     per_device_train_batch_size=8,
#     num_train_epochs=3,
# )

# trainer = Trainer(
#     model=model,
#     args=training_args,
#     train_dataset=tokenized_dataset['train'],
#     eval_dataset=tokenized_dataset['validation'],
# )

# trainer.train()