from transformers import pipeline

qa_pipeline = pipeline("question-answering", model="allenai/longformer-base-4096", tokenizer="allenai/longformer-base-4096")

context = """
The Longformer model is specifically designed to handle long documents by using a combination of local 
and global attention mechanisms. Unlike traditional transformers, which use quadratic complexity self-attention, 
Longformer uses a sliding window of attention, which allows it to process long documents efficiently. 
For certain tokens, global attention is applied, giving Longformer the capability to attend across 
the entire document. This structure makes Longformer well-suited for tasks that require understanding 
of long sequences of text, such as legal documents, research papers, and other multi-paragraph content.
"""

question = "How does Longformer handle long documents efficiently?"
result = qa_pipeline(question=question, context=context, max_answer_len=50)

print("Answer:", result['answer'])