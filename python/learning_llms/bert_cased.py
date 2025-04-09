from transformers import pipeline

qa_pipeline = pipeline("question-answering", model="distilbert-base-cased-distilled-squad", device=0)

context = """
XLNet is a generalized autoregressive model that differs from BERT in how it learns context. 
Unlike BERT, which is a bidirectional transformer model, XLNet uses a permutation-based training objective.
This allows XLNet to capture dependencies in a more generalized manner. Additionally, XLNet integrates 
the Transformer-XL mechanism for handling longer sequences, which BERT does not have.
"""

question = "What is the key difference between XLNet and BERT's approach to context learning?"
result = qa_pipeline(question=question, context=context, max_answer_len=50)
print("Answer:", result['answer'])