"""
Mistral is a better version of llama2 that can
do the same things as it, including text generation, 
question answering and text classification 

Mistral is a 7B parameter model that outperforms 
Llama 2 13B on all benchmarks, outperforms Llama 1 
34B on many benchmarks, and approaches CodeLlama 7B 
performance on code, while remaining good at English tasks
"""

import ollama

client = ollama.Client()

# text_chunk = "Seven people are being investigated for alleged illegal betting activities following an enforcement operation, said police in a news release on Saturday (Aug 20).  The operation took place in the vicinity of Yishun Street 1 on Wednesday.  A 75-year-old man and 62-year-old woman were alleged to have acted as a gambling service provider to four other men aged between 48 and 72.  Another man, 61, was also allegedly involved in illegal betting activities with another gambling service provider. About S$2,280 in cash and seven mobile phones were seized. The police said they take a serious view of all forms of illegal betting activities and will continue to take tough enforcement action against offenders.  Those found guilty of being involved in unlawful betting operations can be jailed up to five years and are liable to a fine of not less than S$200,000.  If convicted of gambling with an unlicensed gambling service provider, the offender can be jailed up to six months, fined up to S$10,000, or both"
# question = "What general categories would you categorise this article under?"

text_chunk = """
\n     \n       BILLS INTRODUCED  \n      \n     \n       SINGAPORE TOTALISATOR BOARD (AMENDMENT) BILL  \n      \n     \n        \"to amend the Singapore Totalisator Board Act (Chapter 305A of the 1988 Revised Edition)\", presented by the Minister for Finance (Dr Richard Hu Tsu Tau); read the First time; to be read a Second time on the next available sitting of Parliament, and to be printed.  \n      \n 
"""
question = "Who are the ministers in the text, and what are their appointments?"
prompt = f"Here is some text:\n{text_chunk}\n\nBased on the text, answer the following question: {question}"

response = client.generate(
    prompt=prompt,
    model="mistral:latest"
)

# print(response)

print(response["response"])