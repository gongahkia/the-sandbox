# Learning ChromaDB

ChromaDB is an open-source vector database designed for storing and retrieving vector embeddings for use with AI and ML applications.

`built_in.py` uses ChromaDB alongside its default provided pretrained model, *all-MiniLM-L6-v2*, a smaller version of BERT optimised for generating sentence embeddings. It is then used through the embedding function

`pretrained.py` uses ChromaDB alongside a locally pulled ollama model. The vector embeddings are fed through the *ollama_embedding_function()* function.

Test datasets taken from [dudeonthehorse/datasets](https://github.com/dudeonthehorse/datasets/blob/master/amazon.books.json).

```usage
$ python3 -m venv myenv
$ source myenv/bin/activate
$ pip install -r requirements.txt
$ python3 pretrained.py
```