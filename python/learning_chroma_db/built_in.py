# ----- REQUIRED IMPORTS -----

import json
import chromadb
from chromadb.utils import embedding_functions

# ----- HELPER FUNCTIONS -----

def load_json_to_chromadb(json_file_path, collection_name):
    client = chromadb.PersistentClient(path="./chroma_db") # initialize chromadb client
    embedding_function = embedding_functions.SentenceTransformerEmbeddingFunction() # embedding function that uses a smaller version of BERT, optimised for generating sentence embedding
    collection = client.get_or_create_collection(
        name=collection_name,
        embedding_function=embedding_function
    )
    with open(json_file_path, 'r') as file:
        data = json.load(file)
    documents = []
    metadatas = []
    ids = []
    for item in data:
        document = f"{item['title']}\n{item['shortDescription']}"
        documents.append(document)
        metadata = {
            "isbn": item["isbn"],
            "pageCount": item["pageCount"],
            "publishedDate": item["publishedDate"]["$date"],
            "status": item["status"],
            "authors": ", ".join(item["authors"]),
            "categories": ", ".join(item["categories"])
        }
        metadatas.append(metadata)
        ids.append(str(item["_id"]))
    collection.add(
        documents=documents,
        metadatas=metadatas,
        ids=ids
    )
    print(f"Added {len(documents)} items to the collection '{collection_name}'")
    return collection

# ----- EXECUTION CODE -----

if __name__ == "__main__":
    json_file_path = "amazon_books.json"
    collection_name = "books_collection"
    collection = load_json_to_chromadb(json_file_path, collection_name)
    query = "What is Android?"
    results = collection.query(
        query_texts=[query],
        n_results=2
    )
    print("Query:", query)
    print("Results:", results)