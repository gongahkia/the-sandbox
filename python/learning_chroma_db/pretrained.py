# ----- REQUIRED IMPORTS -----

import os
import json
import ollama
import chromadb
from chromadb.utils import embedding_functions
from chromadb.api.types import Documents, Embeddings

# ----- HELPER FUNCTIONS -----

def start_model():
    """
    Attempts to start and return an ollama model client 
    """
    try:
        client = ollama.Client()
        return client
    except Exception as e:
        print(f"Error starting model: {e}")
        return None

def generate_response(client, model_name, prompt, context):
    """
    Generates a response from the specified ollama model based on the provided prompt and context
    """
    augmented_prompt = f"Context: {context}\n\nQuestion: {prompt}\n\nProvide a comprehensive answer based on the given context:"
    print(augmented_prompt)
    try:
        response = client.generate(prompt=augmented_prompt, model=model_name)
        print("------")
        print("Complete response from model:")
        print(response)
        response_text = response["response"].strip()
        return response_text
    except Exception as e:
        print(f"Error generating response: {e}")
        return None

def load_json_to_chromadb(json_file_path, collection_name):

    chroma_db_path = os.path.join(os.getcwd(), "chroma_db")
    os.makedirs(chroma_db_path, exist_ok=True)
    os.chmod(chroma_db_path, 0o755)
    client = chromadb.PersistentClient(path=chroma_db_path)
    ollama_client = start_model()
    if not ollama_client:
        return None

    def ollama_embedding_function(texts):
        embeddings = []
        for text in texts:
            response = generate_response(ollama_client, "llama2", f"Generate an embedding for: {text}", "")
            embedding = [float(x) for x in response.split()]
            embeddings.append(embedding)
        return embeddings

    collection = client.get_or_create_collection(
        name=collection_name,
        embedding_function=ollama_embedding_function
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
    if collection:
        query = "What is Android?"
        results = collection.query(
            query_texts=[query],
            n_results=2
        )
        print("Query:", query)
        print("Results:", results)
        ollama_client = start_model()
        if ollama_client:
            context = "\n".join([doc for doc in results['documents'][0]])
            prompt = "What is Android? Provide a comprehensive explanation based on the given context."
            response = generate_response(ollama_client, "llama2", prompt, context)
            print("Ollama Response:", response)
    else:
        print("Failed to create or load the collection.")