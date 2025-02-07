from typing import Union, Dict
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from fastapi.middleware.cors import CORSMiddleware

"""
Cross-Origin Resource Sharing (CORS) 
is a security mechanism that allows web apps
to access resources from domains other than the one
serving the application

REQURIED FOR OFFICIAL DEPLOYMENT
"""


app = FastAPI()

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  
    allow_credentials=True,
    allow_methods=["*"],  
    allow_headers=["*"], 
)

mock_db: Dict[str, str] = {
    "hash1": "Value for hash 1",
    "hash2": "Value for hash 2",
    "hash3": "Value for hash 3",
}

class Item(BaseModel):
    name: str
    price: float
    is_offer: Union[bool, None] = None

class HashValue(BaseModel):
    value: str

@app.get("/")
def read_root():
    return {"Hello": "World"}

@app.get("/items/{item_id}")
def read_item(item_id: int, q: Union[str, None] = None):
    return {"item_id": item_id, "q": q}

@app.put("/items/{item_id}")
def update_item(item_id: int, item: Item):
    if item_id < 0:
        raise HTTPException(status_code=400, detail="Invalid item ID")
    return {"item_name": item.name, "item_id": item_id}

@app.get("/retrieve/{hash_value}")
def retrieve_value(hash_value: str):
    """
    retrieve value from the mock database using the provided hash
    """
    value = mock_db.get(hash_value)
    if value is None:
        raise HTTPException(status_code=404, detail="Hash not found")
    return {"hash": hash_value, "value": value}

@app.put("/store/{hash_value}")
def store_value(hash_value: str, hash_value_data: HashValue):
    """
    store or update the value associated with the provided hash
    """
    mock_db[hash_value] = hash_value_data.value
    return {"hash": hash_value, "value": hash_value_data.value}