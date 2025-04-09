import os
from dotenv import load_dotenv
from typing import Union, Dict
from fastapi import FastAPI, HTTPException, Depends
from pydantic import BaseModel
from fastapi.middleware.cors import CORSMiddleware
from supabase import create_client, Client

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

def get_supabase_credentials():
    load_dotenv()
    supabase_url = os.getenv('SUPABASE_URL')
    supabase_key = os.getenv('SUPABASE_KEY')
    if not supabase_url or not supabase_key:
        raise ValueError("SUPABASE_URL or SUPABASE_KEY not found in .env file")
    return supabase_url, supabase_key

supabase_url, supabase_key = get_supabase_credentials()
supabase: Client = create_client(supabase_url, supabase_key)
print(supabase)

class HashValue(BaseModel):
    value: str

def get_supabase():
    return supabase

@app.get("/retrieve/{hash_value}")
async def retrieve_value(hash_value: str, supabase: Client = Depends(get_supabase)):
    try:
        response = supabase.table("hashbrown").select("VALUE").eq("HASH", hash_value).execute()
        if len(response.data) == 0:
            raise HTTPException(status_code=404, detail="Hash not found")
        return {"hash": hash_value, "value": response.data[0]['VALUE']}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.put("/store/{hash_value}")
async def store_value(hash_value: str, hash_value_data: HashValue, supabase: Client = Depends(get_supabase)):
    try:
        response = supabase.table("hashbrown").upsert({"HASH": hash_value, "VALUE": hash_value_data.value}).execute()
        return {"hash": hash_value, "value": hash_value_data.value}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Add these new classes for update operations
class UpdateHashValue(BaseModel):
    value: str

@app.delete("/delete/{hash_value}")
async def delete_value(hash_value: str, supabase: Client = Depends(get_supabase)):
    try:
        response = supabase.table("hashbrown").delete().eq("HASH", hash_value).execute()
        if len(response.data) == 0:
            raise HTTPException(status_code=404, detail="Hash not found")
        return {"message": f"Hash {hash_value} deleted successfully"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.patch("/update/{hash_value}")
async def update_value(hash_value: str, update_data: UpdateHashValue, supabase: Client = Depends(get_supabase)):
    try:
        response = supabase.table("hashbrown").update({"VALUE": update_data.value}).eq("HASH", hash_value).execute()
        if len(response.data) == 0:
            raise HTTPException(status_code=404, detail="Hash not found")
        return {"hash": hash_value, "value": update_data.value}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))