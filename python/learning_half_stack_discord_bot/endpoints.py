from datetime import datetime
from fastapi import FastAPI
from pydantic import BaseModel

class Book(BaseModel):
    id: int = None
    title: str
    author: str
    year: int
    isbn: str
    cover_url: str = None
    created_at: datetime = None

def setup_api_endpoints(app: FastAPI, supabase):
    @app.post("/books/")
    async def create_book(book: Book):
        response = supabase.table("books").insert(book.dict()).execute()
        return response.data

    @app.get("/books/")
    async def list_books():
        response = supabase.table("books").select("*").execute()
        return response.data

    @app.get("/books/{title}")
    async def find_book(title: str):
        response = supabase.table("books").select("*").eq("title", title).execute()
        return response.data

    @app.delete("/books/{title}")
    async def delete_book(title: str):
        response = supabase.table("books").delete().eq("title", title).execute()
        return response.data