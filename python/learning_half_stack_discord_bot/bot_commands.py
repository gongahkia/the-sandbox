import requests
from discord.ext import commands

def setup_bot_commands(bot: commands.Bot, supabase):
    @bot.command(name='borrow')
    async def borrow_book(ctx, *, title: str):
        try:
            book_info = await get_book_info(title)
            if book_info:
                response = supabase.table("books").insert(book_info).execute()
                if response.data:
                    await ctx.send(f"Book '{title}' has been borrowed and added to the database.")
                else:
                    await ctx.send("Failed to add the book to the database.")
            else:
                await ctx.send(f"No book found with the title '{title}'.")
        except Exception as e:
            await ctx.send(f"An error occurred: {str(e)}")

    @bot.command(name='return')
    async def return_book(ctx, *, title: str):
        try:
            response = supabase.table("books").delete().eq("title", title).execute()
            if response.data:
                await ctx.send(f"Book '{title}' has been returned and removed from the database.")
            else:
                await ctx.send("Failed to remove the book from the database.")
        except Exception as e:
            await ctx.send(f"An error occurred: {str(e)}")

    @bot.command(name='list')
    async def list_books(ctx):
        try:
            response = supabase.table("books").select("*").execute()
            if response.data:
                books = response.data
                book_list = "\n".join([f"- {book['title']} by {book['author']}" for book in books])
                await ctx.send(f"Available books:\n{book_list}")
            else:
                await ctx.send("No books available in the database.")
        except Exception as e:
            await ctx.send(f"An error occurred: {str(e)}")

    @bot.command(name='find')
    async def find_book(ctx, *, title: str):
        try:
            book_info = await get_book_info(title)
            if book_info:
                response = f"**Title:** {book_info['title']}\n"
                response += f"**Author(s):** {book_info['author']}\n"
                response += f"**First Published:** {book_info['year']}\n"
                response += f"**ISBN:** {book_info['isbn']}\n"
                if book_info['cover_url']:
                    response += f"**Cover:** {book_info['cover_url']}\n"
                await ctx.send(response)
                if book_info['cover_url']:
                    await ctx.send(book_info['cover_url'])
            else:
                await ctx.send(f"No book found with the title '{title}'.")
        except Exception as e:
            await ctx.send(f"An error occurred: {str(e)}")

async def get_book_info(title: str):
    search_url = f"https://openlibrary.org/search.json?title={title}"
    search_response = requests.get(search_url)
    search_data = search_response.json()
    if search_data['numFound'] == 0:
        return None
    book = search_data['docs'][0]
    return {
        "title": book.get('title', 'N/A'),
        "author": ', '.join(book.get('author_name', ['N/A'])),
        "year": book.get('first_publish_year', 0),
        "isbn": book.get('isbn', ['N/A'])[0] if book.get('isbn') else 'N/A',
        "cover_url": f"https://covers.openlibrary.org/b/id/{book.get('cover_i')}-L.jpg" if book.get('cover_i') else None
    }