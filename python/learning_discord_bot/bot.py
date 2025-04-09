import os
import discord
from discord.ext import commands
from dotenv import load_dotenv
import requests

load_dotenv()
TOKEN = os.getenv('DISCORD_TOKEN')

intents = discord.Intents.default()
intents.message_content = True
bot = commands.Bot(command_prefix='!', intents=intents)

@bot.command(name='book')
async def get_book_info(ctx, *, title: str):
    try:
        search_url = f"https://openlibrary.org/search.json?title={title}"
        search_response = requests.get(search_url)
        search_data = search_response.json()
        if search_data['numFound'] == 0:
            await ctx.send(f"No books found with the title '{title}'.")
            return
        book = search_data['docs'][0]
        book_key = book['key']
        book_url = f"https://openlibrary.org{book_key}.json"
        book_response = requests.get(book_url)
        book_data = book_response.json()
        response = f"**Title:** {book_data.get('title', 'N/A')}\n"
        response += f"**Author(s):** {', '.join(book.get('author_name', ['N/A']))}\n"
        response += f"**First Published:** {book.get('first_publish_year', 'N/A')}\n"
        response += f"**ISBN:** {book.get('isbn', ['N/A'])[0] if book.get('isbn') else 'N/A'}\n"
        cover_id = book.get('cover_i')
        if cover_id:
            cover_url = f"https://covers.openlibrary.org/b/id/{cover_id}-L.jpg"
            response += f"**Cover:** {cover_url}\n"
        else:
            response += "**Cover:** Not available\n"
        await ctx.send(response)
        if cover_id:
            await ctx.send(cover_url)
    except Exception as e:
        await ctx.send(f"An error occurred: {str(e)}")

if __name__ == "__main__":
    print("Test discord bot is starting...")
    bot.run(TOKEN)