import os
from dotenv import load_dotenv
from discord.ext import commands
import discord
from fastapi import FastAPI
from supabase import create_client, Client
import uvicorn
from threading import Thread
from bot_commands import setup_bot_commands
from endpoints import setup_api_endpoints

load_dotenv()
TOKEN = os.getenv('DISCORD_TOKEN')

intents = discord.Intents.default()
intents.message_content = True
bot = commands.Bot(command_prefix='!', intents=intents)

app = FastAPI()
supabase: Client = create_client(
    os.environ.get("SUPABASE_URL"),
    os.environ.get("SUPABASE_KEY")
)

setup_bot_commands(bot, supabase)
setup_api_endpoints(app, supabase)

if __name__ == "__main__":
    def run_fastapi():
        print("FastAPI is starting...")
        uvicorn.run(app, host="0.0.0.0", port=8000)

    fastapi_thread = Thread(target=run_fastapi)
    fastapi_thread.start()

    print("Discord bot is starting...")
    bot.run(TOKEN)