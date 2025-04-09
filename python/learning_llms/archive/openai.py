import chatlab
import random
import asyncio
from dotenv import load_dotenv
import os

load_dotenv() # load openai api key from .env file

async def main():
    def flip_a_coin():
        '''Returns heads or tails'''
        return random.choice(['heads', 'tails'])

    chat = chatlab.Chat()
    chat.register(flip_a_coin)

    await chat("Please flip a coin for me")

if __name__ == "__main__":
    openai_api_key = os.getenv('OPENAI_API_KEY')
    if not openai_api_key:
        raise ValueError("OPENAI_API_KEY is not set in the environment variables.")
    asyncio.run(main())
