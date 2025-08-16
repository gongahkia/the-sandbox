import requests
import json

book = input("Enter the book name: ")
chapter = input("Enter the chapter number: ")
verse_num = input("Enter the verse number: ")
verse = f"{book.lower()} {chapter}:{verse_num}"

bible_api = requests.get(f"https://bible-api.com/{verse}?translation=kjv")

verse_dict = json.loads(bible_api.text)

#print(f"exit code: {improv_mx.status_code}")
print(f'{verse}:\n{verse_dict["verses"][0]["text"]}')
