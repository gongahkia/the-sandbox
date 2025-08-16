# Learning to write a Discord Bot that uses half a stack

*'Half a stack'* specifically refers to FastAPI and Supabase for DB integration.

Ideally a smarter, better-structured version of [`learning_discord_bot`](./../learning_discord_bot/).

## Commands

| Command | Description | Example |
| :--- | :--- | :---: |
| `!find <book title>` | searches for a book and displays its information without adding it to the database |![](./find.png) |
| `!borrow <book title>` | searches for a book and adds it to the database | ![](./borrow.png) |
| `!list` | lists all books currently in the database | ![](./list.png) |
| `!return <book title>` | removes a book from the database | ![](./return.png) |

## Architecture

### Overview

```mermaid
sequenceDiagram
    participant User
    participant DiscordBot
    participant FastAPI
    participant Supabase
    participant OpenLibrary

    User->>DiscordBot: Send command (e.g., !find, !borrow, !return, !list)
    DiscordBot->>FastAPI: Forward command
    FastAPI->>OpenLibrary: Search for book info
    OpenLibrary-->>FastAPI: Return book data
    FastAPI->>Supabase: Store/retrieve book data
    Supabase-->>FastAPI: Confirm data operation
    FastAPI-->>DiscordBot: Send response
    DiscordBot-->>User: Display result
```

### DB structure

```mermaid
erDiagram
    BOOKS {
        int id PK
        string title
        string author
        int year
        string isbn
        string cover_url
        datetime created_at
    }
```

## Usage (local hosting)

The bot below is locally hosted.

First, create a discord application [here](https://discord.com/developers/applications) and a bot for that application.

Also enable the relevant permissions under the Bot tab.

Second, create a supabase table with the [`create.sql`](./create.sql) script.

Next place your discord bot token, supabase URL and supabase key in the `.env` file.

```env
DISCORD_TOKEN=XXX
SUPABASE_URL=XXX
SUPABASE_KEY=XXX
```

Then run the below.

```console
$ python3 -m venv myenv
$ source myenv source/bin/activate
$ pip install fastapi uvicorn supabase discord.py python-dotenv requests
$ python3 main.py
```

![](https://media0.giphy.com/media/6f15PceJUw8WGlj4uu/giphy.gif?cid=6c09b9523huev5xhgkdz38wrgduif9fc520sghm1cium0fu4&ep=v1_internal_gif_by_id&rid=giphy.gif&ct=g)