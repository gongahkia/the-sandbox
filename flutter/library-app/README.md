# Library App

A highly insecure, simple book catalogue app that calls [Open Library API](https://openlibrary.org/) and stores data with [Supabase](https://supabase.com/).

Made to experience writing a Flutter app that interacts with a DB.


![](https://media1.tenor.com/m/vUTUIPJuBBsAAAAd/illumi-zoldyck-hunter-x-hunter.gif)

## Usage

1. Connect phone to laptop via USB.
2. Allow USB debugging on phone.
3. Create a Supabase DB with the schema under `create.sql`.
4. Add your supabase credentials to a `.env` file in project root.

```env
SUPABASE_URL=XXX
SUPABASE_ANON_KEY=XXX
```

5. Run the below.

```console
$ cd illumi2
$ flutter run
```

6. Open the demo on phone.

## Architecture 

### DB

```mermaid
erDiagram
    USERS {
        bigint id PK
        text email
        text password
        uuid auth_uid
        timestamp created_at
    }
    FAVORITES {
        bigint id PK
        bigint user_id FK
        text book_key
        text title
        text author
        timestamp created_at
    }
    USERS ||--o{ FAVORITES : has
```

### Overview

```mermaid
sequenceDiagram
    participant FA as Flutter App
    participant SA as Supabase Auth
    participant SD as Supabase Database
    participant OL as OpenLibrary API

    FA->>SA: User Registration/Login
    SA->>SD: Store User Data
    FA->>SD: Fetch Favorites
    FA->>OL: Search Books
    FA->>SD: Add Favorites
```