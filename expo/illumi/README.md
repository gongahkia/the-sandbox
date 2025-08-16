# `Illumi`

Personal booktracking app written in [React Native](https://reactnative.dev/) and powered by [Expo](https://expo.dev/).

Stores data with [Supabase](https://supabase.com/).

<div align="center">
    <img src="./illumi.webp" width="60%">
</div>

## Screenshots

<div style="display: flex; justify-content: space-between;">
  <img src="./login.png" width="32%">
  <img src="./search.png" width="32%">
  <img src="./library.png" width="32%">
</div>

## Usage

First create a Supabase database with the [`create.sql`](./src/create.sql) and place your Supabase URL and Supabase anonymous public key within [`src/illumi-app/src/lib/supabase.ts`](./src/illumi-app/src/lib/supabase.ts).

```ts
const supabaseUrl = "XXX";
const supabaseAnonKey = "XXX";
```

Then run.

```console
$ cd src/illumi-app
$ npm i
$ npx expo start -c --tunnel
```

Then scan the QR code with the [Camera app](https://docs.expo.dev/versions/latest/sdk/camera/) on IOS or the [Expo Go](https://play.google.com/store/apps/details?id=host.exp.exponent&hl=en_SG) app on Android.

## Architecture

### DB

```mermaid
erDiagram
    user_auth {
        UUID id PK
        TEXT username 
        TEXT password
        TIMESTAMP created_at
    }

    user_profiles {
        UUID id PK 
        TEXT username 
    }

    user_books {
        UUID id PK
        UUID user_id FK
        TEXT book_id
        TEXT title
        TEXT author
        TEXT cover_url
        TIMESTAMP date_added
    }

    user_auth ||--o{ user_profiles : "has"
    user_auth ||--o{ user_books : "owns"

```

### Overview

```mermaid
sequenceDiagram
    participant User
    participant ExpoApp
    participant Supabase
    participant OpenLibrary

    User->>ExpoApp: Open App
    User->>ExpoApp: Login/Register
    ExpoApp->>Supabase: Check/Create User in user_auth table
    Supabase-->>ExpoApp: User Authenticated
    User->>ExpoApp: Search for Books
    ExpoApp->>OpenLibrary: Fetch Book Data
    OpenLibrary-->>ExpoApp: Return Book Data
    User->>ExpoApp: Add Book to Library
    ExpoApp->>Supabase: Insert Book into user_books table
    Supabase-->>ExpoApp: Book Added
    User->>ExpoApp: View My Library
    ExpoApp->>Supabase: Fetch Books from user_books table
    Supabase-->>ExpoApp: Return Book List
    ExpoApp-->>User: Display Library
```