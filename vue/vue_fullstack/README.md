# Vue fullstack

Simple [Vue.js](https://vuejs.org/) fullstack web application that generates and saves your passwords to [Supabase](https://supabase.com/).

![](./full.png)

## Usage

Add your supabase url and supabase api key in a `.env` file.

```env
VITE_SUPABASE_URL=XXX
VITE_SUPABASE_ANON_KEY=XXX
```

Then run the below.

```console
$ npm install @supabase/supabase-js
$ npm run serve
```

## Architecture

### DB

```mermaid
erDiagram
    PASSWORDS {
        UUID id PK
        STRING password
        STRING description
        TIMESTAMP created_at
    }
```

### Overview

```mermaid
sequenceDiagram
    participant User
    participant Vue as Vue.js App
    participant Supabase
    participant Database

    User->>Vue: Enter password details
    Vue->>Supabase: Insert password
    Supabase->>Database: Store password
    Database-->>Supabase: Confirm storage
    Supabase-->>Vue: Return result
    Vue->>User: Display confirmation

    User->>Vue: Request saved passwords
    Vue->>Supabase: Fetch passwords
    Supabase->>Database: Query passwords
    Database-->>Supabase: Return passwords
    Supabase-->>Vue: Return passwords
    Vue->>User: Display passwords
```