# Learning Stripe *(with FastAPI and Supabase)*

*The values in the 2 images below are **test data** given by stripe.*

![](./existing.png)
![](./new.png)

## Usage via locally hosting

First store your stripe secret key, supabase URL and supabase key in a `.env` file.

```env
STRIPE_SECRET_KEY=XXX
SUPABASE_URL=XXX
SUPABASE_KEY=XXX
```

Next add your stripe publishable key to `script.js`.

```js
// other code above...

async function initializeStripe() {
    const publishableKey = "XXX" // add it here
    stripe = Stripe(publishableKey);
    const elements = stripe.elements();
    const cardElement = elements.create('card');
    cardElement.mount('#card-element');
}

// other code below...
```

Then create the supabase db with the [`create.sql`](./create.sql) file.

Then run the below.

```console
$ python3 -m venv myenv
$ sourc myenv/bin/activate
$ pip install fastapi uvicorn stripe supabase python-dotenv
$ python3 main.py
```

Use the [test card numbers](https://docs.stripe.com/testing#cards) provided by stripe.

## Architecture

### Overview

#### Structure

```mermaid
graph TD
    A[index.html] --> B[script.js]
    B --> C[Stripe.js]
    B --> D[main.py]
    D --> E[FastAPI]
    D --> F[Stripe API]
    D --> Supabase@{shape: cyl}
    E --> H[create-payment]
    E --> I[webhook]
    E --> J[signup]
    E --> K[login]
    E --> L[logout]
    E --> M[protected]
    H --> F
    H --> Supabase
    I --> F
    I --> Supabase
    J --> Supabase
    K --> Supabase
    L --> Supabase
    M --> Supabase
```

#### Sequence

```mermaid
sequenceDiagram
    actor User
    participant Frontend
    participant Backend
    participant Stripe
    participant Supabase

    User->>Frontend: Enter payment details
    Frontend->>Stripe: Create token
    Stripe-->>Frontend: Return token
    Frontend->>Backend: Send payment request
    Backend->>Stripe: Create charge
    Stripe-->>Backend: Confirm charge
    Backend->>Supabase: Check if user exists
    Supabase-->>Backend: User data
    alt User doesn't exist
        Backend->>Supabase: Create new user
    else User exists
        Backend->>Supabase: Update user payment
    end
    Supabase-->>Backend: Confirmation
    Backend-->>Frontend: Payment result
    Frontend-->>User: Display result
```

### DB

```mermaid
erDiagram
    USERS {
        VARCHAR(255) id PK
        NUMERIC(194) payment_amount
    }
```
