# `learning_clerk`

[Clerk](https://clerk.com/) helps you implement authentication as a plug-and-play component in your frontend projects.

Below is an example app that implements Clerk's sign up and profile management.

## Architecture 

```mermaid
sequenceDiagram

    participant User
    participant ReactApp
    participant ClerkComponents
    participant ClerkAPI
    participant Backend

    User->>ReactApp: Interacts with app
    ReactApp->>ClerkComponents: Renders authentication components
    User->>ClerkComponents: Signs in/up
    ClerkComponents->>ClerkAPI: Authenticates user
    ClerkAPI-->>ClerkComponents: Returns authentication token
    ClerkComponents-->>ReactApp: Provides user session
    ReactApp->>Backend: Makes authenticated requests
    Backend-->>ReactApp: Returns protected data
```

## Usage

1. Place your clerk token within `.env.local`.

```env
NEXT_PUBLIC_CLERK_PUBLISHABLE_KEY=XXX
CLERK_SECRET_KEY=XXX
```

2. Run the following.

```console
$ cd src/example
$ npm install @clerk/nextjs
$ npm run dev
```

3. Use the test account email, phone number and verification code provided by Clerk to sign up and sign in.
