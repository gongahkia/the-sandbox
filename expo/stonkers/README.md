# `Stonkers`

Personal step tracking app written in 

[React Native](https://reactnative.dev/) and powered by [Expo](https://expo.dev/), with [Clerk](https://clerk.com/) authentication and [Supabase](https://supabase.com/) for DB storage.

## Screenshots

<div style="display: flex; justify-content: space-between;">
  <img src="./login.png" width="32%">
  <img src="./search.png" width="32%">
  <img src="./library.png" width="32%">
</div>

## Usage

First create a Supabase database with the [`create.sql`](./src/create.sql) and place your Supabase URL and Supabase anonymous public key within the `.env` file.

Then create a Clerk developer account for Expo and place your Clerk publishable key within the `.env` file.

```env
EXPO_PUBLIC_CLERK_PUBLISHABLE_KEY=XXX
EXPO_PUBLIC_SUPABASE_URL=XXX
EXPO_PUBLIC_SUPABASE_ANON_KEY=XXX
```

Then run.

```console
$ cd src/stonkers-app
$ npm i
$ npx expo start -c --tunnel
```

Then scan the QR code with the [Camera app](https://docs.expo.dev/versions/latest/sdk/camera/) on IOS or the [Expo Go](https://play.google.com/store/apps/details?id=host.exp.exponent&hl=en_SG) app on Android.

## Architecture

### DB

```mermaid
```

### Overview

```mermaid
```