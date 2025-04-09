# `Stonkers`

Personal step tracking app written in  [React Native](https://reactnative.dev/) and powered by [Expo](https://expo.dev/), with [Clerk](https://clerk.com/) authentication and [Supabase](https://supabase.com/) for DB storage.

## Usage

First create a Supabase database with the [`create.sql`](./src/create.sql) and place your Supabase URL and Supabase anonymous public key within `clerk.ts`.

Then create a Clerk developer account for Expo and place your Clerk publishable key within `supabase.ts`.

Then run.

```console
$ cd src/stonkers-app
$ npm i
$ npx expo start --clear -c --tunnel
```

Finally scan the QR code with the [Camera app](https://docs.expo.dev/versions/latest/sdk/camera/) on IOS or the [Expo Go](https://play.google.com/store/apps/details?id=host.exp.exponent&hl=en_SG) app on Android.