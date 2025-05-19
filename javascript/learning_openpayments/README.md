# Learning [Open Payments](https://github.com/interledger/open-payments)

By building a rudimentary web app. Transferring currencies should be seamless, easy and guaranteed.

![](https://files.readme.io/83f6fb0-Open_Payments_standard_logo.svg)

## Usage

Create a `.env` file with the following details.

```env
WALLET_ADDRESS=XXX
PRIVATE_KEY=XXX
AUTH_SERVER=XXX
PORT=3000
```

Then run the following.

```console
$ npm install express @interledger/open-payments dotenv
$ node backend.js
```