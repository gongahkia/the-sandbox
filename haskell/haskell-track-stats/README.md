# `Haskell track stats`

Remade something I saw recently in Haskell, taking clear inspiration from [Spotify Wrapped](https://www.spotify.com/sg-en/wrapped/).
  
Outputs the following.  

```txt
----------------------------------------------------
            🎶 YOUR TOP TRACKS RECEIPT 🎶          
----------------------------------------------------

Business Name: Spotify
Address: 123 Music Lane, Tune City, CA 90210

Date: December 20, 2024
Receipt No: 001234

----------------------------------------------------
Track Name                | Artist(s)              
----------------------------------------------------
1. Blinding Lights        | The Weeknd            
2. Levitating             | Dua Lipa ft. DaBaby   
3. Watermelon Sugar       | Harry Styles          
4. Save Your Tears        | The Weeknd            
5. Peaches                | Justin Bieber ft. Daniel Caesar
----------------------------------------------------

Total Tracks: 5
Average Popularity: 85/100
Total Duration: 20 min 15 sec

----------------------------------------------------
```

## Usage

1. Place your details in an .env file in the following format.

```env
CLIENT_ID=your_client_id
CLIENT_SECRET=your_client_secret
```

2. Run the below commands.

```console
$ cabal install dotenv
$ sudo apt install runhaskell
$ runhaskell main.hs
```
