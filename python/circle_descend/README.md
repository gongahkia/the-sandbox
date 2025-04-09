# `circle descend`

A [Crossclimb](https://www.linkedin.com/games/crossclimb/) bot that plays the game for you.

| File name | What it do |
| :--- | :--- |
| `guesser.py` | generates a list of all 4-letter words based off [this repo](https://github.com/dwyl/english-words) |
| `context.py` | generates guesses of possible 4-letter words with huggingface's gpt2 text-generation pipeline |
| `scrape.py` | scrapes the linkedin crossclimb website |

## usage

```console
$ make
```