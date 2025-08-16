# `QuestionAIre`

Let an LLM fill out [google forms](https://docs.google.com/forms/) for you.

## Architecture

```mermaid
graph TD
    B([<code>config.json</code>]) -->|Read into| C([<code>fill.py</code>])
    C -->|Calls| D[[Playwright]]
    D -->|Automate filling| E[Google Form]
    C -->|Generates Answers| F[[Local LLM]]
    F -->|Provides answers for long-form questions|D
    E -->|Submits Responses to| H[(Google Forms<br>DB)]
```

## Usage

1. Specify your forms filling configuration withi `config.json`.
2. Run the following commands.

```console
$ make config
$ make 
```