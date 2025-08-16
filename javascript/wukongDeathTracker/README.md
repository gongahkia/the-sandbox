# Wukong Death Tracker

Tracks your deaths, dodges and utilities in [Black Myth Wukong](https://store.steampowered.com/app/2358720/Black_Myth_Wukong/).

For me to learn how to use [Overwolf SDK](https://dev.overwolf.com/ow-native/getting-started/overview/).

Uses Overwolf's event definitions for BMW per [here](https://dev.overwolf.com/ow-native/live-game-data-gep/supported-games/black-myth-wukong/?game-tab=docs).

Uses OCR to extract the Bossname and a hardcoded timer to track the dodge recharge.

## Architecture

```mermaid
sequenceDiagram
    participant Game as Black Myth Wukong Game
    participant Overwolf as Overwolf Events API
    participant App as JavaScript App
    participant OCR as Tesseract OCR
    participant UI as User Interface

    Note over Game,UI: Boss Fight Tracking Flow

    Game->>Overwolf: Trigger game event (match_start)
    Overwolf->>App: onNewEvents listener (match_start)
    activate App
    App->>Overwolf: Request screenshot
    Overwolf->>App: Return screenshot URL
    App->>OCR: Process screenshot with Tesseract
    OCR->>App: Return extracted text
    App->>App: Parse text to find boss name
    App->>App: Set isInBossFight = true
    App->>App: Set currentBoss = extracted name
    App->>App: Reset deathCount = 0
    App->>UI: Update display (boss name, death count)
    deactivate App

    Game->>Overwolf: Trigger game event (death)
    Overwolf->>App: onNewEvents listener (death)
    activate App
    App->>App: Check if isInBossFight
    App->>App: Increment deathCount
    App->>UI: Update display (death count)
    deactivate App

    Game->>Overwolf: Trigger game event (match_end)
    Overwolf->>App: onNewEvents listener (match_end)
    activate App
    App->>App: Check if isInBossFight
    App->>App: Save stats (boss name, death count)
    App->>App: Set isInBossFight = false
    App->>App: Set currentBoss = null
    deactivate App

    Note over Game,UI: State Variables
```
