# Tele Me My Expenses

Telegram Bot for tracking your expenses and income that writes directly to a Google Sheets.

Bot code to be hosted on [Heroku](https://www.heroku.com/).

Usage requires creation of a populated `.env` file with the below details.

```env
TELEGRAM_TOKEN=XXX
GOOGLE_SHEET_NAME=XXX
GOOGLE_CREDENTIALS_FILE=XXX
```

## Architecture

```mermaid
sequenceDiagram
    actor User
    participant Telegram as Telegram API
    participant Bot as Tele Me My Expenses botcode (hosted on Heroku)
    participant Google as Google Sheets API
    
    Note over User,Google: Initial Setup
    Bot->>Google: Authenticate with service account credentials
    Google->>Bot: Authentication successful
    Bot->>Google: Open specified spreadsheet
    Google->>Bot: Return spreadsheet object
    
    Note over User,Google: Bot Startup
    Bot->>Telegram: Register bot with token
    Telegram->>Bot: Connection established
    Bot->>Telegram: Set up command handlers
    
    Note over User,Google: User Interaction Flow
    User->>Telegram: Send /start command
    Telegram->>Bot: Forward /start command
    Bot->>Telegram: Send welcome message
    Telegram->>User: Display welcome message
    
    Note over User,Google: Expense Logging
    User->>Telegram: Send "/expense 50 Groceries"
    Telegram->>Bot: Forward expense command
    Bot->>Bot: Parse command, amount, description
    Bot->>Bot: Format current date
    Bot->>Google: Search for today's date in spreadsheet
    
    alt Date found in spreadsheet
        Google->>Bot: Return row with today's date
    else Date not found
        Bot->>Google: Find first empty row
        Bot->>Google: Write date to first column
        Google->>Bot: Confirm update
    end
    
    Bot->>Google: Read current expense value
    Google->>Bot: Return current value
    Bot->>Google: Update expense cell (col 5) with new total
    Google->>Bot: Confirm update
    
    Bot->>Google: Read current description
    Google->>Bot: Return current description
    Bot->>Google: Update description cell (col 2) with appended text
    Google->>Bot: Confirm update
    
    Bot->>Google: Read income value (col 4)
    Google->>Bot: Return income value
    Bot->>Google: Calculate and update net value (col 6)
    Google->>Bot: Confirm update
    
    Bot->>Telegram: Send confirmation message
    Telegram->>User: Display "Expense of 50 logged successfully"
    
    Note over User,Google: Income Logging
    User->>Telegram: Send "/income 1000 Salary"
    Telegram->>Bot: Forward income command
    Bot->>Bot: Parse command, amount, description
    Bot->>Google: Search for today's date in spreadsheet
    Google->>Bot: Return row with today's date
    
    Bot->>Google: Read current income value
    Google->>Bot: Return current value
    Bot->>Google: Update income cell (col 4) with new total
    Google->>Bot: Confirm update
    
    Bot->>Google: Read current description
    Google->>Bot: Return current description
    Bot->>Google: Update description cell (col 2) with appended text
    Google->>Bot: Confirm update
    
    Bot->>Google: Read expense value (col 5)
    Google->>Bot: Return expense value
    Bot->>Google: Calculate and update net value (col 6)
    Google->>Bot: Confirm update
    
    Bot->>Telegram: Send confirmation message
    Telegram->>User: Display "Income of 1000 logged successfully"
    
    Note over User,Google: Error Handling
    User->>Telegram: Send "/expense abc Coffee"
    Telegram->>Bot: Forward expense command
    Bot->>Bot: Try to parse amount (fails)
    Bot->>Telegram: Send error message
    Telegram->>User: Display "Amount must be a number"
```