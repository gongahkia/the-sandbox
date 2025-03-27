# ----- required imports -----

import os
from telegram.ext import Updater, CommandHandler, MessageHandler, Filters
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from dotenv import load_dotenv
from datetime import datetime

# ----- initialisation code -----

load_dotenv()
TELEGRAM_TOKEN = os.getenv('TELEGRAM_TOKEN')
SHEET_NAME = os.getenv('GOOGLE_SHEET_NAME')
CREDENTIALS_FILE = os.getenv('GOOGLE_CREDENTIALS_FILE')
scope = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name(CREDENTIALS_FILE, scope)
client = gspread.authorize(creds)
sheet = client.open(SHEET_NAME).sheet1

# ----- helper functions -----

def start(update, context):
    update.message.reply_text('Welcome! Use /expense or /income to log transactions.')

def log_transaction(update, context):
    message = update.message.text.split()
    command = message[0][1:]  
    if len(message) < 3:
        update.message.reply_text('Usage: /expense amount description or /income amount description')
        return
    try:
        amount = float(message[1])
        description = ' '.join(message[2:])
    except ValueError:
        update.message.reply_text('Amount must be a number.')
        return
    date = datetime.now().strftime('%d/%m/%Y')
    try:
        cell = sheet.find(date)
        row = cell.row
    except:
        row = next((i for i, cell in enumerate(sheet.col_values(1), 1) if not cell), sheet.row_count + 1)
        sheet.update_cell(row, 1, date)  
    if command == 'expense':
        current_value = sheet.cell(row, 5).value
        current_value = float(current_value) if current_value else 0
        sheet.update_cell(row, 5, current_value + amount)
        current_desc = sheet.cell(row, 2).value
        new_desc = f"{current_desc}, {description}" if current_desc else description
        sheet.update_cell(row, 2, new_desc)
    elif command == 'income':
        current_value = sheet.cell(row, 4).value
        current_value = float(current_value) if current_value else 0
        sheet.update_cell(row, 4, current_value + amount)
        current_desc = sheet.cell(row, 2).value
        new_desc = f"{current_desc}, {description}" if current_desc else description
        sheet.update_cell(row, 2, new_desc)
    income = float(sheet.cell(row, 4).value or 0)
    expense = float(sheet.cell(row, 5).value or 0)
    sheet.update_cell(row, 6, income - expense)
    update.message.reply_text(f'{command.capitalize()} of {amount} logged successfully.')

def main():
    updater = Updater(TELEGRAM_TOKEN, use_context=True)
    dp = updater.dispatcher
    dp.add_handler(CommandHandler("start", start))
    dp.add_handler(CommandHandler("expense", log_transaction))
    dp.add_handler(CommandHandler("income", log_transaction))
    updater.start_polling()
    updater.idle()

# ----- execution code -----

if __name__ == '__main__':
    main()