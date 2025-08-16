import pyautogui
import time
import random
import requests
import json
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By

# ------------

# fill in your actual username and password before pushing this to Git for the FINAL time
lichess_username = ""
lichess_password = ""

# ~Coordinate List~
# Bullet 2 + 1 [960, 240]
# Blitz 5 + 0 [960, 410] 
# Rapid 10 + 0 [770, 580]
# Rapid 10 + 5 [960, 580]
# Classical 30 + 20 [960, 670]

game_type_x_coord = 770
game_type_y_coord = 580

# ~Do not change the values below~

chat_x_coord = 385
chat_y_coord = 868

bible_api = requests.get(f"https://bible-api.com/Matthew {random.randint(1,29)}:{random.randint(1,20)}?translation=kjv")
verse_dict = json.loads(bible_api.text)
message = f"{verse_dict['reference']} {verse_dict['verses'][0]['text']}"
print(message)

# ------------

chromehand = webdriver.Chrome('../../../chromedriver.exe')
chromehand.get("https://lichess.org/login?referrer=/")
chromehand.maximize_window()
assert "lichess" in chromehand.title

# ------------

# (1) Logging in

login_username = chromehand.find_element(By.NAME, "username")
login_password = chromehand.find_element(By.NAME, "password")
submit_button = chromehand.find_element(By.XPATH, '//button[text()="Sign in"]')

login_username.send_keys(lichess_username)
login_password.send_keys(lichess_password)
submit_button.click()

# ------------

# (2) Game selection

pyautogui.moveTo(game_type_x_coord,game_type_y_coord,1.5) 
pyautogui.click()

print(chromehand.current_url) # prints "https://lichess.org/" (lichess home page URL)

while chromehand.current_url == "https://lichess.org/": # pauses program until the game has loaded in
    pass

print(chromehand.current_url) # prints unique URL for the respective game in the format of "https://lichess.org/XXXXXXXXXXXX"

# ------------

# (3) Add pyautogui to click the chat and send in the bible verse, click send. (Perhaps implement selenium if pyautogui cannot do the trick.)

pyautogui.moveTo(chat_x_coord, chat_y_coord, 1.5)
pyautogui.click()

pyautogui.write("Hello! Just wanted to share something with you.")
pyautogui.press('enter')

pyautogui.write(message)
pyautogui.press('enter')

pyautogui.write("Thanks again for your time. Have a blessed day!")
pyautogui.press('enter')

time.sleep(7)
chromehand.close()

# ------------
