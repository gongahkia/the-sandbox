from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import time

chromehand = webdriver.Chrome('../../../../../../Public/chromedriver')
chromehand.get("https://lichess.org/login?referrer=/")
assert "lichess" in chromehand.title

# ----------

# (1) Logging in

login_username = chromehand.find_element(By.NAME, "username")
login_password = chromehand.find_element(By.NAME, "password")
submit_button = chromehand.find_element(By.XPATH, '//button[text()="Sign in"]')

# remember to fill in your actual username and password before pushing this to Git for the FINAL time
login_username.send_keys("")
login_password.send_keys("")
submit_button.click()

# ----------

# (2) Entering a game

# use pyautogui to click the relevant buttons

time.sleep(10)

chromehand.close()
