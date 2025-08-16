from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time

def get_clues_and_input_boxes(driver, url):
    driver.get(url)
    print("Please click the start button manually before continuing...")
    WebDriverWait(driver, 300).until(
        EC.element_to_be_clickable((By.ID, "ember36")) 
    )
    time.sleep(3)  
    clues = driver.find_elements(By.CLASS_NAME, "crossclimb__clue-section")
    input_boxes = driver.find_elements(By.CLASS_NAME, "crossclimb__guess__inner")
    clues_text = []
    for input_box in input_boxes:
        input_box.click() 
        time.sleep(1) 
        clue = driver.find_element(By.CLASS_NAME, "crossclimb__clue-section").text
        clues_text.append(clue)
        print(f"Clue found: {clue}") 
    return clues_text

if __name__ == "__main__":
    url = "https://www.linkedin.com/games/crossclimb"
    driver = webdriver.Chrome()
    try:
        clues = get_clues_and_input_boxes(driver, url)
        print("All clues found:", clues)
    finally:
        driver.quit() 
