# ----- REQUIRED IMPORTS -----

from playwright.sync_api import sync_playwright
import time

# ----- HELPER FUNCTIONS -----

def fill_google_form(form_url, predefined_inputs):
    """
    fill a google form with predefined inputs
    """
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        page = browser.new_page()
        page.goto(form_url)
        time.sleep(2)  
        for field, value in predefined_inputs.items():
            try:
                input_element = page.wait_for_selector(f"input[aria-label='{field}']", timeout=5000)
                input_element.fill(value)
            except:
                similar_fields = page.query_selector_all("input[aria-label]")
                for element in similar_fields:
                    if field.lower() in element.get_attribute('aria-label').lower():
                        element.fill(value)
                        break
        submit_button = page.query_selector("//span[text()='Submit']")
        submit_button.click()
        time.sleep(2)  
        browser.close()

# ----- EXECUTION CODE -----

if __name__ == "__main__":
    form_url = ""
    predefined_inputs = {
        "Name": "John Doe",
        "Email": "johndoe@example.com",
        "Age": "30",
        "Occupation": "Developer"
    }
    fill_google_form(form_url, predefined_inputs)