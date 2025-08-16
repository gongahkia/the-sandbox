import json
from playwright.sync_api import sync_playwright
from transformers import pipeline

def generate_answer(prompt):
    """
    generates an answer to the given prompt
    """
    llm = pipeline('text-generation', model='gpt2')  
    return llm(prompt, max_length=50)[0]['generated_text']

def fill_form(form_url, config):
    """
    fills out the form with the given configuration
    """

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        page = browser.new_page()
        page.goto(form_url)

        for field_type, fields in config.items():

            for question, answer in fields.items():

                if field_type == "text":
                    page.fill(f'input:has-text("{question}")', answer)

                elif field_type == "multiple_choice":
                    page.check(f'label:has-text("{question}") >> input[value="{answer}"]')

                elif field_type == "long_answer":
                    long_answer = generate_answer(answer)
                    page.fill(f'textarea:has-text("{question}")', long_answer)

        page.click('button[type="submit"]')
        browser.close()

# ----- main execution code -----

if __name__ == "__main__":
    with open('config.json') as f:
        config = json.load(f)
    fill_form('YOUR_GOOGLE_FORM_URL', config)