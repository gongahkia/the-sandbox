from playwright.sync_api import sync_playwright


def scrape_linkedin(
    target_url,
    login_username,
    login_password,
    blacklist_array,
    screenshot_filepath,
    log_filepath,
):
    """
    automate access to linkedin
    """

    errors = []

    try:

        p = sync_playwright().start()
        browser = p.chromium.launch(
            headless=False, slow_mo=1000
        )  # for easier debugging but FUA to make this headless later
        # browser = p.chromium.launch(headless=True)
        page = browser.new_page()

        try:

            # ---------- LOGIN CREDENTIALS ----------

            page.goto(target_url)

            page.wait_for_selector("input#username")
            page.wait_for_selector("input#password")
            page.wait_for_selector(
                "div.login__form_action_container button.btn__primary--large"
            )

            page.screenshot(path=f"{screenshot_filepath}login.png")

            print(f"navigating to {target_url}")

            username_input = page.query_selector("input#username")
            password_input = page.query_selector("input#password")
            signin_button = page.query_selector(
                "div.login__form_action_container button.btn__primary--large"
            )

            page.fill("input#username", login_username)
            page.fill("input#password", login_password)
            signin_button.click()

            page.wait_for_timeout(6000)
            page.wait_for_load_state("networkidle")

            # ---------- NAVIGATE TO USER FEED ----------

            page.screenshot(path=f"{screenshot_filepath}feed.png")

            # here is where I give up because 2fa is required and I don't want people to have to specify so many details

        except Exception as e:
            errors.append(f"Error: Unable to process the target URL {target_url}: {e}")

        finally:
            print("Closing browser...")
            browser.close()

    except Exception as e:
        errors.append(f"Error: Failed to initialize Playwright: {e}")

    return errors
