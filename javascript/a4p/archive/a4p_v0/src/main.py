# ----- REQUIRED IMPORTS -----

import bot as b
import helper as h

# ----- SAMPLE EXECUTION CODE -----

if __name__ == "__main__":
    TARGET_URl = "https://www.linkedin.com/feed/"
    SCREENSHOT_FILEPATH = "./screenshot_log/"
    LOG_FILEPATH = "./generated_log/"
    credentials = h.read_credentials()
    h.pretty_print_json(credentials)
    b.scrape_linkedin(
        TARGET_URl,
        credentials["username"],
        credentials["password"],
        credentials["blacklist_array"],
        SCREENSHOT_FILEPATH,
        LOG_FILEPATH,
    )
