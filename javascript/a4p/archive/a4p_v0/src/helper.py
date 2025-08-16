import os
import json
from dotenv import load_dotenv


def read_credentials():
    """
    read credentials from a .env file
    """
    load_dotenv()
    username = os.getenv("LOGIN_USERNAME")
    password = os.getenv("LOGIN_PASSWORD")
    blacklist_phrases = os.getenv("BLACKLIST")
    if username and password and blacklist_phrases:
        return {
            "username": username.strip(),
            "password": password.strip(),
            "blacklist_array": [
                el.strip() for el in blacklist_phrases.strip().split(",")
            ],
        }
    else:
        print(
            "One or more credentials are missing in the .env file. See the README.md for the required details."
        )


def pretty_print_json(json_object):
    """
    pretty prints json data to
    the cli for easy viewing
    """
    print(json.dumps(json_object, indent=4))


def write_json(json_object, filename):
    """
    write a python dictionary to a
    local JSON file
    """
    with open(filename, "w") as json_file:
        json.dump(json_object, json_file, indent=4)
        print(f"json file written to filepath: {filename}")
