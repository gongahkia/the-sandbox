import requests

def download_word_list(url, output_file):
    response = requests.get(url)
    response.raise_for_status() 
    with open(output_file, 'w') as file:
        file.write(response.text)

def extract_four_letter_words(input_file):
    with open(input_file, 'r') as file:
        words = file.readlines()
    four_letter_words = "\n".join([word.strip() for word in words if "." not in word and "," not in word and "-" not in word and len(word.strip()) == 4])
    return four_letter_words

def download_4_letter_words(four_letter_words, output_file):
    with open(output_file, 'w') as file:
        file.write("")
    with open(output_file, 'a') as file:
        file.write(four_letter_words)

def load_words(dictionary_file):
    with open(dictionary_file, 'r') as file:
        words = [line.strip() for line in file if len(line.strip()) == 4]
    return words