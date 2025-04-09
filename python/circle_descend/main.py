import scrape as sc
import guesser as g
import sorter as so
from selenium import webdriver

if __name__ == "__main__":
    url = "https://raw.githubusercontent.com/dwyl/english-words/master/words.txt"
    temp_file = "words.txt"
    temp_file_2 = "4_letter_words.txt"
    g.download_word_list(url, temp_file)
    four_letter_words = g.extract_four_letter_words(temp_file)
    g.download_4_letter_words(four_letter_words, temp_file_2)
    print(f"4-letter words have been extracted to {temp_file_2}.")