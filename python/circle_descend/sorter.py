def one_letter_difference(word1, word2):
    if len(word1) != len(word2):
        return False
    differences = sum(1 for a, b in zip(word1, word2) if a != b)
    return differences == 1

def sort_words_by_letter_difference(words):
    sorted_words = [words.pop(0)] 
    while words:
        for i, word in enumerate(words):
            if one_letter_difference(sorted_words[-1], word):
                sorted_words.append(word)
                words.pop(i)
                break
    return sorted_words

if __name__ == "__main__":
    possible_words = ["game", "came", "camp", "lamp", "lame"] # FUA Example list
    sorted_words = sort_words_by_letter_difference(possible_words)
    print("Sorted words by one-letter difference:", sorted_words)