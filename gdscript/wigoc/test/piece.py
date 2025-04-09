import os
import json

COLOR_CYAN = '\033[96m'
COLOR_GREEN = '\033[92m'
COLOR_RED = '\033[91m'
COLOR_RESET = '\033[0m'

def get_files_in_directory(directory):
    return set(f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f)))

def get_files_in_directory_redone(directory):
    return set(os.listdir(directory))

def get_base_name(filename):
    base = os.path.splitext(filename)[0]
    if base.startswith('black_'):
        return base[6:]  
    elif base.startswith('white_'):
        return base[6:]  
    return base

# now deprecated, using validate_piece_files_against_json() instead
def validate_piece_files_against_each_other(black_dir, white_dir):
    print(f"{COLOR_CYAN}~ FINDING PIECE FILES PAIRS ~{COLOR_RESET}")
    black_files = get_files_in_directory(black_dir)
    white_files = get_files_in_directory(white_dir)
    black_bases = {get_base_name(f) for f in black_files} # check the black pieces against the white pieces
    for base in black_bases:
        black_file = f"black_{base}.png"
        white_file = f"white_{base}.png"
        if black_file in black_files and white_file in white_files:
            print(f"{COLOR_GREEN}Match found{COLOR_RESET}: {black_file} {COLOR_CYAN}and{COLOR_RESET} {white_file}")
        else:
            if black_file not in white_files:
                print(f"{black_file}: {COLOR_RED}Equivalent missing in white directory{COLOR_RESET}")
            if white_file not in black_files:
                print(f"{white_file}: {COLOR_RED}Equivalent missing in black directory{COLOR_RESET}")

def validate_piece_files_against_json(black_dir, white_dir, pieces_file):
    num_found = 0
    num_missing = 0
    print(f"\n{COLOR_CYAN}~ CHECKING PIECE FILES ~{COLOR_RESET}\n")
    with open(pieces_file, 'r') as file:
        pieces = json.load(file)
    black_files = get_files_in_directory_redone(black_dir)
    white_files = get_files_in_directory_redone(white_dir)
    for piece_number, piece_name in pieces.items():
        black_piece_file = f"black_{piece_name}.png"
        white_piece_file = f"white_{piece_name}.png"
        if black_piece_file in black_files and white_piece_file in white_files:
            print(f"Piece No #{piece_number}, {black_piece_file}: {COLOR_GREEN}Found in black directory{COLOR_RESET}")
            print(f"Piece No #{piece_number}, {white_piece_file}: {COLOR_GREEN}Found in white directory{COLOR_RESET}")
            num_found += 2
        else:
            if black_piece_file not in black_files:
                print(f"Piece No #{piece_number}, {black_piece_file}: {COLOR_RED}Missing in black directory{COLOR_RESET}")
                num_missing += 1
            if white_piece_file not in white_files:
                print(f"Piece No #{piece_number}, {white_piece_file}: {COLOR_RED}Missing in white directory{COLOR_RESET}")
                num_missing += 1
    print(f"""
{COLOR_CYAN}---{COLOR_RESET}
{num_found} piece files {COLOR_GREEN}FOUND{COLOR_RESET}
{num_missing} piece files {COLOR_RED}MISSING{COLOR_RESET}
{COLOR_CYAN}---{COLOR_RESET}
          """)

# ----- ACTUAL EXECUTION CODE -----

def main():
    black_dir = os.path.join("..", "src", "asset", "piece", "black")
    white_dir = os.path.join("..", "src", "asset", "piece", "white")
    json_file_path = os.path.join("..", "src", "asset", "piece", "piece.json")
    validate_piece_files_against_json(black_dir, white_dir, json_file_path)
    # validate_piece_files_against_each_other(black_dir, white_dir)

if __name__ == "__main__":
    main()
