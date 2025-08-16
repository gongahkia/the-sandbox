all:config

start:
	@echo "executing try.py..."
	python3 try.py

config:
	@echo "installing dependancies..."
	pip install pytesseract
	pip uninstall numpy pandas
	pip install numpy pandas
	sudo apt update
	sudo apt install tesseract-ocr
	sudo apt install libtesseract-dev
	sudo apt install tesseract-ocr-fra
