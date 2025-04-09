all:config

config:
	@echo "cloning remote repository..."
	@git clone https://github.com/realitix/pyshaderc.git
	@cd pyshaderc
	@python3 setup.py build
	@python3 setup.py install
	@echo "installing dependancies..."
	@pip install -r requirements.txt