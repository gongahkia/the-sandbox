VENV_NAME = venv
PYTHON = python3

config:
	@pip install transformers torch diffusers chatlab dotenv cohere requests google-cloud-language sentencepiece

.PHONY: venv
venv:
	@echo "creating virtual environment..."
	$(PYTHON) -m venv $(VENV_NAME)
	@echo "virtual environment created"

.PHONY: activate
act:
	@echo "activate the virtual environment with:"
	@echo "python3 -m venv $(VENV_NAME)"
	@echo "source $(VENV_NAME)/bin/activate"

.PHONY: deactivate
deact:
	@echo "deactivate manually by running: deactivate"