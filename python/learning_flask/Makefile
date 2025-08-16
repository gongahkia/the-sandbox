all:start

install:
	@echo "if you don't have a venv open when you install this i pray for your soul"
	@echo "installing from local directory..."
	pip install -e .

start:flaskr
	@echo "running flaskr app"
	@flask --app flaskr run --debug

test:flaskr
	@echo "logging initial testing..."
	@pytest
	@echo "logging detailed test coverage..."
	@coverage run -m pytest
	@echo "logging html test coverage..."
	@coverage html