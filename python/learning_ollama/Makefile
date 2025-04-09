all:config

onetime:
	pip install ollama
	sudo snap install ollama
	ollama download llama-7b
	curl -fsSL https://ollama.com/install.sh | sh

stable:
	ollama run stablelm-zephyr

orca:
	ollama run orca-mini

phi:
	ollama run phi3:mini

vicuna:
	ollama run vicuna

star:
	ollama run starcoder2

geex:
	ollama run codegeex4

code:
	ollama run codellama

mistral:
	ollama run mistral-small

two:
	ollama run llama2