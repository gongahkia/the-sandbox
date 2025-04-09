all:run

run:
	@echo "executing main.odin..."
	@odin run main.odin

config:
	@echo "installing odin config files..."
	@git clone https://github.com/odin-lang/Odin.git
	@cd Odin
	@make release-native
	@echo 'export PATH="$HOME/Odin:$PATH"' >> ~/.bashrc
	@source ~/.bashrc
	@odin version

