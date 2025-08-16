all: dev

dev:
	@clear
	@echo "Starting Sisyphus IDE in dev mode..."
	pnpm install 
	node scripts/build.js && node scripts/dev.js

release:
	@clear
	@echo "Packaging Sisyphus IDE for release..."
	pnpm install 
	node scripts/build.js && node scripts/release.js