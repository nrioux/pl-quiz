install:
	stack build --copy-bins
	cp -r bin/static-files /app/.local/bin
