install:
	stack build --copy-bins
	cp -r static-files /app/.local/bin
