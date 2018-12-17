run: build
	stack runghc -- $$(ls -t src/*.hs | head -n 1) < input

build:
	stack build

ghci:
	stack ghci
