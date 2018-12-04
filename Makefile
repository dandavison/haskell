all:
	stack runghc -- $$(ls -t src/*.hs | head -n 1) < input

ghci:
	rlwrap stack ghci
