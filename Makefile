test:
	stack test
test-watch:
	stack exec ghcid -- -c="stack ghci test/Spec.hs" -T=":main --color=always"

watch:
	stack exec ghcid
