test-watch:
	stack test --fast --haddock-deps --file-watch

hoogle-generate:
	stack hoogle -- generate --local

hoogle-serve: hoogle-generate
	stack hoogle -- server --local --port=8080

# test-watch:
#         stack exec ghcid -- -c="stack ghci test/Spec.hs" -T=":main --color=always"

# watch:
#         stack exec ghcid
