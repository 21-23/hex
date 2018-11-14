setup_dev:
	stack build --copy-compiler-tool intero hoogle hlint brittany

hoogle: hoogle_generate hoogle_server

hoogle_server:
	stack hoogle -- server --local

hoogle_generate:
	stack hoogle -- generate --local

