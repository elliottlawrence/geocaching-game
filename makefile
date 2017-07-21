build:
	stack build --stack-yaml=desktop/stack.yaml
	stack exec geocaching-game-exe --stack-yaml=desktop/stack.yaml

buildjs:
	stack build --stack-yaml=js/stack.yaml
	open js/.stack-work/install/x86_64-osx/lts-6.30/ghcjs-0.2.0.9006030_ghc-7.10.3/bin/geocaching-game-exe.jsexe/index.html
