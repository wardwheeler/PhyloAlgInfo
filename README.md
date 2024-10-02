# PhyloAlgInfo
Phylogenetic Algorithmic Information

Haskell source code

To install tools:

which ghcup || curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
which ghc   || ghcup install ghc   latest
which cabal || ghcup install cabal latest
cabal update

To compile:

cabal build phylocomplexity --with-compiler ghc-9.10.1 --allow-newer

execution:  phyloComplexity machineConfigurationfile outputFileNameStub


Haskell compiler and Cabal can be installed vi GHCUP:
	https://www.haskell.org/ghcup/
