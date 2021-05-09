# How to build a package with dependencies from hackage:

cabal update
cd ListLike-4.2.0
cabal sandbox init    # The sandbox is associated with the current directory
cabal install vector   # install the too-new version of vector into the sandbox
(Edit the dependencies of ListLike-4.2.0 to require vector-0.11)
cabal build

cabal update
cabal sandbox init
cabal configure
cabal install --dependencies-only

# How to build haskell-names with an updated version of haskell-src-exts

cabal update
cd haskell-names
cabal sandbox init
cabal install ../haskell-src-exts.one-ast
cabal configure --enable-tests
cabal install --dependencies-only

# How to build haskell-src-meta with new haskell-src-exts and haskell-src-exts-simple

cd haskell-src-meta.bmillwood
cabal sandbox init
cabal install haskell-src-exts haskell-src-exts-simple
cabal configure --enable-tests
cabal install --Logi
