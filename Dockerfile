FROM haskell

RUN cabal update
RUN cabal install doctest
