Game of Life
============

Conway's game of life implemented in Haskell. To get this running, clone the repo, cd inside it and do:

    cabal install --only-dependencies
    
Once you've done that (assuming you have an up-to-date version of cabal and the above command actually worked) you can run the program by typing:

    runhaskell main.hs <example file>

Where the example file describes an initial board state. You could, for example, do:

    runhaskell main.hs examples/glider-gun.txt

**Note:** Should the example file specified be missing, things will explode violently.