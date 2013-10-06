#free-prezen

Presentation tool for Haskell with Free monad!

##INSTALL

```
$ git clone https://github.com/alucky0707/free-prezen.git && cd free-prezen
$ cabal install
```

##EXAMPLE

```hs
import Graphics.UI.FreePrezen

main = loadFont "VL-PGothic-Regular.ttf" >>= \font -> runPrezen def $ do
  page (PageConfig { pageFont = font, pageBackground = Left (white, black)}) $ do
    text 60 black "Hello, World!"
```

You can see more example in `example` directory.
