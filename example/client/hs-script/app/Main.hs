module Main (main) where

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.NonElementParentNode (getElementByIdUnchecked)
import GHCJS.DOM.Element (setInnerHTML)

main :: IO ()
main = do
  doc <- currentDocumentUnchecked
  hsOutput <- getElementByIdUnchecked doc "hs-output"
  setInnerHTML hsOutput "Hello from Haskell"
