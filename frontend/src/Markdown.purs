module Markdown (renderMarkdownToDiv) where

import Data.Unit (Unit)
import Effect (Effect)

foreign import renderMarkdownToDiv ∷ String -> String -> Effect Unit

