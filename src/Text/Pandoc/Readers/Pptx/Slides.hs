{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Pptx.Slides
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Conversion of PPTX slides to Pandoc AST blocks.
-}
module Text.Pandoc.Readers.Pptx.Slides
  ( pptxToOutput
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.Pptx.Parse

-- | Convert Pptx intermediate representation to Pandoc AST
pptxToOutput :: PandocMonad m => ReaderOptions -> Pptx -> m (Meta, [Block])
pptxToOutput _opts pptx = do
  let presDoc = pptxPresentation pptx
      slideRefs = presSlideIds presDoc

  -- Create headers for each slide
  let headers = map slideIdToHeader slideRefs

  return (mempty, headers)

-- | Convert SlideId to Header block (minimal implementation)
slideIdToHeader :: (SlideId, Text) -> Block
slideIdToHeader (SlideId n, _relId) =
  Header 2 (slideIdent, [], []) [Str slideTitle]
  where
    slideIdent = "slide-" <> T.pack (show n)
    slideTitle = "Slide " <> T.pack (show n)
