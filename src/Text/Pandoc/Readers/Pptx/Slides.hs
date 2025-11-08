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

import Data.List (find)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.Readers.Pptx.Parse
import Text.Pandoc.XML.Light

-- | Convert Pptx intermediate representation to Pandoc AST
pptxToOutput :: PandocMonad m => ReaderOptions -> Pptx -> m (Meta, [Block])
pptxToOutput _opts pptx = do
  let slides = pptxSlides pptx

  -- Convert each slide to blocks
  let slideBlocks = concatMap slideToBlocks slides

  return (mempty, slideBlocks)

-- | Convert slide to blocks
slideToBlocks :: PptxSlide -> [Block]
slideToBlocks slide =
  let SlideId n = slideId slide
      slideElem = slideElement slide
      ns = elemToNameSpaces slideElem

      -- Extract title from title placeholder
      title = extractSlideTitle ns slideElem

      -- Extract all text from slide (simple approach: get all <a:t> text)
      allText = extractAllText slideElem

      -- Create header
      slideIdent = "slide-" <> T.pack (show n)
      headerText = if T.null title
                   then "Slide " <> T.pack (show n)
                   else title
      header = Header 2 (slideIdent, [], []) [Str headerText]

      -- Create paragraph with content (if any)
      contentBlock = if T.null allText
                     then []
                     else [Para [Str allText]]

   in header : contentBlock

-- | Extract title from title placeholder
extractSlideTitle :: NameSpaces -> Element -> Text
extractSlideTitle ns slideElem =
  case findChildByName ns "p" "cSld" slideElem >>=
       findChildByName ns "p" "spTree" of
    Nothing -> ""
    Just spTree ->
      -- Find shape with ph type="title"
      let shapes = onlyElems $ elContent spTree
          titleShape = find (isTitlePlaceholder ns) shapes
       in maybe "" strContent titleShape

-- | Check if shape is title placeholder
isTitlePlaceholder :: NameSpaces -> Element -> Bool
isTitlePlaceholder ns elem =
  case findChildByName ns "p" "nvSpPr" elem >>=
       findChildByName ns "p" "nvPr" >>=
       findChildByName ns "p" "ph" of
    Just phElem ->
      case findAttr (unqual "type") phElem of
        Just phType -> phType == "title" || phType == "ctrTitle"
        Nothing -> False
    Nothing -> False

-- | Extract all text content from slide (simple recursive extraction)
extractAllText :: Element -> Text
extractAllText elem =
  let textNodes = filterElementsName (\qn -> qName qn == "t") elem
      texts = map strContent textNodes
   in T.unwords texts
