{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Pptx.Shapes
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Parsing of PPTX shapes (text boxes, images, tables, diagrams).
-}
module Text.Pandoc.Readers.Pptx.Shapes
  ( PptxShape(..)
  , parseShapes
  , shapeToBlocks
  , isTitlePlaceholder
  ) where

import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry)
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
-- (removed Text.Pandoc.Builder import)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Definition
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.XML.Light

-- | Shape types in PPTX slides
data PptxShape
  = PptxTextBox Text                    -- Plain text content
  | PptxPicture
      { picRelId  :: Text               -- Relationship ID (lazy loading)
      , picTitle  :: Text
      , picAlt    :: Text
      }
  | PptxTable [[Text]]                  -- Simple text cells for now
  | PptxGraphic Text                    -- Placeholder for other graphics
  deriving (Show)

-- | Parse all shapes from shape tree
parseShapes :: NameSpaces -> Element -> [PptxShape]
parseShapes ns spTreeElem =
  let shapeElems = onlyElems $ elContent spTreeElem
      -- Merge parent namespaces with element namespaces
      ns' = ns <> elemToNameSpaces spTreeElem
   in mapMaybe (parseShape ns') shapeElems

-- | Parse individual shape element
parseShape :: NameSpaces -> Element -> Maybe PptxShape
parseShape ns elem
  -- Text box: <p:sp> with <p:txBody>
  | isElem ns "p" "sp" elem =
      case findChildByName ns "p" "txBody" elem of
        Just txBody ->
          let text = extractDrawingMLText txBody
           in if T.null (T.strip text)
              then Nothing
              else Just $ PptxTextBox text
        Nothing -> Nothing

  -- Picture: <p:pic>
  | isElem ns "p" "pic" elem = do
      nvPicPr <- findChildByName ns "p" "nvPicPr" elem
      cNvPr <- findChildByName ns "p" "cNvPr" nvPicPr

      let title = maybe "" id $ findAttr (unqual "name") cNvPr
          alt = maybe "" id $ findAttr (unqual "descr") cNvPr

      -- Get blip relationship ID
      blipFill <- findChildByName ns "p" "blipFill" elem
      blip <- findChildByName ns "a" "blip" blipFill
      relId <- findAttrByName ns "r" "embed" blip

      return $ PptxPicture relId title alt

  -- Table: <p:graphicFrame> with table
  | isElem ns "p" "graphicFrame" elem = do
      graphic <- findChildByName ns "a" "graphic" elem
      graphicData <- findChildByName ns "a" "graphicData" graphic
      uri <- findAttr (unqual "uri") graphicData

      if "table" `T.isInfixOf` uri
        then do
          tbl <- findChildByName ns "a" "tbl" graphicData
          let rows = parseTableRows ns tbl
          return $ PptxTable rows
        else
          -- Could be diagram or chart
          return $ PptxGraphic (strContent graphicData)

  -- Skip other shapes for now
  | otherwise = Nothing

-- | Parse table rows (simple text extraction)
parseTableRows :: NameSpaces -> Element -> [[Text]]
parseTableRows ns tblElem =
  let trElems = findChildrenByName ns "a" "tr" tblElem
   in map (parseTableRow ns) trElems

parseTableRow :: NameSpaces -> Element -> [Text]
parseTableRow ns trElem =
  let tcElems = findChildrenByName ns "a" "tc" trElem
   in map extractCellText tcElems
  where
    extractCellText tcElem =
      -- Get text from txBody/a:p/a:r/a:t
      case findChildByName ns "a" "txBody" tcElem of
        Just txBody -> extractDrawingMLText txBody
        Nothing -> ""

-- | Convert shape to Pandoc blocks
shapeToBlocks :: PandocMonad m => Archive -> [(Text, Text)] -> PptxShape -> m [Block]
shapeToBlocks _archive _rels (PptxTextBox text) =
  if T.null (T.strip text)
    then return []  -- Skip empty
    else return [Para [Str text]]

shapeToBlocks archive rels (PptxPicture relId title alt) = do
  -- Resolve relationship to get media path
  case lookup relId rels of
    Nothing -> return []  -- Image not found
    Just target -> do
      let mediaPath = resolveMediaPath target

      -- Load image bytes and add to MediaBag
      case loadMediaFromArchive archive mediaPath of
        Nothing -> return []
        Just mediaBytes -> do
          P.insertMedia (T.unpack mediaPath) Nothing mediaBytes

          let altText = if T.null alt then [] else [Str alt]
          return [Para [Image nullAttr altText (mediaPath, title)]]

shapeToBlocks _archive _rels (PptxTable rows) =
  -- Simple table representation for now
  case rows of
    [] -> return []
    (headerRow:bodyRows) -> do
      let makeCell text = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str text]]
          headerCells = map makeCell headerRow
          bodyCells = map (map makeCell) bodyRows
          caption = Caption Nothing []
          colSpec = replicate (length headerRow) (AlignDefault, ColWidthDefault)
          headerRow' = Row nullAttr headerCells
          bodyRows' = map (Row nullAttr) bodyCells
          thead = TableHead nullAttr [headerRow']
          tbody = [TableBody nullAttr 0 [] bodyRows']
          tfoot = TableFoot nullAttr []
      return [Table nullAttr caption colSpec thead tbody tfoot]

shapeToBlocks _archive _rels (PptxGraphic text) =
  -- Placeholder for diagrams/charts
  return [Para [Str $ "[Graphic: " <> text <> "]"]]

-- | Resolve media path (handle relative paths)
resolveMediaPath :: Text -> Text
resolveMediaPath target =
  if "../media/" `T.isPrefixOf` target
    then "ppt/media/" <> T.drop 9 target  -- "../media/" = 9 chars
    else if "media/" `T.isPrefixOf` target
      then "ppt/" <> target
      else target

-- | Load media file from archive
loadMediaFromArchive :: Archive -> Text -> Maybe B.ByteString
loadMediaFromArchive archive path =
  case findEntryByPath (T.unpack path) archive of
    Just entry -> Just $ fromEntry entry
    Nothing -> Nothing

-- | Extract text from DrawingML element (finds all <a:t> descendants)
extractDrawingMLText :: Element -> Text
extractDrawingMLText elem =
  let textElems = filterElementsName (\qn -> qName qn == "t") elem
      texts = map strContent textElems
   in T.unwords $ filter (not . T.null) texts

-- | Check if shape is title placeholder (also used in Slides module)
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
