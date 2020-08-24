module Zipper
  (
    -- inlineUp
    -- inlineTo,
  ) where

import Data.List (break)
import Markdown

{-
Markdown
    Paragraph
        Link
        Image
        Text
        Italic
            Link
            Image
            Text
            Italic
            Strong
        Strong
            Link
            Image
            Text
            Italic
            Strong
        InlineCode
    Headering
        ...
    Quote
        ...
    List
        ...
    Divider
    Code
-}

data MDItem = ParagraphItem
            | HeaderingItem HeadingType
            | QuoteItem
            | OrderedListItem
            | UnorderedListItem

            -- Inline types
            | ItalicItem
            | StrongItem deriving (Show, Eq)

data MDCrumb = MDCrumb Index MDItem [Inline] [Inline] deriving (Show)

type Zipper = (Block, Inline, [MDCrumb])

testTree :: Markdown
testTree = [
            Paragraph 6 [
              Italic 2 [
                Text 1 "Italic"
              ],
              Text 3 " \t",
              Italic 5 [
                Text 4 "Italic"
              ]
            ]
          ]

-- run example: map inlineUp (map zipper testTree)
zipper :: Block -> Zipper
zipper b = (b, None, [])

zipperUp :: Zipper -> Zipper
-- First parse for all Blocks
zipperUp (_, inline, (MDCrumb index ParagraphItem ls rs):bs) = (Paragraph index (ls ++ [inline] ++ rs), None, [])
zipperUp (_, inline, (MDCrumb index (HeaderingItem br) ls rs):bs) = (Headering br index (ls ++ [inline] ++ rs), None, [])
zipperUp (_, inline, (MDCrumb index QuoteItem ls rs):bs) = (Quote index (ls ++ [inline] ++ rs), None, [])
zipperUp (_, inline, (MDCrumb index OrderedListItem ls rs):bs) = (List index OrderedList [(ls ++ [inline] ++ rs)], None, [])
zipperUp (_, inline, (MDCrumb index UnorderedListItem ls rs):bs) = (List index UnorderedList [(ls ++ [inline] ++ rs)], None, [])

zipperUp (block, inline, (MDCrumb index ItalicItem ls rs):bs) = (block, Italic index (ls ++ [inline] ++ rs), bs)
zipperUp (block, inline, (MDCrumb index StrongItem ls rs):bs) = (block, Strong index (ls ++ [inline] ++ rs), bs)


inlineTo' :: Index -> Zipper -> Maybe Zipper
-- Parse for Paragraph
inlineTo' index (Paragraph index' items, None, bs) =
    let (ls, rs) = break (zipperIs index) items
        block = Paragraph index' items
    in  inlineTo'' index' block ParagraphItem ls rs bs
-- Parse for Headering
inlineTo' index (Headering br index' items, None, bs) =
    let (ls, rs) = break (zipperIs index) items
        block = Headering br index' items
    in  inlineTo'' index' block (HeaderingItem br) ls rs bs
-- Parse for Quote
inlineTo' index (Quote index' items, None, bs) =
    let (ls, rs) = break (zipperIs index) items
        block = Quote index' items
    in  inlineTo'' index' block QuoteItem ls rs bs
-- Parse for Italic
inlineTo' index (block, Italic index' (item:items), bs) =
    let (ls, rs) = break (zipperIs index) [item]
    in  inlineTo'' index' block ItalicItem ls rs bs
-- Parse for Strong
inlineTo' index (block, Strong index' items, bs) = 
    let (ls, rs) = break (zipperIs index) items
    in  inlineTo'' index' block StrongItem ls rs bs


inlineTo'' :: Index -> Block -> MDItem -> [Inline] -> [Inline] -> [MDCrumb] -> Maybe Zipper
inlineTo'' _ _ _ _ [] _ = Nothing
inlineTo'' index block type' ls (item:rs) bs = Just (block, item, (MDCrumb index type' ls rs) : bs)


zipperIs :: Index -> Inline -> Bool
zipperIs index (Link index' _ _) = index' == index
zipperIs index (Image index' _ _) = index' == index
zipperIs index (Text index' _) = index' == index
zipperIs index (Italic index' _) = index' == index
zipperIs index (Strong index' _) = index' == index
zipperIs index (InlineCode index' _) = index' == index
