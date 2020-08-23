module Markdown
  (
    Markdown
  , Block(..)
  , Inline(..)
  , ListType(..)
  , HeadingType
  ) where

data Block = Paragraph Index [Inline]
           | Headering HeadingType Index [Inline]
           | Quote Index [Inline]
           | List Index ListType [[Inline]]
           | Divider Index
           | Code Index Language Code deriving (Eq, Show)

data HeadingType = H1 | H2 | H3 | H4 | H5 | H6 deriving (Eq, Show, Enum)

data Inline = Link Index LinkName LinkAddress
            | Image Index ImageAlt ImageAddress
            | Text Index String
            | Italic Index [Inline]
            | Strong Index [Inline]
            | InlineCode Index Code deriving (Eq, Show)

data ListType = OrderedList | UnorderedList deriving (Eq, Show)

type Markdown = [Block]

type Language = Maybe String
type Code = String
type LinkName = String
type LinkAddress = String
type ImageAlt = String
type ImageAddress = String

type Index = Int
