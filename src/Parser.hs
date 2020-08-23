module Parser
  (
    parserMarkdown
  ) where

import Text.Parsec
import Text.Parsec.String
import Markdown
import Control.Monad ( guard )

parserMarkdown :: Parsec String Int Markdown
parserMarkdown = many parseBlock <* eof

parseBlock :: Parsec String Int Block
parseBlock = choice [ try parseHeading
                    , try parseQuote
                    , try parseList
                    , try parseDivider
                    , try parseCode
                    , parseParagraph ]

parseHeading :: Parsec String Int Block
parseHeading = do
  chars <- many1 $ char '#'
  let charL = length chars
  guard (charL <= 6)
  spaces''
  inlines <- parseInlines
  modifyState (+1)
  index <- getState
  return $ Headering (toEnum $ pred charL) index inlines

parseQuote :: Parsec String Int Block
parseQuote = do
  char '>' <* spaces''
  inlines <- parseInlines
  modifyState (+1)
  index <- getState
  return $ Quote index inlines

parseList :: Parsec String Int Block
parseList = parseOrderedList <|> parseUnorderedList
  where parseOrderedList :: Parsec String Int Block
        parseOrderedList = do
          inlines <- many1 orderedListItem
          modifyState (+1)
          index <- getState
          return $ List index OrderedList inlines

        orderedListItem :: Parsec String Int [Inline]
        orderedListItem = ((many1 digit >> oneOf ".)") <* spaces'') >> parseInlines

        parseUnorderedList :: Parsec String Int Block
        parseUnorderedList = do
          inlines <- many1 unorderedListItem
          modifyState (+1)
          index <- getState
          return $ List index UnorderedList inlines

        unorderedListItem :: Parsec String Int [Inline]
        unorderedListItem = ((oneOf "*-") <* spaces'') >> parseInlines

parseDivider :: Parsec String Int Block
parseDivider = (hyphen <|> asterisk) <* spaces' <* endOfLine
  where hyphen :: Parsec String Int Block
        hyphen = do
          chars <- many1 $ char '-'
          guard (length chars >= 3)
          modifyState (+1)
          index <- getState
          return $ Divider index
        
        asterisk :: Parsec String Int Block
        asterisk = do
          chars <- many1 $ char '*'
          guard (length chars >= 3)
          modifyState (+1)
          index <- getState
          return $ Divider index

parseCode :: Parsec String Int Block
parseCode = do
  string "```"
  language <- between' spaces' (spaces' <* endOfLine) anyChar
  code <- manyTill anyChar (string "```")
  spaces'
  endOfLine
  modifyState (+1)
  index <- getState
  return $ Code index (if null language then Nothing else Just language) code

parseParagraph :: Parsec String Int Block
parseParagraph = do
  inlines <- parseInlines
  modifyState (+1)
  index <- getState
  return $ Paragraph index inlines

parseInlines :: Parsec String Int [Inline]
parseInlines = manyTill parseInline endOfLine

parseInline :: Parsec String Int Inline
parseInline = choice [ try parseLink
                     , try parseImage
                     , try parseInlineCode
                     , try parseStrong
                     , try parseItalic
                     , parseString ]

parseLink :: Parsec String Int Inline
parseLink = do
  name <- between' (char '[') (char ']') anyChar
  address <- between' (char '(') (char ')') anyChar
  modifyState (+1)
  index <- getState
  return $ Link index name address

parseInlineCode :: Parsec String Int Inline
parseInlineCode = do
  code <- between' (char '`') (char '`') anyChar
  modifyState (+1)
  index <- getState
  return $ InlineCode index code

parseImage :: Parsec String Int Inline
parseImage = do
  alt <- between' (string "![") (char ']') anyChar
  address <- between' (char '(') (char ')') anyChar
  modifyState (+1)
  index <- getState
  return $ Image index alt address

parseItalic :: Parsec String Int Inline
parseItalic = parseItalicWith '*' <|> parseItalicWith '_'
  where
    parseItalicWith :: Char -> Parsec String Int Inline
    parseItalicWith token = do
      inlines <- betweenItalic token (char token) (char token) parseInline
      modifyState (+1)
      index <- getState
      return $ Italic index inlines
        where
          betweenItalic :: Char -> Parsec String Int a -> Parsec String Int b -> Parsec String Int c -> Parsec String Int [c]
          betweenItalic token a b c = a *> manyTill c (try ( do
                              { b ; notFollowedBy (char token) }))

parseStrong :: Parsec String Int Inline
parseStrong = parseStrongWith "**" <|> parseStrongWith "__"
  where
      parseStrongWith :: String -> Parsec String Int Inline
      parseStrongWith token = do
        inlines <- between' (string token) (string token) parseInline
        modifyState (+1)
        index <- getState
        return $ Strong index inlines

parseString :: Parsec String Int Inline
parseString = do
  c <- anyChar -- Explicitly take one char to avoid empty strings
  cs <- manyTill anyChar (lookAhead $ oneOf "![*_" <|> endOfLine)
  modifyState (+1)
  index <- getState
  return $ Text index (c:cs)

between' :: Parsec String Int a -> Parsec String Int b -> Parsec String Int c -> Parsec String Int [c]
between' a b c = a *> manyTill c (try b)

spaces' :: Parsec String Int ()
spaces' = skipMany (char ' ')

spaces'' :: Parsec String Int ()
spaces'' = skipMany1 (char ' ')
