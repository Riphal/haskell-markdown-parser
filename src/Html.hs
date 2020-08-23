module Html
  (
    genHtml
  ) where

import Markdown

genHtml :: Markdown -> Html
genHtml [] = ""
genHtml (x:xs) = htmlBlock x <> genHtml xs

htmlBlock :: Block -> Html
htmlBlock (Paragraph _ xs) = "<p>" <> htmlInlines xs <> "</p>"
htmlBlock (Headering t _ xs) = "<h" <> show (fromEnum (succ t)) <> ">"
                                  <> htmlInlines xs
                                  <> "</h" <> show (fromEnum (succ t)) <> ">"
htmlBlock (Quote _ xs) = "<blockquote>" <> htmlInlines xs <> "</blockquote>"
htmlBlock (List _ OrderedList xs) = "<ol>" <> concatMap (\x -> "<li>" <> htmlInlines x <> "</li>") xs <> "</ol>"
htmlBlock (List _ UnorderedList xs) = "<ul>" <> concatMap (\x -> "<li>" <> htmlInlines x <> "</li>") xs <> "</ul>"
htmlBlock (Divider _) = "<hr />"
htmlBlock (Code _ Nothing code) = "<pre><code>" <> text code <> "</code></pre>"
htmlBlock (Code _ (Just language) code) = "<pre><code class=\"" <> language <> "\">" <> text code <> "</code></pre>"

htmlInlines :: [Inline] -> Html
htmlInlines [] = ""
htmlInlines (x:xs) = htmlInline x <> htmlInlines xs

htmlInline :: Inline -> Html
htmlInline (Link _ name address) = "<a href=\"" <> address <> "\">" <> text name <> "</a>"
htmlInline (Image _ alt address) = "<img src=\"" <> address <> "\" alt=\"" <> alt <> "\" />"
htmlInline (Text _ str) = text str
htmlInline (Italic _ xs) = "<em>" <> htmlInlines xs <> "</em>"
htmlInline (Strong _ xs) = "<strong>" <> htmlInlines xs <> "</strong>"
htmlInline (InlineCode _ code) = "<code>" <> text code <> "</code>"

type Html = String

text :: String -> String
text = concatMap char
  where char :: Char -> String
        char '<' = "&lt;"
        char '>' = "&gt;"
        char '&' = "&amp;"
        char '"' = "&quot;"
        char '\'' = "&#39;"
        char x = [x]
