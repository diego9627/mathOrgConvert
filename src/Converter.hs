{-# LANGUAGE RecordWildCards #-}
module Converter ( toMathOrgMD) where

import           Text.Regex.Posix
import           Text.Regex
import           Text.Pandoc
import           Text.Pandoc.UTF8
import           Text.Pandoc.Walk
import           Data.Monoid
import           Data.List
import           Data.Char (toLower)
-- import qualified Text.Pandoc.UTF8 as UTF8

data Change = Change {
  klass :: String, 
  keywords :: [String], 
  alterationI :: [Inline] -> [Inline],
  alterationB :: [Block]  -> [Block]
}

surround :: String -> String -> [Inline] -> [Inline]
surround x y z = ((Str x):z) ++ [Str y] 

surroundB :: String -> String -> [Block] -> [Block]
surroundB x y z = ((Plain [Str x]):z) ++ [Plain [Str y]]

solutions :: Change
solutions = Change "solution" ["solution","answer"] (surround "(((" ")))") (surroundB "(((" ")))")

problems :: Change
problems  = Change "problem" ["determine","prove","show","problem","exercise","example"] (surround "[[[" "]]]") (surroundB "[[[" "]]]")

changes :: [Change]
changes   = [solutions, problems]

markdown_specs :: WriterOptions
markdown_specs = (def {writerExtensions = multimarkdownExtensions , writerHighlight = True })

identify :: Change -> Pandoc -> Pandoc
identify Change{..} (Pandoc m bs) = (Pandoc m (processBlocks bs))
  where
    processBlocks :: [Block] -> [Block]
    processBlocks [] = 
      []
    processBlocks (a@(Header _ _ _):(b@(OrderedList la bss):cs)) | getAny (query (Any . goodInline) a) =
      a:(OrderedList la (map ((\x -> [x]) . Div ("",[klass],[]) . alterationB) bss)):(processBlocks cs)
    processBlocks (a@(Header _ _ _):(b@(BulletList bss):cs)) | getAny (query (Any . goodInline) a) =
      a:(BulletList (map ((\x -> [x]) . Div ("",[klass],[]) . alterationB) bss)):(processBlocks cs)
    processBlocks (a:bs) = 
      ((processBlock a):(processBlocks bs))
    processBlock :: Block -> Block
    processBlock (Plain is) =
      Plain (processInlines is)
    processBlock (Para is) =
      Para (processInlines is)
    processBlock (BlockQuote is) =
      BlockQuote (processBlocks is)
    processBlock (OrderedList la is) =
      OrderedList la (map processBlocks is)
    processBlock (BulletList is) =
      BulletList (map processBlocks is)
    processBlock (Div at b) =
      Div at (processBlocks b)
    processBlock x =
      x
    processInlines  :: [Inline] -> [Inline]
    processInlines = fst . processInlines'
    processInlines' :: [Inline] -> ([Inline],(Bool,Maybe [Inline]))
    processInlines' [] = 
      ([],(False,Nothing))
    processInlines' (x@(Str s):ys) | fitsModel s =
      consumeUntil x ys
    processInlines' (x@(RawInline _ s):ys) | fitsModel s =
      consumeUntil x ys
    processInlines' (x@(Emph is):ys) = 
      tryLifting Emph is ys
    processInlines' (x@(Strong is):ys) = 
      tryLifting Strong is ys
    processInlines' (x@(Strikeout is):ys) = 
      tryLifting Strikeout is ys
    processInlines' (x@(Superscript is):ys) = 
      tryLifting Superscript is ys
    processInlines' (x@(Subscript is):ys) = 
      tryLifting Subscript is ys
    processInlines' (x@(SmallCaps is):ys) = 
      tryLifting SmallCaps is ys
    processInlines' (x@(Quoted qt is):ys) = 
      tryLifting (Quoted qt) is ys
    processInlines' (x@(Cite cs is):ys) = 
      tryLifting (Cite cs) is ys
    processInlines' (x@(Link is tg):ys) = 
      tryLifting (flip Link tg) is ys
    processInlines' (x@(Image is tg):ys) = 
      tryLifting (flip Image tg) is ys
    processInlines' (LineBreak:ys) =
      let (ty,(by,bc)) = processInlines' ys
       in (LineBreak:ty,(by,Nothing))
    processInlines' (x:ys) = 
      let (ty,(by,mi)) = processInlines' ys
       in case mi of 
            Just is -> ((wrapSpan (x:is)):(tail ty),(by,Just (x:is)))
            Nothing -> (x:ty,(by,Nothing))
    consumeUntil :: Inline -> [Inline] -> ([Inline],(Bool,Maybe [Inline]))
    consumeUntil x [] =
       ([wrapSpan [x]], (True,Just [x]))
    consumeUntil x ys =
      let (a,b) = twiceBreak ys
       in ((wrapSpan (x:a)):processInlines b, (null b,Just (x:a)))
    tryLifting :: ([Inline] -> Inline) -> [Inline] -> [Inline] -> ([Inline],(Bool,Maybe [Inline]))
    tryLifting fx is ys = 
      let (ti,(bi,mi)) = processInlines' is
          (ty,(by,my)) = processInlines' ys
       in case bi of
            True  -> consumeUntil (fx is) ys
            False -> ((fx ti):ty,(by,Nothing))
    twiceBreak :: [Inline] -> ([Inline],[Inline])
    twiceBreak (LineBreak:LineBreak:ys) = ([],LineBreak:LineBreak:ys)
    twiceBreak (x:ys) = 
      let (a,b) = twiceBreak ys
       in (x:a,b)
    twiceBreak [] = ([],[])
    goodInline :: Inline -> Bool
    goodInline (Str a) = fitsModel a
    goodInline (RawInline _ a) = fitsModel a
    goodInline _ = False
    fitsModel :: String -> Bool
    fitsModel str = any (((=~).map toLower) str) keywords 
    wrapSpan :: [Inline] -> Inline 
    wrapSpan xs = 
      Span ("",[klass],[]) (alterationI xs)
    insertToSp :: [Inline] -> Inline -> [Inline]
    insertToSp ((Span at ss):rs) x = ((Span at (x:ss)):rs)

 
regexChanges :: [(String,String)]
regexChanges = 
  [
    ("\\\\\\\\([\\\\(\\\\)])","$" ), -- Changes inline math
    ("\\\\\\\\([][])","\\\\\\1")
  ]

stripSpansP :: Pandoc -> Pandoc
stripSpansP (Pandoc m bs) = (Pandoc m (map stripSpansB bs))

stripSpansB :: Block -> Block
stripSpansB (Plain is) = Plain (stripSpansI is)
stripSpansB (Para is)  = Para  (stripSpansI is) 
stripSpansB (BlockQuote bs) = BlockQuote (map stripSpansB bs)
stripSpansB (OrderedList at bs) = (OrderedList at (map (map stripSpansB) bs)) 
stripSpansB (BulletList bs) = (BulletList (map (map stripSpansB) bs)) 
stripSpansB (DefinitionList ibs) = DefinitionList (map (\(is,bss) -> (stripSpansI is,map (map stripSpansB) bss)) ibs)
stripSpansB (Div at bs) = (Div at (map stripSpansB bs))
stripSpansB x = x

stripSpansI :: [Inline] -> [Inline]
stripSpansI ((Emph is):ys) = (Emph (stripSpansI is)):(stripSpansI ys)
stripSpansI ((Strong is):ys) = (Strong (stripSpansI is)):(stripSpansI ys)
stripSpansI ((Strikeout is):ys) = (Strikeout (stripSpansI is)):(stripSpansI ys)
stripSpansI ((Superscript is):ys) = (Superscript (stripSpansI is)):(stripSpansI ys)
stripSpansI ((Subscript is):ys) = (Subscript (stripSpansI is)):(stripSpansI ys)
stripSpansI ((SmallCaps is):ys) = (SmallCaps (stripSpansI is)):(stripSpansI ys)
stripSpansI ((Quoted q is):ys) = (Quoted q (stripSpansI is)):(stripSpansI ys)
stripSpansI ((Cite cs is):ys) = (Cite cs(stripSpansI is)):(stripSpansI ys)
stripSpansI ((Link is tg):ys) = (Link (stripSpansI is) tg):(stripSpansI ys)
stripSpansI ((Image is tg):ys) = (Image (stripSpansI is) tg):(stripSpansI ys)
stripSpansI ((Note bs):ys) = (Note (map stripSpansB bs)):(stripSpansI ys)
stripSpansI ((Span at is):ys) = (stripSpansI is) ++ (stripSpansI ys)
stripSpansI (x:ys) = (x:(stripSpansI ys))
stripSpansI [] = []

toMathOrgMD :: String -> String
toMathOrgMD = 
  foldl (.) id transformations . writeMarkdown markdown_specs . stripSpansP . foldl (.) id (map identify changes) . readLaTeX def
    where 
      transformations = map (uncurry (flip . subRegex . mkRegex)) regexChanges
  

