{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedStrings, FlexibleContexts, PartialTypeSignatures, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.Async
import Control.Exception
import Data.Bool
import Data.Card
import Data.Colour
import Data.Colour.SRGB
import Data.Foldable
import qualified Data.List as List
import qualified Data.List.Split as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Diagrams.Attributes
import qualified Diagrams.Backend.SVG as Svg
import Diagrams.Combinators
import Diagrams.Core.Types
import Diagrams.Core.V
import Diagrams.Size
import Diagrams.Transform
import Diagrams.TwoD.Align
import Diagrams.TwoD.Attributes
import Diagrams.TwoD.Combinators
import Diagrams.TwoD.Image
import Diagrams.TwoD.Size
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Types
import qualified Graphics.SVG.Font.Read as SVG
import qualified Graphics.SVG.Font.Text as SVG
import Paths_imagine
import Text.Regex.Applicative (RE)
import qualified Text.Regex.Applicative as RE
import Util
import Util.Map

main :: IO ()
main = do
    [mPlantinFont, magicMedievalFont] <-
        traverse (\ fontName ->
                  fst . SVG.prepareFont . SVG.parseFont "" <$>
                  (getDataFileName ("dat/fonts/" ++ fontName ++ ".svg") >>= readFile))
        ["MPlantin", "MagicMedieval"]
    (_, frames :: Map Frame (QDiagram _ V2 Double Any)) <-
        fmap (id *** fmap (alignTL . scaleUToX 5 . image)) . traverseEither loadImageEmb =<<
        (sequenceA . Map.fromList) [(x, getDataFileName ("dat/frames/" ++ show x ++ ".png")) | x <- enumFrom W]
    (_, symbols :: Map Char (QDiagram _ V2 Double Any)) <-
        fmap (id *** fmap (scaleUToX 0.25 . image)) . traverseEither loadImageEmb =<<
        (sequenceA . Map.fromList) [(x, getDataFileName ("dat/symbols/" ++ x : ".png")) | x <- "0123456789WUBRGTXYZ"]
    let draw :: Card -> QDiagram _ V2 Double Any -> [Char] -> QDiagram _ V2 Double Any
        draw (Card {..}) art artistName =
            position [(p2 (0.572, -0.540), title),
                      (p2 (4.610, -0.570), manaCost),
                      (p2 (0.535, -4.050), frameText id 0.01 mPlantinFont 0.250 cardType),
                      (p2 (0.667, -4.500), rulesText),
                      (p2 (0.667, -5.250), flavText),
                      (p2 (4.346, -6.400), pt),
                      (p2 (2.500, -6.320), artCred),
                      (p2 (2.500, -6.490), copyNote),
                      (p2 (0,      0),     fromMaybe (error ("Frame not found: " ++ show frame)) $
                                           Map.lookup frame frames),
                      (p2 (2.500, -0.675), (alignT . centerX . scaleUToY 3.123) art)]
          where
            frame = codeFrame cardCode
            title = frameText id 0.01 magicMedievalFont 0.300 $ bool cardName (show cardCode) (null cardName)
            pt = maybe mempty (frameText (centerXY {-# . bold #-}) 0.02 mPlantinFont 0.300) $ cardPT
            manaCost = maybe mempty (alignBR . hsep 0.030 . fmap findSymbol) cardCost
            rulesText = fillColor black $ alignL $ vsep rulesParagraphVSpace $ rulesParagraphs
            rulesParagraphs =
                wrapText (inlineSymbol . findSymbol)
                ((<> strutY (9/16*rulesTextSize)) . lwG 0 .
                 SVG.text' mPlantinFont (textOpts rulesTextSize)) 3.667 . parseRulesText <$>
                lines cardText
            rulesParagraphVSpace = min (1/2*rulesTextSize)
                                       ((1.800 - height (vcat rulesParagraphs)) /
                                        max (List.genericLength rulesParagraphs - 1) 1)
            flavText = mempty
            artCred = frameText centerXY 0.01 mPlantinFont 0.220 ("Illus. " ++ artistName)

        frameText f δ font size = fillColor (sRGB 1 1 1) . dropShadow (V2 δ (-δ)) . f . lwG 0 . SVG.text' font (textOpts size)

        copyNote = frameText centerXY 0.005 mPlantinFont 0.110 "™ & © Wizards of the Coast"

        rulesTextSize = 0.2625

        textOpts size = SVG.TextOpts (SVG.InsideH size) True False

        inlineSymbol = alignY (-1/sqrt 2) . (<> liftA2 (<>) (alignL . strutX) strutY 0.2) . alignL . scaleUToY (7/10*rulesTextSize)

        findSymbol :: Char -> QDiagram _ V2 Double Any
        findSymbol x = fromMaybe (error ("Symbol not found: " ++ [x])) $ Map.lookup x symbols

    fmap fold $ getContents >>=
                List.splitOn "\n\n\n" &
                fmap (\ xs ->
                      fromMaybe (error ("Ill-formed card:\n\n" ++ xs)) $
                      RE.match cardRE xs) &
                ordNubOn cardCode &
                mapConcurrently
                (\ card@(Card {..}) -> do
                     art <- either (pure mempty) image <$> loadImageEmb ("/dev/fd/3/" ++ cardName)
                     cred <- either (pure Nothing :: SomeException -> _) Just <$> (try . readFile) ("/dev/fd/3/" ++ cardName ++ ".cred")
                     Svg.renderSVG ("/dev/fd/1/" ++ show cardCode ++ ".svg") (dims (V2 787 1087)) $ draw card art (fromMaybe "Nilbert Nullingsworth" cred))

parseRulesText :: [Char] -> [Either Char Char]
parseRulesText = fromMaybe (error "bad rules text") .
                 RE.match (concat <$> many (fmap Right <$> (many . RE.psym) (/= '{') <|> fmap Left <$> symbolsRE))
  where symbolsRE :: RE Char [Char]
        symbolsRE = RE.sym '{' *> (many . RE.psym) (/= '}') <* RE.sym '}'

dropShadow :: (Transformable a, _) => Vn a -> a -> a
dropShadow v = liftA2 (<>) id $ translate v . fillColor black

wrapText :: (RealFloat n) => (a -> QDiagram b V2 n Any) -> ([Char] -> QDiagram b V2 n Any) -> n -> [Either a Char] -> QDiagram b V2 n Any
wrapText f g maxWidth = List.splitWhen (\ case Right ' ' -> True; _ -> False) & fmap draw & go
  where draw = groupEithers & fmap (either (fmap f & hcat) g) & hcat
        wordSpace = 3/4 * width (g " ")
        go [] = mempty
        go (d:ds) = case spanFoldl (< maxWidth) (\ w d -> w + wordSpace + width d) (width d) ds
                    of ((d:) -> xs, ys) -> hsep wordSpace xs === go ys
