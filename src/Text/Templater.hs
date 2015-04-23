{-# LANGUAGE OverloadedStrings #-}
module Text.Templater where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Either          (partitionEithers)
import           Data.List            (intercalate)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Prelude              hiding (takeWhile)

data TemplateItem
    = Literal Text
    | Variable Text
    deriving (Show, Eq)

type Context = Text -> Maybe Text

template :: Text -> Context -> Either String Text
template "" _  = Right ""
template st ctx = case parseOnly templateP st of
                  Right items -> expandItems ctx items
                  Left e -> Left e

expandItems :: Context -> [TemplateItem] -> Either String Text
expandItems context items =
    let (lefts, rights) = partitionEithers $ expandItem context <$> items
    in if null lefts
       then Right $ T.concat rights
       else Left $ intercalate ", " lefts

expandItem :: Context -> TemplateItem -> Either String Text
expandItem ctx (Literal v) = Right v
expandItem ctx (Variable name) =
    case ctx name of
    Nothing -> Left $ "key '" <> T.unpack name <> "' not found in context"
    Just v  -> Right v

templateP :: Parser  [TemplateItem]
templateP = many1 templateItemP <* endOfInput

templateItemP :: Parser TemplateItem
templateItemP = choice [escapedOrVariableP, literalItemP]

literalItemP :: Parser TemplateItem
literalItemP = Literal <$> takeWhile1 (/= '%')

escapedOrVariableP :: Parser TemplateItem
escapedOrVariableP = char '%' *> choice [variableItemP, escapedPercentP]
    where variableItemP   = Variable <$> ("{" *> takeWhile (/= '}') <* "}")
          escapedPercentP = Literal <$> "%"
