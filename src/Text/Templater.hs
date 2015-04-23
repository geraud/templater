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

type Context = Text -> Maybe Text
type TemplatingError = Text

template :: Text -> Context -> Either String Text
template "" _  = Right ""
template st ctx = case parseOnly templateP st of
                  Right items -> templateItems items ctx
                  Left e -> Left e

templateItems :: [TemplateItem] -> Context -> Either String Text
templateItems items context =
    let (lefts,rights) = partitionEithers $ expandItem context <$> items
    in if null lefts
       then Right $ T.concat rights
       else Left $ intercalate ", " lefts

expandItem :: Context -> TemplateItem -> Either String Text
expandItem ctx (Literal v) = Right v
expandItem ctx (Variable name) =
    case ctx name of
    Nothing -> Left $ "key '" <> T.unpack name <> "' not found in context"
    Just v  -> Right v

data TemplateItem
    = Literal Text
    | Variable Text
    deriving (Show, Eq)

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
