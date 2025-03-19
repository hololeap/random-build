module GHRB.Core.Utils
  ( prettySuccess
  , prettyFail
  , prettyPreMerge
  , prettyMessage
  , prettyTry
  ) where

import           GHRB.Core.Types               (failure, message,
                                                preMergeFailure, success, try)
import           Prettyprinter                 (Doc, SimpleDocStream,
                                                defaultLayoutOptions,
                                                layoutPretty, pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle)

prettify ::
     (Doc AnsiStyle -> Doc AnsiStyle) -> String -> SimpleDocStream AnsiStyle
prettify prettifier = layoutPretty defaultLayoutOptions . prettifier . pretty

prettySuccess :: String -> SimpleDocStream AnsiStyle
prettySuccess = prettify success

prettyFail :: String -> SimpleDocStream AnsiStyle
prettyFail = prettify failure

prettyPreMerge :: String -> SimpleDocStream AnsiStyle
prettyPreMerge = prettify preMergeFailure

prettyMessage :: String -> SimpleDocStream AnsiStyle
prettyMessage = prettify message

prettyTry :: String -> SimpleDocStream AnsiStyle
prettyTry = prettify try
