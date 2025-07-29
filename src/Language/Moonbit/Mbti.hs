module Language.Moonbit.Mbti
  ( parseMbti,
  )
where

import Data.Text.Lazy (Text)
import Language.Moonbit.Mbti.Parser
import Language.Moonbit.Mbti.Syntax
import Text.Parsec

-- | Parses a full .mbti file content.
parseMbti :: SourceName -> Text -> Either ParseError MbtiFile
parseMbti = parse pMbtiFile
