{-# LANGUAGE LambdaCase #-}
module Netw.Internal.Protocols where
import Netw.Internal.Type

import Language.Haskell.TH
import Data.Char (toUpper)

declareProtocols :: Name -> DecsQ
declareProtocols constructor = do
  runIO protoents >>= mapM
    (\ proto ->
      do
        patSynD (mkName $ normalize (pName proto))
                (prefixPatSyn [])
                implBidir
                (conP constructor [litP (integerL $ fromIntegral $ pProto proto)]))
  where normalize =
          let sanitize = \case '-'  -> '_'
                               '/'  -> '_'
                               '\\' -> '_'
                               ' '  -> '_'
                               '+'  -> 'p'
                               '.'  -> '_'
                               char -> char
          in  ("IPPROTO_"++) . map (sanitize . toUpper)
