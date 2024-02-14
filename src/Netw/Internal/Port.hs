{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Netw.Internal.Port where
import Netw.Internal.Type

import Data.List
import Data.Char
import qualified Data.List.NonEmpty as N

import Data.Word
import Language.Haskell.TH


declarePorts :: Name -> DecsQ
declarePorts constructor =
  -- runIO (print . grpOnNameAndPort =<< servents) >> return []
  runIO servents >>= emit . concatMap service . grpOnNameAndPort
  where emit :: [(String, Word16)] -> DecsQ
        emit (sortOn fst -> ports) =
          let grp1 name (a:as)
                | name == fst a = (a:) <$> grp1 name as
                | otherwise     = (a:as,[])
              grp1 _    [] = ([], [])
              grp2 (a:as) =
                let (as', p) = grp1 (fst a) as
                in  (a N.:| p):grp2 as'
              grp2 []     = []
              list =
                let f (a N.:| as)
                      | null as   = [a]
                      | otherwise = map (\ (name, port) -> (name ++ '_' : show port, port)) (a:as)
                in  concatMap f (grp2 ports)
        --  in  runIO (print list) >> return []
          in  sequence [ patSynD (mkName name)
                                 (prefixPatSyn [])
                                 implBidir
                                 (conP constructor [w16ToLit port])
                       | (name, port) <- list ]
        service :: (String, N.NonEmpty (Word16, N.NonEmpty String)) -> [(String, Word16)]
        service (name, pp N.:| pps)
          | null pps  = [("IPPORT_" ++ normalize name, fst pp)]
          | otherwise = concat
            [ 
              [ ("IPPORT_" ++ normalize name ++ '_' : normalize proto, port)
              | proto <- N.toList protos]
            | (port, protos) <- pp:pps]
        w16ToLit = litP . integerL . fromIntegral
        stage1 :: [Servent] -> [(String, N.NonEmpty (Word16, String))]
        stage1 (sortOn sName -> db) =
          let grp1 name (e@(sName -> name'):es)
                | name == name' = ((sPort e, sProto e):) <$> grp1 name es
                | otherwise     = (e:es, [])
              grp1 _ [] = ([], [])
              grp2 (e@(sName -> name):es) = 
                let (es', ports) = grp1 name es
                in  (name, (sPort e, sProto e) N.:| ports):grp2 es'
              grp2 [] = []
          in  grp2 db
        stage2 :: [(String, N.NonEmpty (Word16, String))] -> [(String, N.NonEmpty (Word16, N.NonEmpty String))]
        stage2 = 
          map $ 
          \ (serviceName, N.sortWith fst -> portProt) -> 
            let grp1 port (pp@(port', proto):pps)
                  | port == port' = (proto:) <$> grp1 port pps
                  | otherwise     = (pp:pps, [])
                grp1 _    [] = ([], [])
                grp2 ((port, proto):pps) =
                  let (pps', protos) = grp1 port pps
                  in  (port, proto N.:| protos):grp2 pps'
                grp2 [] = []
                grp3 ((port, proto) N.:| pps) =
                  let (pps', protos) = grp1 port pps
                  in  (port, proto N.:| protos) N.:| grp2 pps'
            in  (serviceName, grp3 portProt)
        grpOnNameAndPort :: [Servent] -> [(String, N.NonEmpty (Word16, N.NonEmpty String))]
        grpOnNameAndPort = stage2 . stage1
        normalize = map $ (\case '-' -> '_'; other -> other) . toUpper
