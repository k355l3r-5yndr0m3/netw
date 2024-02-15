module Main where
import Netw.Inet
import System.Exit

main :: IO ()
main
  | and tests = exitSuccess
  | otherwise = exitFailure
  where tests = [ IPPORT_TCPMUX == 1
                , IPPORT_QOTD   == 17 ]
