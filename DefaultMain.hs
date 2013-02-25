-- Copyright © 2013 Bart Massey
-- [This work is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Default main for "Genuine Sieve of Eratosthenes" implementations.

module DefaultMain (defaultMain)
where

import Data.List (foldl')
import Data.Word
import System.Console.ParseArgs

data ArgLabel = 
  ArgUseLimit | ArgPrint | ArgLimit
  deriving (Enum, Ord, Eq, Show)

argd :: [Arg ArgLabel]
argd = [ Arg { argIndex = ArgUseLimit,
               argAbbr = Just 'l',
               argName = Just "use-limit",
               argData = Nothing,
               argDesc = "Use the prime size limit to optimize generation." },
         Arg { argIndex = ArgPrint,
               argAbbr = Just 'p',
               argName = Just "print",
               argData = Nothing,
               argDesc = "Print primes instead of counting them." },
         Arg { argIndex = ArgLimit,
               argAbbr = Nothing,
               argName = Nothing,
               argData = argDataOptional "limit" ArgtypeInteger,
               argDesc = "Largest candidate prime to use in count." } ]

defaultMain :: (Integral a, Show a) => [a] -> Maybe (a -> [a]) -> IO ()
defaultMain primes primesLimit = do
  argv <- parseArgsIO ArgsComplete argd
  let limit = 
        case getArg argv ArgLimit :: Maybe Integer of
          Just l -> fromIntegral l
          Nothing ->
            case gotArg argv ArgPrint of
              True -> 100
              False -> 2000000
  let ps =
        case gotArg argv ArgUseLimit of
          True ->
            case primesLimit of
              Nothing -> 
                usageError argv "Use-limit flag unsupported by this sieve."
              Just pl -> 
                pl limit
          False -> 
            takeWhile (<= limit) primes
  case gotArg argv ArgPrint of
    True -> 
      print ps
    False -> do
      let (n, p) = 
            foldl' next (0 :: Word64, undefined) ps
            where
              next (a, _) x = (a + 1, x)
      putStrLn $ show p ++ " " ++ show n
