-- Copyright © 2013 Bart Massey
-- [This work is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Default main for "Genuine Sieve of Eratosthenes" tests.

module DefaultMain (defaultMain)
where

import System.Console.ParseArgs

data ArgLabel = ArgUseLimit | ArgLimit deriving (Enum, Ord, Eq, Show)

argd :: [Arg ArgLabel]
argd = [ Arg { argIndex = ArgUseLimit,
               argAbbr = Just 'l',
               argName = Just "use-limit",
               argData = Nothing,
               argDesc = "Use the prime size limit to optimize generation" },
         Arg { argIndex = ArgLimit,
               argAbbr = Nothing,
               argName = Nothing,
               argData = argDataDefaulted "limit" ArgtypeInteger 2000000,
               argDesc = "Largest candidate prime to use in count." } ]

defaultMain :: [Integer] -> Maybe (Integer -> [Integer]) -> IO ()
defaultMain primes primesLimit = do
  argv <- parseArgsIO ArgsComplete argd
  let limit = getRequiredArg argv ArgLimit
  case gotArg argv ArgUseLimit of
    True ->
      case primesLimit of
        Nothing -> usageError argv "Use-limit flag unsupported by this sieve."
        Just pl -> print $ length $ pl limit
    False -> print $ length $ takeWhile (<= limit) primes
