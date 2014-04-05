module Data.ByteStringBuilder where

import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Word

data ByteStringBuilder a = ByteStringBuilder ByteString a
type Builder = ByteStringBuilder ()

put :: ByteString -> Builder 
put = flip ByteStringBuilder ()

putS :: String -> Builder
putS = put . BSLC.pack

putSLn :: String -> Builder
putSLn str = putS str >> putC '\n'

putC :: Char -> Builder
putC = put . BSLC.singleton

putB :: Word8 -> Builder
putB = put . BSL.singleton

runBuilder :: Builder -> ByteString
runBuilder (ByteStringBuilder bs _) = bs

instance Monad ByteStringBuilder where
    ByteStringBuilder a _ >> ByteStringBuilder b c = ByteStringBuilder (a `append` b) c
    a@(ByteStringBuilder _ b) >>= func = a >> func b
    return = ByteStringBuilder BSL.empty
    fail = error
