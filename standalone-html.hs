{-# LANGUAGE OverloadedStrings #-}
import Text.HTML.TagSoup
import Network.URI (isAbsoluteURI, parseURI)
import Network.Browser
import Network.HTTP
import Data.ByteString.Base64
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import System.FilePath (takeExtension)
import Data.Char (toLower)

getItem :: String -> IO ByteString
getItem f =
  if isAbsoluteURI f
     then openURL f
     else B.readFile f

openURL :: String -> IO ByteString
openURL u = getResponseBody =<< simpleHTTP (getReq u)
  where getReq u = case parseURI u of
                     Nothing  -> error $ "Could not parse URI: " ++ u
                     Just u'  -> mkRequest GET u'

convertTag :: Tag ByteString -> IO (Tag ByteString)
convertTag t@(TagOpen "img" as) =
       case fromAttrib "src" t of
         src | not (B.null src)  -> do
           let src' = toString src
           raw <- getItem src'
           let mime = case map toLower (takeExtension src') of
                           ".jpg"  -> "image/jpeg"
                           ".jpeg" -> "image/jpeg"
                           ".png"  -> "image/png"
                           ".gif"  -> "image/gif"
                           _       -> error $ "Mime type for " ++ src' ++ " not known."
           let enc = if B.null raw
                        then src
                        else "data:" `B.append` mime `B.append` ";base64," `B.append` encode raw
           return $ TagOpen "img" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"])
         _ -> return t
convertTag t = return t

main :: IO ()
main = do
  inp <- B.getContents
  let tags = parseTags inp
  out <- mapM convertTag tags
  B.putStr $ renderTagsOptions renderOptions{ optMinimize = (\t -> t == "br" || t == "img") } out

