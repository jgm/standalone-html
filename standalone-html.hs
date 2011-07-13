{-# LANGUAGE OverloadedStrings #-}
import Text.HTML.TagSoup
import Network.URI (isAbsoluteURI, parseURI, escapeURIString)
import Network.Browser
import Network.HTTP
import Data.ByteString.Base64
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import System.FilePath (takeExtension)
import Data.Char (toLower, isAscii)

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

mimeTypeFor :: String -> ByteString
mimeTypeFor s = case lookup s mimetypes of
                     Nothing -> error $ "Could not find mime type for " ++ s
                     Just x  -> x
  where mimetypes = [ -- taken from MissingH
                (".a", "application/octet-stream"),
                (".ai", "application/postscript"),
                (".aif", "audio/x-aiff"),
                (".aifc", "audio/x-aiff"),
                (".aiff", "audio/x-aiff"),
                (".au", "audio/basic"),
                (".avi", "video/x-msvideo"),
                (".bat", "text/plain"),
                (".bcpio", "application/x-bcpio"),
                (".bin", "application/octet-stream"),
                (".bmp", "image/x-ms-bmp"),
                (".c", "text/plain"),
                (".cdf", "application/x-netcdf"),
                (".cpio", "application/x-cpio"),
                (".csh", "application/x-csh"),
                (".css", "text/css"),
                (".dll", "application/octet-stream"),
                (".doc", "application/msword"),
                (".dot", "application/msword"),
                (".dvi", "application/x-dvi"),
                (".eml", "message/rfc822"),
                (".eps", "application/postscript"),
                (".etx", "text/x-setext"),
                (".exe", "application/octet-stream"),
                (".gif", "image/gif"),
                (".gtar", "application/x-gtar"),
                (".h", "text/plain"),
                (".hdf", "application/x-hdf"),
                (".htm", "text/html"),
                (".html", "text/html"),
                (".ief", "image/ief"),
                (".jpe", "image/jpeg"),
                (".jpeg", "image/jpeg"),
                (".jpg", "image/jpeg"),
                (".js", "application/x-javascript"),
                (".ksh", "text/plain"),
                (".latex", "application/x-latex"),
                (".m1v", "video/mpeg"),
                (".man", "application/x-troff-man"),
                (".me", "application/x-troff-me"),
                (".mht", "message/rfc822"),
                (".mhtml", "message/rfc822"),
                (".mif", "application/x-mif"),
                (".mov", "video/quicktime"),
                (".movie", "video/x-sgi-movie"),
                (".mp2", "audio/mpeg"),
                (".mp3", "audio/mpeg"),
                (".mpa", "video/mpeg"),
                (".mpe", "video/mpeg"),
                (".mpeg", "video/mpeg"),
                (".mpg", "video/mpeg"),
                (".ms", "application/x-troff-ms"),
                (".nc", "application/x-netcdf"),
                (".nws", "message/rfc822"),
                (".o", "application/octet-stream"),
                (".obj", "application/octet-stream"),
                (".oda", "application/oda"),
                (".p12", "application/x-pkcs12"),
                (".p7c", "application/pkcs7-mime"),
                (".pbm", "image/x-portable-bitmap"),
                (".pdf", "application/pdf"),
                (".pfx", "application/x-pkcs12"),
                (".pgm", "image/x-portable-graymap"),
                (".pl", "text/plain"),
                (".png", "image/png"),
                (".pnm", "image/x-portable-anymap"),
                (".pot", "application/vnd.ms-powerpoint"),
                (".ppa", "application/vnd.ms-powerpoint"),
                (".ppm", "image/x-portable-pixmap"),
                (".pps", "application/vnd.ms-powerpoint"),
                (".ppt", "application/vnd.ms-powerpoint"),
                (".ps", "application/postscript"),
                (".pwz", "application/vnd.ms-powerpoint"),
                (".py", "text/x-python"),
                (".pyc", "application/x-python-code"),
                (".pyo", "application/x-python-code"),
                (".qt", "video/quicktime"),
                (".ra", "audio/x-pn-realaudio"),
                (".ram", "application/x-pn-realaudio"),
                (".ras", "image/x-cmu-raster"),
                (".rdf", "application/xml"),
                (".rgb", "image/x-rgb"),
                (".roff", "application/x-troff"),
                (".rtx", "text/richtext"),
                (".sgm", "text/x-sgml"),
                (".sgml", "text/x-sgml"),
                (".sh", "application/x-sh"),
                (".shar", "application/x-shar"),
                (".snd", "audio/basic"),
                (".so", "application/octet-stream"),
                (".src", "application/x-wais-source"),
                (".sv4cpio", "application/x-sv4cpio"),
                (".sv4crc", "application/x-sv4crc"),
                (".swf", "application/x-shockwave-flash"),
                (".t", "application/x-troff"),
                (".tar", "application/x-tar"),
                (".tcl", "application/x-tcl"),
                (".tex", "application/x-tex"),
                (".texi", "application/x-texinfo"),
                (".texinfo", "application/x-texinfo"),
                (".tif", "image/tiff"),
                (".tiff", "image/tiff"),
                (".tr", "application/x-troff"),
                (".tsv", "text/tab-separated-values"),
                (".txt", "text/plain"),
                (".ustar", "application/x-ustar"),
                (".vcf", "text/x-vcard"),
                (".wav", "audio/x-wav"),
                (".wiz", "application/msword"),
                (".xbm", "image/x-xbitmap"),
                (".xlb", "application/vnd.ms-excel"),
                (".xls", "application/vnd.ms-excel"),
                (".xml", "text/xml"),
                (".xpm", "image/x-xpixmap"),
                (".xsl", "application/xml"),
                (".xwd", "image/x-xwindowdump"),
                (".zip", "application/zip"),
                (".jpg", "image/jpg"),
                (".mid", "audio/midi"),
                (".midi", "audio/midi"),
                (".pct", "image/pict"),
                (".pic", "image/pict"),
                (".pict", "image/pict"),
                (".rtf", "application/rtf"),
                (".xul", "text/xul")
                ]

convertTag :: Tag ByteString -> IO (Tag ByteString)
convertTag t@(TagOpen "img" as) =
       case fromAttrib "src" t of
         src | not (B.null src) -> do
           (raw, mime) <- getRaw t src
           let enc = "data:" `B.append` mime `B.append` ";base64," `B.append` encode raw
           return $ TagOpen "img" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"])
         _   -> return t
convertTag t@(TagOpen "script" as) =
  case fromAttrib "src" t of
       src | not (B.null src) -> do
           (raw, mime) <- getRaw t src
           let enc = "data:" `B.append` mime `B.append` "," `B.append`
                       (B.pack $ escapeURIString isAscii $ B.unpack raw)
           return $ TagOpen "script" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"]) 
       _    -> return t
convertTag t@(TagOpen "style" as) = return t -- TODO
convertTag t = return t

getRaw :: Tag ByteString -> ByteString -> IO (ByteString, ByteString)
getRaw t src = do
  let src' = toString src
  let mime = case fromAttrib "type" t of
                  x | not (B.null x) -> x
                  _ -> mimeTypeFor (map toLower $ takeExtension src')
  raw <- getItem src'
  return (raw, mime)

main :: IO ()
main = do
  inp <- B.getContents
  let tags = parseTags inp
  out <- mapM convertTag tags
  B.putStr $ renderTagsOptions renderOptions{ optMinimize = (\t -> t == "br" || t == "img") } out

