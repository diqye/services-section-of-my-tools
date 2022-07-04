module Endpoint.Static.MyFile where

-- 若欲知某处预期类型，_ 之,    _ 者虫洞也，以其代之得类型于编译期
-- 若未码毕欲REPL undefined 之。
import qualified Web.Static.Static as Static
import qualified Web.AppM as W
import Control.Applicative((<|>))
import qualified Data.Text as T
import qualified Data.ByteString as B
import Module.TemplateGetter
import Control.Monad(guard)

-- 静态文件服务
-- 于文件，见 private 则 auth 之
-- 于目录，以json求之，以JSON返回, 默认求之，以HTML返回
myFiles :: W.AppIO
myFiles = authLogic
  <|> Static.dirServe shareFolder []
  <|> jsonBrowse
  <|> myBrowse

--  静态文件服务文件夹，绝对路径 
shareFolder = "/Users"

-- 路径见 private 则 auth 之
authLogic = do
  req <- W.getRequest
  let rawPath = W.rawPathInfo req
  let needed = not $ B.null $ snd $ B.breakSubstring "private" rawPath
  guard $ needed
  val <- Static.authBasicValue
  guard $ val /= Just ("diqye","password")
  Static.authBasic
          
-- 若末尾非斜杠则302加之
myBrowse = do
  req <- W.getRequest
  let infos = W.pathInfo req
  let lastText = if null infos then "null" else last infos
  -- 善哉！IF表达式之竟能排版如此。
  if lastText == ""
  then Static.dirBrowse shareFolder
  else do
    W.putHeader W.hLocation (W.rawPathInfo req <> "/")
    W.respLBS W.status302 ""

-- 若Accept JSON请求之，则以JSON响应之
jsonBrowse = do
  req <- W.getRequest
  let contentType = lookup W.hAccept $ W.requestHeaders req
  let hasJSON = not . B.null . _2 . B.breakSubstring "json"
  guard $ Just True == fmap hasJSON contentType
  Static.dirBrowseJSON shareFolder