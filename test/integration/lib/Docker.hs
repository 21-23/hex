module Docker
  ( removeImage
  , getPsImages
  , getBuiltImages
  , stopContainersByImage
  )
where

import           Data.Functor                             ( ($>) )
import           Data.ByteString.Lazy.Char8               ( unpack )
import           System.Process.Typed

removeImage :: String -> IO ()
removeImage name = execProcess $ "docker image rm -f " ++ name

getBuiltImages :: IO [String]
getBuiltImages = evalProcess "docker images --format '{{.Repository}}'"

getPsImages :: IO [String]
getPsImages = evalProcess "docker ps -a --format '{{.Image}}'"

stopContainersByImage :: [String] -> IO ()
stopContainersByImage names =
  evalProcess ("docker ps -a -q " ++ filters) >>= mapM_ stop
  where
    filters = unwords $ nameToFilter <$> names
    nameToFilter name = "--filter 'ancestor=" ++ name ++ "'"
    stop id = do
      execProcess $ "docker stop " ++ id
      execProcess $ "docker rm " ++ id

execProcess :: String -> IO ()
execProcess p = readProcess_ (shell p) $> ()

evalProcess :: String -> IO [String]
evalProcess p = lines . unpack . fst <$> readProcess_ (shell p)

