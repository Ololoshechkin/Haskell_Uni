{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module FSL where

import           Control.Lens
import           System.FilePath     (replaceExtension)
import           Data.Text           (Text, unpack)

data FS
    = Dir
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название файла, не полный путь
          }
    deriving (Show)

makeLenses ''FS
makePrisms ''FS

isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

isDir :: FS -> Bool
isDir = not . isFile

cd :: FilePath -> Traversal' FS FS
cd dName = contents . traversed . filtered (\x -> isDir x && x ^. name == dName)

ls :: Traversal' FS FS
ls = contents . each

file :: FilePath -> Traversal' FS FS
file fName =
    contents . traversed . filtered (\x -> isFile x && x ^. name == fName)

changeEnd :: Text -> FS -> FS
changeEnd newExtension =  contents . traversed . filtered isFile . name %~ (`replaceExtension` unpack newExtension)

deleteEmptySubdir :: FilePath -> FS -> FS
deleteEmptySubdir fName fs = fs & contents .~ newContent
    where newContent = fs ^.. contents . traversed . filtered (\x -> not $ x ^. name == fName && null (x ^. contents))
