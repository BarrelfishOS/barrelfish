module TreeDB(
    DirList,
    dlEmpty, dlByExt, dlAdd, dlAddByExt,

    TreeDB,
    tdbEmpty, tdbByDir, tdbAdd, tdbAddDir,
    tdbBuild,

    tdbByDirExt
    )
where

import qualified Data.ByteString.Char8 as C
import Data.Trie(Trie)
import qualified Data.Trie as T
import Data.Typeable

import System.FilePath

--
-- The files in a directory, partitioned by extension.
--
type DirList = [(String, [String])]

dlEmpty :: DirList
dlEmpty = []

-- Linear search for files by extension, in a single directory.
dlByExt :: String -> DirList -> [String]
dlByExt _ [] = []
dlByExt ext ((ext', names) : dirlist)
    | ext == ext' = names
    | otherwise = dlByExt ext dirlist

-- Insert a file, given its extension.  Again linear.
dlAdd :: FilePath -> DirList -> DirList
dlAdd file dirList =
    dlAddByExt (takeExtension file) (dropExtension file) dirList

dlAddByExt ::  String -> String -> DirList -> DirList
dlAddByExt ext name [] = [(ext, [name])]
dlAddByExt ext name ((ext', names):dirlist)
    | ext == ext' = (ext', name:names):dirlist
    | otherwise = (ext', names):(dlAddByExt ext name dirlist)

--
-- A map from directory to contents, excluding subdirectories.
--
type TreeDB = Trie DirList

deriving instance Typeable1 Trie

tdbEmpty :: TreeDB
tdbEmpty  = T.empty

-- Get directory contents by directory path
tdbByDir :: FilePath -> TreeDB -> Maybe DirList
tdbByDir path treeDB = T.lookup (C.pack path) treeDB

-- Add a file
tdbAdd :: FilePath -> TreeDB -> TreeDB
tdbAdd path treeDB
    | T.member dirS treeDB =
        T.adjust (\dirList -> dlAdd file dirList) dirS treeDB
    | otherwise =
        T.insert dirS (dlAdd file dlEmpty) treeDB
    where
        dir = takeDirectory path
        file = takeFileName path
        dirS = C.pack dir

-- Add a directory, complete with (relative) contents
tdbAddDir :: FilePath -> [FilePath] -> TreeDB -> TreeDB
tdbAddDir dir files treeDB
    | T.member dirS treeDB =
        T.adjust (\dirList -> foldr dlAdd dirList files) dirS treeDB
    | otherwise =
        T.insert dirS (foldr dlAdd dlEmpty files) treeDB
    where
        dirS = C.pack dir

tdbBuild :: [FilePath] -> TreeDB
tdbBuild files = foldr tdbAdd tdbEmpty files

--
-- Combined queries
--

-- Find files by directory and extension
tdbByDirExt :: FilePath -> String -> TreeDB -> Maybe [FilePath]
tdbByDirExt path ext treeDB = do
    dirList <- tdbByDir path treeDB
    let basenames = dlByExt ext dirList
    return [ path </> base <.> ext | base <- basenames ]
