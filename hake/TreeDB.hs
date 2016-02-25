module TreeDB(
    DirList,
    dlEmpty, dlByExt, dlByExts, dlAdd, dlAddByExt,

    TreeDB,
    tdbEmpty, tdbByDir, tdbAdd, tdbAddDir,
    tdbBuild, tdbMerge,

    tdbByDirExt, tdbByDirExts
    )
where

import qualified Data.ByteString.Char8 as C
import Data.List
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
    | ext' == ext = [n <.> ext' | n <- names]
    | otherwise = dlByExt ext dirlist

-- Search for multiple extensions at once.  'exts' must be sorted, with no
-- duplicates.
dlByExts :: [String] -> DirList -> [String]
dlByExts _ [] = []
dlByExts [] _ = []
dlByExts (ext:exts) ((ext', names):dirlist) =
    case compare ext ext' of
        -- 'ext' isn't in the list.
        LT -> dlByExts exts ((ext', names):dirlist)
        -- 'ext' is right here.
        EQ -> [n <.> ext' | n <- names] ++ dlByExts exts dirlist
        -- 'ext' may be in the remainder.  Nothing else can match here.
        GT -> dlByExts (ext:exts) dirlist

-- Insert a file, given its extension.  Again linear.
dlAdd :: FilePath -> DirList -> DirList
dlAdd file dirList =
    dlAddByExt (takeExtension file) (dropExtension file) dirList

-- Keeps the list sorted by extension
dlAddByExt ::  String -> String -> DirList -> DirList
dlAddByExt ext name [] = [(ext, [name])]
dlAddByExt ext name ((ext', names):dirlist) =
    case compare ext ext' of
        LT -> (ext, [name]):(ext', names):dirlist
        EQ -> (ext', name:names):dirlist
        GT -> (ext', names):(dlAddByExt ext name dirlist)

--
-- A map from directory to contents, excluding subdirectories.
--
type TreeDB = Trie DirList

deriving instance Typeable Trie

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

tdbMerge :: TreeDB -> TreeDB -> TreeDB
tdbMerge = T.unionL

--
-- Combined queries
--

-- Find files by directory and extension
tdbByDirExt :: FilePath -> String -> TreeDB -> Maybe [FilePath]
tdbByDirExt path ext treeDB = do
    dirList <- tdbByDir path treeDB
    let filenames = dlByExt ext dirList
    return [ path </> file | file <- filenames ]

-- Look for multiple extensions.  'exts' need not be sorted.
tdbByDirExts :: FilePath -> [String] -> TreeDB -> Maybe [FilePath]
tdbByDirExts path exts treeDB = do
    dirList <- tdbByDir path treeDB
    let filenames = dlByExts (sort exts) dirList
    return [ path </> file | file <- filenames ]
